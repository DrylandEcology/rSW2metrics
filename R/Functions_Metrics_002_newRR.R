
#------ New metrics ------

get_rh <- function(path, name_sw2_run, id_scen, years) {
  # Extract a variable from outputs as template
  res <- extract_from_sw2(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen = id_scen,
    years = years,
    sw2_tp = "Day",
    sw2_outs = "TEMP",
    sw2_vars = "avg_C",
    varnames_are_fixed = TRUE
  )

  # Provide correct name and initialize
  res[["values"]][[1]][] <- NA
  names(res[["values"]]) <- "rh"

  # Extract RH (from inputs)
  sim_input <- new.env(parent = emptyenv())
  load(
    file = file.path(path, name_sw2_run, "sw_input.RData"),
    envir = sim_input
  )

  # nolint start
  mm <- rSOILWAT2::swCloud_Humidity(
    sim_input[["swRunScenariosData"]][[id_scen]]
  )

  # Interpolate from monthly normals to daily values
  mon <- seq_len(12)
  sp <- splines::periodicSpline(mm ~ mon, period = 12)
  # nolint end

  for (yr in unique(res[["time"]][, "Year"])) {
    ids <- res[["time"]][, "Year"] %in% yr
    doys <- res[["time"]][ids, "Day"]
    res[["values"]][["rh"]][ids] <- predict(
      sp,
      doys * 12 / length(doys)
    )[["y"]]
  }

  res
}


get_vpd <- function(
  path, name_sw2_run,
  id_scen, years, group_by_month, first_month_of_year, ...
) {
  stopifnot(requireNamespace("rSW2data"))

  if (!missing(years)) {
    warning("'years' is not implemented but provided as argument!")
  }

  temp_min <- get_values_from_sw2(
    id_scen = id_scen,
    path, name_sw2_run,
    group_by_month = seq_len(12), #rep(0, 12),
    first_month_of_year = first_month_of_year,
    sw2_tp = "Day",
    sw2_out = "TEMP",
    sw2_var = "min_C",
    varnames_are_fixed = TRUE
  )

  temp_max <- get_values_from_sw2(
    id_scen = id_scen,
    path, name_sw2_run,
    group_by_month = seq_len(12), #rep(0, 12),
    first_month_of_year = first_month_of_year,
    sw2_tp = "Day",
    sw2_out = "TEMP",
    sw2_var = "max_C",
    varnames_are_fixed = TRUE
  )

  rh <- get_rh(path, name_sw2_run, id_scen = id_scen)


  cbind(
    rh[, 1:2],
    rSW2data::vpd(
      Tmin = temp_min[["vals"]][[1]],
      Tmax = temp_max[["vals"]][[1]],
      RHmean = rh[, "rh"]
    )
  )
}



#--- CWD = climatic water deficit [mm] = PET - ET
metric_CWD <- function(
  path, name_sw2_run,
  id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  ...
) {
  out <- match.arg(out)
  stopifnot(check_metric_arguments(out = "ts_years"))

  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    # Daily PET and ET and monthly temperature
    sim_data <- collect_sw2_sim_data(
      path = path,
      name_sw2_run = name_sw2_run,
      id_scen = id_scen_used[k1],
      years = list_years_scen_used[[k1]],
      output_sets = list(
        day = list(
          sw2_tp = "Day",
          sw2_outs = c("PET", "AET"),
          sw2_vars = c(pet = "pet_cm", et = "evapotr_cm"),
          varnames_are_fixed = TRUE
        ),
        mon = list(
          sw2_tp = "Month",
          sw2_outs = "TEMP",
          sw2_vars = c(tmean = "avg_C"),
          varnames_are_fixed = TRUE
        )
      )
    )

    cwd_daily <- list(
      time = sim_data[["day"]][["time"]],
      values = list(
        cwd = 10 * (
          sim_data[["day"]][["values"]][["pet"]] -
          sim_data[["day"]][["values"]][["et"]]
        )
      )
    )

    res[[k1]] <- if (out == "ts_years") {
      t(get_new_yearly_aggregations(
        x_daily = cwd_daily,
        # (Monthly) mean air temperature
        temp_monthly = sim_data[["mon"]],
        fun_time = sum,
        fun_extreme = max,
        output = c(
          "values", "seasonal_variability", "seasonality",
          "extreme_mean010day"
        )
      ))

    } else if (out == "raw") {
      cwd_daily
    }
  }

  res
}


#--- MDD = Soil moisture degree days [C x day] =
# degree days where soil water potential lt or gt limit

# wet based on any(SWC[i] > SWC_limit)
# dry based on all(SWC[i] < SWC_limit)
calc_wetdry <- function(
  sim_swp_daily,
  soils,
  used_depth_range_cm = NULL,
  sm_periods = list(op = `>`, limit = -Inf)
) {

  # Check whether op is `>` or `>=`
  is_op_lt <- do.call(sm_periods[["op"]], list(1, 0))

  if (is.finite(sm_periods[["limit"]])) {
    id_slyrs <- determine_used_soillayers(
      soil_depths_cm = soils[["depth_cm"]],
      used_depth_range_cm = used_depth_range_cm,
      n_slyrs_has = ncol(sim_swp_daily[["values"]][["swp"]])
    )

    # Days that meet soil moisture criterion
    sm <- do.call(
      what = sm_periods[["op"]],
      args = list(
        - 1 / 10 * sim_swp_daily[["values"]][["swp"]][, id_slyrs, drop = FALSE],
        sm_periods[["limit"]]
      )
    )

  } else {
    # Shortcut for infinite soil moisture limits --> no need for actual data
    tmp <- rep(TRUE, nrow(sim_swp_daily[["time"]]))

    sm <- matrix(
      data = if (is_op_lt) {
        if (sm_periods[["limit"]] == Inf) !tmp else tmp
      } else {
        if (sm_periods[["limit"]] == Inf) tmp else !tmp
      },
      ncol = 1
    )
  }


  list(
    time = sim_swp_daily[["time"]],
    values = if (is_op_lt) {
      # wet: op = `>` --> wet(profile) = any(wet[i])
      list(apply(sm, 1, any))
    } else {
      # dry: op = `<` --> dry(profile) = all(dry[i])
      list(apply(sm, 1, all))
    }
  )
}

# v1b: Temp > limit & sm <> limit & snow == 0
# wet based on any(SWC[i] > SWC_limit)
# dry based on all(SWC[i] < SWC_limit)
calc_MDD_daily <- function(
  sim_data,
  soils,
  used_depth_range_cm = NULL,
  t_periods = list(op = `>`, limit = 5),
  sm_periods = list(op = `>`, limit = -Inf),
  snow_periods = list(op = `<=`, limit = 0)
) {

  # Daily wet/dry conditions
  sm <- calc_wetdry(
    sim_swp_daily = sim_data[["swp_daily"]],
    soils = soils,
    used_depth_range_cm = used_depth_range_cm,
    sm_periods = sm_periods
  )


  # Days that meet air temperature criterion
  dg <- do.call(
    what = t_periods[["op"]],
    args = list(
      sim_data[["temp_daily"]][["values"]][["tmean"]],
      t_periods[["limit"]]
    )
  )

  # Days that meet snow criterion
  snw <- do.call(
    what = snow_periods[["op"]],
    args = list(
      sim_data[["swe_daily"]][["values"]][["swe"]],
      snow_periods[["limit"]]
    )
  )

  # Temperature when all criteria are met
  ids <- sm[["values"]][[1]] & dg & snw
  mdd <- rep(0, length(ids))
  mdd[ids] <-
    sim_data[["temp_daily"]][["values"]][["tmean"]][ids] -
    t_periods[["limit"]]

  list(
    time = sim_data[["swp_daily"]][["time"]],
    values = list(mdd = mdd)
  )
}


#--- TDD = Total degree days [C x day] = MDD(op = `>`, limit_MPa = -Inf)
#--- Annual sum of daily TDDat5C
metric_TDDat5C <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  soils, ...
) {
  out <- match.arg(out)

  stopifnot(check_metric_arguments(
    out = "ts_years",
    req_soil_vars = "depth_cm"
  ))

  used_depth_range_cm <- NULL
  Temp_limit_C <- 5
  SWP_limit_MPa <- -Inf

  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    sim_data <- collect_sw2_sim_data(
      path = path,
      name_sw2_run = name_sw2_run,
      id_scen = id_scen_used[k1],
      years = list_years_scen_used[[k1]],
      output_sets = list(
        temp_daily = list(
          sw2_tp = "Day",
          sw2_outs = "TEMP",
          sw2_vars = c(tmean = "avg_C"),
          varnames_are_fixed = TRUE
        ),
        swe_daily = list(
          sw2_tp = "Day",
          sw2_outs = "SNOWPACK",
          sw2_vars = c(swe = "snowpackWaterEquivalent_cm"),
          varnames_are_fixed = TRUE
        )
      )
    )

    # TDD doesn't need SWP, but time is still required
    sim_data <- c(
      sim_data,
      list(
        swp_daily = list(
          time = sim_data[["temp_daily"]][["time"]],
          values = NULL
        )
      )
    )

    tdd_daily <- calc_MDD_daily(
      sim_data = sim_data,
      soils = soils,
      used_depth_range_cm = used_depth_range_cm,
      t_periods = list(op = `>`, limit = Temp_limit_C),
      sm_periods = list(op = `>`, limit = SWP_limit_MPa)
    )

    # Aggregate daily TDD to annual values
    res[[k1]] <- if (out == "ts_years") {
      t(get_new_yearly_aggregations(
        x_daily = tdd_daily,
        fun_time = sum,
        fun_extreme = max,
        periods = list(op = `>`, limit = 0),
        output = c(
          "values", "seasonal_variability",
          "extreme_duration_consecutive_periods_days"
        )
      ))

    } else if (out == "raw") {
      tdd_daily
    }
  }

  res
}


#--- WDD = Wet degree days [C x day] = MDD(op = `>`, limit_MPa = -1.5 MPa)
#--- Annual sum of daily WDD
metric_WDDat5C0to100cm15bar <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  soils, ...
) {
  out <- match.arg(out)

  stopifnot(check_metric_arguments(
    out = "ts_years",
    req_soil_vars = "depth_cm"
  ))

  used_depth_range_cm <- c(0, 100)
  Temp_limit_C <- 5
  SWP_limit_MPa <- -1.5

  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    sim_data <- collect_sw2_sim_data(
      path = path,
      name_sw2_run = name_sw2_run,
      id_scen = id_scen_used[k1],
      years = list_years_scen_used[[k1]],
      output_sets = list(
        swp_daily = list(
          sw2_tp = "Day",
          sw2_outs = "SWPMATRIC",
          sw2_vars = c(swp = "Lyr"),
          varnames_are_fixed = FALSE
        ),
        temp_daily = list(
          sw2_tp = "Day",
          sw2_outs = "TEMP",
          sw2_vars = c(tmean = "avg_C"),
          varnames_are_fixed = TRUE
        ),
        temp_monthly = list(
          sw2_tp = "Month",
          sw2_outs = "TEMP",
          sw2_vars = c(tmean = "avg_C"),
          varnames_are_fixed = TRUE
        ),
        swe_daily = list(
          sw2_tp = "Day",
          sw2_outs = "SNOWPACK",
          sw2_vars = c(swe = "snowpackWaterEquivalent_cm"),
          varnames_are_fixed = TRUE
        )
      )
    )

    wdd_daily <- calc_MDD_daily(
      sim_data = sim_data,
      soils = soils,
      used_depth_range_cm = used_depth_range_cm,
      t_periods = list(op = `>`, limit = Temp_limit_C),
      sm_periods = list(op = `>`, limit = SWP_limit_MPa)
    )

    # Aggregate daily WDD to annual values
    res[[k1]] <- if (out == "ts_years") {
      t(get_new_yearly_aggregations(
        x_daily = wdd_daily,
        temp_monthly = sim_data[["temp_monthly"]],
        fun_time = sum,
        output = c("values", "seasonality")
      ))

    } else if (out == "raw") {
      wdd_daily
    }
  }

  res
}



#--- DDD = Dry degree days [C x day] = MDD(op = `<`, limit_MPa = -3.0 MPa)
calc_DDD_yearly <- function(
  path, name_sw2_run, id_scen_used,
  list_years_scen_used,
  out = c("ts_years", "raw"),
  soils,
  used_depth_range_cm = NULL,
  Temp_limit_C = 5,
  SWP_limit_MPa = -3,
  output = c("values", "extreme_value_consecutive_periods"),
  ...
) {
  out <- match.arg(out)
  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    sim_data <- collect_sw2_sim_data(
      path = path,
      name_sw2_run = name_sw2_run,
      id_scen = id_scen_used[k1],
      years = list_years_scen_used[[k1]],
      output_sets = list(
        swp_daily = list(
          sw2_tp = "Day",
          sw2_outs = "SWPMATRIC",
          sw2_vars = c(swp = "Lyr"),
          varnames_are_fixed = FALSE
        ),
        temp_daily = list(
          sw2_tp = "Day",
          sw2_outs = "TEMP",
          sw2_vars = c(tmean = "avg_C"),
          varnames_are_fixed = TRUE
        ),
        swe_daily = list(
          sw2_tp = "Day",
          sw2_outs = "SNOWPACK",
          sw2_vars = c(swe = "snowpackWaterEquivalent_cm"),
          varnames_are_fixed = TRUE
        )
      )
    )

    ddd_daily <- calc_MDD_daily(
      sim_data = sim_data,
      soils = soils,
      used_depth_range_cm = used_depth_range_cm,
      t_periods = list(op = `>`, limit = Temp_limit_C),
      sm_periods = list(op = `<`, limit = SWP_limit_MPa)
    )

    res[[k1]] <- if (out == "ts_years") {
      t(get_new_yearly_aggregations(
        x_daily = ddd_daily,
        fun_time = sum,
        fun_extreme = max,
        periods = list(op = `>`, limit = 0),
        output = output
      ))

    } else if (out == "raw") {
      ddd_daily
    }
  }

  res
}

metric_DDDat5C0to030cm30bar <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  soils, ...
) {
  out <- match.arg(out)

  stopifnot(check_metric_arguments(
    out = "ts_years",
    req_soil_vars = "depth_cm"
  ))

  calc_DDD_yearly(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    out = out,
    soils = soils,
    Temp_limit_C = 5,
    SWP_limit_MPa = -3,
    used_depth_range_cm = c(0, 30),
    output = "extreme_value_consecutive_periods",
    ...
  )
}

metric_DDDat5C0to100cm30bar <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  soils, ...
) {
  out <- match.arg(out)

  stopifnot(check_metric_arguments(
    out = "ts_years",
    req_soil_vars = "depth_cm"
  ))

  calc_DDD_yearly(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    out = out,
    soils = soils,
    Temp_limit_C = 5,
    SWP_limit_MPa = -3,
    used_depth_range_cm = c(0, 100),
    output = c("values", "extreme_value_consecutive_periods"),
    ...
  )
}


#' Calculate soil water availability \var{SWA}
#'
#' We define available soil water `SWA` as the amount of soil water `SWC`
#' that exceeds some base (= critical) amount of soil water `SWC_crit[i]`,
#' i.e.,
#' \deqn{SWA[t,i] = max{0, SWC[t,i] - SWC_crit[i]}}
#' for day t and soil layer i.
#'
#' @section Details:
#' The base (= critical) amount of soil water `SWC_crit[i]` is specified via a
#' critical soil water potential `SWP_crit`, here \code{SWP_limit_MPa}, e.g.,
#' `SWP_crit = -3.0 MPa`.
#' A pedotransfer function of the water release curve is used to
#' translate `SWP` into volumetric water content `VWC`, i.e.,
#' \deqn{VWC_crit[i,matric] = f(SWP_crit, soil texture[i])}
#' where soil texture is the weight fractions of sand, clay, and silt of
#' the matric soil component.
#' However, the translation is only valid for the matric soil, i.e.,
#' the component without coarse fragments.
#'
#' `SWA` in the presence of coarse fragments is calculated as
#'
# nolint start
#' \deqn{SWA[t,i] = max{0, SWC[t,i] - w[i] * (1 - cfrag[i]) * VWC_crit[i,matric]}}
#' or equivalently
#' \deqn{SWA[t,i] = max{0, w[i] * (1 - cfrag[i]) * (VWC[t,i,matric] - VWC_crit[i,matric])}}
# nolint end
#'
#' where matric volumetric water `VWC` is multiplied by `w[i] * (1 - cfrag[i])`
#' to calculate the amount of water in soil layer i,
#' correcting for the volume occupied by coarse fragments.
#'
#' For day t, soil layer i, soil layer width `w[i]`, and
#' `cfrag[i]` = fraction of coarse fragments.
#'
#' @param sim_swc_daily A named list with a "time" element and a
#'    "values" element containing daily \var{"swc"} for each soil layer
#'    in units of centimeters.
#' @param soils A named list with soil parameters \var{"depth_cm"},
#'   \var{"sand_frac"},\var{"clay_frac"}, and \var{"gravel_content"}
#'   as numeric vectors with values for each soil layer.
#' @param used_depth_range_cm A numeric vector of length two.
#' @param SWP_limit_MPa A numeric value.
#'
#' @return A list with elements "time" and "values" where values represent
#'   available soil water in units of millimeters above \code{SWP_limit_MPa}
#'   summed across \code{used_depth_range_cm}.
#'
#' @md
calc_SWA_mm <- function(
  sim_swc_daily,
  soils,
  used_depth_range_cm = NULL,
  SWP_limit_MPa = -Inf
) {
  widths_cm <- calc_soillayer_weights(
    soil_depths_cm = soils[["depth_cm"]],
    used_depth_range_cm = used_depth_range_cm,
    n_slyrs_has = ncol(sim_swc_daily[["values"]][["swc"]])
  )

  id_slyrs <- which(!is.na(widths_cm))

  if (length(id_slyrs) > 0) {
    widths_cm <- widths_cm[id_slyrs]

    # Calculate SWC threshold (corrected for coarse fragments)
    # SWC <-> VWC exists only for the matric component
    base_SWC_mm <- if (is.finite(SWP_limit_MPa)) {
      rSOILWAT2::SWPtoVWC(
        swp = SWP_limit_MPa,
        sand = soils[["sand_frac"]][id_slyrs],
        clay = soils[["clay_frac"]][id_slyrs]
      ) * 10 * widths_cm * (1 - soils[["gravel_content"]][id_slyrs])

    } else {
      rep(0, length(id_slyrs))
    }

    # Determine SWA [mm] for each soil layer as SWC - SWC_base
    swa_by_layer <- sweep(
      x = 10 * sim_swc_daily[["values"]][["swc"]][, id_slyrs, drop = FALSE],
      MARGIN = 2,
      STATS = base_SWC_mm,
      FUN = "-"
    )
    swa_by_layer[swa_by_layer < 0] <- 0

    # Sum SWA across soil profile
    swa_daily_values <- apply(
      X = swa_by_layer,
      MARGIN = 1,
      FUN = sum
    )

  } else {
    # No soil layers in the depth range
    swa_daily_values <- rep(NA, nrow(sim_swc_daily[["time"]]))
  }

  list(
    time = sim_swc_daily[["time"]],
    values = list(swa_daily_values)
  )
}



# soils must have "depth_cm", "sand_frac", "clay_frac", and "gravel_content"
get_SWA <- function(
  path, name_sw2_run, id_scen_used,
  list_years_scen_used,
  out = c("ts_years", "raw"),
  soils,
  used_depth_range_cm = NULL,
  SWP_limit_MPa = -Inf,
  ...
) {
  out <- match.arg(out)
  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    #--- Soil moisture
    sim_data <- collect_sw2_sim_data(
      path = path,
      name_sw2_run = name_sw2_run,
      id_scen = id_scen_used[k1],
      years = list_years_scen_used[[k1]],
      output_sets = list(
        swc_daily = list(
          sw2_tp = "Day",
          sw2_outs = "SWCBULK",
          sw2_vars = c(swc = "Lyr"),
          varnames_are_fixed = FALSE
        ),
        mon = list(
          sw2_tp = "Month",
          sw2_outs = "TEMP",
          sw2_vars = c(tmean = "avg_C"),
          varnames_are_fixed = TRUE
        )
      )
    )

    swa_daily <- calc_SWA_mm(
      sim_swc_daily = sim_data[["swc_daily"]],
      soils = soils,
      SWP_limit_MPa = SWP_limit_MPa,
      used_depth_range_cm = used_depth_range_cm
    )

    res[[k1]] <- if (out == "ts_years") {
      t(get_new_yearly_aggregations(
        x_daily = swa_daily,
        temp_monthly = sim_data[["mon"]],
        fun_time = mean,
        output = c("values", "seasonal_variability", "seasonality")
      ))

    } else if (out == "raw") {
      swa_daily
    }
  }

  res
}


metric_SWAat0to100cm30bar <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  soils, ...
) {
  out <- match.arg(out)

  stopifnot(check_metric_arguments(
    out = "ts_years",
    req_soil_vars = c("depth_cm", "sand_frac", "clay_frac", "gravel_content")
  ))

  get_SWA(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    out = out,
    soils = soils,
    SWP_limit_MPa = -3,
    used_depth_range_cm = c(0, 100),
    ...
  )
}

metric_SWAat0to100cm39bar <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  soils, ...
) {
  out <- match.arg(out)

  stopifnot(check_metric_arguments(
    out = "ts_years",
    req_soil_vars = c("depth_cm", "sand_frac", "clay_frac", "gravel_content")
  ))

  get_SWA(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    out = out,
    soils = soils,
    SWP_limit_MPa = -3.9,
    used_depth_range_cm = c(0, 100),
    ...
  )
}


#--- DSI = Duration of dry soil intervals at -1.5 and -3.0 SWP thresholds

# dry based on MDD
calc_DSI <- function(
  path, name_sw2_run, id_scen_used,
  list_years_scen_used,
  out = c("ts_years", "raw"),
  soils,
  used_depth_range_cm = NULL,
  SWP_limit_MPa = -Inf,
  fun_periods = function(x) c(max = max(x), mean = mean(x), N = length(x)),
  include_year = FALSE,
  ...
) {
  out <- match.arg(out)
  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    sim_data <- collect_sw2_sim_data(
      path = path,
      name_sw2_run = name_sw2_run,
      id_scen = id_scen_used[k1],
      years = list_years_scen_used[[k1]],
      output_sets = list(
        swp_daily = list(
          sw2_tp = "Day",
          sw2_outs = "SWPMATRIC",
          sw2_vars = c(swp = "Lyr"),
          varnames_are_fixed = FALSE
        )
      )
    )

    # Daily wet/dry conditions
    dry_daily <- calc_wetdry(
      sim_swp_daily = sim_data[["swp_daily"]],
      soils = soils,
      used_depth_range_cm = used_depth_range_cm,
      sm_periods = list(op = `<`, limit = SWP_limit_MPa)
    )

    tmp_dsi <- tapply(
      X = dry_daily[["values"]][[1]],
      INDEX = dry_daily[["time"]][, "Year"],
      FUN = function(x) {
        tmp <- rle(x)
        if (any(tmp[["values"]] == 1)) {
          tmp[["lengths"]][tmp[["values"]]]
        } else {
          0
        }
      }
    )

    if (out == "ts_years") {

      tmp <- lapply(tmp_dsi, FUN = fun_periods)

      tmp2 <- array(
        unlist(tmp),
        dim = c(length(tmp[[1]]), length(tmp)),
        dimnames = list(names(tmp[[1]]), NULL)
      )

      res[[k1]] <- if (include_year) {
        rbind(
          Year = as.integer(names(tmp)),
          tmp2
        )
      } else {
        tmp2
      }

    } else if (out == "raw") {
      res[[k1]] <- tmp_dsi
    }
  }

  res
}

metric_DSIat0to100cm15bar <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  soils, ...
) {
  out <- match.arg(out)

  stopifnot(check_metric_arguments(
    out = "ts_years",
    req_soil_vars = "depth_cm"
  ))

  calc_DSI(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    out = out,
    soils = soils,
    SWP_limit_MPa = -1.5,
    used_depth_range_cm = c(0, 100),
    fun_periods = function(x) c(mean = mean(x), N = length(x))
  )
}

metric_DSIat0to100cm30bar <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  soils, ...
) {
  out <- match.arg(out)

  stopifnot(check_metric_arguments(
    out = "ts_years",
    req_soil_vars = "depth_cm"
  ))

  calc_DSI(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    out = out,
    soils = soils,
    SWP_limit_MPa = -3.0,
    used_depth_range_cm = c(0, 100),
    fun_periods = function(x) c(max = max(x))
  )
}


#--- Frost: Consistency (across years) of last and first frost events
# (StDev of DOY)
# For the last event before mid-year and first event after mid-year where
# mid-year = July 15 (circa summer solstice + 1 month)
get_frost_doy <- function(
  sim_tmin_daily,
  Temp_limit_C = -5,
  hemisphere_NS = c("N", "S"),
  include_year = FALSE,
  ...
) {
  hemisphere_NS <- match.arg(hemisphere_NS)
  stopifnot(hemisphere_NS == "N") #TODO: implement for southern hemisphere

  is_frost <- sim_tmin_daily[["values"]][["tmin"]] < Temp_limit_C

  # Mid-year: summer solstice + 1 month
  # North: June solstice (Jun 20-22 = 171-173)
  # South: December solstice (Dec 20-23 = 354-357)
  doy_mid <- 196 # July 15 (in non-leap year)

  res <- as.matrix(aggregate(
    x = is_frost,
    by = list(Year = sim_tmin_daily[["time"]][, "Year"]),
    function(x) {
      tmp <- which(x)
      c(
        last = max(tmp[tmp <= doy_mid]),
        first = min(tmp[tmp > doy_mid])
      )
    }
  ))
  colnames(res) <- c("Year", "LastFrost", "FirstFrost")

  if (include_year) res else res[, -1]
}


metric_FrostDaysAtNeg5C <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  ...
) {
  out <- match.arg(out)

  stopifnot(check_metric_arguments(out = "ts_years"))

  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    #--- (Daily) minimum air temperature
    sim_data <- collect_sw2_sim_data(
      path = path,
      name_sw2_run = name_sw2_run,
      id_scen = id_scen_used[k1],
      years = list_years_scen_used[[k1]],
      output_sets = list(
        day = list(
          sw2_tp = "Day",
          sw2_outs = "TEMP",
          sw2_vars = c(tmin = "min_C"),
          varnames_are_fixed = TRUE
        )
      )
    )

    res[[k1]] <- t(get_frost_doy(
      sim_tmin_daily = sim_data[["day"]],
      Temp_limit_C = -5
    ))
  }

  res
}

# CorTempPPT: Correlation between monthly temperature and
# precipitation by year
metric_CorTempPPT <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  include_year = FALSE,
  ...
) {
  out <- match.arg(out)
  stopifnot(check_metric_arguments(out = "ts_years"))

  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    sim_data <- collect_sw2_sim_data(
      path = path,
      name_sw2_run = name_sw2_run,
      id_scen = id_scen_used[k1],
      years = list_years_scen_used[[k1]],
      output_sets = list(
        mon = list(
          sw2_tp = "Month",
          sw2_outs = c("TEMP", "PRECIP"),
          sw2_vars = c(tmin = "min_C", "ppt"),
          varnames_are_fixed = TRUE
        )
      )
    )

    tmp <- as.vector(by(
      data = cbind(
        sim_data[["mon"]][["values"]][["tmin"]],
        sim_data[["mon"]][["values"]][["ppt"]]
      ),
      INDICES = sim_data[["mon"]][["time"]][, "Year"],
      FUN = function(x) cor(x[, 1], x[, 2])
    ))

    res[[k1]] <- if (include_year) {
      rbind(
        Year = unique(sim_data[["mon"]][["time"]][, "Year"]),
        seasonality = tmp
      )
    } else {
      rbind(seasonality = tmp)
    }
  }

  res
}



get_SW2flux <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  sw2_out, sw2_var,
  transform = function(x) x,
  out_labels,
  include_year = FALSE,
  timestep = c("yearly", "monthly"),
  ...
) {
  timestep <- match.arg(timestep)

  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    sim_data <- collect_sw2_sim_data(
      path = path,
      name_sw2_run = name_sw2_run,
      id_scen = id_scen_used[k1],
      years = list_years_scen_used[[k1]],
      output_sets = list(
        val = list(
          sw2_tp = switch(timestep, yearly = "Year", monthly = "Month"),
          sw2_outs = sw2_out,
          sw2_vars = sw2_var,
          varnames_are_fixed = TRUE
        )
      )
    )

    foo <- switch(
      EXPR = timestep,
      yearly = format_yearly_to_matrix,
      monthly = format_monthly_to_matrix
    )

    ts_years <- unique(sim_data[["val"]][["time"]][, "Year"])

    res[[k1]] <- foo(
      x = lapply(sim_data[["val"]][["values"]], transform),
      years = ts_years,
      out_labels = out_labels
    )


    if (include_year) {
      res[[k1]] <- rbind(
        Year = ts_years,
        res[[k1]]
      )
    }
  }

  res
}

#--- Annual and monthly fluxes:
#' Evapotranspiration (ET) [mm]
#' @noRd
metric_ET_annual <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  ...
) {
  out <- match.arg(out)
  stopifnot(check_metric_arguments(out = "ts_years"))

  get_SW2flux(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    sw2_out = "AET",
    sw2_var = "evapotr_cm",
    transform = function(x) 10 * x,
    out_labels = "ET_mm",
    timestep = "yearly"
  )
}

metric_ET_monthly <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  ...
) {
  out <- match.arg(out)
  stopifnot(check_metric_arguments(out = "ts_years"))

  get_SW2flux(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    sw2_out = "AET",
    sw2_var = "evapotr_cm",
    transform = function(x) 10 * x,
    out_labels = "ET_mm",
    timestep = "monthly"
  )
}


#' Diffuse Recharge (DR) = Deep Drainage [mm]
#' @noRd
metric_DR_annual <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  ...
) {
  out <- match.arg(out)
  stopifnot(check_metric_arguments(out = "ts_years"))

  get_SW2flux(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    sw2_out = "DEEPSWC",
    sw2_var = "lowLayerDrain_cm",
    transform = function(x) 10 * x,
    out_labels = "DeepDrainage_mm",
    timestep = "yearly"
  )
}


metric_DR_monthly <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  ...
) {
  out <- match.arg(out)
  stopifnot(check_metric_arguments(out = "ts_years"))

  get_SW2flux(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    sw2_out = "DEEPSWC",
    sw2_var = "lowLayerDrain_cm",
    transform = function(x) 10 * x,
    out_labels = "DeepDrainage_mm",
    timestep = "monthly"
  )
}

#' Annual radiation (H) [MJ/m2]
#' @noRd
metric_Radiation_annual <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  ...
) {
  out <- match.arg(out)
  stopifnot(check_metric_arguments(out = "ts_years"))

  get_SW2flux(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    sw2_out = "PET",
    sw2_var = c("H_oh_MJm-2", "H_gt_MJm-2"),
    out_labels = c("H_oh_MJm-2", "H_gt_MJm-2"),
    timestep = "yearly"
  )
}

#' Monthly radiation (H) [MJ/m2]
#' @noRd
metric_Radiation_monthly <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  ...
) {
  out <- match.arg(out)
  stopifnot(check_metric_arguments(out = "ts_years"))

  get_SW2flux(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    sw2_out = "PET",
    sw2_var = c("H_oh_MJm-2", "H_gt_MJm-2"),
    out_labels = c("H_oh_MJm-2", "H_gt_MJm-2"),
    timestep = "monthly"
  )
}



metric_Climate_annual <- function(
  path, name_sw2_run,
  id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  include_year = FALSE,
  ...
) {
  out <- match.arg(out)
  stopifnot(check_metric_arguments(out = "ts_years"))

  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    sim_data <- collect_sw2_sim_data(
      path = path,
      name_sw2_run = name_sw2_run,
      id_scen = id_scen_used[k1],
      years = list_years_scen_used[[k1]],
      output_sets = list(
        day = list(
          sw2_tp = "Day",
          sw2_outs = c("TEMP", "TEMP", "TEMP"),
          sw2_vars = c(tmax = "max_C", tmin = "min_C", tmean = "avg_C"),
          varnames_are_fixed = TRUE
        ),
        mon = list(
          sw2_tp = "Month",
          sw2_outs = c("PRECIP", "TEMP"),
          sw2_vars = c(ppt = "ppt", tmean = "avg_C"),
          varnames_are_fixed = TRUE
        ),
        yr = list(
          sw2_tp = "Year",
          sw2_outs = c(
            "PRECIP", "PRECIP", "PRECIP",
            "TEMP", "TEMP", "TEMP",
            "SNOWPACK",
            "PET", "AET"
          ),
          sw2_vars = c(
            "ppt", "rain", "snow_fall",
            tmax = "max_C", tmin = "min_C", tmean = "avg_C",
            swe = "snowpackWaterEquivalent_cm",
            pet = "pet_cm", et = "evapotr_cm"
          ),
          varnames_are_fixed = TRUE
        )
      )
    )


    # Helper variables
    ts_years <- sim_data[["yr"]][["time"]][, "Year"]

    PPTinJAS <- tapply(
      X = sim_data[["mon"]][["values"]][["ppt"]],
      INDEX = sim_data[["mon"]][["time"]][, "Year"],
      FUN = function(x) sum(x[7:9])
    )


    # Output
    res[[k1]] <- rbind(
      # Potential evapotranspiration
      format_yearly_to_matrix(
        x = 10 * sim_data[["yr"]][["values"]][["pet"]],
        years = ts_years,
        out_labels = "PET_mm"
      ),

      # Climatic water deficit
      format_yearly_to_matrix(
        x = 10 * (
          sim_data[["yr"]][["values"]][["pet"]] -
          sim_data[["yr"]][["values"]][["et"]]
        ),
        years = ts_years,
        out_labels = "CWD_mm"
      ),

      # Maximum temperature (mean across year)
      format_yearly_to_matrix(
        x = sim_data[["yr"]][["values"]][["tmax"]],
        years = ts_years,
        out_labels = "Tmax_mean_C"
      ),

      # Mean temperature
      format_yearly_to_matrix(
        x = sim_data[["yr"]][["values"]][["tmean"]],
        years = ts_years,
        out_labels = "Tmean_mean_C"
      ),

      # Minimum temperature (mean across year)
      format_yearly_to_matrix(
        x = sim_data[["yr"]][["values"]][["tmin"]],
        years = ts_years,
        out_labels = "Tmin_mean_C"
      ),

      # Temperature range (difference between tmax and tmin)
      Trange_diurnal_C = tapply(
        X =
          sim_data[["day"]][["values"]][["tmax"]] -
          sim_data[["day"]][["values"]][["tmin"]],
        INDEX = sim_data[["day"]][["time"]][, "Year"],
        FUN = mean
      ),
      format_yearly_to_matrix(
        x = tapply(
          X =
            sim_data[["day"]][["values"]][["tmax"]] -
            sim_data[["day"]][["values"]][["tmin"]],
          INDEX = sim_data[["day"]][["time"]][, "Year"],
          FUN = mean
        ),
        years = ts_years,
        out_labels = "Trange_diurnal_C"
      ),

      # SD of mean temperature
      format_yearly_to_matrix(
        x = tapply(
          X = sim_data[["day"]][["values"]][["tmean"]],
          INDEX = sim_data[["day"]][["time"]][, "Year"],
          FUN = sd
        ),
        years = ts_years,
        out_labels = "Tmean_C_SD"
      ),

      # Temperature of hottest month
      format_yearly_to_matrix(
        x = tapply(
          X = sim_data[["mon"]][["values"]][["tmean"]],
          INDEX = sim_data[["mon"]][["time"]][, "Year"],
          FUN = max
        ),
        years = ts_years,
        out_labels = "Tmean_hottestmonth_C"
      ),

      # Temperature of coldest month
      format_yearly_to_matrix(
        x = tapply(
          X = sim_data[["mon"]][["values"]][["tmean"]],
          INDEX = sim_data[["mon"]][["time"]][, "Year"],
          FUN = min
        ),
        years = ts_years,
        out_labels = "Tmean_coldestmonth_C"
      ),

      # Precipitation
      format_yearly_to_matrix(
        x = 10 * sim_data[["yr"]][["values"]][["ppt"]],
        years = ts_years,
        out_labels = "PPT_mm"
      ),

      format_yearly_to_matrix(
        x = 10 * sim_data[["yr"]][["values"]][["rain"]],
        years = ts_years,
        out_labels = "Rain_mm"
      ),

      format_yearly_to_matrix(
        x = 10 * sim_data[["yr"]][["values"]][["snow_fall"]],
        years = ts_years,
        out_labels = "Snowfall_mm"
      ),

      format_yearly_to_matrix(
        x = 10 * sim_data[["yr"]][["values"]][["swe"]],
        years = ts_years,
        out_labels = "Snowpack_SWE_mm"
      ),

      # Ratio of rain to all precipitation
      format_yearly_to_matrix(
        x =
          sim_data[["yr"]][["values"]][["rain"]] /
          sim_data[["yr"]][["values"]][["ppt"]],
        years = ts_years,
        out_labels = "Rain_to_PPT"
      ),

      # Ratio of July, August, and September precipitation to annual
      format_yearly_to_matrix(
        x = PPTinJAS / sim_data[["yr"]][["values"]][["ppt"]],
        years = ts_years,
        out_labels = "PPTinJAS_to_PPT"
      ),

      # Sum of precipitation in months of July, August, and September
      format_yearly_to_matrix(
        x = 10 * PPTinJAS,
        years = ts_years,
        out_labels = "PPTinJAS_mm"
      ),

      # Precipitation of the wettest month
      format_yearly_to_matrix(
        x = 10 * tapply(
          X = sim_data[["mon"]][["values"]][["ppt"]],
          INDEX = sim_data[["mon"]][["time"]][, "Year"],
          FUN = max
        ),
        years = ts_years,
        out_labels = "PPT_wettestmonth_mm"
      ),


      # Precipitation of the driest month
      format_yearly_to_matrix(
        x = 10 * tapply(
          X = sim_data[["mon"]][["values"]][["ppt"]],
          INDEX = sim_data[["mon"]][["time"]][, "Year"],
          FUN = min
        ),
        years = ts_years,
        out_labels = "PPT_driestmonth_mm"
      )
    )


    if (include_year) {
      res[[k1]] <- rbind(
        Year = ts_years,
        res[[k1]]
      )
    }
  }

  res
}


metric_Climate_monthly <- function(
  path, name_sw2_run,
  id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  include_year = FALSE,
  ...
) {
  out <- match.arg(out)
  stopifnot(check_metric_arguments(out = "ts_years"))

  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    sim_data <- collect_sw2_sim_data(
      path = path,
      name_sw2_run = name_sw2_run,
      id_scen = id_scen_used[k1],
      years = list_years_scen_used[[k1]],
      output_sets = list(
        day = list(
          sw2_tp = "Day",
          sw2_outs = c("TEMP", "TEMP", "TEMP"),
          sw2_vars = c(tmax = "max_C", tmin = "min_C", tmean = "avg_C"),
          varnames_are_fixed = TRUE
        ),
        mon = list(
          sw2_tp = "Month",
          sw2_outs = c(
            "PRECIP", "PRECIP", "PRECIP", "PRECIP",
            "TEMP", "TEMP", "TEMP",
            "SNOWPACK",
            "PET", "AET"
          ),
          sw2_vars = c(
            "ppt", "rain", "snow_fall", "snowmelt",
            tmax = "max_C", tmin = "min_C", tmean = "avg_C",
            swe = "snowpackWaterEquivalent_cm",
            pet = "pet_cm", et = "evapotr_cm"
          ),
          varnames_are_fixed = TRUE
        )
      )
    )


    # Helper variables
    ts_years <- unique(sim_data[["mon"]][["time"]][, "Year"])


    # Output
    res[[k1]] <- rbind(
      # Potential evapotranspiration
      format_monthly_to_matrix(
        x = 10 * sim_data[["mon"]][["values"]][["pet"]],
        years = ts_years,
        out_labels = "PET_mm"
      ),

      # Climatic water deficit
      format_monthly_to_matrix(
        x = 10 * (
          sim_data[["mon"]][["values"]][["pet"]] -
            sim_data[["mon"]][["values"]][["et"]]
        ),
        years = ts_years,
        out_labels = "CWD_mm"
      ),

      # Maximum temperature (mean across year)
      format_monthly_to_matrix(
        x = sim_data[["mon"]][["values"]][["tmax"]],
        years = ts_years,
        out_labels = "Tmax_mean_C"
      ),

      # Mean temperature
      format_monthly_to_matrix(
        x = sim_data[["mon"]][["values"]][["tmean"]],
        years = ts_years,
        out_labels = "Tmean_mean_C"
      ),

      # Minimum temperature (mean across year)
      format_monthly_to_matrix(
        x = sim_data[["mon"]][["values"]][["tmin"]],
        years = ts_years,
        out_labels = "Tmin_mean_C"
      ),

      # Temperature range (difference between tmax and tmin)
      format_monthly_to_matrix(
        x = t(tapply(
          X =
            sim_data[["day"]][["values"]][["tmax"]] -
            sim_data[["day"]][["values"]][["tmin"]],
          INDEX = list(
            sim_data[["day"]][["time"]][, "Year"],
            sim_data[["day"]][["time"]][, "Month"]
          ),
          FUN = mean
        )),
        years = ts_years,
        out_labels = "Trange_diurnal_C"
      ),

      # SD of mean temperature
      format_monthly_to_matrix(
        x = t(tapply(
          X = sim_data[["day"]][["values"]][["tmean"]],
          INDEX = list(
            sim_data[["day"]][["time"]][, "Year"],
            sim_data[["day"]][["time"]][, "Month"]
          ),
          FUN = sd
        )),
        years = ts_years,
        out_labels = "Tmean_C_SD"
      ),

      # Precipitation
      format_monthly_to_matrix(
        x = 10 * sim_data[["mon"]][["values"]][["ppt"]],
        years = ts_years,
        out_labels = "PPT_mm"
      ),

      format_monthly_to_matrix(
        x = 10 * sim_data[["mon"]][["values"]][["rain"]],
        years = ts_years,
        out_labels = "Rain_mm"
      ),

      format_monthly_to_matrix(
        x = 10 * sim_data[["mon"]][["values"]][["snow_fall"]],
        years = ts_years,
        out_labels = "Snowfall_mm"
      ),

      format_monthly_to_matrix(
        x = 10 * sim_data[["mon"]][["values"]][["swe"]],
        years = ts_years,
        out_labels = "Snowpack_SWE_mm"
      ),

      # Monthly snowmelt
      format_monthly_to_matrix(
        x = 10 * sim_data[["mon"]][["values"]][["snowmelt"]],
        years = ts_years,
        out_labels = "Snowmelt_mm"
      )
    )


    if (include_year) {
      res[[k1]] <- rbind(
        Year = ts_years,
        res[[k1]]
      )
    }
  }

  res
}


calc_Tmean_monthly <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = c("ts_years", "across_years"),
  ...
) {
  res <- list()

  if (out == "ts_years" && !isTRUE(is.list(list_years_scen_used[[1]]))) {
    # Code below expects lists of lists of year vectors
    # as provided by default if out is "across_years" --> convert
    list_years_scen_used <- lapply(list_years_scen_used, function(x) list(x))
  }

  for (k1 in seq_along(id_scen_used)) {
    tmp <- lapply(
      list_years_scen_used[[k1]],
      function(yrs) {
        sim_data <- collect_sw2_sim_data(
          path = path,
          name_sw2_run = name_sw2_run,
          id_scen = id_scen_used[k1],
          years = yrs,
          output_sets = list(
            mon = list(
              sw2_tp = "Month",
              sw2_outs = "TEMP",
              sw2_vars = c(tmean = "avg_C"),
              varnames_are_fixed = TRUE
            )
          )
        )

        if (out == "ts_years") {
          format_monthly_to_matrix(
            x = sim_data[["mon"]][["values"]][["tmean"]],
            years = unique(sim_data[["mon"]][["time"]][, "Year"]),
            out_labels = "Tmean_C"
          )
        } else if (out == "across_years") {
          format_monthly_to_matrix(
            x = tapply(
              X = sim_data[["mon"]][["values"]][["tmean"]],
              INDEX = sim_data[["mon"]][["time"]][, "Month"],
              FUN = mean
            ),
            years = NA,
            out_labels = "Tmean_C"
          )
        }
      }
    )

    res[[k1]] <- do.call(cbind, tmp)
  }

  res
}


metric_Tmean_monthly <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  calc_Tmean_monthly(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    out = out,
    ...
  )
}

metric_Tmean_monthly_clim <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "across_years",
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  calc_Tmean_monthly(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    out = out,
    ...
  )
}



#--- Soil moisture regimes / soil temperature regimes
metric_SMTRs <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "across_years",
  ...
) {
  stopifnot(requireNamespace("rSW2funs", quietly = TRUE))
  stopifnot(check_metric_arguments(out = match.arg(out)))

  res <- list()

  sim_input <- new.env(parent = emptyenv())
  load(
    file = file.path(path, name_sw2_run, "sw_input.RData"),
    envir = sim_input
  )

  for (k1 in seq_along(id_scen_used)) {
    sim_data <- new.env(parent = emptyenv())
    load(
      file = file.path(
        path,
        name_sw2_run,
        paste0("sw_output_sc", id_scen_used[k1], ".RData")
      ),
      envir = sim_data
    )

    tmp <- lapply(
      list_years_scen_used[[k1]],
      function(yrs) {
        # nolint start
        rSOILWAT2::swYears_EndYear(sim_input[["swRunScenariosData"]][[k1]]) <- 9999
        rSOILWAT2::swYears_StartYear(sim_input[["swRunScenariosData"]][[k1]]) <-
          yrs[1]
        rSOILWAT2::swYears_EndYear(sim_input[["swRunScenariosData"]][[k1]]) <-
          yrs[length(yrs)]
        # nolint end

        tmp <- rSW2funs::calc_SMTRs(
          sim_in = sim_input[["swRunScenariosData"]][[k1]],
          sim_out = sim_data[["runDataSC"]]
        )

        cbind(tmp[["STR"]], tmp[["SMR"]])
      }
    )

    res[[k1]] <- t(do.call(rbind, tmp))
  }

  res
}



#--- AI = aridity index [mm/mm] = PPT/PET
metric_AI <- function(
  path, name_sw2_run,
  id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  ...
) {
  out <- match.arg(out)
  stopifnot(check_metric_arguments(out = "ts_years"))

  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    # Annual PET and PPT
    sim_data <- collect_sw2_sim_data(
      path = path,
      name_sw2_run = name_sw2_run,
      id_scen = id_scen_used[k1],
      years = list_years_scen_used[[k1]],
      output_sets = list(
        yr = list(
          sw2_tp = "Year",
          sw2_outs = c("PET", "PRECIP"),
          sw2_vars = c(pet = "pet_cm", ppt = "ppt"),
          varnames_are_fixed = TRUE
        )
      )
    )

    res[[k1]] <- matrix(
      data =
        sim_data[["yr"]][["values"]][["ppt"]] /
        sim_data[["yr"]][["values"]][["pet"]],
      nrow = 1,
      dimnames = list("AI", NULL)
    )
  }

  res
}
