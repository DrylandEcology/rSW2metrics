
#------ New metrics ------

# Type of functions
#  * `metric_*()` load simulations and calculate a specific response
#  * `get_*()` functions load simulations and calculate a response
#  * `calc_*()` functions calculate a response


get_rh <- function(path, name_sw2_run, id_scen, years, zipped_runs = FALSE) {
  # Extract a variable from outputs as template
  res <- extract_from_sw2(
    path = path,
    name_sw2_run = name_sw2_run,
    zipped_runs = zipped_runs,
    id_scen = id_scen,
    years = years,
    sw2_tp = "Day",
    sw2_outs = "TEMP",
    sw2_vars = "avg_C",
    varnames_are_fixed = TRUE
  )

  # Provide correct name and initialize
  res[["values"]][[1]][] <- NA # nolint: extraction_operator_linter.
  names(res[["values"]]) <- "rh"

  # Extract RH (from inputs)
  sim_input <- load_sw2_rda(
    path = file.path(path, name_sw2_run),
    fname = "sw_input.RData",
    zipped_runs = zipped_runs
  )

  # nolint start: object_usage_linter.
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
  id_scen, years, group_by_month, first_month_of_year,
  zipped_runs = FALSE,
  ...
) {
  stopifnot(requireNamespace("rSW2data"))

  if (!missing(years)) {
    warning("'years' is not implemented but provided as argument!")
  }

  temp_min <- get_values_from_sw2(
    id_scen = id_scen,
    path, name_sw2_run,
    zipped_runs = zipped_runs,
    group_by_month = seq_len(12),
    first_month_of_year = first_month_of_year,
    sw2_tp = "Day",
    sw2_out = "TEMP",
    sw2_var = "min_C",
    varnames_are_fixed = TRUE
  )

  temp_max <- get_values_from_sw2(
    id_scen = id_scen,
    path, name_sw2_run,
    zipped_runs = zipped_runs,
    group_by_month = seq_len(12),
    first_month_of_year = first_month_of_year,
    sw2_tp = "Day",
    sw2_out = "TEMP",
    sw2_var = "max_C",
    varnames_are_fixed = TRUE
  )

  rh <- get_rh(path, name_sw2_run, id_scen = id_scen, zipped_runs = zipped_runs)


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
calc_CWD_mm <- function(pet_cm, et_cm) {
  10 * (pet_cm - et_cm)
}

metric_CWD <- function(
  path, name_sw2_run,
  id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  zipped_runs = FALSE,
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
      ),
      zipped_runs = zipped_runs
    )

    cwd_daily <- list(
      time = sim_data[["day"]][["time"]],
      values = list(
        cwd = calc_CWD_mm(
          pet_cm = sim_data[["day"]][["values"]][["pet"]],
          et_cm = sim_data[["day"]][["values"]][["et"]]
        )
      )
    )

    res[[k1]] <- if (out == "ts_years") {
      t(calc_new_yearly_aggregations(
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
  swp_daily_negbar,
  time_daily,
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
      n_slyrs_has = ncol(swp_daily_negbar)
    )

    # Days that meet soil moisture criterion
    sm <- do.call(
      what = sm_periods[["op"]],
      args = list(
        - 1 / 10 * swp_daily_negbar[, id_slyrs, drop = FALSE],
        sm_periods[["limit"]]
      )
    )

  } else {
    # Shortcut for infinite soil moisture limits --> no need for actual data
    tmp <- rep(TRUE, nrow(time_daily))

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
    time = time_daily,
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
#' @param sim_data A named list or environment with the following elements
#'   - `"swp_daily"` with two elements
#'       - `"values"`, a list which contains the named element `"swp"`,
#'         a two-dimensional object of daily soil water potential `[-bar]`
#'       - `"time"` (that is passed through)
#'   - `"temp_daily"`, a list `"values"` which contains the named element
#'     `"tmean"`, a vector of daily mean air temperatures `[C]`
#'   - `"swe_daily"`, a list `"values"` which contains the named element
#'     `"swe"`, a vector of daily snow-water equivalents `[cm]`
#'
#' @noRd
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
    swp_daily_negbar = sim_data[["swp_daily"]][["values"]][["swp"]],
    time_daily = sim_data[["swp_daily"]][["time"]],
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

  # Temperature when all criteria are met (propagate NAs in sm, dg, snw)
  mdd <- rep(NA, length(dg))
  ids <- sm[["values"]][[1]] & dg & snw
  mdd[which(ids)] <-
    sim_data[["temp_daily"]][["values"]][["tmean"]][which(ids)] -
    t_periods[["limit"]]
  mdd[which(!ids)] <- 0

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
  zipped_runs = FALSE,
  soils,
  ...
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
      ),
      zipped_runs = zipped_runs
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
      t(calc_new_yearly_aggregations(
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
  zipped_runs = FALSE,
  soils,
  ...
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
      ),
      zipped_runs = zipped_runs
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
      t(calc_new_yearly_aggregations(
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
get_DDD_yearly <- function(
  path, name_sw2_run, id_scen_used,
  list_years_scen_used,
  out = c("ts_years", "raw"),
  zipped_runs = FALSE,
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
      ),
      zipped_runs = zipped_runs
    )

    ddd_daily <- calc_MDD_daily(
      sim_data = sim_data,
      soils = soils,
      used_depth_range_cm = used_depth_range_cm,
      t_periods = list(op = `>`, limit = Temp_limit_C),
      sm_periods = list(op = `<`, limit = SWP_limit_MPa)
    )

    res[[k1]] <- if (out == "ts_years") {
      t(calc_new_yearly_aggregations(
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
  zipped_runs = FALSE,
  soils,
  ...
) {
  out <- match.arg(out)

  stopifnot(check_metric_arguments(
    out = "ts_years",
    req_soil_vars = "depth_cm"
  ))

  get_DDD_yearly(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    zipped_runs = zipped_runs,
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
  zipped_runs = FALSE,
  soils,
  ...
) {
  out <- match.arg(out)

  stopifnot(check_metric_arguments(
    out = "ts_years",
    req_soil_vars = "depth_cm"
  ))

  get_DDD_yearly(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    zipped_runs = zipped_runs,
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
#' The water release curve employed during the simulation run is used to
#' translate `SWP` into volumetric water content `VWC`, i.e.,
#' \deqn{VWC_crit[i,matric] = f(SWP_crit, SWRCp[i])}
#' where `SWRCp` are the parameters describing the water release curve.
#' However, the translation is only valid for the matric soil, i.e.,
#' the component without coarse fragments.
#'
#' `SWA` in the presence of coarse fragments is calculated as
#'
# nolint start: line_length_linter.
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
#' @param method A character string.
#'
#' @return A list with elements "time" and "values" where values represent
#'   available soil water in units of millimeters above \code{SWP_limit_MPa}:
#'   if \code{method} is \var{\dQuote{across_profile}},
#'   then summed across \code{used_depth_range_cm},
#'   if \code{method} is \var{\dQuote{by_layer}},
#'   then columns contain values for each soil layer within
#'    \code{used_depth_range_cm}.
calc_SWA_mm <- function(
  sim_swc_daily,
  soils,
  used_depth_range_cm = NULL,
  SWP_limit_MPa = -Inf,
  method = c("across_profile", "by_layer")
) {
  method <- match.arg(method)

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

    values <- if (method == "across_profile") {
      # Sum SWA across soil profile
      rowSums(swa_by_layer)

    } else if (method == "by_layer") {
      swa_by_layer
    }

  } else {
    # No soil layers in the depth range
    values <- rep(NA, nrow(sim_swc_daily[["time"]]))
  }

  list(
    time = sim_swc_daily[["time"]],
    values = list(values)
  )
}



# soils must have "depth_cm", "sand_frac", "clay_frac", and "gravel_content"
get_SWA <- function(
  path, name_sw2_run, id_scen_used,
  list_years_scen_used,
  out = c("ts_years", "raw"),
  zipped_runs = FALSE,
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
      ),
      zipped_runs = zipped_runs
    )

    swa_daily <- calc_SWA_mm(
      sim_swc_daily = sim_data[["swc_daily"]],
      soils = soils,
      SWP_limit_MPa = SWP_limit_MPa,
      used_depth_range_cm = used_depth_range_cm,
      method = "across_profile"
    )

    res[[k1]] <- if (out == "ts_years") {
      t(calc_new_yearly_aggregations(
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
  zipped_runs = FALSE,
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
    zipped_runs = zipped_runs,
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
  zipped_runs = FALSE,
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
    zipped_runs = zipped_runs,
    list_years_scen_used = list_years_scen_used,
    out = out,
    soils = soils,
    SWP_limit_MPa = -3.9,
    used_depth_range_cm = c(0, 100),
    ...
  )
}


#--- DSI = Duration of dry soil intervals at -1.5 and -3.0 SWP thresholds

calc_DSI <- function(
  swp_daily_negbar,
  time_daily,
  soils,
  used_depth_range_cm,
  SWP_limit_MPa
) {
  # Daily wet/dry conditions
  dry_daily <- calc_wetdry(
    swp_daily_negbar = swp_daily_negbar,
    time_daily = time_daily,
    soils = soils,
    used_depth_range_cm = used_depth_range_cm,
    sm_periods = list(op = `<`, limit = SWP_limit_MPa)
  )

  calc_durations_consecutive_periods(
    x_periods = dry_daily[["values"]][[1]],
    ts_years = dry_daily[["time"]][, "Year"]
  )
}


get_DSI <- function(
  path, name_sw2_run, id_scen_used,
  list_years_scen_used,
  out = c("ts_years", "raw"),
  zipped_runs = FALSE,
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
      ),
      zipped_runs = zipped_runs
    )

    tmp_dsi <- calc_DSI(
      swp_daily_negbar = sim_data[["swp_daily"]][["values"]][["swp"]],
      time_daily = sim_data[["swp_daily"]][["time"]],
      soils = soils,
      used_depth_range_cm = used_depth_range_cm,
      SWP_limit_MPa = SWP_limit_MPa
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
  zipped_runs = FALSE,
  soils,
  ...
) {
  out <- match.arg(out)

  stopifnot(check_metric_arguments(
    out = "ts_years",
    req_soil_vars = "depth_cm"
  ))

  get_DSI(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    zipped_runs = zipped_runs,
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
  zipped_runs = FALSE,
  soils,
  ...
) {
  out <- match.arg(out)

  stopifnot(check_metric_arguments(
    out = "ts_years",
    req_soil_vars = "depth_cm"
  ))

  get_DSI(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    zipped_runs = zipped_runs,
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
calc_frost_doy <- function(
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
  doy_mid <- 196L # July 15 (in non-leap year)

  res <- as.matrix(aggregate(
    x = is_frost,
    by = list(Year = sim_tmin_daily[["time"]][, "Year"]),
    function(x) {
      tmp <- which(x)
      is_last <- tmp <= doy_mid
      is_first <- tmp > doy_mid
      c(
        last = if (any(is_last)) max(tmp[is_last]) else NA_integer_,
        first = if (any(is_first)) min(tmp[is_first]) else NA_integer_
      )
    }
  ))
  colnames(res) <- c("Year", "LastFrost", "FirstFrost")

  if (include_year) res else res[, -1]
}


metric_FrostDaysAtNeg5C <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  zipped_runs = FALSE,
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
      ),
      zipped_runs = zipped_runs
    )

    res[[k1]] <- t(calc_frost_doy(
      sim_tmin_daily = sim_data[["day"]],
      Temp_limit_C = -5
    ))
  }

  res
}

# Correlation between x and y by year
# Seasonal timing (if y is monthly temperature)
metric_CorTempPPT <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  include_year = FALSE,
  zipped_runs = FALSE,
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
      ),
      zipped_runs = zipped_runs
    )

    tmp <- calc_CorXY_byYear(
      # TODO: why `tmin` and not `tmean`?
      x = sim_data[["mon"]][["values"]][["tmin"]],
      y =  sim_data[["mon"]][["values"]][["ppt"]],
      ts_year = sim_data[["mon"]][["time"]][, "Year"]
    )

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
  zipped_runs = FALSE,
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
      ),
      zipped_runs = zipped_runs
    )

    res[[k1]] <- format_values_to_matrix(
      x = unname(lapply(sim_data[["val"]][["values"]], transform)),
      ts_years = sim_data[["val"]][["time"]][, "Year"],
      out_label = out_labels,
      timestep = timestep
    )

    if (include_year) {
      res[[k1]] <- rbind(
        Year = unique(sim_data[["val"]][["time"]][, "Year"]),
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
  zipped_runs = FALSE,
  ...
) {
  out <- match.arg(out)
  stopifnot(check_metric_arguments(out = "ts_years"))

  get_SW2flux(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    zipped_runs = zipped_runs,
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
  zipped_runs = FALSE,
  ...
) {
  out <- match.arg(out)
  stopifnot(check_metric_arguments(out = "ts_years"))

  get_SW2flux(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    zipped_runs = zipped_runs,
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
  zipped_runs = FALSE,
  ...
) {
  out <- match.arg(out)
  stopifnot(check_metric_arguments(out = "ts_years"))

  get_SW2flux(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    zipped_runs = zipped_runs,
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
  zipped_runs = FALSE,
  ...
) {
  out <- match.arg(out)
  stopifnot(check_metric_arguments(out = "ts_years"))

  get_SW2flux(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    zipped_runs = zipped_runs,
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
  zipped_runs = FALSE,
  ...
) {
  out <- match.arg(out)
  stopifnot(check_metric_arguments(out = "ts_years"))

  get_SW2flux(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    zipped_runs = zipped_runs,
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
  zipped_runs = FALSE,
  ...
) {
  out <- match.arg(out)
  stopifnot(check_metric_arguments(out = "ts_years"))

  get_SW2flux(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    zipped_runs = zipped_runs,
    list_years_scen_used = list_years_scen_used,
    sw2_out = "PET",
    sw2_var = c("H_oh_MJm-2", "H_gt_MJm-2"),
    out_labels = c("H_oh_MJm-2", "H_gt_MJm-2"),
    timestep = "monthly"
  )
}


#' Annual climate variables
#'
#' @return A return object where `group` contains the following annual
#' variables:
#'   * `"PET_mm_annual"`
#'   * `"CWD_mm_annual"`
#'   * `"Tmax_mean_C_annual"`
#'   * `"Tmean_mean_C_annual"`
#'   * `"Tmin_mean_C_annual"`
#'   * `"Trange_diurnal_C_annual"`
#'   * `"Tmean_C_SD_annual"`
#'   * `"Tmean_hottestmonth_C_annual"`
#'   * `"Tmean_coldestmonth_C_annual"`
#'   * `"PPT_mm_annual"`
#'   * `"Rain_mm_annual"`
#'   * `"Snowfall_mm_annual"`
#'   * `"Snowpack_SWE_mm_annual"`
#'   * `"Rain_to_PPT_annual"`
#'   * `"PPTinJAS_to_PPT_annual"`
#'   * `"PPTinJAS_mm_annual"`
#'   * `"PPT_wettestmonth_mm_annual"`
#'   * `"PPT_driestmonth_mm_annual"`
#'
#' @noRd
metric_Climate_annual <- function(
  path, name_sw2_run,
  id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  include_year = FALSE,
  zipped_runs = FALSE,
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
      ),
      zipped_runs = zipped_runs
    )


    # Helper variables
    ts_years <- sim_data[["yr"]][["time"]][, "Year"]

    PPTinJAS <- unname(tapply(
      X = sim_data[["mon"]][["values"]][["ppt"]],
      INDEX = sim_data[["mon"]][["time"]][, "Year"],
      FUN = function(x) sum(x[7:9])
    ))


    # Output
    res[[k1]] <- rbind(
      # Potential evapotranspiration
      PET_mm_annual = 10 * sim_data[["yr"]][["values"]][["pet"]],

      # Climatic water deficit
      CWD_mm_annual = 10 * (
        sim_data[["yr"]][["values"]][["pet"]] -
          sim_data[["yr"]][["values"]][["et"]]
      ),

      # Maximum temperature
      Tmax_mean_C_annual = sim_data[["yr"]][["values"]][["tmax"]],

      # Mean temperature
      Tmean_mean_C_annual = sim_data[["yr"]][["values"]][["tmean"]],

      # Minimum temperature
      Tmin_mean_C_annual = sim_data[["yr"]][["values"]][["tmin"]],

      # Temperature range (difference between tmax and tmin)
      Trange_diurnal_C_annual = unname(tapply(
        X =
          sim_data[["day"]][["values"]][["tmax"]] -
          sim_data[["day"]][["values"]][["tmin"]],
        INDEX = sim_data[["day"]][["time"]][, "Year"],
        FUN = mean
      )),

      # SD of mean temperature
      Tmean_C_SD_annual = unname(tapply(
        X = sim_data[["day"]][["values"]][["tmean"]],
        INDEX = sim_data[["day"]][["time"]][, "Year"],
        FUN = sd
      )),

      # Temperature of hottest month
      Tmean_hottestmonth_C_annual = unname(tapply(
        X = sim_data[["mon"]][["values"]][["tmean"]],
        INDEX = sim_data[["mon"]][["time"]][, "Year"],
        FUN = max
      )),

      # Temperature of coldest month
      Tmean_coldestmonth_C_annual = unname(tapply(
        X = sim_data[["mon"]][["values"]][["tmean"]],
        INDEX = sim_data[["mon"]][["time"]][, "Year"],
        FUN = min
      )),

      # Precipitation
      PPT_mm_annual = 10 * sim_data[["yr"]][["values"]][["ppt"]],

      Rain_mm_annual = 10 * sim_data[["yr"]][["values"]][["rain"]],

      Snowfall_mm_annual = 10 * sim_data[["yr"]][["values"]][["snow_fall"]],

      Snowpack_SWE_mm_annual = 10 * sim_data[["yr"]][["values"]][["swe"]],

      # Ratio of rain to all precipitation
      Rain_to_PPT_annual =
        sim_data[["yr"]][["values"]][["rain"]] /
        sim_data[["yr"]][["values"]][["ppt"]],


      # Ratio of July, August, and September precipitation to annual
      PPTinJAS_to_PPT_annual = PPTinJAS / sim_data[["yr"]][["values"]][["ppt"]],

      # Sum of precipitation in months of July, August, and September
      PPTinJAS_mm_annual = 10 * PPTinJAS,

      # Precipitation of the wettest month
      PPT_wettestmonth_mm_annual = 10 * unname(tapply(
        X = sim_data[["mon"]][["values"]][["ppt"]],
        INDEX = sim_data[["mon"]][["time"]][, "Year"],
        FUN = max
      )),

      # Precipitation of the driest month
      PPT_driestmonth_mm_annual = 10 * unname(tapply(
        X = sim_data[["mon"]][["values"]][["ppt"]],
        INDEX = sim_data[["mon"]][["time"]][, "Year"],
        FUN = min
      ))
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
  zipped_runs = FALSE,
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
      ),
      zipped_runs = zipped_runs
    )


    # Helper variables
    ts_years <- sim_data[["mon"]][["time"]][, "Year"]

    # Output
    res[[k1]] <- rbind(
      # Potential evapotranspiration
      format_values_to_matrix(
        x = 10 * sim_data[["mon"]][["values"]][["pet"]],
        ts_years = ts_years,
        timestep = "monthly",
        out_label = "PET_mm"
      ),

      # Climatic water deficit
      format_values_to_matrix(
        x = 10 * (
          sim_data[["mon"]][["values"]][["pet"]] -
            sim_data[["mon"]][["values"]][["et"]]
        ),
        ts_years = ts_years,
        timestep = "monthly",
        out_label = "CWD_mm"
      ),

      # Maximum temperature (mean across year)
      format_values_to_matrix(
        x = sim_data[["mon"]][["values"]][["tmax"]],
        ts_years = ts_years,
        timestep = "monthly",
        out_label = "Tmax_mean_C"
      ),

      # Mean temperature
      format_values_to_matrix(
        x = sim_data[["mon"]][["values"]][["tmean"]],
        ts_years = ts_years,
        timestep = "monthly",
        out_label = "Tmean_mean_C"
      ),

      # Minimum temperature (mean across year)
      format_values_to_matrix(
        x = sim_data[["mon"]][["values"]][["tmin"]],
        ts_years = ts_years,
        timestep = "monthly",
        out_label = "Tmin_mean_C"
      ),

      # Temperature range (difference between tmax and tmin)
      format_values_to_matrix(
        x = as.vector(t(tapply(
          X =
            sim_data[["day"]][["values"]][["tmax"]] -
            sim_data[["day"]][["values"]][["tmin"]],
          INDEX = list(
            sim_data[["day"]][["time"]][, "Year"],
            sim_data[["day"]][["time"]][, "Month"]
          ),
          FUN = mean
        ))),
        ts_years = ts_years,
        timestep = "monthly",
        out_label = "Trange_diurnal_C"
      ),

      # SD of mean temperature
      format_values_to_matrix(
        x = as.vector(t(tapply(
          X = sim_data[["day"]][["values"]][["tmean"]],
          INDEX = list(
            sim_data[["day"]][["time"]][, "Year"],
            sim_data[["day"]][["time"]][, "Month"]
          ),
          FUN = sd
        ))),
        ts_years = ts_years,
        timestep = "monthly",
        out_label = "Tmean_C_SD"
      ),

      # Precipitation
      format_values_to_matrix(
        x = 10 * sim_data[["mon"]][["values"]][["ppt"]],
        ts_years = ts_years,
        timestep = "monthly",
        out_label = "PPT_mm"
      ),

      format_values_to_matrix(
        x = 10 * sim_data[["mon"]][["values"]][["rain"]],
        ts_years = ts_years,
        timestep = "monthly",
        out_label = "Rain_mm"
      ),

      format_values_to_matrix(
        x = 10 * sim_data[["mon"]][["values"]][["snow_fall"]],
        ts_years = ts_years,
        timestep = "monthly",
        out_label = "Snowfall_mm"
      ),

      format_values_to_matrix(
        x = 10 * sim_data[["mon"]][["values"]][["swe"]],
        ts_years = ts_years,
        timestep = "monthly",
        out_label = "Snowpack_SWE_mm"
      ),

      # Monthly snowmelt
      format_values_to_matrix(
        x = 10 * sim_data[["mon"]][["values"]][["snowmelt"]],
        ts_years = ts_years,
        timestep = "monthly",
        out_label = "Snowmelt_mm"
      )
    )


    if (include_year) {
      res[[k1]] <- rbind(
        Year = unique(ts_years),
        res[[k1]]
      )
    }
  }

  res
}


get_Tmean_monthly <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = c("ts_years", "across_years"),
  fun_aggs_across_yrs = mean,
  zipped_runs = FALSE,
  ...
) {
  res <- list()

  if (out == "ts_years" && !isTRUE(is.list(list_years_scen_used[[1]]))) {
    # Code below expects lists of lists of year vectors
    # as provided by default if out is "across_years" --> convert
    list_years_scen_used <- lapply(list_years_scen_used, list)
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
          ),
          zipped_runs = zipped_runs
        )

        if (out == "ts_years") {
          format_values_to_matrix(
            x = sim_data[["mon"]][["values"]][["tmean"]],
            ts_years = sim_data[["mon"]][["time"]][, "Year"],
            timestep = "monthly",
            out_label = "Tmean_C"
          )
        } else if (out == "across_years") {
          format_values_to_matrix(
            x = calc_climatology(
              X = sim_data[["mon"]][["values"]][["tmean"]],
              INDEX = sim_data[["mon"]][["time"]][, "Month"],
              FUN = fun_aggs_across_yrs
            ),
            ts_years = NA,
            timestep = "monthly",
            out_label = "Tmean_C"
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
  zipped_runs = FALSE,
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  get_Tmean_monthly(
    path = path,
    name_sw2_run = name_sw2_run,
    zipped_runs = zipped_runs,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    out = out,
    ...
  )
}

#' Across-year average monthly mean air temperature
#' @section Notes: Un-simulated but requested time steps propagate NAs.
#' @noRd
metric_Tmean_monthlyClim <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "across_years",
  zipped_runs = FALSE,
  fun_aggs_across_yrs = mean,
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  get_Tmean_monthly(
    path = path,
    name_sw2_run = name_sw2_run,
    zipped_runs = zipped_runs,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    out = out,
    fun_aggs_across_yrs = fun_aggs_across_yrs,
    ...
  )
}

#' Across-year average monthly precipitation amount [mm]
#' @section Notes: Un-simulated but requested time steps propagate NAs.
#' @noRd
metric_PPT_monthlyClim <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "across_years",
  fun_aggs_across_yrs = mean,
  zipped_runs = FALSE,
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  res <- list()

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
              sw2_outs = "PRECIP",
              sw2_vars = "ppt",
              varnames_are_fixed = TRUE
            )
          ),
          zipped_runs = zipped_runs
        )

        format_values_to_matrix(
          x = calc_climatology(
            X = 10 * sim_data[["mon"]][["values"]][["ppt"]],
            INDEX = sim_data[["mon"]][["time"]][, "Month"],
            FUN = fun_aggs_across_yrs
          ),
          ts_years = NA,
          timestep = "monthly",
          out_label = "PPT_mm"
        )
      }
    )

    res[[k1]] <- do.call(cbind, tmp)
  }

  res
}



#--- Soil moisture regimes / soil temperature regimes
metric_SMTRs <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "across_years",
  zipped_runs = FALSE,
  ...
) {
  stopifnot(
    requireNamespace("rSW2funs", quietly = TRUE),
    check_metric_arguments(out = match.arg(out))
  )

  res <- list()

  sim_input <- load_sw2_rda(
    path = file.path(path, name_sw2_run),
    fname = "sw_input.RData",
    zipped_runs = zipped_runs
  )

  for (k1 in seq_along(id_scen_used)) {
    sim_data <- load_sw2_rda(
      path = file.path(path, name_sw2_run),
      fname = paste0("sw_output_sc", id_scen_used[k1], ".RData"),
      zipped_runs = zipped_runs
    )

    tmp <- lapply(
      list_years_scen_used[[k1]],
      function(yrs) {
        tmp_swin <- sim_input[["swRunScenariosData"]][[k1]]

        # Update with selected time period
        rSOILWAT2::swYears_EndYear(tmp_swin) <- 1L + max(
          rSOILWAT2::swYears_StartYear(tmp_swin),
          yrs[length(yrs)]
        )
        rSOILWAT2::swYears_StartYear(tmp_swin) <- yrs[[1]]
        rSOILWAT2::swYears_EndYear(tmp_swin) <- yrs[length(yrs)]

        tmp <- rSW2funs::calc_SMTRs(
          sim_in = tmp_swin,
          sim_out = sim_data[["runDataSC"]]
        )

        cbind(tmp[["STR"]], tmp[["SMR"]])
      }
    )

    res[[k1]] <- t(do.call(rbind, tmp))
  }

  res
}


calc_AI <- function(ppt, pet) ppt / pet

#' Annual AI = aridity index [mm/mm] = PPT/PET
#'
#' @return A return object where `group` contains the following annual
#' variable:
#'   * `"AI"`
#'
#' @noRd
metric_AI <- function(
  path, name_sw2_run,
  id_scen_used, list_years_scen_used,
  out = c("ts_years", "raw"),
  zipped_runs = FALSE,
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
      ),
      zipped_runs = zipped_runs
    )

    res[[k1]] <- matrix(
      data = calc_AI(
        ppt = sim_data[["yr"]][["values"]][["ppt"]],
        pet = sim_data[["yr"]][["values"]][["pet"]]
      ),
      nrow = 1,
      dimnames = list("AI", NULL)
    )
  }

  res
}


get_RR2022predictors_annual <- function(
  path,
  name_sw2_run,
  id_scen_used,
  list_years_scen_used,
  out = c("ts_years", "raw"),
  zipped_runs = FALSE,
  soils,
  ...
) {
  res <- list()

  out <- match.arg(out)

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
        ),
        day = list(
          sw2_tp = "Day",
          sw2_outs = c("TEMP", "TEMP"),
          sw2_vars = c(
            tmax = "max_C",
            tmin = "min_C"
          ),
          varnames_are_fixed = TRUE
        ),
        mon = list(
          sw2_tp = "Month",
          sw2_outs = c("PRECIP", "TEMP", "TEMP", "PET", "AET"),
          sw2_vars = c(
            ppt = "ppt",
            tmean = "avg_C",
            tmin = "min_C",
            pet = "pet_cm",
            et = "evapotr_cm"
          ),
          varnames_are_fixed = TRUE
        ),
        yr = list(
          sw2_tp = "Year",
          sw2_outs = c("PRECIP", "PRECIP", "TEMP", "PET", "AET", "DEEPSWC"),
          sw2_vars = c(
            ppt = "ppt",
            rain = "rain",
            tmean = "avg_C",
            pet = "pet_cm",
            et = "evapotr_cm",
            drainage = "lowLayerDrain_cm"
          ),
          varnames_are_fixed = TRUE
        )
      ),
      zipped_runs = zipped_runs
    )


    #--- Intermediate variables
    tmp_cwd <- calc_CWD_mm(
      pet_cm = sim_data[["mon"]][["values"]][["pet"]],
      et_cm = sim_data[["mon"]][["values"]][["et"]]
    )

    # DDDat5C0to100cm30bar
    tmp_ddd <- calc_MDD_daily(
      sim_data = sim_data,
      soils = soils,
      used_depth_range_cm = c(0, 100),
      t_periods = list(op = `>`, limit = 5),
      sm_periods = list(op = `<`, limit = -3)
    )

    # DSIat0to100cm15bar-mean
    tmp_dsi <- vapply(
      calc_DSI(
        swp_daily_negbar = sim_data[["swp_daily"]][["values"]][["swp"]],
        time_daily = sim_data[["swp_daily"]][["time"]],
        soils = soils,
        used_depth_range_cm = c(0, 100),
        SWP_limit_MPa = -1.5
      ),
      mean,
      FUN.VALUE = NA_real_
    )


    #--- Annual time series to derive RandomForest R&R predictors (2022-Feb-07)
    res[[k1]] <- rbind(
      # Annual mean temperature
      #   Tmean -- TA-MAT_C
      Tmean = sim_data[["yr"]][["values"]][["tmean"]],

      # Temperature range (difference between tmax and tmin)
      #   Trange_diurnal -- Climate-Trange_diurnal_C_annual
      Trange_diurnal_mean = tapply(
        X =
          sim_data[["day"]][["values"]][["tmax"]] -
          sim_data[["day"]][["values"]][["tmin"]],
        INDEX = sim_data[["day"]][["time"]][, "Year"],
        FUN = mean
      ),

      # Mean temperature of coldest month
      #   Tmean_coldestmonth --
      #     Climate-Tmean_coldestmonth_C_annual
      Tmean_coldestmonth = tapply(
        X = sim_data[["mon"]][["values"]][["tmean"]],
        INDEX = sim_data[["mon"]][["time"]][, "Year"],
        FUN = min
      ),

      # Mean temperature of hottest month
      #   Tmean_hottestmonth -- Climate-Tmean_hottestmonth_C_annual
      Tmean_hottestmonth = tapply(
        X = sim_data[["mon"]][["values"]][["tmean"]],
        INDEX = sim_data[["mon"]][["time"]][, "Year"],
        FUN = max
      ),

      # Annual precipitation amount
      #   PPT -- PPT-MAP_mm
      PPT = 10 * sim_data[["yr"]][["values"]][["ppt"]],

      # Annual rainfall amount
      #   Rain -- Climate-Rain_mm_annual
      Rain = 10 * sim_data[["yr"]][["values"]][["rain"]],

      # Precipitation amount in months of July, August, and September
      #   PPTinJAS -- Climate-PPTinJAS_mm_annual
      PPTinJAS = 10 * tapply(
        X = sim_data[["mon"]][["values"]][["ppt"]],
        INDEX = sim_data[["mon"]][["time"]][, "Year"],
        FUN = function(x) sum(x[7:9])
      ),

      # Precipitation amount of the driest month
      #   PPT_driestmonth -- Climate-PPT_driestmonth_mm_annual
      PPT_driestmonth = 10 * tapply(
        X = sim_data[["mon"]][["values"]][["ppt"]],
        INDEX = sim_data[["mon"]][["time"]][, "Year"],
        FUN = min
      ),

      # Potential evapotranspiration
      #   PET -- Climate-PET_mm_annual
      PET = 10 * sim_data[["yr"]][["values"]][["pet"]],

      # Evapotranspiration
      #   ET -- ET-ET_mm_annual
      ET = 10 * sim_data[["yr"]][["values"]][["et"]],

      # Climatic water deficit
      #   CWD -- CWD-values
      CWD = calc_CWD_mm(
        pet_cm = sim_data[["yr"]][["values"]][["pet"]],
        et_cm = sim_data[["yr"]][["values"]][["et"]]
      ),

      #   CWD_mon_corr_temp -- CWD-seasonality
      CWD_mon_corr_temp = calc_CorXY_byYear(
        # correlate against Tmean (unlike other seasonal timing metrics)
        # because `metric_CWD()` uses Tmean
        x = sim_data[["mon"]][["values"]][["tmean"]],
        y = tmp_cwd,
        ts_year = sim_data[["mon"]][["time"]][, "Year"]
      ),

      #   CWD_mon_cv -- CWD-seasonal_variability
      CWD_mon_cv = calc_seasonal_variability(
        x = tmp_cwd,
        ts_year = sim_data[["mon"]][["time"]][, "Year"]
      ),

      #   DDD -- DDDat5C0to100cm30bar-values
      DDD = tapply(
        X = tmp_ddd[["values"]][["mdd"]],
        INDEX = tmp_ddd[["time"]][, "Year"],
        FUN = sum
      ),

      #   DSI_duration -- DSIat0to100cm15bar-mean
      DSI_duration = tmp_dsi,

      #   CorTempPPT -- CorTempPPT-seasonality
      CorTempPPT = calc_CorXY_byYear(
        # TODO: why `tmin` and not `tmean`?
        x = sim_data[["mon"]][["values"]][["tmin"]],
        y =  sim_data[["mon"]][["values"]][["ppt"]],
        ts_year = sim_data[["mon"]][["time"]][, "Year"]
      ),

      #   DeepDrainage -- DR-DeepDrainage_mm_annual
      DeepDrainage = 10 * sim_data[["yr"]][["values"]][["drainage"]]
    )
  }

  res
}


#' Annual time series underlying the 19 R&R predictors
#'
#' The 19 predictors of resilience & resistance indicators
#' (see `metric_RR2022predictors_annualClim()`) are long-term
#' means, standard deviations, or coefficients of variation of
#' annual values.
#'
#' @references Chambers et al. (2023)
#' New indicators of ecological resilience and invasion resistance to support
#' prioritization and management in the sagebrush biome, United States.
#' Frontiers in Ecology and Evolution, 10, 1–17.
#' \url{https://doi.org/10.3389/fevo.2022.1009268}
#'
#' @seealso [RR2022predictors_annualClim()] and
#' [metric_RR2022predictors_annual()]
#'
#' @name RR2022predictors_annual
#' @md
metric_RR2022predictors_annual <- function(
  path,
  name_sw2_run,
  id_scen_used,
  list_years_scen_used,
  out = c("ts_years", "raw"),
  zipped_runs = FALSE,
  soils,
  ...
) {
  out <- match.arg(out)

  stopifnot(check_metric_arguments(
    out = out,
    req_soil_vars = "depth_cm"
  ))

  get_RR2022predictors_annual(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    out = out,
    zipped_runs = zipped_runs,
    soils = soils,
    ...
  )
}



#' 19 predictors of resilience & resistance indicators
#'
#' The 19 predictors of resilience & resistance indicators are long-term
#' means, standard deviations, or coefficients of variation of
#' annual values (see `metric_RR2022predictors_annual()`).
#'
#' @return A return object where `group` contains the following annual
#' variables:
#'    * `"Tmean_mean"`
#'    * `"Trange_diurnal_mean"`
#'    * `"Tmean_coldestmonth_mean"`
#'    * `"Tmean_coldestmonth_sd"`
#'    * `"Tmean_hottestmonth_sd"`
#'    * `"PPT_mean"`
#'    * `"PPT_cv"`
#'    * `"Rain_mean"`
#'    * `"PPTinJAS_mean"`
#'    * `"PPT_driestmonth_mean"`
#'    * `"PET_cv"`
#'    * `"ET_cv"`
#'    * `"CWD_mean"`
#'    * `"CWD_mon_corr_temp_mean"`
#'    * `"CWD_mon_cv_mean"`
#'    * `"DDD_mean"`
#'    * `"DSI_duration_mean"`
#'    * `"CorTempPPT_mean"`
#'    * `"DeepDrainage_mean"`
#'
#'
#' @section Notes:
#'   * Argument `fun_aggs_across_yrs` is ignored.
#'   * Values are `NA` if any year is requested but was not simulated.
#'
#' @references Chambers et al. (2023)
#' New indicators of ecological resilience and invasion resistance to support
#' prioritization and management in the sagebrush biome, United States.
#' Frontiers in Ecology and Evolution, 10, 1–17.
#' \url{https://doi.org/10.3389/fevo.2022.1009268}
#'
#' @seealso [RR2022predictors_annual()] and
#' [metric_RR2022predictors_annualClim()]
#'
#' @name RR2022predictors_annualClim
#' @md
metric_RR2022predictors_annualClim <- function(
  path,
  name_sw2_run,
  id_scen_used,
  list_years_scen_used,
  out = "across_years",
  zipped_runs = FALSE,
  soils,
  ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = "depth_cm"
  ))


  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    tmp <- lapply(
      list_years_scen_used[[k1]],
      function(yrs) {
        tmpx <- get_RR2022predictors_annual(
          path = path,
          name_sw2_run = name_sw2_run,
          id_scen_used = id_scen_used[k1],
          list_years_scen_used = list(yrs),
          out = "ts_years",
          zipped_runs = zipped_runs,
          soils = soils,
          ...
        )[[1L]]


        #--- Calculate RandomForest R&R predictors (2022-Feb-07)
        # Long-term means, standard deviations, or coefficients of variation of
        # annual values
        as.matrix(c(
          # Annual mean temperature
          #   Tmean_mean -- TA-MAT_C__mean
          Tmean_mean = mean(tmpx["Tmean", , drop = TRUE]),

          # Temperature range (difference between tmax and tmin)
          #   Trange_diurnal_mean -- Climate-Trange_diurnal_C_annual__mean
          Trange_diurnal_mean = mean(
            tmpx["Trange_diurnal_mean", , drop = TRUE]
          ),

          # Mean temperature of coldest month
          #   Tmean_coldestmonth_mean --
          #     Climate-Tmean_coldestmonth_C_annual__mean
          Tmean_coldestmonth_mean = mean(
            tmpx["Tmean_coldestmonth", , drop = TRUE]
          ),

          #   Tmean_coldestmonth_sd -- Climate-Tmean_coldestmonth_C_annual__sd
          Tmean_coldestmonth_sd = sd(
            tmpx["Tmean_coldestmonth", , drop = TRUE]
          ),

          # Mean temperature of hottest month
          #   Tmean_hottestmonth_sd -- Climate-Tmean_hottestmonth_C_annual__sd
          Tmean_hottestmonth_sd = sd(
            tmpx["Tmean_hottestmonth", , drop = TRUE]
          ),

          # Annual precipitation amount
          #   PPT_mean -- PPT-MAP_mm__mean
          PPT_mean = mean(tmpx["PPT", , drop = TRUE]),

          #   PPT_cv -- PPT-MAP_mm__cv
          PPT_cv = cv(tmpx["PPT", , drop = TRUE]),

          # Annual rainfall amount
          #   Rain_mean -- Climate-Rain_mm_annual__mean
          Rain_mean = mean(tmpx["Rain", , drop = TRUE]),

          # Precipitation amount in months of July, August, and September
          #   PPTinJAS_mean -- Climate-PPTinJAS_mm_annual__mean
          PPTinJAS_mean = mean(tmpx["PPTinJAS", , drop = TRUE]),

          # Precipitation amount of the driest month
          #   PPT_driestmonth_mean -- Climate-PPT_driestmonth_mm_annual__mean
          PPT_driestmonth_mean = mean(tmpx["PPT_driestmonth", , drop = TRUE]),

          # Potential evapotranspiration
          #   PET_cv -- Climate-PET_mm_annual__cv
          PET_cv = cv(tmpx["PET", , drop = TRUE]),

          # Evapotranspiration
          #   ET_cv -- ET-ET_mm_annual__cv
          ET_cv = cv(tmpx["ET", , drop = TRUE]),

          # Climatic water deficit
          #   CWD_mean -- CWD-values__mean
          CWD_mean = mean(tmpx["CWD", , drop = TRUE]),

          #   CWD_mon_corr_temp_mean -- CWD-seasonality__mean
          CWD_mon_corr_temp_mean = mean(
            tmpx["CWD_mon_corr_temp", , drop = TRUE]
          ),

          #   CWD_mon_cv_mean -- CWD-seasonal_variability__mean
          CWD_mon_cv_mean = mean(tmpx["CWD_mon_cv", , drop = TRUE]),

          #   DDD_mean -- DDDat5C0to100cm30bar-values__mean
          DDD_mean = mean(tmpx["DDD", , drop = TRUE]),

          #   DSI_duration_mean -- DSIat0to100cm15bar-mean__mean
          DSI_duration_mean = mean(tmpx["DSI_duration", , drop = TRUE]),

          #   CorTempPPT_mean -- CorTempPPT-seasonality__mean
          CorTempPPT_mean = mean(tmpx["CorTempPPT", , drop = TRUE]),

          #   DeepDrainage_mean -- DR-DeepDrainage_mm_annual__mean
          DeepDrainage_mean = mean(tmpx["DeepDrainage", , drop = TRUE])
        ))
      }
    )

    res[[k1]] <- do.call(cbind, tmp)
  }

  res
}





get_EcologicalDroughtMetrics2023_annual <- function(
  path,
  name_sw2_run,
  id_scen_used,
  list_years_scen_used,
  out = c("ts_years", "raw"),
  zipped_runs = FALSE,
  soils,
  ...
) {
  res <- list()

  out <- match.arg(out)

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
        swc_daily = list(
          sw2_tp = "Day",
          sw2_outs = "SWCBULK",
          sw2_vars = c(swc = "Lyr"),
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
        ),
        day = list(
          sw2_tp = "Day",
          sw2_outs = c("TEMP", "PET", "AET"),
          sw2_vars = c(
            tmin = "min_C",
            pet = "pet_cm",
            et = "evapotr_cm"
          ),
          varnames_are_fixed = TRUE
        ),
        mon = list(
          sw2_tp = "Month",
          sw2_outs = c("PRECIP", "TEMP", "PET", "AET"),
          sw2_vars = c(
            ppt = "ppt",
            tmean = "avg_C",
            pet = "pet_cm",
            et = "evapotr_cm"
          ),
          varnames_are_fixed = TRUE
        ),
        yr = list(
          sw2_tp = "Year",
          sw2_outs = c(
            "PRECIP",
            "TEMP", "TEMP", "TEMP",
            "PET", "AET"
          ),
          sw2_vars = c(
            ppt = "ppt",
            tmean = "avg_C", tmin = "min_C", tmax = "max_C",
            pet = "pet_cm", et = "evapotr_cm"
          ),
          varnames_are_fixed = TRUE
        )
      ),
      zipped_runs = zipped_runs
    )


    #--- Intermediate variables
    tmp_cwd_monthly <- calc_CWD_mm(
      pet_cm = sim_data[["mon"]][["values"]][["pet"]],
      et_cm = sim_data[["mon"]][["values"]][["et"]]
    )

    # TDDat5C
    tmp_tdd_daily <- calc_MDD_daily(
      # nolint start: commented_code_linter
      # required content of sim_data: list(
      #   swp_daily = list(time, values = swp),
      #   temp_daily = list(values = tmean),
      #   swe_daily = list(values = swe)
      # )
      # nolint end: commented_code_linter
      sim_data = sim_data,
      soils = soils,
      used_depth_range_cm = NULL,
      t_periods = list(op = `>`, limit = 5),
      sm_periods = list(op = `>`, limit = -Inf)
    )

    # DDDat5C0to100cm30bar
    tmp_ddd_daily <- calc_MDD_daily(
      sim_data = sim_data,
      soils = soils,
      used_depth_range_cm = c(0, 100),
      t_periods = list(op = `>`, limit = 5),
      sm_periods = list(op = `<`, limit = -3)
    )

    # DSIat0to100cm15bar
    tmp_dsi_daily <- calc_DSI(
      swp_daily_negbar = sim_data[["swp_daily"]][["values"]][["swp"]],
      time_daily = sim_data[["swp_daily"]][["time"]],
      soils = soils,
      used_depth_range_cm = c(0, 100),
      SWP_limit_MPa = -1.5
    )

    # WDDat5C0to100cm15bar
    tmp_wdd_daily <- calc_MDD_daily(
      sim_data = sim_data,
      soils = soils,
      used_depth_range_cm = c(0, 100),
      t_periods = list(op = `>`, limit = 5),
      sm_periods = list(op = `>`, limit = -1.5)
    )

    # Frost
    tmp_frost_doys <- calc_frost_doy(
      # required content of sim_data: list(values = tmin, time = Year)
      sim_tmin_daily = sim_data[["day"]],
      Temp_limit_C = -5
    )

    # Recruitment (metric_RecruitmentIndex_v5)
    tmp_recruit <- calc_RecruitmentIndex_v3(
      sim_data = sim_data, # passed to `calc_MDD_daily()`
      soils = soils,
      out = "ts_years",
      hemisphere_NS = "N",
      recruitment_depth_range_cm = c(10, 20),
      Temp_limit_C = 5,
      Wet_SWP_limit_MPa = -1.5,
      Dry_SWP_limit_MPa = -3,
      init_WDD = 15,
      init_days = 3,
      init_depth_range_cm = c(0, 10),
      stop_DDD = 15,
      stop_days_DDD = 3,
      stop_depth_range_cm = c(0, 20),
      stop_TDD = 0,
      stop_days_TDD = 3
    )

    # SWA
    tmp_swa_daily <- calc_SWA_mm(
      # required content of sim_data: list(time, values = swc)
      sim_swc_daily = sim_data[["swc_daily"]],
      soils = soils,
      SWP_limit_MPa = -3.9,
      used_depth_range_cm = c(0, 100),
      method = "across_profile"
    )

    tmp_swa_monthly <- as.vector(
      tapply(
        X = tmp_swa_daily[["values"]][[1L]],
        INDEX = list(
          Month = tmp_swa_daily[["time"]][, "Month"],
          Year = tmp_swa_daily[["time"]][, "Year"]
        ),
        FUN = mean
      )
    )


    #--- Annual time series of ecological drought metrics
    res[[k1]] <- rbind(
      # Aridity index `[mm / mm]` (where `AI = PPT / PET`)
      AI = calc_AI(
        ppt = sim_data[["yr"]][["values"]][["ppt"]],
        pet = sim_data[["yr"]][["values"]][["pet"]]
      ),

      # Potential evapotranspiration `[mm]`
      PET = 10 * sim_data[["yr"]][["values"]][["pet"]],

      # Mean daily air temperature `[C]`
      Tmean = sim_data[["yr"]][["values"]][["tmean"]],

      # Minimum daily air temperature `[C]`
      Tmin = sim_data[["yr"]][["values"]][["tmin"]],

      # Maximum daily air temperature `[C]`
      Tmax = sim_data[["yr"]][["values"]][["tmax"]],

      # Precipitation amount `[mm]`
      PPT = 10 * sim_data[["yr"]][["values"]][["ppt"]],

      # Seasonal timing of precipitation `[-]`
      # (where `PPTsst = cor(monthly PPT, monthly Tmean)`)
      PPTsst = calc_CorXY_byYear(
        x = sim_data[["mon"]][["values"]][["ppt"]],
        # correlate against Tmean unlike `metric_CorTempPPT()` which uses Tmin
        y = sim_data[["mon"]][["values"]][["tmean"]],
        ts_year = sim_data[["mon"]][["time"]][, "Year"]
      ),

      # Total growing degree days `[C x day]`
      TDD = tapply(
        X = tmp_tdd_daily[["values"]][["mdd"]],
        INDEX = tmp_tdd_daily[["time"]][, "Year"],
        FUN = sum
      ),

      # Seasonal variability of total growing degree days `[C x day]`
      # (where `TDDssv = mean(monthly TDD) / sd(monthly TDD)`)
      TDDssv = calc_seasonal_variability(
        x = as.vector(
          tapply(
            X = tmp_tdd_daily[["values"]][["mdd"]],
            INDEX = list(
              Month = tmp_tdd_daily[["time"]][, "Month"],
              Year = tmp_tdd_daily[["time"]][, "Year"]
            ),
            FUN = mean
          )
        ),
        ts_year = sim_data[["mon"]][["time"]][, "Year"]
      ),

      # Warm season length `[day]`
      # (where `GrowingSeasonDuration` is the longest spell of days
      # with a positive `TDD`)
      GrowingSeasonDuration = vapply(
        X = calc_durations_consecutive_periods(
          x_periods = calc_condition(
            x = tmp_tdd_daily[["values"]][["mdd"]],
            condition = list(op = `>`, limit = 0)
          ),
          ts_years = tmp_tdd_daily[["time"]][, "Year"]
        ),
        FUN = max,
        FUN.VALUE = NA_integer_
      ),

      # (Actual) evapotranspiration [`mm`]
      ET = 10 * sim_data[["yr"]][["values"]][["et"]],

      # Climatic water deficit `[mm]` (where `CWD = PPT - ET`)
      CWD = calc_CWD_mm(
        pet_cm = sim_data[["yr"]][["values"]][["pet"]],
        et_cm = sim_data[["yr"]][["values"]][["et"]]
      ),

      # 10-day extreme climatic water deficit `[mm]`
      # (where `CWDextreme10d` is the largest daily `CWD`
      # averaged over 10-day periods)
      CWDextreme010d = calc_extreme_funNday(
        x = calc_CWD_mm(
          pet_cm = sim_data[["day"]][["values"]][["pet"]],
          et_cm = sim_data[["day"]][["values"]][["et"]]
        ),
        ts_year = sim_data[["day"]][["time"]][, "Year"],
        n_days = 10L,
        fun_time = mean,
        fun_extreme = max
      ),

      # Seasonal variability of climatic water deficit `[mm / mm]`
      # (where `CWDssv = mean(monthly CWD) / sd(monthly CWD)`)
      CWDssv = calc_seasonal_variability(
        x = tmp_cwd_monthly,
        ts_year = sim_data[["mon"]][["time"]][, "Year"]
      ),

      # Seasonal timing of climatic water deficit `[-]`
      # (where `CWDsst = cor(monthly CWD, monthly Tmean)`)
      CWDsst = calc_CorXY_byYear(
        x = tmp_cwd_monthly,
        # correlate against Tmean as does `metric_CWD()`
        y = sim_data[["mon"]][["values"]][["tmean"]],
        ts_year = sim_data[["mon"]][["time"]][, "Year"]
      ),

      # Dry-degree days (0-100 cm, <-3 MPa) `[C x day]`
      DDD = tapply(
        X = tmp_ddd_daily[["values"]][["mdd"]],
        INDEX = tmp_ddd_daily[["time"]][, "Year"],
        FUN = sum
      ),

      # Dry-degree days of longest spell `[C x day]`
      DDDmaxSpell = calc_extreme_value_consecutive_periods(
        x = tmp_ddd_daily[["values"]][["mdd"]],
        x_periods = calc_condition(
          x = tmp_ddd_daily[["values"]][["mdd"]],
          condition = list(op = `>`, limit = 0)
        ),
        ts_years = tmp_ddd_daily[["time"]][, "Year"],
        fun_time = sum,
        fun_extreme = max
      ),

      # Wet-degree days (0-100 cm, any >-1.5 MPa) `[C x day]`
      WDD = tapply(
        X = tmp_wdd_daily[["values"]][["mdd"]],
        INDEX = tmp_wdd_daily[["time"]][, "Year"],
        FUN = sum
      ),

      # Seasonal timing of wet-degree days `[-]`
      # (where `WDDsst = cor(monthly WDD, monthly Tmean)`)
      WDDsst = calc_CorXY_byYear(
        x = as.vector(
          tapply(
            X = tmp_wdd_daily[["values"]][["mdd"]],
            INDEX = list(
              Month = tmp_wdd_daily[["time"]][, "Month"],
              Year = tmp_wdd_daily[["time"]][, "Year"]
            ),
            FUN = sum
          )
        ),
        # correlate against Tmean as does `metric_WDDat5C0to100cm15bar()`
        y = sim_data[["mon"]][["values"]][["tmean"]],
        ts_year = sim_data[["mon"]][["time"]][, "Year"]
      ),

      # Mean spell length of dry soils (0-100 cm, <-1.5 MPa) `[day]`
      DSI = vapply(tmp_dsi_daily, mean, FUN.VALUE = NA_real_),

      # Number of dry soils intervals `[#]`
      nDSI = lengths(tmp_dsi_daily),

      # First fall frost (<-5 C) `[day of year]`
      FrostFallFirst = as.vector(tmp_frost_doys[, "FirstFrost"]),

      # Last spring frost (<-5 C) `[day of year]`
      FrostSpringLast = as.vector(tmp_frost_doys[, "LastFrost"]),

      # Fall recruitment onset `[day of year]`
      RecruitmentFallOnset = as.vector(tmp_recruit[, "FallRecruitment_DOY"]),

      # Fall recruitment duration `[day]`
      RecruitmentFallDuration =
        as.vector(tmp_recruit[, "FallRecruitment_DurationDays"]),

      # Fall recruitment wet-degree days `[C x day]`
      RecruitmentFallWDD = as.vector(tmp_recruit[, "FallRecruitment_maxWDD"]),

      # Spring recruitment onset `[day of year]`
      RecruitmentSpringOnset =
        as.vector(tmp_recruit[, "SpringRecruitment_DOY"]),

      # Spring recruitment duration `[day]`
      RecruitmentSpringDuration =
        as.vector(tmp_recruit[, "SpringRecruitment_DurationDays"]),

      # Spring recruitment wet-degree days `[C x day]`
      RecruitmentSpringWDD =
        as.vector(tmp_recruit[, "SpringRecruitment_maxWDD"]),

      # Available soil moisture (0-100 cm, >-3.9 MPa) `[mm]`
      SWA = tapply(
        X = tmp_swa_daily[["values"]][[1L]],
        INDEX = tmp_swa_daily[["time"]][, "Year"],
        FUN = mean
      ),

      # Seasonal variability of available soil moisture `[mm / mm]`
      # (where `SWAssv = mean(monthly SWA) / sd(monthly CWD)`)
      SWAssv = calc_seasonal_variability(
        x = tmp_swa_monthly,
        ts_year = sim_data[["mon"]][["time"]][, "Year"]
      ),

      # Seasonal timing of available soil moisture `[-]`
      # (where `SWAsst = cor(monthly SWA, monthly Tmean)`)
      SWAsst = calc_CorXY_byYear(
        x = tmp_swa_monthly,
        # correlate against Tmean as does `get_SWA()`
        y = sim_data[["mon"]][["values"]][["tmean"]],
        ts_year = sim_data[["mon"]][["time"]][, "Year"]
      )
    )
  }

  res
}



#' Annual time series of ecological drought metrics
#'
#' @references Chenoweth et al. (2023)
#' Ecologically relevant moisture and temperature metrics for assessing
#' dryland ecosystem dynamics.
#' Ecohydrology, 16(3), e2509. \url{https://doi.org/10.1002/eco.2509}
#'
#' @seealso [EcologicalDroughtMetrics2023_annualClim()] and
#' [metric_EcologicalDroughtMetrics2023_annual()]
#'
#' @section Details:
#' The following functions produce all metrics used by Chenoweth et al.:
#'   * climate metrics
#'       * `metric_Climate_annual()`
#'       * `metric_AI()`
#'   * overall conditions
#'       * `metric_CWD()`
#'       * `metric_SWAat0to100cm39bar()`
#'       * `metric_TDDat5C()`
#'       * `metric_DDDat5C0to100cm30bar()`
#'       * `metric_WDDat5C0to100cm15bar()`
#'       * `metric_FrostDaysAtNeg5C()`
#'   * extreme drought
#'       * `metric_CWD()`
#'       * `metric_DDDat5C0to100cm30bar()`
#'       * `metric_DSIat0to100cm15bar()`
#'       * `metric_DSIat0to100cm15bar()`
#'   * seasonal timing
#'       * `metric_CorTempPPT()`
#'       * `metric_CWD()`
#'       * `metric_SWAat0to100cm39bar()`
#'       * `metric_WDDat5C0to100cm15bar()`
#'   * seasonal variability
#'       * `metric_CWD()`
#'       * `metric_SWAat0to100cm39bar()`
#'       * `metric_TDDat5C()`
#'
#' @return A return object where `group` contains the following
#' annual variables:
#'
# nolint start: line_length_linter
#'    * Aridity index `[mm / mm]`: `"AI"`
#'      (where `AI = PPT / PET`)
#'    * Potential evapotranspiration amount `[mm]`: `"PET"`
#'    * Mean daily air temperature `[C]`: `"Tmean"`
#'    * Minimum daily air temperature `[C]`: `"Tmin"`
#'    * Maximum daily air temperature `[C]`: `"Tmax"`
#'    * Precipitation amount `[mm]`: `"PPT"`
#'    * Seasonal timing of precipitation `[-]`: `"PPTsst"`
#'      (where `PPTsst = cor(monthly PPT, monthly Tmean)`)
#'    * Total growing degree days `[C x day]`: `"TDD"`
#'    * Seasonal variability of total growing degree days `[C x day / C x day]`: `"TDDssv"`
#'      (where `TDDssv = mean(monthly TDD) / sd(monthly TDD)`)
#'    * Warm season length `[day]`: `"GrowingSeasonDuration_(mean)|(cv)"`
#'      (where `GrowingSeasonDuration` is the longest spell of days with a positive `TDD`)
#'
#'    * Evapotranspiration amount `[mm]`: `"ET"`
#'    * Climatic water deficit amount `[mm]`: `"CWD"`
#'      (where `CWD = PPT - ET`)
#'    * 10-day extreme climatic water deficit `[mm]`: `"CWDextreme010d"`
#'      (where `CWDextreme10d` is the largest daily `CWD` averaged over 10-day periods)
#'    * Seasonal variability of climatic water deficit `[mm / mm]`: `"CWDssv"`
#'      (where `CWDssv = mean(monthly CWD) / sd(monthly CWD)`)
#'    * Seasonal timing of climatic water deficit `[-]`: `"CWDsst_"`
#'      (where `CWDsst = cor(monthly CWD, monthly Tmean)`)
#'
#'    * Dry-degree days (0-100 cm, <-3 MPa) `[C x day]`: `"DDD"`
#'    * Dry-degree days of longest spell `[C x day]`: `"DDDmaxSpell"`
#'    * Wet-degree days (0-100 cm, any >-1.5 MPa) `[C x day]`: `"WDD"`
#'    * Seasonal timing of wet-degree days `[-]`: `"WDDsst"`
#'      (where `WDDsst = cor(monthly WDD, monthly Tmean)`)
#'
#'    * Mean spell length of dry soils (0-100 cm, <-1.5 MPa) `[day]`: `"DSI"`
#'    * Number of dry soils intervals `[#]`: `"nDSI"`
#'
#'    * First fall frost (<-5 C) `[day of year]`: `"FrostFallFirst"`
#'    * Last spring frost (<-5 C) `[day of year]`: `"FrostSpringLast"`
#'
#'    * Fall recruitment onset `[day of year]`: `"RecruitmentFallOnset"`
#'    * Fall recruitment duration `[day]`: `"RecruitmentFallDuration"`
#'    * Fall recruitment wet-degree days `[C x day]`: `"RecruitmentFallWDD"`
#'    * Spring recruitment onset `[day of year]`: `"RecruitmentSpringOnset"`
#'    * Spring recruitment duration `[day]`: `"RecruitmentSpringDuration_"`
#'    * Spring recruitment wet-degree days `[C x day]`: `"RecruitmentSpringWDD"`
#'
#'    * Available soil moisture (0-100 cm, >-3.9 MPa) `[mm]`: `"SWA"`
#'    * Seasonal variability of available soil moisture `[mm / mm]`: `"SWAssv"`
#'      (where `SWAssv = mean(monthly SWA) / sd(monthly CWD)`)
#'    * Seasonal timing of available soil moisture `[-]`: `"SWAsst"`
#'      (where `SWAsst = cor(monthly SWA, monthly Tmean)`)
# nolint end: line_length_linter
#'
#' @name EcologicalDroughtMetrics2023_annual
#' @md
metric_EcologicalDroughtMetrics2023_annual <- function(
  path,
  name_sw2_run,
  id_scen_used,
  list_years_scen_used,
  out = c("ts_years", "raw"),
  zipped_runs = FALSE,
  soils,
  ...
) {
  out <- match.arg(out)

  stopifnot(check_metric_arguments(
    out = out,
    req_soil_vars = "depth_cm"
  ))

  get_EcologicalDroughtMetrics2023_annual(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    out = out,
    zipped_runs = zipped_runs,
    soils = soils,
    ...
  )
}




#' Climatologies of annual time series of ecological drought metrics
#'
#' Long-term means, standard deviations, or coefficients of variation of
#' annual values of metrics used by Chenoweth et al.
#' (see `metric_EcologicalDroughtMetrics2023_annual()`).
#'
#' @seealso [EcologicalDroughtMetrics2023_annual()] and
#' [metric_EcologicalDroughtMetrics2023_annualClim()]
#'
#' @return A return object where `group` contains the following climatologies of
#' annual variables summarized across years by means `mean`,
#' standard deviations `sd`, coefficients of variation `cv`, or
#' frequency of occurrence `frq`:
#'
# nolint start: line_length_linter
#'    * Aridity index `[mm / mm]`: `"AI_(mean)|(cv)"`
#'      (where `AI = PPT / PET`)
#'    * Potential evapotranspiration amount `[mm]`: `"PET_(mean)|(cv)"`
#'    * Mean daily air temperature `[C]`: `"Tmean_(mean)|(sd)"`
#'    * Minimum daily air temperature `[C]`: `"Tmin_(mean)|(sd)"`
#'    * Maximum daily air temperature `[C]`: `"Tmax_(mean)|(sd)"`
#'    * Precipitation amount `[mm]`: `"PPT_(mean)|(cv)"`
#'    * Seasonal timing of precipitation `[-]`: `"PPTsst_(mean)|(sd)"`
#'      (where `PPTsst = cor(monthly PPT, monthly Tmean)`)
#'    * Total growing degree days `[C x day]`: `"TDD_(mean)|(cv)"`
#'    * Seasonal variability of total growing degree days `[C x day / C x day]`: `"TDDssv_(mean)|(cv)"`
#'      (where `TDDssv = mean(monthly TDD) / sd(monthly TDD)`)
#'    * Warm season length `[day]`: `"GrowingSeasonDuration_(mean)|(cv)"`
#'      (where `GrowingSeasonDuration` is the longest spell of days with a positive `TDD`)
#'
#'    * Evapotranspiration amount `[mm]`: `"ET_(mean)|(cv)"`
#'    * Climatic water deficit amount `[mm]`: `"CWD_(mean)|(cv)"`
#'      (where `CWD = PPT - ET`)
#'    * 10-day extreme climatic water deficit `[mm]`: `"CWDextreme010d_(mean)|(cv)"`
#'      (where `CWDextreme10d` is the largest daily `CWD` averaged over 10-day periods)
#'    * Seasonal variability of climatic water deficit `[mm / mm]`: `"CWDssv_(mean)|(cv)"`
#'      (where `CWDssv = mean(monthly CWD) / sd(monthly CWD)`)
#'    * Seasonal timing of climatic water deficit `[-]`: `"CWDsst_(mean)|(sd)"`
#'      (where `CWDsst = cor(monthly CWD, monthly Tmean)`)
#'
#'    * Dry-degree days (0-100 cm, <-3 MPa) `[C x day]`: `"DDD_(mean)|(cv)"`
#'    * Dry-degree days of longest spell `[C x day]`: `"DDDmaxSpell_(mean)|(cv)"`
#'    * Wet-degree days (0-100 cm, any >-1.5 MPa) `[C x day]`: `"WDD_(mean)|(cv)"`
#'    * Seasonal timing of wet-degree days `[-]`: `"WDDsst_(mean)|(cv)"`
#'      (where `WDDsst = cor(monthly WDD, monthly Tmean)`)
#'
#'    * Mean spell length of dry soils (0-100 cm, <-1.5 MPa) `[day]`: `"DSI_(mean)|(cv)"`
#'    * Number of dry soils intervals `[#]`: `"nDSI_(mean)|(cv)"`
#'
#'    * First fall frost (<-5 C) `[day of year]`: `"FrostFallFirst_(mean)|(sd)"`
#'      Also, frequency of years with fall frost events `[# / #]`: `"FrostFall_frq"`
#'    * Last spring frost (<-5 C) `[day of year]`: `"FrostSpringLast_(mean)|(sd)"`
#'      Also, frequency of years with spring frost events `[# / #]`: `"FrostSpring_frq"`
#'    * Fall recruitment onset `[day of year]`: `"RecruitmentFallOnset_(mean)|(sd)"`
#'      Also, frequency of years with fall recruitment `[# / #]`: `"RecruitmentFall_frq"`
#'
#'    * Fall recruitment duration `[day]`: `"RecruitmentFallDuration_(mean)|(cv)"`
#'    * Fall recruitment wet-degree days `[C x day]`: `"RecruitmentFallWDD_(mean)|(cv)"`
#'    * Spring recruitment onset `[day of year]`: `"RecruitmentSpringOnset_(mean)|(sd)"`
#'      Also, frequency of years with spring recruitment `[# / #]`: `"RecruitmentSpring_frq"`
#'    * Spring recruitment duration `[day]`: `"RecruitmentSpringDuration_(mean)|(cv)"`
#'    * Spring recruitment wet-degree days `[C x day]`: `"RecruitmentSpringWDD_(mean)|(cv)"`
#'
#'    * Available soil moisture (0-100 cm, >-3.9 MPa) `[mm]`: `"SWA_(mean)|(cv)"`
#'    * Seasonal variability of available soil moisture `[mm / mm]`: `"SWAssv_(mean)|(cv)"`
#'      (where `SWAssv = mean(monthly SWA) / sd(monthly CWD)`)
#'    * Seasonal timing of available soil moisture `[-]`: `"SWAsst_(mean)|(sd)"`
#'      (where `SWAsst = cor(monthly SWA, monthly Tmean)`)
# nolint end: line_length_linter
#'
#'
#' @section Notes:
#'   * Argument `fun_aggs_across_yrs` is ignored.
#'   * Values are `NA` if any year is requested but was not simulated.
#'
#' @references Chenoweth et al. (2023)
#' Ecologically relevant moisture and temperature metrics for assessing
#' dryland ecosystem dynamics.
#' Ecohydrology, 16(3), e2509. \url{https://doi.org/10.1002/eco.2509}
#'
#' @name EcologicalDroughtMetrics2023_annualClim
#' @md
metric_EcologicalDroughtMetrics2023_annualClim <- function(
  path,
  name_sw2_run,
  id_scen_used,
  list_years_scen_used,
  out = "across_years",
  zipped_runs = FALSE,
  soils,
  ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = "depth_cm"
  ))


  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    tmp <- lapply(
      list_years_scen_used[[k1]],
      function(yrs) {
        tmpx <- get_EcologicalDroughtMetrics2023_annual(
          path = path,
          name_sw2_run = name_sw2_run,
          id_scen_used = id_scen_used[k1],
          list_years_scen_used = list(yrs),
          out = "ts_years",
          zipped_runs = zipped_runs,
          soils = soils,
          ...
        )[[1L]]


        #--- Calculate EcologicalDroughtMetrics2023
        # Long-term means, standard deviations, coefficients of variation, or
        # frequency of values across annual values
        as.matrix(c(
          #--- Climate
          # Aridity index `[mm / mm]` (where `AI = PPT / PET`)
          AI_mean = mean(tmpx["AI", , drop = TRUE]),
          AI_cv = cv(tmpx["AI", , drop = TRUE]),

          # Potential evapotranspiration `[mm]`
          PET_mean = mean(tmpx["PET", , drop = TRUE]),
          PET_cv = cv(tmpx["PET", , drop = TRUE]),

          # Mean daily air temperature `[C]`
          Tmean_mean = mean(tmpx["Tmean", , drop = TRUE]),
          Tmean_sd = sd(tmpx["Tmean", , drop = TRUE]),

          # Minimum daily air temperature `[C]`
          Tmin_mean = mean(tmpx["Tmin", , drop = TRUE]),
          Tmin_sd = sd(tmpx["Tmin", , drop = TRUE]),

          # Maximum daily air temperature `[C]`
          Tmax_mean = mean(tmpx["Tmax", , drop = TRUE]),
          Tmax_sd = sd(tmpx["Tmax", , drop = TRUE]),

          # Precipitation amount `[mm]`
          PPT_mean = mean(tmpx["PPT", , drop = TRUE]),
          PPT_cv = cv(tmpx["PPT", , drop = TRUE]),

          # Seasonal timing of precipitation `[-]`
          # (where `PPTsst = cor(monthly PPT, monthly Tmean)`)
          PPTsst_mean = mean(tmpx["PPTsst", , drop = TRUE]),
          PPTsst_sd = sd(tmpx["PPTsst", , drop = TRUE]),

          # Total growing degree days `[C x day]`
          TDD_mean = mean(tmpx["TDD", , drop = TRUE]),
          TDD_cv = cv(tmpx["TDD", , drop = TRUE]),

          # Seasonal variability of growing degree days `[C x day / C x day]`
          # (where `TDDssv = mean(monthly TDD) / sd(monthly TDD)`)
          TDDssv_mean = mean(tmpx["TDDssv", , drop = TRUE]),
          TDDssv_cv = cv(tmpx["TDDssv", , drop = TRUE]),

          # Warm season length `[day]`
          # (where `GrowingSeasonDuration` is the longest spell of days
          # with a positive `TDD`)
          GrowingSeasonDuration_mean =
            mean(tmpx["GrowingSeasonDuration", , drop = TRUE]),
          GrowingSeasonDuration_cv =
            cv(tmpx["GrowingSeasonDuration", , drop = TRUE]),


          #--- Climatic water deficit
          # Evapotranspiration `[mm]`: `"ET_(mean)|(cv)"`
          ET_mean = mean(tmpx["ET", , drop = TRUE]),
          ET_cv = cv(tmpx["ET", , drop = TRUE]),

          # Climatic water deficit `[mm]` (where `CWD = PPT - ET`)
          CWD_mean = mean(tmpx["CWD", , drop = TRUE]),
          CWD_cv = cv(tmpx["CWD", , drop = TRUE]),

          # 10-day extreme climatic water deficit `[mm]`
          # (where `CWDextreme10d` is the largest daily `CWD`
          # averaged over 10-day periods)
          CWDextreme010d_mean = mean(tmpx["CWDextreme010d", , drop = TRUE]),
          CWDextreme010d_cv = cv(tmpx["CWDextreme010d", , drop = TRUE]),

          # Seasonal variability of climatic water deficit `[mm / mm]`
          # (where `CWDssv = mean(monthly CWD) / sd(monthly CWD)`)
          CWDssv_mean = mean(tmpx["CWDssv", , drop = TRUE]),
          CWDssv_cv = cv(tmpx["CWDssv", , drop = TRUE]),

          # Seasonal timing of climatic water deficit `[-]`
          # (where `CWDsst = cor(monthly CWD, monthly Tmean)`)
          CWDsst_mean = mean(tmpx["CWDsst", , drop = TRUE]),
          CWDsst_sd = sd(tmpx["CWDsst", , drop = TRUE]),


          #--- Dry-degree days
          # Dry-degree days (0-100 cm, <-3 MPa) `[C x day]`
          DDD_mean = mean(tmpx["DDD", , drop = TRUE]),
          DDD_cv = cv(tmpx["DDD", , drop = TRUE]),

          # Dry-degree days of longest spell `[C x day]`
          DDDmaxSpell_mean = mean(tmpx["DDDmaxSpell", , drop = TRUE]),
          DDDmaxSpell_cv = cv(tmpx["DDDmaxSpell", , drop = TRUE]),

          # Wet-degree days (0-100 cm, any >-1.5 MPa) `[C x day]`
          WDD_mean = mean(tmpx["WDD", , drop = TRUE]),
          WDD_cv = cv(tmpx["WDD", , drop = TRUE]),

          # Seasonal timing of wet-degree days `[-]`
          # (where `WDDsst = cor(monthly WDD, monthly Tmean)`)
          WDDsst_mean = mean(tmpx["WDDsst", , drop = TRUE]),
          WDDsst_cv = cv(tmpx["WDDsst", , drop = TRUE]),


          #--- Dry soils intervals
          # Mean spell length of dry soils (0-100 cm, <-1.5 MPa) `[day]`
          DSI_mean = mean(tmpx["DSI", , drop = TRUE]),
          DSI_cv = cv(tmpx["DSI", , drop = TRUE]),

          # Number of dry soils intervals `[#]`
          nDSI_mean = mean(tmpx["nDSI", , drop = TRUE]),
          nDSI_cv = cv(tmpx["nDSI", , drop = TRUE]),


          #--- Frost
          # First fall frost (<-5 C) `[day of year]`
          # Also, frequency of years with fall frost events `[# / #]`
          FrostFall_frq = frq(tmpx["FrostFallFirst", , drop = TRUE]),
          FrostFallFirst_mean = mean(
            tmpx["FrostFallFirst", , drop = TRUE],
            na.rm = TRUE
          ),
          FrostFallFirst_sd = sd(
            tmpx["FrostFallFirst", , drop = TRUE],
            na.rm = TRUE
          ),

          # Last spring frost (<-5 C) `[day of year]`
          # Also, frequency of years with spring frost events `[# / #]`
          FrostSpring_frq = frq(tmpx["FrostSpringLast", , drop = TRUE]),
          FrostSpringLast_mean = mean(
            tmpx["FrostSpringLast", , drop = TRUE],
            na.rm = TRUE
          ),
          FrostSpringLast_sd = sd(
            tmpx["FrostSpringLast", , drop = TRUE],
            na.rm = TRUE
          ),


          #--- Recruitment index
          # Fall recruitment onset `[day of year]`
          # Also, frequency of years with fall recruitment `[# / #]`
          RecruitmentFall_frq = frq(
            tmpx["RecruitmentFallOnset", , drop = TRUE]
          ),
          RecruitmentFallOnset_mean = mean(
            tmpx["RecruitmentFallOnset", , drop = TRUE],
            na.rm = TRUE
          ),
          RecruitmentFallOnset_sd = sd(
            tmpx["RecruitmentFallOnset", , drop = TRUE],
            na.rm = TRUE
          ),

          # Fall recruitment duration `[day]`
          RecruitmentFallDuration_mean = mean(
            tmpx["RecruitmentFallDuration", , drop = TRUE],
            na.rm = TRUE
          ),
          RecruitmentFallDuration_cv = cv(
            tmpx["RecruitmentFallDuration", , drop = TRUE],
            na.rm = TRUE
          ),

          # Fall recruitment wet-degree days `[C x day]`
          RecruitmentFallWDD_mean = mean(
            tmpx["RecruitmentFallWDD", , drop = TRUE],
            na.rm = TRUE
          ),
          RecruitmentFallWDD_cv = cv(
            tmpx["RecruitmentFallWDD", , drop = TRUE],
            na.rm = TRUE
          ),

          # Spring recruitment onset `[day of year]`
          # Also, frequency of years with spring recruitment `[# / #]`
          RecruitmentSpring_frq = frq(
            tmpx["RecruitmentSpringOnset", , drop = TRUE]
          ),
          RecruitmentSpringOnset_mean = mean(
            tmpx["RecruitmentSpringOnset", , drop = TRUE],
            na.rm = TRUE
          ),
          RecruitmentSpringOnset_sd = sd(
            tmpx["RecruitmentSpringOnset", , drop = TRUE],
            na.rm = TRUE
          ),

          # Spring recruitment duration `[day]`
          RecruitmentSpringDuration_mean = mean(
            tmpx["RecruitmentSpringDuration", , drop = TRUE],
            na.rm = TRUE
          ),
          RecruitmentSpringDuration_cv = cv(
            tmpx["RecruitmentSpringDuration", , drop = TRUE],
            na.rm = TRUE
          ),

          # Spring recruitment wet-degree days `[C x day]`
          RecruitmentSpringWDD_mean = mean(
            tmpx["RecruitmentSpringWDD", , drop = TRUE],
            na.rm = TRUE
          ),
          RecruitmentSpringWDD_cv = cv(
            tmpx["RecruitmentSpringWDD", , drop = TRUE],
            na.rm = TRUE
          ),


          #--- Available soil moisture
          # Available soil moisture (0-100 cm, >-3.9 MPa) `[mm]`
          SWA_mean = mean(tmpx["SWA", , drop = TRUE]),
          SWA_cv = cv(tmpx["SWA", , drop = TRUE]),

          # Seasonal variability of available soil moisture `[mm / mm]`
          # (where `SWAssv = mean(monthly SWA) / sd(monthly CWD)`)
          SWAssv_mean = mean(tmpx["SWAssv", , drop = TRUE]),
          SWAssv_cv = cv(tmpx["SWAssv", , drop = TRUE]),

          # Seasonal timing of available soil moisture `[-]`
          # (where `SWAsst = cor(monthly SWA, monthly Tmean)`)
          SWAsst_mean = mean(tmpx["SWAsst", , drop = TRUE]),
          SWAsst_sd = sd(tmpx["SWAsst", , drop = TRUE])
        ))
      }
    )

    res[[k1]] <- do.call(cbind, tmp)
  }

  res
}
