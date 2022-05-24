
#' Across-year average daily mean air temperature [C]
#' @section Notes: Un-simulated but requested time steps propagate NAs.
#' @noRd
metric_Tmean_dailyClim <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "across_years",
  fun_aggs_across_yrs = mean,
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
            day = list(
              sw2_tp = "Day",
              sw2_outs = "TEMP",
              sw2_vars = c(tmean = "avg_C"),
              varnames_are_fixed = TRUE
            )
          )
        )

        format_values_to_matrix(
          x = calc_climatology(
            X = sim_data[["day"]][["values"]][["tmean"]],
            INDEX = sim_data[["day"]][["time"]][, "Day"],
            FUN = fun_aggs_across_yrs
          ),
          ts_years = NA,
          timestep = "daily",
          out_label = "Tmean_C"
        )
      }
    )

    res[[k1]] <- do.call(cbind, tmp)
  }

  res
}


#' Across-year average daily precipitation amount [mm]
#' @section Notes: Un-simulated but requested time steps propagate NAs.
#' @noRd
metric_PPT_dailyClim <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "across_years",
  fun_aggs_across_yrs = mean,
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
            day = list(
              sw2_tp = "Day",
              sw2_outs = "PRECIP",
              sw2_vars = "ppt",
              varnames_are_fixed = TRUE
            )
          )
        )

        format_values_to_matrix(
          x = calc_climatology(
            X = 10 * sim_data[["day"]][["values"]][["ppt"]],
            INDEX = sim_data[["day"]][["time"]][, "Day"],
            FUN = fun_aggs_across_yrs
          ),
          ts_years = NA,
          timestep = "daily",
          out_label = "PPT_mm"
        )
      }
    )

    res[[k1]] <- do.call(cbind, tmp)
  }

  res
}



#' Across-year average daily SWA amount [mm] at 0-20 cm depth above -3.9 MPa
#' @noRd
metric_SWAat0to020cm39bar_dailyClim <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "across_years",
  fun_aggs_across_yrs = mean,
  soils,
  ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = c("depth_cm", "sand_frac", "clay_frac", "gravel_content")
  ))

  used_depth_range_cm <- c(0, 20)

  check_soillayer_availability(
    soil_depths_cm = soils[["depth_cm"]],
    used_depth_range_cm = used_depth_range_cm,
    strict = TRUE,
    type = "warn"
  )

  get_SWA_daily(
    path, name_sw2_run, id_scen_used,
    list_years_scen_used = list_years_scen_used,
    out = out,
    fun_aggs_across_yrs = fun_aggs_across_yrs,
    out_label = "SWAat0to020cm39bar_mm",
    soils = soils,
    used_depth_range_cm = used_depth_range_cm,
    SWP_limit_MPa = -3.9
  )
}


#' Across-year average daily SWA amount [mm] at 20-100 cm depth above -3.9 MPa
#' @noRd
metric_SWAat20to100cm39bar_dailyClim <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "across_years",
  fun_aggs_across_yrs = mean,
  soils,
  ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = c("depth_cm", "sand_frac", "clay_frac", "gravel_content")
  ))

  used_depth_range_cm <- c(20, 100)

  check_soillayer_availability(
    soil_depths_cm = soils[["depth_cm"]],
    used_depth_range_cm = used_depth_range_cm,
    strict = TRUE,
    type = "warn"
  )

  get_SWA_daily(
    path, name_sw2_run, id_scen_used,
    list_years_scen_used = list_years_scen_used,
    out = out,
    fun_aggs_across_yrs = fun_aggs_across_yrs,
    out_label = "SWAat20to100cm39bar_mm",
    soils = soils,
    used_depth_range_cm = used_depth_range_cm,
    SWP_limit_MPa = -3.9
  )
}




#' Determine soil water potential \var{SWP}
#'
#' @section Details:
#' After soil moisture has been aggregated across soil layers
#' if requested (`method` is `"across_profile"`) and
#' aggregated across years if requested (`out` is `"across_years"`), then
#' a pedotransfer function of the water release curve is used to
#' translate volumetric water content `VWC` into `SWP`, i.e.,
#' \deqn{SWP[i,matric] = f(VWC, soil texture[i])}
#' where soil texture is the weight fractions of sand, clay, and silt of
#' the matric soil component.
#' Note, the translation is only valid for the matric soil, i.e.,
#' the component without coarse fragments.
#'
#'
#' @param sim_swc_daily A numeric two-dimensional object with
#'    daily \var{"swc"} for each soil layer in units of centimeters.
#' @param time A numeric vector. Time for each row of `sim_swc_daily`.
#' @param soils A named list with soil parameters \var{"depth_cm"},
#'   \var{"sand_frac"},\var{"clay_frac"}, and \var{"gravel_content"}
#'   as numeric vectors with values for each soil layer.
#' @param used_depth_range_cm A numeric vector of length two.
#' @param method A character string.
#' @param out A character string.
#' @param fun_aggs_across_yrs A function that calculates across-year
#'   summaries that returns a named or unnamed vector.
#'
#' @return A list with elements "time" and "values" where values represent
#'  soil water potential in units `MPa`:
#'   * if \code{method} is \var{\dQuote{across_profile}},
#'     then summed across \code{used_depth_range_cm},
#'   * if \code{method} is \var{\dQuote{by_layer}},
#'     then columns contain values for each soil layer within
#'     \code{used_depth_range_cm};
#'   * if `out` is `"ts_years`,
#'     then rows contain values for each time step,
#'   * if `out` is `"across_years"`,
#'     then rows contain averaged values across unique time steps.
#'
#' @md
calc_SWP_MPa <- function(
  sim_swc_daily,
  time,
  soils,
  used_depth_range_cm = NULL,
  method = c("across_profile", "by_layer"),
  out = c("ts_years", "across_years"),
  fun_aggs_across_yrs = mean
) {
  method <- match.arg(method)
  out <- match.arg(out)

  stopifnot(length(time) == nrow(sim_swc_daily))

  widths_cm <- calc_soillayer_weights(
    soil_depths_cm = soils[["depth_cm"]],
    used_depth_range_cm = used_depth_range_cm,
    n_slyrs_has = ncol(sim_swc_daily)
  )

  id_slyrs <- which(!is.na(widths_cm))

  if (length(id_slyrs) > 0) {
    widths_cm <- widths_cm[id_slyrs]

    if (method == "across_profile") {
      # (i) aggregate SWC [cm] across soil layers (if requested)
      x <- rowSums(
        sim_swc_daily[, id_slyrs, drop = FALSE]
      )

      # (ii) convert SWC [cm] to matric VWC [cm/cm] (coarse fragments!)
      x <- x / sum(widths_cm * (1 - soils[["gravel_content"]][id_slyrs]))

      sand_used <- weighted.mean(soils[["sand_frac"]][id_slyrs], widths_cm)
      clay_used <- weighted.mean(soils[["clay_frac"]][id_slyrs], widths_cm)

    } else if (method == "by_layer") {
      # (ii) convert SWC [cm] to matric VWC [cm/cm] (coarse fragments!)
      x <- sweep(
        sim_swc_daily[, id_slyrs, drop = FALSE],
        MARGIN = 2,
        STATS = widths_cm * (1 - soils[["gravel_content"]][id_slyrs]),
        FUN = "/"
      )

      colnames(x) <- paste0("L", seq_len(ncol(x)))

      sand_used <- soils[["sand_frac"]][id_slyrs]
      clay_used <- soils[["clay_frac"]][id_slyrs]
    }


    # (iii) aggregate across years (if requested)
    if (out == "across_years") {
      x <- calc_climatology(X = x, INDEX = time, FUN = fun_aggs_across_yrs)
      used_time <- unique(time)

    } else if (out == "ts_years") {
      used_time <- time
    }

    # (iv) translate matric VWC to SWP [MPa]
    values <- as.data.frame(rSOILWAT2::VWCtoSWP(
      as.matrix(x),
      sand = sand_used,
      clay = clay_used
    ))
    # `rSOILWAT2::VWCtoSWP()` currently removes column names
    colnames(values) <- colnames(x)

  } else {
    # No soil layers in the depth range
    used_time <- switch(out, ts_years = time, across_years = unique(time))
    values <- as.data.frame(rep_len(NA, length.out = length(used_time)))
  }

  list(
    time = used_time,
    values = values
  )
}



# soils must have "depth_cm", "sand_frac", "clay_frac", and "gravel_content"
get_SWP_daily <- function(
  path, name_sw2_run, id_scen_used,
  list_years_scen_used,
  soils,
  used_depth_range_cm = NULL,
  method = "across_profile",
  out = c("ts_years", "across_years"),
  fun_aggs_across_yrs = mean,
  out_label = "SWP_MPa",
  include_year = FALSE,
  ...
) {
  out <- match.arg(out)
  method <- match.arg(method)

  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    tmp <- lapply(
      if (out == "across_years") {
        list_years_scen_used[[k1]]
      } else {
        list(list_years_scen_used[[k1]])
      },
      function(yrs) {
        sim_data <- collect_sw2_sim_data(
          path = path,
          name_sw2_run = name_sw2_run,
          id_scen = id_scen_used[k1],
          years = yrs,
          output_sets = list(
            swc_daily = list(
              sw2_tp = "Day",
              sw2_outs = "SWCBULK",
              sw2_vars = c(swc = "Lyr"),
              varnames_are_fixed = FALSE
            )
          )
        )

        swp_daily <- calc_SWP_MPa(
          sim_swc_daily = sim_data[["swc_daily"]][["values"]][["swc"]],
          time = sim_data[["swc_daily"]][["time"]][, "Day"],
          soils = soils,
          used_depth_range_cm = used_depth_range_cm,
          method = "across_profile",
          out = out,
          fun_aggs_across_yrs = fun_aggs_across_yrs
        )

        if (out == "across_years") {
          format_values_to_matrix(
            x = swp_daily[["values"]],
            ts_years = NA,
            timestep = "daily",
            out_label = out_label
          )
        } else {
          format_values_to_matrix(
            x = swp_daily[["values"]],
            ts_years = sim_data[["swc_daily"]][["time"]][["Year"]],
            timestep = "daily",
            out_label = out_label,
            include_year = include_year
          )
        }
      }
    )

    res[[k1]] <- if (out == "across_years") {
      do.call(cbind, tmp)
    } else {
      tmp[[1]]
    }
  }

  res
}


#' Across-year average daily SWP [MPa] at 0-20 cm depth
#' @noRd
metric_SWPat0to020cm_dailyClim <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "across_years",
  fun_aggs_across_yrs = mean,
  soils,
  ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = c("depth_cm", "sand_frac", "clay_frac", "gravel_content")
  ))

  used_depth_range_cm <- c(0, 20)

  check_soillayer_availability(
    soil_depths_cm = soils[["depth_cm"]],
    used_depth_range_cm = used_depth_range_cm,
    strict = TRUE,
    type = "warn"
  )

  get_SWP_daily(
    path, name_sw2_run, id_scen_used,
    list_years_scen_used = list_years_scen_used,
    out = out,
    fun_aggs_across_yrs = fun_aggs_across_yrs,
    out_label = "SWPat0to020cm_MPa",
    soils = soils,
    used_depth_range_cm = used_depth_range_cm
  )
}


#' Across-year average daily SWP [MPa] at 20-100 cm depth
#' @noRd
metric_SWPat20to100cm_dailyClim <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "across_years",
  fun_aggs_across_yrs = mean,
  soils,
  ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = c("depth_cm", "sand_frac", "clay_frac", "gravel_content")
  ))

  used_depth_range_cm <- c(20, 100)

  check_soillayer_availability(
    soil_depths_cm = soils[["depth_cm"]],
    used_depth_range_cm = used_depth_range_cm,
    strict = TRUE,
    type = "warn"
  )

  get_SWP_daily(
    path, name_sw2_run, id_scen_used,
    list_years_scen_used = list_years_scen_used,
    out = out,
    fun_aggs_across_yrs = fun_aggs_across_yrs,
    out_label = "SWPat20to100cm_MPa",
    soils = soils,
    used_depth_range_cm = used_depth_range_cm
  )
}
