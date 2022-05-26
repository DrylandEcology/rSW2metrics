
#--- Helper functions ----------------------------------------------------------
check_all_output_available_of_run <- function(
  path_to_run, N_scen, check_input = TRUE
) {
  files <- list.files(file.path(path_to_run), full.names = TRUE)

  fnum <- length(files)
  fsizes <- sapply(files, function(f) file.size(f), USE.NAMES = FALSE)

  # Expect: One output file per scenario plus one file for inputs
  isTRUE(all(
    fnum == N_scen + if (check_input) 1 else 0,
    all(fsizes > 0)
  ))
}


#' Shorten full names of \pkg{rSFSW2} simulation runs
#'
#' Full names of \pkg{rSFSW2} simulation runs are composed of three parts
#' \itemize{
#'   \item a run number
#'   \item experimental treatment
#'   \item site label
#' }
#' and concatenated by \var{"_"} as separator, for instance,
#' \var{"001_Default_SiteA"}, \var{"002_Default_SiteB"}, etc.
#'
#' It may be desirable to shorten the full names in cases where only none/one
#' experimental treatment was used, for instance,
#' \var{SiteA"}, \var{"SiteB"}, etc.
#'
#' @param run_names A vector of character strings.
#' @param element_sep A character value. The separating character.
#' @param N_discard An integer value. The number of elements separated by
#'   \code{element_sep} to discard.
#'
#' @examples
#' shorten_run_names(c("001_Default_SiteA", "002_Default_SiteB"))
#'
#' @export
shorten_run_names <- function(run_names, element_sep = "_", N_discard = 2) {
  discard <- seq_len(N_discard)
  sapply(
    strsplit(run_names, split = element_sep, fixed = TRUE),
    function(x) paste(x[- discard], collapse = element_sep)
  )
}



prepare_soils_for_site <- function(
  path,
  name_sw2_run,
  name_sw2_run_soils = NULL,
  soils = NULL,
  soil_variables = NULL,
  var_soilsite = "site"
) {
  used_soil <- NULL

  if (!missing(soils) && !is.null(soils) && !is.null(name_sw2_run_soils)) {
    #--- Soils are pre-extracted: subset `soils`

    # Check that we got good soil variable names
    names_soil_variables <- names(soil_variables)

    stopifnot(
      names_soil_variables %in% names(soils),
      names_soil_variables %in% names(list_soil_variables())
    )

    # Locate site
    idss <- lapply(
      names_soil_variables,
      function(k) match(name_sw2_run_soils, soils[[k]][, var_soilsite])
    )
    names(idss) <- names_soil_variables

    stopifnot(sapply(idss, is.finite), sapply(idss, length) == 1)

    # Prepare soil variable values
    used_soil <- lapply(
      names_soil_variables,
      function(k) {
        ics <- grepl("_L[[:digit:]]+$", colnames(soils[[k]]))
        unlist(soils[[k]][idss[[k]], ics])
      }
    )
    names(used_soil) <- names_soil_variables

  } else {
    #--- Soils are not pre-extracted: read values from files
    if (is.null(soil_variables)) {
      soil_variables <- list_soil_variables()
    }
    nsv <- names(soil_variables)

    tmp_soils <- get_soillayers_variable(
      path = path,
      name_sw2_run = name_sw2_run,
      id_scen = 1,
      sw2_soil_var = nsv
    )

    used_soil <- list(
      depth_cm = if ("depth_cm" %in% nsv) tmp_soils["depth_cm", ],
      sand_frac = if ("sand_frac" %in% nsv) tmp_soils["sand_frac", ],
      clay_frac = if ("clay_frac" %in% nsv) tmp_soils["clay_frac", ],
      gravel_content = if ("gravel_content" %in% nsv) {
        tmp_soils["gravel_content", ]
      }
    )
  }

  used_soil
}



#--- Obtain input values -------------------------------------------------------

#' Determine widths (as weights) of soil layers within a zone
#'
#' @param soil_depths_cm A numeric vector.
#'   The lower depth limits of soil layers in \var{[cm]}.
#' @param used_depth_range_cm A numeric vector of length two.
#'   The upper and lower depth limit of the zone (depth range) to consider.
#' @param n_slyrs_has An integer value. The number of simulated soil layers
#'   (optional). The code throws an error if there are fewer soil layers
#'   than selected by \code{used_depth_range_cm} from \code{soil_depths_cm}.
#'
#' @return A numeric vector of the same length as \code{soil_depths_cm}.
#'   The values correspond to soil layer widths that fall within the
#'   selected zone. Other values, outside the zone, are set to \code{NA}.
#'
#' @examples
#' calc_soillayer_weights(c(5, 10, 30, 50), NULL)
#' calc_soillayer_weights(c(5, 10, 30, 50), c(0, 100))
#' calc_soillayer_weights(c(5, 10, 30, 50), c(0, 40))
#' calc_soillayer_weights(c(5, 10, 30, 50), c(20, 40))
#' calc_soillayer_weights(c(5, 10, 30, 50), c(20, 50))
#' calc_soillayer_weights(c(5, 10, 30, 50), c(20, 30), 3)
#' \dontrun{
#' calc_soillayer_weights(c(5, 10, 30, 50), c(20, 50), 3)
#' }
#'
#' @export
calc_soillayer_weights <- function(
  soil_depths_cm,
  used_depth_range_cm = NULL,
  n_slyrs_has = NULL
) {
  # if used_depth_range_cm is NULL, then ids is `logical(0)`
  ids <-
    soil_depths_cm <= used_depth_range_cm[1] |
    soil_depths_cm > used_depth_range_cm[2]

  x <- diff(c(0, soil_depths_cm))
  x[ids] <- NA

  if (!is.null(n_slyrs_has)) {
    if (isTRUE((tmp <- max(which(!is.na(x)))) > n_slyrs_has)) {
      stop(
        "Deeper soil layers requested than actually simulated:",
        "\n  * position of deepest requested soil layers = ", tmp,
        "\n  * simulated number of soil layers = ", n_slyrs_has
      )
    }

    # Make sure that returned object is not longer than `n_slyrs_has`
    if (n_slyrs_has < length(x)) {
      x <- x[seq_len(n_slyrs_has)]
    }
  }

  x
}


#' Check whether soil layers are available in a specified zone
#'
#' @inheritParams calc_soillayer_weights
#' @param strict A logical vector either of length one or equal to the
#'  length of \code{used_depth_range_cm}. \code{TRUE} indicates that
#'  the corresponding value (or all) of \code{used_depth_range_cm} is required
#'  to be either zero or one of the values of \code{soil_depths_cm}.
#' @param type A character string. If a value is not present, then either
#'  a warning is issued or an error.
#'
#' @return (Invisibly) a logical value indicating whether at least one
#'  required value is missing (\code{FALSE}) or all are present (\code{TRUE}).
#'
#' @examples
#' check_soillayer_availability(
#'   c(5, 10, 30, 50), c(0, 40),
#'   strict = TRUE,
#'   type = "warn"
#' )
#' check_soillayer_availability(
#'   c(5, 10, 30, 50), c(0, 40),
#'   strict = c(TRUE, FALSE),
#'   type = "warn"
#' )
#' check_soillayer_availability(
#'   c(5, 10, 30, 50), c(20, 30, 40),
#'   strict = TRUE,
#'   type = "warn"
#' )
#'
#' @export
check_soillayer_availability <- function(
  soil_depths_cm,
  used_depth_range_cm = NULL,
  strict = TRUE,
  type = c("warn", "error")
) {
  sim_soil_depths_cm <- unique(sort(c(0, soil_depths_cm)))

  msg <- character(0)

  if (any(strict) && !is.null(used_depth_range_cm)) {
    used_depth_range_cm <- unique(sort(used_depth_range_cm))
    strict <- rep_len(strict, length(used_depth_range_cm))
    ids <- used_depth_range_cm[strict] %in% sim_soil_depths_cm
    if (any(!ids)) {
      msg <- paste(
        "Requested soil depth(s) at",
        paste(used_depth_range_cm[strict][!ids], collapse = ", "),
        "are not available in the simulated set of soil layers of",
        paste(sim_soil_depths_cm, collapse = ", ")
      )

      if (match.arg(type) == "error") stop(msg) else warning(msg)
    }
  }

  invisible(length(msg) == 0)
}


#' Determine positions of soil layers within a zone
#'
#' @inheritParams calc_soillayer_weights
#'
#' @return An integer vector.
#'
#' @examples
#' determine_used_soillayers(c(5, 10, 30, 50))
#' determine_used_soillayers(c(5, 10, 30, 50), c(0, 100))
#' determine_used_soillayers(c(5, 10, 30, 50), c(0, 40))
#' determine_used_soillayers(c(5, 10, 30, 50), c(20, 40))
#' determine_used_soillayers(c(5, 10, 30, 50), c(20, 50))
#' determine_used_soillayers(c(5, 10, 30, 50), c(20, 30), 3)
#' \dontrun{
#' determine_used_soillayers(c(5, 10, 30, 50), c(20, 50), 3)
#' }
#'
#' @export
determine_used_soillayers <- function(
  soil_depths_cm,
  used_depth_range_cm = NULL,
  n_slyrs_has = NULL
) {

  soil_depths_cm <- soil_depths_cm[!is.na(soil_depths_cm)]

  x <- seq_along(soil_depths_cm)

  # if used_depth_range_cm is NULL, then ids is `logical(0)`
  ids <-
    soil_depths_cm <= used_depth_range_cm[1] |
    soil_depths_cm > used_depth_range_cm[2]

  if (any(ids)) {
    x <- x[which(!ids)]
  }

  if (
    !is.null(n_slyrs_has) &&
    length(x) > 0 &&
    isTRUE((tmp <- max(x)) > n_slyrs_has)
  ) {
    stop(
      "Deeper soil layers requested than actually simulated:",
      "\n  * position of deepest requested soil layers = ", tmp,
      "\n  * simulated number of soil layers = ", n_slyrs_has
    )
  }

  x
}




#------ SEUG DEVELOPMENT ERA ------
groupid_by_days <- function(
  start_year,
  end_year,
  group_by_month,
  first_month_of_year = 1L
) {
  days <- seq(
    from = ISOdate(start_year, 1, 1, tz = "UTC"),
    to = ISOdate(end_year, 12, 31, tz = "UTC"),
    by = "1 day"
  )
  months <- as.POSIXlt(days)$mon + 1
  rlels <- rle(months)[["lengths"]]

  list(
    groupid = rep(rep_len(group_by_month, length(rlels)), times = rlels),
    ids_adj_yrs = if (first_month_of_year > 1) {
      months >= first_month_of_year
    }
  )
}





weighted_mean_across_soillayers <- function(x, w) {
  ids_lyrs <- intersect(seq_along(x), which(!is.na(w)))
  weighted.mean(x[ids_lyrs], w[ids_lyrs])
}


#' Extract values, determine time series, and assign time steps to groups
#' based on an \pkg{rSOILWAT2} output as generated by \pkg{rSFSW2}
#'
#' @param id_scen An integer value. The climate scenario ID as generated by
#'   \pkg{rSFSW2}.
#' @param path A character string. The directory path where \pkg{rSFSW2}
#'   stored the \pkg{rSOILWAT2} output, by default \dQuote{3_Runs}.
#' @param name_sw2_run A character string. The \pkg{rSFSW2} name identifying
#'   a run, i.e., the full experimental label x site name. The folder name
#'   within \code{path}.
#' @param group_by_month An integer vector of length 12. The group IDs of each
#'   month. If no grouping is requested, then provide,
#'   e.g., \code{rep(0, 12)}.
#' @param first_month_of_year An integer value. The number of the month [1-12]
#'   when the "year" should start, e.g., \code{10} for "water-year".
#'   Note: incomplete years, i.e., the first and last year will be discarded.
#' @param sw2_tp A character string. One of the \pkg{rSOILWAT2} output time
#'   slots. Note: currently, only \dQuote{Month} and \dQuote{Day} are
#'   implemented.
#' @param sw2_out A character vector. One (or multiple) of the \pkg{rSOILWAT2}
#'   output groups. These will be recycled if \code{sw_var} is longer.
#' @param sw2_var A character vector. One (or multiple) of the column names
#'   of the output group(s) corresponding to the \code{sw2_out}.
#' @param varnames_are_fixed A logical value. If \code{TRUE}, then
#'   \code{sw2_var} is taken literally. Otherwise, \code{sw2_var}
#'   are interpreted as regular expressions
#'   (and may thus return multiple columns).
#'
#' @return A list with three elements: \itemize{
#'   \item \var{vals} A list with one element for each \code{sw2_var}. Each
#'         element is a vector of values for that \code{sw2_var},
#'         if \code{varnames_are_fixed} is \code{TRUE}; otherwise, a vector
#'         or matrix where columns represent all the \pkg{SOILWAT2} output
#'         corresponding to that \code{sw2_var} interpreted as
#'         regular expression.
#'         The length of vectors (or the number of rows) corresponds to the
#'         number time steps requested by \code{sw2_tp}.
#'   \item \var{year} A vector of calendar years for each time step.
#'   \item \var{groups_by_time} A vector of group ID, based on
#'         \code{group_by_month}, for each time step.
#' }
get_values_from_sw2 <- function(id_scen, path, name_sw2_run,
  group_by_month, first_month_of_year,
  sw2_tp, sw2_out, sw2_var, varnames_are_fixed = TRUE
) {

  x <- extract_from_sw2(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen = id_scen,
    sw2_tp = sw2_tp,
    sw2_outs = sw2_out,
    sw2_vars = sw2_var,
    varnames_are_fixed = varnames_are_fixed
  )

  # Grouping by seasons and
  # year-adjustment for incomplete first/last occurrence of first group
  x_years <- x[["time"]][, "Year"]

  if (sw2_tp == "Month") {
    x_groups <- rep_len(group_by_month, length(x_years))

    if (first_month_of_year > 1) {
      ids <- x[["time"]][, "Month"] >= first_month_of_year
      x_years[ids] <- x_years[ids] + 1
    }

  } else if (sw2_tp == "Day") {
    ids <- groupid_by_days(
      start_year = x_years[1],
      end_year = x_years[length(x_years)],
      group_by_month,
      first_month_of_year
    )

    x_groups <- ids[["groupid"]]

    if (first_month_of_year > 1) {
      x_years[ids[["ids_adj_yrs"]]] <- x_years[ids[["ids_adj_yrs"]]] + 1
    }
  }

  list(
    vals = x[["values"]],
    years = x_years,
    groups_by_time = x_groups
  )
}



#------ NEWRR DEVELOPMENT ERA ------


get_swp_weighted <- function(
  path, name_sw2_run, id_scen,
  years,
  soils,
  used_depth_range_cm = NULL,
  ...
) {
  warning("`get_swp_weighted()` uses matric-VWC!")

  vwc <- extract_from_sw2(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen = id_scen,
    years = years,
    sw2_tp = "Day",
    sw2_outs = "VWCMATRIC",
    sw2_vars = "Lyr",
    varnames_are_fixed = FALSE
  )

  N_days <- nrow(vwc[["values"]][[1]])

  T_by_lyr <- extract_from_sw2(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen = id_scen,
    years = years,
    sw2_tp = "Day",
    sw2_outs = "TRANSP",
    sw2_vars = "transp_total_Lyr",
    varnames_are_fixed = FALSE
  )

  widths_cm <- calc_soillayer_weights(
    soil_depths_cm = soils[["depth_cm"]],
    used_depth_range_cm = used_depth_range_cm
  )

  id_slyrs <- which(!is.na(widths_cm))
  widths_cm <- widths_cm[id_slyrs]


  tmp <- rep(NA, N_days)

  for (k in seq_len(N_days)) {
    tmp[k] <- if (sum(T_by_lyr[["values"]][[1]][k, id_slyrs]) > 0) {
      # values weighted by transpiration per layer
      rSOILWAT2::VWCtoSWP(
        vwc = weighted.mean(
          x = vwc[["values"]][[1]][k, id_slyrs],
          w = T_by_lyr[["values"]][[1]][k, id_slyrs]
        ),
        sand = weighted.mean(
          x = soils[["sand_frac"]][id_slyrs],
          w = T_by_lyr[["values"]][[1]][k, id_slyrs]
        ),
        clay = weighted.mean(
          x = soils[["clay_frac"]][id_slyrs],
          w = T_by_lyr[["values"]][[1]][k, id_slyrs]
        )
      )

    } else {
      # values weighted by layer width
      rSOILWAT2::VWCtoSWP(
        vwc = weighted.mean(
          x = vwc[["values"]][[1]][k, id_slyrs],
          w = widths_cm
        ),
        sand = weighted.mean(
          x = soils[["sand_frac"]][id_slyrs],
          w = widths_cm
        ),
        clay = weighted.mean(
          x = soils[["clay_frac"]][id_slyrs],
          w = widths_cm
        )
      )
    }
  }

  list(
    time = vwc[["time"]],
    values = list(swp_weighted = tmp)
  )
}


calc_new_yearly_aggregations <- function(
  x_daily,
  temp_monthly,
  fun_time, # sum
  fun_extreme, # max
  periods, # list(op = `>`, limit = 0),
  output = c(
    "values", "5roll", "seasonal_variability", "seasonality",
    "mean_day", "extreme_mean005day", "extreme_mean010day",
    "extreme_value_consecutive_periods",
    "mean_duration_consecutive_periods_days",
    "extreme_duration_consecutive_periods_days"
  ),
  include_year = FALSE
) {
  # one value per year
  #   - mean/sum per year
  #   - mean/sum per month: CV across months, cor(., temperature)
  #   - moving mean across 5- and 10-day windows: extreme across smoothed days

  output <- match.arg(output, several.ok = TRUE)

  stopifnot(length(x_daily[["values"]]) == 1)

  years <- unique(x_daily[["time"]][, "Year"])

  res <- matrix(
    data = NA,
    nrow = length(years),
    ncol = length(output) + as.integer(include_year),
    dimnames = list(NULL, if (include_year) c("Year", output) else output)
  )

  if (include_year) {
    res[, "Year"] <- unique(x_daily[["time"]][, "Year"])
  }

  if (any(c("values", "5roll") %in% output)) {
    stopifnot(!missing(fun_time), !is.null(fun_time))

    x_yearly <- tapply(
      X = x_daily[["values"]][[1]],
      INDEX = x_daily[["time"]][, "Year"],
      FUN = fun_time
    )

    if ("values" %in% output) {
      res[, "values"] <- x_yearly
    }

    if ("5roll" %in% output && requireNamespace("zoo", quietly = TRUE)) {
      res[, "5roll"] <- zoo::rollapply(
        data = x_yearly,
        width = 5,
        FUN = mean,
        fill = NA,
        partial = TRUE,
        align = "center"
      )
    }
  }


  if (any(c("seasonal_variability", "seasonality") %in% output)) {
    stopifnot(!missing(fun_time), !is.null(fun_time))

    x_monthly <- aggregate(
      x = x_daily[["values"]][[1]],
      by = list(
        Month = x_daily[["time"]][, "Month"],
        Year = x_daily[["time"]][, "Year"]
      ),
      FUN = fun_time
    )

    if ("seasonal_variability" %in% output) {
      res[, "seasonal_variability"] <- tapply(
        X = x_monthly[["x"]],
        INDEX = x_monthly[["Year"]],
        FUN = function(x) sd(x) / mean(x)
      )
    }

    if ("seasonality" %in% output) {
      stopifnot(
        !missing(temp_monthly),
        !is.null(temp_monthly),
        x_monthly[["Year"]] == temp_monthly[["time"]][, "Year"]
      )

      res[, "seasonality"] <- as.vector(by(
        data = cbind(x_monthly[["x"]], temp_monthly[["values"]][[1]]),
        INDICES = x_monthly[["Year"]],
        FUN = function(x) cor(x[, 1], x[, 2])
      ))
    }
  }


  if ("mean_day" %in% output) {
    res[, "mean_day"] <- tapply(
      X = x_daily[["values"]][[1]],
      INDEX = x_daily[["time"]][, "Year"],
      FUN = mean
    )
  }

  if (
    "extreme_mean005day" %in% output &&
    requireNamespace("zoo", quietly = TRUE)
  ) {
    stopifnot(!missing(fun_extreme), !is.null(fun_extreme))

    res[, "extreme_mean005day"] <- tapply(
      X = zoo::rollapply(
        data = x_daily[["values"]][[1]],
        width = 5,
        FUN = mean,
        fill = NA,
        partial = TRUE,
        align = "center"
      ),
      INDEX = x_daily[["time"]][, "Year"],
      FUN = fun_extreme
    )
  }

  if (
    "extreme_mean010day" %in% output &&
    requireNamespace("zoo", quietly = TRUE)
  ) {
    stopifnot(!missing(fun_extreme), !is.null(fun_extreme))

    res[, "extreme_mean010day"] <- tapply(
      X = zoo::rollapply(
        data = x_daily[["values"]][[1]],
        width = 10,
        FUN = mean,
        fill = NA,
        partial = TRUE,
        align = "center"
      ),
      INDEX = x_daily[["time"]][, "Year"],
      FUN = fun_extreme
    )
  }


  var_durations <- c(
    "mean_duration_consecutive_periods_days",
    "extreme_duration_consecutive_periods_days"
  )
  var_periods <- c(
    "extreme_value_consecutive_periods",
    var_durations
  )

  if (any(var_periods %in% output)) {
    stopifnot(!missing(periods), !is.null(periods))

    x_periods <- do.call(
      what = periods[["op"]],
      args = list(
        x_daily[["values"]][[1]],
        periods[["limit"]]
      )
    )


    if ("extreme_value_consecutive_periods" %in% output) {
      stopifnot(
        !missing(fun_time), !is.null(fun_time),
        !missing(fun_extreme), !is.null(fun_extreme)
      )

      res[, "extreme_value_consecutive_periods"] <- sapply(
        X = by(
          data = cbind(x_periods, x_daily[["values"]][[1]]),
          INDICES = x_daily[["time"]][, "Year"],
          FUN = function(x) {
            tmp <- rle(x[, 1])
            if (anyNA(tmp[["values"]])) {
              # Propagate NAs
              NA
            } else if (any(tmp[["values"]] == 1, na.rm = TRUE)) {
              # Create index of days for each spell when
              # `x_periods` is TRUE (i.e., `tmp[["values"]] == 1`)
              ips <- seq_along(tmp[["lengths"]])
              ids <- rep(ips, tmp[["lengths"]])
              ids[ids %in% ips[tmp[["values"]] != 1]] <- NA

              tapply(X = x[, 2], INDEX = ids, FUN = fun_time)

            } else {
              # No days when `x_periods` is TRUE
              0
            }
          }
        ),
        FUN = fun_extreme
      )
    }


    if (any(var_periods %in% output)) {
      x_consecutive_periods_days <- tapply(
        X = x_periods,
        INDEX = x_daily[["time"]][, "Year"],
        FUN = function(x) {
          tmp <- rle(x)
          if (anyNA(tmp[["values"]])) {
            # Propagate NAs
            NA
          } else if (any(tmp[["values"]] == 1, na.rm = TRUE)) {
            # Select duration of spells when
            # `x_periods` is TRUE (i.e., `tmp[["values"]] == 1`)
            tmp[["lengths"]][tmp[["values"]]]
          } else {
            # No days when `x_periods` is TRUE
            0
          }
        }
      )

      if ("mean_duration_consecutive_periods_days" %in% output) {
        res[, "mean_duration_consecutive_periods_days"] <- sapply(
          X = x_consecutive_periods_days,
          FUN = mean
        )
      }

      if ("extreme_duration_consecutive_periods_days" %in% output) {
        stopifnot(!missing(fun_extreme), !is.null(fun_extreme))

        res[, "extreme_duration_consecutive_periods_days"] <- sapply(
          X = x_consecutive_periods_days,
          FUN = fun_extreme
        )
      }
    }
  }

  res
}



#--- DOY of 10%, 50%, and 90% percentile of cumulative transpiration
#--- Annual total transpiration (mm)
calc_transp_seasonality <- function(x, time, probs) {
  aggregate(
    x = x,
    by = list(Year = time),
    FUN = function(x) {
      tdc <- cumsum(x)
      ttot <- tdc[length(tdc)]
      c(
        ttot,
        if (is.na(ttot)) {
          rep(NA, length(probs))
        } else {
          sapply(
            ttot * probs,
            function(v) which.min(v >= tdc)
          )
        }
      )
    }
  )
}



#--- DOY of two maxima of a smoothed transpiration

#' @examples
#' ## No or less than two plateaus -> central candidate from among all values
#' central_candidate(1)
#' central_candidate(1:4)
#' central_candidate(c(1, 20:29))
#'
#' ## More than one plateau -> central candidate from longest plateau
#' central_candidate(c(1, 20:30, 40:60))
#' central_candidate(c(1, 20:30, 40:60, 70:80, 100:110))
#' central_candidate(c(1, 20:30, 40:60, 70:90))
#'
#' @noRd
central_candidate <- function(ids) {
  # TODO: consider re-writing in cpp11 or Rcpp

  if (length(ids) > 1) {
    # plateaus or individual peaks?
    tmp <- ids[-1] - ids[-length(ids)] == 1 # faster than `diff(ids) == 1`

    if (any(tmp)) {
      # We have plateaus: now, count number of plateaus
      tmpp <- rle(c(tmp, FALSE))
      tmpps <- which(tmpp[["values"]])

      if (length(tmpps) > 1) {
        # We have multiple plateaus: now, identify longest plateau(s)
        tmppl <- tmpp[["lengths"]][tmpps]
        tmpi <- which(tmppl == max(tmppl))

        tmppsi <- if (length(tmpi) > 1) {
          # We have multiple longest plateaus:
          # now, select nearest even middle one among longest plateaus
          tmpps[tmpi[round(0.5 + length(tmpi) / 2)]]
        } else {
          tmpps[tmpi]
        }

        # Extract start:end of identified longest plateau
        tmpcs <- cumsum(c(1, tmpp[["lengths"]]))
        ids <- ids[tmpcs[tmppsi]:tmpcs[tmppsi + 1]]
      }
    }

    # take central value (from among all peaks or from selected plateau)
    # here, the 50%-quantile of the positions under type 1
    ids[(length(ids) - 1) %/% 2 + 1]

  } else {
    ids
  }
}

identify_simple_peaks <- function(
  x,
  window,
  fun = function(x) which.max(x) == (window + 1) / 2
) {
  stopifnot(requireNamespace("zoo", quietly = TRUE))

  which(zoo::rollapply(
    x,
    width = window,
    FUN = fun,
    align = "center",
    fill = NA
  ))
}

#' @section Notes: Peaks closer than a half-\code{window} width from either
#'   end of \code{x} will not be identified. Add an appropriate
#'   buffer around \code{x} if such peaks are important.
identify_peaks <- function(x, window, type = c("maxima", "minima")) {
  stopifnot(window %% 2L == 1L) # check that window is odd

  type <- match.arg(type)

  if (type == "maxima") {
    fpeak <- function(x) {
      tmpx <- max(x, na.rm = TRUE)
      if (is.finite(tmpx) && isTRUE(tmpx > min(x, na.rm = TRUE))) {
        central_candidate(which(x >= tmpx)) == (window + 1) / 2
      } else {
        FALSE
      }
    }

  } else {
    fpeak <- function(x) {
      tmpx <- min(x, na.rm = TRUE)
      if (is.finite(tmpx) && isTRUE(tmpx < max(x, na.rm = TRUE))) {
        central_candidate(which(x <= tmpx)) == (window + 1) / 2
      } else {
        FALSE
      }
    }
  }

  res <- identify_simple_peaks(x, window, fun = fpeak)

  # We may get runs of peaks if a peak is an almost window-wide plateau
  tmp <- diff(res) == 1
  if (any(tmp)) {
    # We have a run of peaks: select central one
    tmpp <- rle(c(tmp, FALSE))

    tmp2 <- cumsum(c(0, tmpp[["lengths"]])) + 1
    tmps <- tmp2[-length(tmp2)][tmpp[["values"]]]
    tmp <- list(
      start = tmps,
      end = tmps + tmpp[["lengths"]][tmpp[["values"]]]
    )

    for (k1 in seq_along(tmps)) {
      ids <- tmp[["start"]][k1]:tmp[["end"]][k1]
      res[ids][-round(length(ids) / 2)] <- NA
    }

    res <- as.integer(na.exclude(res))
  }

  res
}

# Identify valley bottoms between peaks
identify_valleys_between_peaks <- function(x, peaks) {
  ids_edges <- range(which(!is.na(x)))

  # Peaks located at start/end of non-NA value period
  ids_peak_on_limits <- which(peaks %in% ids_edges)

  # Number of valleys
  # (one on either side of a peak unless peak is on start/end of data)
  n_valleys <- length(peaks) + 1 - length(ids_peak_on_limits)

  valleys <- rep(NA, n_valleys)
  iv <- 1

  for (k in seq_along(peaks)) {
    # Skip to next if current peak on an edge and more than one peak
    if (k %in% ids_peak_on_limits && n_valleys > 1) next

    #--- Identify valley before peak
    if (!(peaks[k] %in% ids_edges[1]) && is.na(valleys[max(1, iv - 1)])) {
      ids_inbetween <- seq.int(
        from = if (k > 1) peaks[k - 1] else 1,
        to = peaks[k]
      )

      tmpx <- min(x[ids_inbetween], na.rm = TRUE)
      if (is.finite(tmpx) && isTRUE(tmpx < max(x, na.rm = TRUE))) {
        valleys[iv] <-
          ids_inbetween[1] - 1 +
          central_candidate(which(x[ids_inbetween] <= tmpx))
        iv <- iv + 1
      }
    }

    #--- Identify valley after peak
    if (!(peaks[k] %in% ids_edges[2])) {
      ids_inbetween <- seq.int(
        from = peaks[k],
        to = if (k == length(peaks)) length(x) else peaks[k + 1]
      )

      tmpx <- min(x[ids_inbetween], na.rm = TRUE)
      if (is.finite(tmpx) && isTRUE(tmpx < max(x, na.rm = TRUE))) {
        valleys[iv] <-
          ids_inbetween[1] - 1 +
          central_candidate(which(x[ids_inbetween] <= tmpx))
        iv <- iv + 1
      }
    }
  }

  valleys
}



peak_size_v1 <- function(pids, peak_type = c("value", "volume"), values) {
  peak_type <- match.arg(peak_type)

  if (peak_type == "value") {
    # smoothed transpiration at time of peak
    pvals <- values[pids]

  } else {
    # summed smoothed transpiration around peaks

    # Identify valley bottoms between peaks
    valleys <- identify_valleys_between_peaks(x = values, peaks = pids)

    vids <- unique(c(1, valleys, length(values)))

    # Sum smoothed transpiration from left to right valley bottoms per peak
    pvals <- sapply(
      pids,
      function(p) {
        tmp <- findInterval(p, vids)
        sum(values[vids[tmp]:vids[tmp + 1]], na.rm = TRUE)
      }
    )
  }

  pvals
}


peak_size_v2 <- function(
  pids, peak_type = c("value", "volume"), window, values
) {
  peak_type <- match.arg(peak_type)

  if (peak_type == "value") {
    # smoothed transpiration at time of peak
    pvals <- values[pids]

  } else {
    # summed smoothed transpiration around peaks

    # Identify valley bottoms between peaks
    valleys1 <- identify_valleys_between_peaks(x = values, peaks = pids)
    # Identify negated peaks
    valleys2 <- identify_peaks(x = values, window = window, type = "minima")

    vids <- sort(unique(c(1, valleys1, valleys2, length(values))))

    # Sum smoothed transpiration from left to right valley bottoms per peak
    pvals <- sapply(
      pids,
      function(p) {
        tmp <- findInterval(p, vids)
        sum(values[vids[tmp]:vids[tmp + 1]], na.rm = TRUE)
      }
    )
  }

  pvals
}


#------ ATLINKAGES DEVELOPMENT ERA ------
get_variable_in_months <- function(
  path, name_sw2_run, id_scen_used,
  list_years_scen_used,
  sw2_out, sw2_var, var_label,
  months = 1:12,
  fun_time = sum,
  var_scaler = 1,
  include_year = FALSE,
  ...
) {
  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    tmp_mon <- collect_sw2_sim_data(
      path = path,
      name_sw2_run = name_sw2_run,
      id_scen = id_scen_used[k1],
      years = list_years_scen_used[[k1]],
      output_sets = list(
        list(
          sw2_tp = "Month",
          sw2_outs = sw2_out,
          sw2_vars = sw2_var,
          varnames_are_fixed = TRUE
        )
      )
    )


    # Calculate and format
    ts_years <- unique(tmp_mon[[1]][["time"]][, "Year"])

    res[[k1]] <- format_values_to_matrix(
      x = var_scaler * unname(tapply(
        X = tmp_mon[[1]][["values"]][[sw2_var]],
        INDEX = tmp_mon[[1]][["time"]][, "Year"],
        FUN = function(x) fun_time(x[months])
      )),
      ts_years = ts_years,
      timestep = "yearly",
      out_label = var_label
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


#------ FORMAT VALUES ------
#' Convert daily, monthly, or yearly values to a matrix
#'
#' @param x A numeric vector (or a list with a vector), data.frame, or a matrix.
#'   Rows represent time and columns separate variables.
#' @param ts_years A numeric vector.
#'   The time series of years that the rows of `x` represent.
#'   If a climatology, then set `ts_years` to `NA`.
#' @param out_label A character vector. The labels representing columns of `x`.
#' @param timestep A character string.
#' @param include_year A logical value. If `TRUE` then adds a first row of
#'   years.
#'
#' @return A matrix where columns represent years and
#'   rows represent within-year values
#'   (repeated blocks if multiple elements/columns in `x`), i.e.,
#'   none for yearly,
#'   month of year 1 through 12 for monthly,
#'   or day of year 1 through 366 for daily time step.
#'   Row names are constructed from the combinations of time step,
#'   the names of `x` (if any), and `out_label` (if not `NULL`).
#'
#' @examples
#' format_values_to_matrix(
#'   data.frame(a = 1:10, b = 20:29),
#'   ts_years = 1:10,
#'   timestep = "yearly",
#'   out_label = NULL
#' )
#' format_values_to_matrix(
#'   data.frame(a = 1:10, b = 20:29),
#'   ts_years = NA,
#'   timestep = "yearly",
#'   out_label = LETTERS[1:10]
#' )
#'
#' @noRd
#' @md
format_values_to_matrix <- function(
  x,
  ts_years,
  timestep = c("daily", "monthly", "yearly"),
  out_label = NULL,
  include_year = FALSE
) {
  timestep <- match.arg(timestep)
  years <- unique(ts_years)
  is_clim <- isTRUE(is.na(years))

  if (!is.list(x)) {
    if (is.matrix(x)) {
      ns_x <- colnames(x)
      x <- as.data.frame(x)
      colnames(x) <- ns_x
    } else {
      x <- list(x)
    }
  }

  if (timestep == "daily") {
    doys366 <- seq_len(366)
    doys365 <- seq_len(365)
    x_template <- array(dim = c(366, length(years)))
  }


  #--- Determine row names
  tmp <- list(
    switch(
      EXPR = timestep,
      yearly = "annual",
      monthly = paste0("mon", formatC(seq_len(12), width = 2, flag = "0")),
      daily = paste0("doy", formatC(doys366, width = 3, flag = "0"))
    ),
    names(x),
    out_label
  )

  ns_row <- apply(
    expand.grid(tmp[lengths(tmp) > 0]),
    MARGIN = 1,
    FUN = function(x) paste0(rev(x), collapse = "_")
  )


  #--- Format as matrix (columns represent years)

  if (timestep == "yearly") {
    tmp <- unlist(x)

    res <- matrix(
      data = tmp,
      nrow = if (is_clim) {
        length(tmp)
      } else if (all(lengths(x) == length(ts_years))) {
        length(x)
      } else {
        1
      },
      ncol = length(ts_years),
      dimnames = list(ns_row, NULL),
      byrow = TRUE
    )

  } else {
    tmp <- if (timestep == "monthly") {
      lapply(
        seq_along(x),
        function(k) {
          array(
            data = x[[k]],
            dim = c(12, length(years))
          )
        }
      )

    } else if (timestep == "daily") {
      lapply(
        seq_along(x),
        function(k1) {
          xtmp <- x_template
          for (k2 in seq_along(years)) {
            if (is_clim) {
              xtmp[seq_along(x[[k1]]), k2] <- x[[k1]]
            } else {
              doys <- if (rSW2utils::isLeapYear(years[k2])) doys366 else doys365
              xtmp[doys, k2] <- x[[k1]][ts_years == years[k2]]
            }
          }
          xtmp
        }
      )
    }

    res <- do.call(rbind, tmp)
    rownames(res) <- ns_row
  }

  if (include_year) {
    res <- rbind(Year = years, res)
  }

  res
}





#------ SEPARATE DATA FROM CALCULATIONS DEVELOPMENT ERA ------
create_sw2simtime <- function(n) {
  data.frame(
    Year = rep(NA, n),
    Month = NA,
    Day = NA,
    mode = NA
  )
}

#' Prepare specified vs. requested time step `data.frame`
#'
#' @param xt A two-dimensional object with columns
#'   `Year`,
#'   `Month` (if `sw2_tp` equals "Month"), and
#'   `Day` (if `sw2_tp` equals "Day"). Each row represents one time step
#'   according to `sw2_tp`.
#' @param req_years An integer vector of Calendar years. If missing or `NULL`,
#'   then time steps contained in `xt` are used.
#'   If not missing and not `NULL` and different than years contained in `xt`,
#'   then time steps combined from requested and simulated are used.
#' @param sw2_tp A character string. The daily, monthly, or yearly time step
#'   describing content of `xt` and determining the same output time step.
#'
#' @return A `data.frame` with four columns `Year`, `Month`, `Day`, `mode`.
#'   Rows represent daily, monthly, or yearly time steps that combine
#'   requested (if `req_years` was provided) and simulated (`xt`) time steps.
#'   `mode` is `"nosim"` if a time step was not simulated (but requested),
#'   `"sim_keep"` if a time step was simulated
#'   (and requested or `req_years` is missing), or
#'   `"sim_discard"` if a time step was simulated (but not requested).
#'
#' @section Notes:
#'   The column `Day` contains day of year (and not day of month)!
#'
#' @examples
#'  xty <- data.frame(Year = 1991:2020)
#'  determine_sw2_sim_time(xty, sw2_tp = "Year")
#'  determine_sw2_sim_time(xty, req_years = 1981:2010, sw2_tp = "Year")
#'  determine_sw2_sim_time(xty, req_years = 2011:2030, sw2_tp = "Year")
#'
#' @export
#' @md
determine_sw2_sim_time <- function(
  xt,
  req_years = NULL,
  sw2_tp = c("Day", "Month", "Year")
) {
  # debugging un-memoized version:
  # assignInNamespace("determine_sw2_sim_time", environment(rSW2metrics::determine_sw2_sim_time)$`_f`, "rSW2metrics") # nolint

  sw2_tp <- match.arg(sw2_tp)

  years_sim <- unique(xt[, "Year"])
  has_req_yrs <-
    !missing(req_years) && length(req_years) > 0 &&
    !setequal(req_years, years_sim)

  years_used <- if (has_req_yrs) {
    sort(unique(c(req_years, years_sim)))
  } else {
    years_sim
  }


  if (sw2_tp == "Year") {
    x_time <- create_sw2simtime(n = length(years_used))

    x_time[, "Year"] <- years_used

    if (has_req_yrs) {
      ids_sim <- x_time[, "Year"] %in% years_sim
    }

  } else if (sw2_tp == "Month") {
    x_time <- create_sw2simtime(n = 12 * length(years_used))

    x_time[, "Year"] <- rep(years_used, each = 12)
    x_time[, "Month"] <- seq_len(12)

    if (has_req_yrs) {
      ids_sim <-
        paste0(x_time[, "Year"], "-", x_time[, "Month"]) %in%
        paste0(xt[, "Year"], "-", xt[, "Month"])
    }

  } else if (sw2_tp == "Day") {
    # Create `x_time` for all used years
    req_ts_days <- as.POSIXlt(seq(
      from = ISOdate(min(years_used), 1, 1, tz = "UTC"),
      to = ISOdate(max(years_used), 12, 31, tz = "UTC"),
      by = "1 day"
    ))

    x_time <- create_sw2simtime(n = length(req_ts_days))

    x_time[, "Year"] <- 1900 + req_ts_days$year
    x_time[, "Month"] <- 1 + req_ts_days$mon
    x_time[, "Day"] <- 1 + req_ts_days$yday


    # Apparently, SW2 output can be generated with incorrect leap/nonleap-years;
    # if so, then the last simulated day of a nonleap year would
    # incorrectly be the 366-th day (which in reality doesn't exist) and
    # for which `as.POSIXlt()` correctly produces an NA (with a warning);
    # similarly, the last simulated day of a leap year would
    # incorrectly be the 365-th day (and the 366th-day would be missing)
    # --> add simulated non-existing leap-days &
    #     remove not simulated existing leap years
    tmp_sim_seq <- paste0(xt[, "Year"], "-", xt[, "Day"])
    sim_ts_days <- as.POSIXlt(tmp_sim_seq, format = "%Y-%j", tz = "UTC")
    has_leap_issues <- anyNA(sim_ts_days)

    if (has_leap_issues) {
      # Create `x_time` for simulated time period
      x_time2 <- create_sw2simtime(n = nrow(xt))
      x_time2[, "Year"] <- xt[, "Year"]
      x_time2[, "Day"] <- xt[, "Day"]
      x_time2[, "Month"] <- sim_ts_days$mon + 1
      x_time2[is.na(sim_ts_days), "Month"] <- 12

      # Add requested but not simulated years from `x_time`
      if (has_req_yrs) {
        ids_notsim <- x_time[, "Year"] %in% setdiff(req_years, years_sim)
        tmp <- merge(
          x_time[ids_notsim, , drop = FALSE],
          x_time2,
          all = TRUE,
          sort = FALSE
        )
        x_time <- tmp[order(tmp$Year, tmp$Day), , drop = FALSE]
        rownames(x_time) <- NULL

      } else {
        # All used years are simulated years
        x_time <- x_time2
      }
    }

    if (has_req_yrs) {
      ids_sim <-
        paste0(x_time[, "Year"], "-", x_time[, "Day"]) %in% tmp_sim_seq
    }
  }


  if (has_req_yrs) {
    ids_req <- x_time[, "Year"] %in% req_years
    x_time[ids_sim & ids_req, "mode"] <- "sim_keep"
    x_time[ids_sim & !ids_req, "mode"] <- "sim_discard"
    x_time[!ids_sim & ids_req, "mode"] <- "nosim"

  } else {
    x_time[, "mode"] <- "sim_keep"
  }

  stopifnot(
    !anyNA(x_time[, "mode"]),
    nrow(x_time) >= nrow(xt)
  )

  x_time
}


collect_sw2_sim_data <- function(
  path,
  name_sw2_run,
  id_scen,
  years,
  output_sets = list(
    list(
      sw2_tp = c("Day", "Month", "Year"),
      sw2_outs = NA_character_,
      sw2_vars = NA_character_,
      varnames_are_fixed = TRUE
    )
  ),
  fail = TRUE
) {
  n_sets <- length(output_sets)

  for (k in seq_len(n_sets)) {
    out <- output_sets[[k]]
    stopifnot(
      length(out[["sw2_tp"]]) == 1,
      out[["sw2_tp"]] %in% c("Day", "Month", "Year"),
      length(out[["sw2_outs"]]) %in% c(1L, length(out[["sw2_vars"]])),
      length(out[["varnames_are_fixed"]]) %in% c(1L, length(out[["sw2_vars"]]))
    )
  }


  #--- Load rSOILWAT2 output object: `runDataSC`
  sim_data <- new.env(parent = emptyenv())
  load(
    file = file.path(
      path,
      name_sw2_run,
      paste0("sw_output_sc", id_scen, ".RData")
    ),
    envir = sim_data
  )

  #--- Extract variables
  res <- vector(mode = "list", length = n_sets)
  if (length(names(output_sets)) > 0) {
    names(res) <- names(output_sets)
  }

  for (k1 in seq_len(n_sets)) {
    out <- output_sets[[k1]]

    nvars <- length(out[["sw2_vars"]])
    sw2_outs <- rep_len(out[["sw2_outs"]], nvars)
    fixed <- rep_len(out[["varnames_are_fixed"]], nvars)

    # Extract slots
    x <- lapply(
      sw2_outs,
      function(ko) slot(slot(sim_data[["runDataSC"]], ko), out[["sw2_tp"]])
    )
    x_vals <- list()

    # Subset columns
    for (k2 in seq_len(nvars)) {
      if (fixed[k2]) {
        x_vals[[k2]] <- try(
          x[[k2]][, out[["sw2_vars"]][k2], drop = fixed[k2]],
          silent = TRUE
        )

      } else {
        tmp <- grep(out[["sw2_vars"]][k2], colnames(x[[k2]]), value = TRUE)
        if (length(tmp) > 0) {
          x_vals[[k2]] <- x[[k2]][, tmp, drop = fixed[k2]]
          colnames(x_vals[[k2]]) <- tmp
        } else {
          x_vals[[k2]] <- try(stop("variables not found"), silent = TRUE)
        }
      }

      if (inherits(x_vals[[k2]], "try-error")) {
        if (fail) {
          stop(x_vals[[k2]])
        } else {
          x_vals[[k2]] <- array(
            dim = c(nrow(x[[k2]]), 1),
            dimnames = list(NULL, out[["sw2_vars"]][k2])
          )
        }
      }
    }

    nv <- names(out[["sw2_vars"]])
    names(x_vals) <- if (is.null(nv)) {
      out[["sw2_vars"]]
    } else {
      tmp <- nv
      ids <- nchar(nv) == 0
      if (any(ids)) tmp[ids] <- out[["sw2_vars"]][ids]
      tmp
    }


    #--- Extract time
    # `determine_sw2_sim_time()` is memoized and a missing `years` passed to
    # argument `req_years` doesn't work correctly
    # (see https://github.com/r-lib/memoise/issues/19)
    x_time <- determine_sw2_sim_time(
      xt = x[[1]],
      req_years = if (missing(years)) NULL else years,
      sw2_tp = out[["sw2_tp"]]
    )

    n_nosim <- sum(x_time[, "mode"] == "nosim")


    #--- Add entries for requested but not simulated time steps "nosim"
    if (n_nosim > 0) {
      n_sim <- nrow(x_time) - n_nosim
      ids <- if (isTRUE(x_time[1, "mode"] == "nosim")) {
        # "nosim" occurs before "sim_keep"
        c(rep(NA, n_nosim), seq_len(n_sim))
      } else {
        # "nosim" occurs after "sim_keep"
        c(seq_len(n_sim), rep(NA, n_nosim))
      }

      x_vals <- lapply(
        x_vals,
        function(x) if (is.null(dim(x))) x[ids] else x[ids, , drop = FALSE]
      )
    }


    #--- Removes entries for un-requested but simulated time steps "sim_discard"
    ids <- which(x_time[, "mode"] == "sim_discard")
    if (length(ids) > 0) {
      x_time <- x_time[-ids, , drop = FALSE]
      x_vals <- lapply(
        x_vals,
        function(x) if (is.null(dim(x))) x[-ids] else x[-ids, , drop = FALSE]
      )
    }


    res[[k1]] <- list(
      time = x_time,
      values = x_vals
    )
  }

  res
}
