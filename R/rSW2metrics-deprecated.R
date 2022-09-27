################################################################################
# rSW2metrics: Calculating metrics from output of SOILWAT2 simulations
# Copyright (C) 2021 Daniel Schlaepfer, John Bradford
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
################################################################################


#' \var{SEUG}-era extractor of \pkg{rSOILWAT2} output data
#'
#' @return A list of matrices.
#'   Each matrix contains the output for one \code{id_scen_used};
#'   rows represent the aggregation groups \code{group_by_month}, and
#'   columns represent time, e.g., calendar years.
#' @noRd
calc_univariate_from_sw2 <- function(
  path, name_sw2_run, id_scen_used,
  list_years_scen_used, group_by_month, first_month_of_year,
  zipped_runs = FALSE,
  group_label = "season",
  req_ts = TRUE,
  sw2_tp, sw2_out, sw2_var,
  varnames_are_fixed = TRUE,
  fun_across_vars,
  fun_across_time, ...
) {

  .Deprecated(new = "collect_sw2_sim_data")

  use_all_yrs <- missing(list_years_scen_used) || is.null(list_years_scen_used)
  do_buffer_yrs <- req_ts && first_month_of_year > 1

  # Output container
  res <- list()

  # Loop over scenarios
  for (k in seq_along(id_scen_used)) {

    x <- get_values_from_sw2(
      id_scen = id_scen_used[k],
      path, name_sw2_run,
      zipped_runs = zipped_runs,
      group_by_month, first_month_of_year,
      sw2_tp, sw2_out, sw2_var, varnames_are_fixed
    )

    # Convert list into data.frame before applying `fun_across_vars`
    if (is.list(x[["vals"]])) {
      nelems <- length(x[["vals"]])
      if (nelems > 1) {
        x[["vals"]] <- data.frame(x[["vals"]])
        colnames(x[["vals"]]) <- NULL

      } else {
        x[["vals"]] <- x[["vals"]][[1]]
      }
    }

    # Aggregate across columns = variables (e.g., soil layers)
    if (!missing(fun_across_vars) && !is.null(fun_across_vars)) {
      ncs <- ncol(x[["vals"]])

      if (!is.null(ncs) && ncs >= 1) {
        x[["vals"]] <- apply(x[["vals"]], 1, FUN = fun_across_vars, ...)
      }
    }

    # `x[["vals"]]` must be 1-dimensional by now;
    # if not, use appropriate `fun_across_vars`

    # Loop over set(s) of years
    has_periods <-
      !use_all_yrs &&
      isTRUE(inherits(list_years_scen_used[[k]], "list"))

    id_periods <- if (use_all_yrs) {
      1
    } else {
      if (has_periods) which(lengths(list_years_scen_used[[k]]) > 0) else 1
    }

    group_labels <- paste0(
      group_label,
      formatC(
        x[["groups_by_time"]],
        width = ceiling(log10(1 + length(unique(x[["groups_by_time"]])))),
        flag = 0
      )
    )

    res_periods <- list()

    for (k2 in seq_along(id_periods)) {
      # Subset years
      ids <- if (use_all_yrs) {
        rep(TRUE, length(x[["years"]]))

      } else {
        yrs <- if (has_periods) {
          list_years_scen_used[[k]][[id_periods[k2]]]
        } else {
          list_years_scen_used[[k]]
        }

        if (do_buffer_yrs) {
          yrs <- c(min(yrs) - 1, yrs, max(yrs) + 1)
        }

        x[["years"]] %in% yrs
      }

      # Aggregate across time
      tmp <- tapply(
        x[["vals"]][ids],
        INDEX = if (req_ts) {
          list(
            groups_by_time = group_labels[ids],
            years = x[["years"]][ids]
          )
        } else {
          list(groups_by_time = group_labels[ids])
        },
        FUN = fun_across_time,
        ...
      )

      res_periods[[k2]] <- if (do_buffer_yrs) {
        # Remove buffer years: first year (spinup) and
        # last seasonally-adjusted year (incomplete)
        tmp[, 2:(ncol(tmp) - 1), drop = FALSE]
      } else {
        tmp
      }

    }

    res[[k]] <- if (req_ts) {
      do.call(cbind, res_periods)

    } else {
      array(
        data = unlist(res_periods),
        dim = c(length(group_by_month), length(id_periods)),
        dimnames = list(
          paste0(
            group_label,
            formatC(
              group_by_month,
              width = ceiling(log10(1 + length(unique(group_by_month)))),
              flag = 0
            )
          ),
          if (use_all_yrs) {
            NULL
          } else {
            if (has_periods) {
              names(list_years_scen_used[[k]])[id_periods]
            } else {
              NULL
            }
          }
        )
      )
    }
  }

  res
}


#' @noRd
calc_multivariate_from_sw2 <- function(
  path, name_sw2_run, id_scen_used,
  list_years_scen_used, group_by_month, first_month_of_year,
  group_label = "season",
  req_ts = TRUE,
  zipped_runs = FALSE,
  sw2_tp, sw2_outs, sw2_vars,
  varnames_are_fixed = TRUE,
  funs_across_each_var,
  fun_across_time, ...
) {

  .Deprecated(new = "collect_sw2_sim_data")

  use_all_yrs <- missing(list_years_scen_used) || is.null(list_years_scen_used)
  do_buffer_yrs <- req_ts && first_month_of_year > 1

  # Output container
  res <- list()

  # Loop over scenarios
  for (k in seq_along(id_scen_used)) {

    x <- get_values_from_sw2(
      id_scen = id_scen_used[k],
      path, name_sw2_run,
      zipped_runs = zipped_runs,
      group_by_month, first_month_of_year,
      sw2_tp, sw2_outs, sw2_vars, varnames_are_fixed
    )

    # Aggregate across columns of each `sw2_vars` (e.g., soil layers)
    if (!missing(funs_across_each_var) && !is.null(funs_across_each_var)) {
      nvars <- length(sw2_vars)
      funs_across_each_var <- rep_len(funs_across_each_var, nvars)

      for (nc in seq_len(nvars)) {
        if (!is.null(funs_across_each_var[[nc]])) {
          ncs <- ncol(x[["vals"]][[nc]])

          if (!is.null(ncs) && ncs > 1) {
            x[["vals"]][[nc]] <- apply(
              X = x[["vals"]][[nc]],
              MARGIN = 1,
              FUN = funs_across_each_var[[nc]],
              ...
            )
          }
        }
      }
    }

    # Loop over set(s) of years
    has_periods <-
      !use_all_yrs &&
      isTRUE(inherits(list_years_scen_used[[k]], "list"))

    id_periods <- if (use_all_yrs) {
      1
    } else {
      if (has_periods) which(lengths(list_years_scen_used[[k]]) > 0) else 1
    }

    group_labels <- paste0(
      group_label,
      formatC(
        x[["groups_by_time"]],
        width = ceiling(log10(1 + length(unique(x[["groups_by_time"]])))),
        flag = 0
      )
    )

    res_periods <- list()

    for (k2 in seq_along(id_periods)) {
      # Subset years
      ids <- if (use_all_yrs) {
        rep(TRUE, length(x[["years"]]))

      } else {
        yrs <- if (has_periods) {
          list_years_scen_used[[k]][[id_periods[k2]]]
        } else {
          list_years_scen_used[[k]]
        }

        if (do_buffer_yrs) {
          yrs <- c(min(yrs) - 1, yrs, max(yrs) + 1)
        }

        x[["years"]] %in% yrs
      }


      # Aggregate across time
      tmp <- by(
        data = lapply(
          X = x[["vals"]],
          FUN = function(x) {
            ncs <- ncol(x)
            if (!is.null(ncs) && ncs > 1) {
              x[ids, , drop = FALSE]
            } else {
              x[ids]
            }
          }
        ),
        INDICES = if (req_ts) {
          list(
            groups_by_time = group_labels[ids],
            years = x[["years"]][ids]
          )
        } else {
          list(groups_by_time = group_labels[ids])
        },
        FUN = match.fun(fun_across_time),
        ...
      )

      tmp <- if (req_ts) {
        matrix(
          as.vector(tmp),
          nrow = length(unique(group_by_month)),
          ncol = length(unique(x[["years"]][ids])),
          dimnames = list(rownames(tmp), NULL)
        )
      } else {
        matrix(
          unlist(tmp),
          nrow = length(unique(group_by_month)),
          byrow = TRUE,
          dimnames = list(rownames(tmp), NULL)
        )
      }

      res_periods[[k2]] <- if (do_buffer_yrs) {
        # Remove buffer years: first year (spinup) and
        # last seasonally-adjusted year (incomplete)
        tmp[, 2:(ncol(tmp) - 1), drop = FALSE]
      } else {
        tmp
      }
    }

    res[[k]] <- if (req_ts) {
      do.call(cbind, res_periods)

    } else {
      array(
        data = unlist(res_periods),
        dim = c(dim(res_periods[[1]]), length(id_periods)),
        dimnames = list(
          NULL,
          NULL,
          if (use_all_yrs) {
            NULL
          } else {
            if (has_periods) {
              names(list_years_scen_used[[k]])[id_periods]
            } else {
              NULL
            }
          }
        )
      )
    }
  }

  res
}

extract_from_sw2 <- function(
  path, name_sw2_run,
  id_scen,
  years,
  sw2_tp = c("Day", "Month", "Year"),
  sw2_outs,
  sw2_vars,
  varnames_are_fixed = TRUE,
  zipped_runs = FALSE
) {

  .Deprecated(new = "collect_sw2_sim_data")

  sw2_tp <- sw2_tp[[1]]
  sw2_tp <- match.arg(sw2_tp)

  #--- Load rSOILWAT2 output object: `runDataSC`
  sim_data <- load_sw2_rda(
    path = file.path(path, name_sw2_run),
    fname = paste0("sw_output_sc", id_scen, ".RData"),
    zipped_runs = zipped_runs
  )

  #--- Extract variables
  nvars <- length(sw2_vars)
  sw2_outs <- rep_len(sw2_outs, nvars)
  varnames_are_fixed <- rep_len(varnames_are_fixed, nvars)

  # Extract slots
  x <- lapply(
    sw2_outs,
    function(k) slot(slot(sim_data[["runDataSC"]], k), sw2_tp)
  )
  x_vals <- list()

  # Subset columns
  for (k in seq_len(nvars)) {
    if (varnames_are_fixed[k]) {
      x_vals[[k]] <- x[[k]][, sw2_vars[k], drop = varnames_are_fixed[k]]
    } else {
      tmp <- grep(sw2_vars[k], colnames(x[[k]]), value = TRUE)
      x_vals[[k]] <- x[[k]][, tmp, drop = varnames_are_fixed[k]]
      colnames(x_vals[[k]]) <- tmp
    }
  }

  names(x_vals) <- sw2_vars


  #--- Extract time
  x_time <- matrix(
    NA,
    nrow = nrow(x[[1]]),
    ncol = 3L,
    dimnames = list(NULL, c("Year", "Month", "Day"))
  )

  x_time[, "Year"] <- x[[1]][, "Year"]

  if (sw2_tp == "Day") {
    x_time[, "Day"] <- x[[1]][, "Day"]

    tmp <- apply(
      X = x_time[, c("Year", "Day"), drop = FALSE],
      MARGIN = 1,
      FUN = paste0,
      collapse = "-"
    )
    # nolint start: extraction_operator_linter.
    x_time[, "Month"] <- as.POSIXlt(tmp, format = "%Y-%j", tz = "UTC")$mon + 1
    # nolint end

  } else if (sw2_tp == "Month") {
    x_time[, "Month"] <- x[[1]][, "Month"]
  }


  #--- Subset to requested years
  if (!missing(years) && length(years) > 0) {
    ids <- x_time[, "Year"] %in% years
    x_time <- x_time[ids, , drop = FALSE]
    x_vals <- lapply(
      x_vals,
      function(x) if (is.null(dim(x))) x[ids] else x[ids, , drop = FALSE]
    )
  }


  list(
    time = x_time,
    values = x_vals
  )
}
