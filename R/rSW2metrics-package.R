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



#' Package \pkg{rSW2metrics}: Collection of functions to calculate
#' ecohydrological metrics from output created by
#' \pkg{rSOILWAT2} or \pkg{rSFSW2} simulation experiments.
#'
#' @section Details:
#' Recommended setup:
#'   1. Copy file \var{\dQuote{Project_Parameters.R}}.
#'      Specify values for your specific project.
#'   2. Copy file \var{\dQuote{Script_to_Extract_Metric.R}}.
#'      In most cases, this script can be used without changes.
#'   3. Copy file \var{\dQuote{Script_Shell_Extracting_rSW2metrics.sh}}.
#'      Specify arguments/options and select calls to requested metrics.
#'   4. Run the extraction by executing
#'      \var{\dQuote{Script_Shell_Extracting_rSW2metrics.sh}}
#'      on the command line.
#'
#' Example code for copying the three files to your project folder:
#' ```
#' file.copy(
#'   from = list.files(
#'     path = system.file("exec", package = "rSW2metrics"),
#'     full.names = TRUE
#'   ),
#'   to = PATH_TO_YOUR_PROJECT
#' )
#' ```
#'
#' @section Notes:
#' The documentation entry \code{\link{metrics}} describes inputs and outputs
#' and provides a list of available metric functions.
#'
#' @docType package
#' @name rSW2metrics
"_PACKAGE"


##------ Package level variables
rSW2_glovars <- new.env()


#------ Export and document all `metric_` functions
#' @exportPattern "^metric_[^\\.]"


md_aliases_of_metrics <- function() {
  tmp <- list_all_metrics()

  # Remove metrics that have their own dedicated documentation
  exclude_metrics <- c(
    "metric_SW2toTable_daily",
    "EcologicalDroughtMetrics2023_annual",
    "EcologicalDroughtMetrics2023_annualClim",
    "RR2022predictors_annual",
    "RR2022predictors_annualClim"
  )

  ids_remove <- unique(unlist(lapply(
    exclude_metrics,
    FUN = grep,
    x = basename(tmp)
  )))

  if (length(ids_remove) > 0L) {
    tmp <- tmp[-ids_remove]
  }

  paste("@aliases", paste(tmp, collapse = " "))
}

rd_section_list_metrics <- function() {
  paste(
    "\\section{List of currently available metrics:}{\n",
    "\\itemize{\n",
    paste(
      "  \\item", # nolint: nonportable_path_linter.
      paste0("\\code{\\link{", list_all_metrics(), "}}"),
      collapse = "\n"
    ),
    "\n}}"
  )
}



#' End-user functions that calculate a specific `metric`
#'
#' These functions have a name that starts with `metric_`.
#' They have at least the following arguments:
#' `path`, `name_sw2_run`, \code{zipped_runs}, `id_scen_used`,
#' `list_years_scen_used`, `out`, and `...`.
#'
#'
#' @param path A character string. The path to the simulation project folder
#'   that contains the individual folder of each simulated site.
#' @param name_sw2_run A character string. The name of the folder
#'   (or zip archive, see \code{zipped_runs}) of the simulated site
#'   for which metrics are to be calculated.
#'   \pkg{rSOILWAT2} input and output is organized following conventions of
#'   \pkg{rSFSW2}, i.e., inputs for each scenario are stored in a list object
#'   named \var{\dQuote{swRunScenariosData}} which is stored
#'   on disk as a file \var{\dQuote{sw_input.RData}};
#'   and output data is stored for each scenario separately in an object
#'   named \var{\dQuote{runDataSC}}
#'   in a file \var{\dQuote{sw_output_scX.RData}} where
#'   \code{X} is the number of the scenario.
#' @param zipped_runs A logical value. Describes whether \code{name_sw2_run}
#'   is a zip archive or a regular folder.
#' @param id_scen_used An integer vector. The numbers of scenarios for which
#'   metrics are to be calculated.
#' @param list_years_scen_used A list of integer vectors.
#'   Each scenario in \code{id_scen_used} must have a corresponding vector of
#'   calendar years (for which the metrics) will be calculated.
#' @param out A character string. Signaling whether the functions returns
#'   a time series of yearly values or an aggregate (e.g., mean) across years.
#'   One of \var{\dQuote{ts_years}} or \var{\dQuote{across_years}}.
#' @param soils A named list of numeric vectors. The presence of the
#'   argument \code{soils} indicates that the function in question requires
#'   soil information as inputs.
#'   The named elements include \var{\dQuote{depth_cm}},
#'   \var{\dQuote{sand_frac}}, and \var{\dQuote{clay_frac}} and
#'   contain the respective values for each soil layer at the site.
#' @param ... Additional arguments
#'
#' @return
#'   - A `data.frame` with a least two columns
#'     "site" (identifying sites) and
#'     "group"
#'     (identifying different variables, soil layers, or sub-annual time steps)
#'
#'   - If `do_collect_inputs`, then
#'     a `data.frame` where rows represent sites (identified by column "site")
#'     and columns represent soil layers
#'     (columns `varname_LX` where X is the soil layer number).
#'
#'   - Otherwise (i.e., for regular metrics), a `data.frame`:
#'       * rows represent combinations of sites
#'         (identified by column "site") and "groups"
#'         (identified by column "group" for different variables, soil layers,
#'         or sub-annual time steps:
#'         seasonal \var{\dQuote{season}}, quarterly \var{\dQuote{Q}},
#'         monthly \var{\dQuote{mon}}, daily \var{\dQuote{doy}});
#'       * values of annual time series for different scenarios are present,
#'         if `is_out_ts`, in columns with names:
#'         `scX_YYYY` (where X is the scenario number and `YYYY` is the
#'         calendar year);
#'       * values of across-year aggregations
#'         are present if the metric itself calculates them or if they were
#'         added by the option `add_aggs_across_yrs`, in columns with names
#'         `fun_scX_YYYY-ZZZZ` (where "fun" is the aggregation function,
#'         X is the scenario, `YYYY` the start year and
#'         `ZZZZ` the end year of the period
#'         across which the aggregation was calculated)
#'
#' @section Details:
#' Each metric function produces a set of variables (one to many) at a
#' time step that is specified in the name of the metric function. The default
#' time step is annual (which may be omitted from the name of the function);
#' sub-annual time steps include (case-insensitive):
#' `Seasonal` (1 to 12 seasons per year); `quarterly`; `monthly`; and `daily`.
#'
#' @section Notes:
#' The metric [metric_SW2toTable_daily()] is an exception and produces
#' a different type of output, i.e., spreadsheets of the most important
#' daily `SOILWAT2` variables, see [`SW2toTable`].
#'
#' @evalRd rd_section_list_metrics()
#'
#' @eval md_aliases_of_metrics()
#'
#' @name metrics
NULL


#------ Export and document all `collect_input_` functions
#' @exportPattern "^collect_input_[^\\.]"


rd_alias_inputcollectors <- function() {
  paste("@aliases", paste(list_all_input_collectors(), collapse = " "))
}

rd_section_listing_inputcollectors <- function() {
  paste(
    "\\section{List of currently available input collectors:}{\n",
    "\\itemize{\n",
    paste(
      "  \\item", # nolint: nonportable_path_linter.
      paste0("\\var{", list_all_input_collectors(), "}"),
      collapse = "\n"
    ),
    "\n}}"
  )
}

#' End-user functions that collect a specific \code{input}
#'
#' These functions have a name that starts with \code{collect_input_}.
#' They have at least the following arguments:
#' \code{path}, \code{name_sw2_run}, \code{zipped_runs}, and \code{...}.
#'
#'
#' @param path A character string. The path to the simulation project folder
#'   that contains the individual folder of each simulated site.
#' @param name_sw2_run A character string. The name of the folder
#'   (or zip archive, see \code{zipped_runs}) of the simulated site
#'   for which metrics are to be calculated.
#'   \pkg{rSOILWAT2} input and output is organized following conventions of
#'   \pkg{rSFSW2}, i.e., inputs for each scenario are stored in a list object
#'   named \var{\dQuote{swRunScenariosData}} which is stored
#'   on disk as a file \var{\dQuote{sw_input.RData}};
#'   and output data is stored for each scenario separately in an object
#'   named \var{\dQuote{runDataSC}}
#'   in a file \var{\dQuote{sw_output_scX.RData}} where
#'   \code{X} is the number of the scenario.
#' @param zipped_runs A logical value. Describes whether \code{name_sw2_run}
#'   is a zip archive or a regular folder.
#' @param id_scen An integer value. The scenario number for which
#'   input values are to be collected.
#' @param ... Additional arguments.
#'
#'
#' @evalRd rd_section_listing_inputcollectors()
#'
#' @eval rd_alias_inputcollectors()
#'
#' @aliases input inputs collect_input collectors
#' @name inputcollectors
NULL



##------ Import from other packages
#' @import methods
#' @importFrom stats aggregate coef complete.cases cor cov var fitted formula
#'   median na.exclude na.omit predict quantile sd weighted.mean
#' @importFrom foreach %dopar%
NULL
