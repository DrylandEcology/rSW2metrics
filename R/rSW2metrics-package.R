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
#' \pkg{rSOILWAT2} or \pkg{rSFSW2}simulation experiments.
#'
#' @section Details:
#' Recommended setup:
#'   1. Copy file \var{\dQuote{Project_Parameters.R}}.
#'      Specify values for your specific project.
#'   2. Copy file \var{\dQuote{Script_to_Extract_Metric.R}}.
#'      In most cases, this script can be used without changes.
#'   3. Copy file \var{\dQuote{Script_Shell_Extracting_rSW2metrics.sh}}.
#'      Specify arguments/options and remove calls to metrics not needed.
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
#'
#' @docType package
#' @name rSW2metrics
#' @md
"_PACKAGE"


##------ Package level variables
rSW2_glovars <- new.env()


#------ Export and document all `metric_` functions
#' @exportPattern "^metric_[^\\.]"


rd_alias_metrics <- function() {
  paste("@aliases", paste(list_all_metrics(), collapse = " "))
}

rd_section_listing_metrics <- function() {
  paste(
    "\\section{List of currently available metrics:}{\n",
    "\\itemize{\n",
    paste(
      "  \\item",
      paste0("\\var{", list_all_metrics(), "}"),
      collapse = "\n"
    ),
    "\n}}"
  )
}

#' End-user functions that return a specific \code{metric}
#'
#' These functions have a name that starts with \code{metric_}.
#' They have at least the following arguments:
#' \code{path}, \code{name_sw2_run}, \code{id_scen_used},
#' \code{list_years_scen_used}, \code{out}, and \code{...}.
#'
#'
#' @param path A character string. The path to the simulation project folder
#'   that contains the individual folder of each simulated site.
#' @param name_sw2_run A character string. The name of the folder of the
#'   simulated site for which metrics are to be calculated.
#'   \pkg{rSOILWAT2} input and output is organized following conventions of
#'   \pkg{rSFSW2}, i.e., inputs for each scenario are stored in a list object
#'   named \var{\dQuote{swRunScenariosData}} which is stored
#'   on disk as a file \var{\dQuote{sw_input.RData}};
#'   and output data is stored for each scenario separately in an object
#'   named \var{\dQuote{runDataSC}}
#'   in a file \var{\dQuote{sw_output_scX.RData}} where
#'   \code{X} is the number of the scenario.
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
#'
#' @evalRd rd_section_listing_metrics()
#'
#' @eval rd_alias_metrics()
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
      "  \\item",
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
#' \code{path}, \code{name_sw2_run}, and \code{...}.
#'
#'
#' @param path A character string. The path to the simulation project folder
#'   that contains the individual folder of each simulated site.
#' @param name_sw2_run A character string. The name of the folder of the
#'   simulated site for which metrics are to be calculated.
#'   \pkg{rSOILWAT2} input and output is organized following conventions of
#'   \pkg{rSFSW2}, i.e., inputs for each scenario are stored in a list object
#'   named \var{\dQuote{swRunScenariosData}} which is stored
#'   on disk as a file \var{\dQuote{sw_input.RData}};
#'   and output data is stored for each scenario separately in an object
#'   named \var{\dQuote{runDataSC}}
#'   in a file \var{\dQuote{sw_output_scX.RData}} where
#'   \code{X} is the number of the scenario.
#' @param ... Additional arguments
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
