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


# Defunct functions are here required to have `...` as the only argument
# so that `list_all_metrics()` can correctly identify and exclude them


#--- Defunct on Jan 3, 2022 ------

#' Defunct functions
#'
#' @param ... Replacing old arguments.
#'
#'
#' @section Notes:
#'   `list_all_metrics()` uses the only argument `...`
#'   to recognize a metric as defunct.
#'
#' @name rSW2metrics-defunct
NULL

#' @rdname rSW2metrics-defunct
metric_DR <- function(...) {
  .Defunct("`metric_DR_annual()` or `metric_DR_monthly()`")
}

#' @rdname rSW2metrics-defunct
metric_ETyr <- function(...) .Defunct("metric_ET_annual")

#' @rdname rSW2metrics-defunct
metric_ET <- function(...) {
  .Defunct("`metric_ET_annual()` or `metric_ET_monthly()`")
}

#' @rdname rSW2metrics-defunct
metric_Radiation <- function(...) {
  .Defunct("`metric_Radiation_annual()` or `metric_Radiation_monthly()`")
}

#' @rdname rSW2metrics-defunct
metric_veg_biomass_v1 <- function(...) {
  .Defunct(paste(
    "`metric_metric_veg_biomass_annual_v1()` or",
    "`metric_metric_veg_biomass_monthly_v1()`"
  ))
}

#' @rdname rSW2metrics-defunct
metric_veg_biomass_v2 <- function(...) {
  .Defunct(paste(
    "`metric_metric_veg_biomass_annual_v2()` or",
    "`metric_metric_veg_biomass_monthly_v2()`"
  ))
}

#' @rdname rSW2metrics-defunct
metric_Tmean_monthly_clim <- function(...) {
  .Defunct("metric_Tmean_monthlyClim")
}

#' @rdname rSW2metrics-defunct
metric_TemperatureMean_MeanMonthly <- function(...) {
  .Defunct("metric_Tmean_monthlyClim")
}

#' @rdname rSW2metrics-defunct
metric_PPT_MeanMonthly <- function(...) {
  .Defunct("metric_PPT_monthlyClim")
}
