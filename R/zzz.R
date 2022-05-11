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




.onLoad <- function(libname, pkgname) {
  #--- Define package level variables that should be hidden from package user
  # 'rSW2_glovars' is defined in rSW2metrics-package.R

  assign("tol", sqrt(.Machine[["double.eps"]]), envir = rSW2_glovars)
  assign("st_mo", seq_len(12L), envir = rSW2_glovars)

  #--- Memoization
  # `memoise::memoise` v2.0.0 recommends to memoize package functions
  # when package is loaded

  # We use `<<-` below to modify the package's namespace
  # in order to prevent build time dependencies on `memoise`
  # as recommended in <http://memoise.r-lib.org/reference/memoise.html#details>.
  # We don't modify the global environment.
  # See <https://github.com/r-lib/memoise/issues/76> for further details.

  determine_sw2_sim_time <<- memoise::memoize(
    f = determine_sw2_sim_time,
    omit_args = "x"
  )

  groupid_by_days <<- memoise::memoize(groupid_by_days)

  invisible()
}
