
#--- Plant recruitment index:
calc_RecruitmentIndex_v2 <- function(...) {
  calc_RecruitmentIndex_v3(..., tol = 0)
}


#' Index estimating recruitment potential for perennial plants
#'
#' Recruitment potential is estimated as the sum of wet degree-days `WDD`
#' during the most favorable continuous periods with warm conditions and
#' wet near-surface soils, see [calc_MDD_daily()].
#' The function calculates the onset timing, duration and accumulated `WDD`
#' for the most favorable period in the spring and fall,
#' see Chenoweth et al. (2023) for additional details.
#'
#' Specifically, recruitment potential considers `WDD` over soil depths of
#' `recruitment_depth_range_cm` during periods that are defined
#' by starting and stopping conditions.
#'
#' The start of a suitable period is identified by a wet period (positive `WDD`)
#' that lasts at least `init_days` and accumulated `WDD` over soil depths of
#' `init_depth_range_cm` reaches at least `init_WDD`.
#'
#' The end of a suitable period is identified either
#' (i) by a dry period (positive dry degree-days `DDD`)
#' that lasts at least `stop_days_DDD` and accumulated `DDD` over soil depths of
#' `stop_depth_range_cm` reaches at least `stop_DDD`, or
#' (ii) by a period with accumulated total degree-days `TDD` of
#' less than `stop_TDD` that lasts at least `stop_days_TDD`.
#'
#' @inheritParams metrics
#' @inheritParams calc_MDD_daily
#' @param hemisphere_NS A character. Hemisphere identifies day of mid-summer
#' (July 15 on the northern hemisphere) that separates spring and fall
#' recruitment periods.
#' @param recruitment_depth_range_cm A numeric vector of length two. Soil depth
#' interval over which wet degree-days are accumulated.
#' @param Temp_limit_C A numeric value. The base temperature used to accumulate
#' degree-days.
#' @param Wet_SWP_limit_MPa A numeric value. Critical soil water potential that
#' identifies moist soil conditions.
#' @param Dry_SWP_limit_MPa A numeric value. Critical soil water potential that
#' identifies dry soil conditions.
#' @param init_WDD A numeric value.
#' @param init_days A numeric value.
#' @param init_depth_range_cm A numeric vector of length two.
#' @param stop_DDD A numeric value.
#' @param stop_days_DDD A numeric value.
#' @param stop_depth_range_cm A numeric vector of length two.
#' @param stop_TDD A numeric value.
#' @param stop_days_TDD A numeric value.
#' @param include_year A logical value.
#' A column `"Year"` is added to the output.
#' @param tol A numeric value.
#'
#' @section Details:
#' Argument `soils` uses only element `"depth_cm"`.
#'
#' @references Chenoweth et al. (2023)
#' Ecologically relevant moisture and temperature metrics for assessing
#' dryland ecosystem dynamics.
#' Ecohydrology, 16(3), e2509. \url{https://doi.org/10.1002/eco.2509}
#'
#' @examples
#' # Prepare data (here using rSOILWAT2)
#' swin <- rSOILWAT2::sw_exampleData
#' soils <- list(depth_cm = rSOILWAT2::swSoils_Layers(swin)[, "depth_cm"])
#' nSoilLayers <- length(soils[["depth_cm"]])
#'
#' sim <- rSOILWAT2::sw_exec(swin)
#' sim_data <- list(
#'   swp_daily = list(
#'     time = sim@SWPMATRIC@Day[, c("Year", "Day")],
#'     values = list(
#'       swp = sim@SWPMATRIC@Day[, paste0("Lyr_", seq_len(nSoilLayers))]
#'     )
#'   ),
#'   temp_daily = list(
#'     values = list(tmean = sim@TEMP@Day[, "avg_C"])
#'   ),
#'   swe_daily = list(
#'     values = list(swe = sim@SNOWPACK@Day[, "snowpackWaterEquivalent_cm"])
#'   )
#' )
#'
#' # Recruitment potential
#' ri <- calc_RecruitmentIndex_v3(
#'   sim_data = sim_data,
#'   soils = soils,
#'   recruitment_depth_range_cm = c(10, 20),
#'   Temp_limit_C = 5,
#'   Wet_SWP_limit_MPa = -1.5,
#'   Dry_SWP_limit_MPa = -3,
#'   init_WDD = 15,
#'   init_days = 3,
#'   init_depth_range_cm = c(0, 10),
#'   stop_DDD = 15,
#'   stop_days_DDD = 3,
#'   stop_depth_range_cm = c(0, 20),
#'   stop_TDD = 0,
#'   stop_days_TDD = 3,
#'   include_year = TRUE
#' )
#'
#' @export
calc_RecruitmentIndex_v3 <- function(
  sim_data,
  soils,
  out = c("ts_years", "raw"),
  hemisphere_NS = c("N", "S"),
  recruitment_depth_range_cm = c(5, 30),
  Temp_limit_C = 5,
  Wet_SWP_limit_MPa = -1.5,
  Dry_SWP_limit_MPa = -3,
  init_WDD = 15,
  init_days = 3,
  init_depth_range_cm = c(0, 5),
  stop_DDD = 0,
  stop_days_DDD = 0,
  stop_depth_range_cm = c(0, 30),
  stop_TDD = 0,
  stop_days_TDD = 0,
  include_year = FALSE,
  tol = sqrt(.Machine[["double.eps"]]),
  ...
) {
  stopifnot(requireNamespace("zoo", quietly = TRUE))

  out <- match.arg(out)

  hemisphere_NS <- match.arg(hemisphere_NS)
  stopifnot(hemisphere_NS == "N") #TODO: implement for southern hemisphere

  # Mid-year: summer solstice + 1 month
  # North: June solstice (Jun 20-22 = 171-173)
  # South: December solstice (Dec 20-23 = 354-357)
  doy_mid <- 196 # July 15 (in non-leap year)


  # WDD that initiates a recruitment period (germination window)
  wdd_start <- calc_MDD_daily(
    sim_data = sim_data,
    soils = soils,
    used_depth_range_cm = init_depth_range_cm,
    t_periods = list(op = `>`, limit = Temp_limit_C),
    sm_periods = list(op = `>`, limit = Wet_SWP_limit_MPa)
  )

  # WDD for recruitment
  wdd_recruit <- calc_MDD_daily(
    sim_data = sim_data,
    soils = soils,
    used_depth_range_cm = recruitment_depth_range_cm,
    t_periods = list(op = `>`, limit = Temp_limit_C),
    sm_periods = list(op = `>`, limit = Wet_SWP_limit_MPa)
  )

  N_days <- length(wdd_recruit[["values"]][[1]])

  # DDD that stops a recruitment period
  ddd_stop <- calc_MDD_daily(
    sim_data = sim_data,
    soils = soils,
    used_depth_range_cm = stop_depth_range_cm,
    t_periods = list(op = `>`, limit = Temp_limit_C),
    sm_periods = list(op = `<`, limit = Dry_SWP_limit_MPa)
  )

  # (Absence of) TDD that stops a recruitment period
  tdd_nostop <- calc_MDD_daily(
    sim_data = sim_data,
    soils = soils,
    t_periods = list(op = `>`, limit = Temp_limit_C),
    sm_periods = list(op = `>`, limit = -Inf)
  )


  # List of possible start days
  if (init_WDD <= 0) {
    ids_start <- which(wdd_start[["values"]][[1L]] > 0)

  } else {
    # (i) after `init_days` with WDD
    tmp1a <- zoo::rollsum(
      wdd_start[["values"]][[1L]] > 0,
      k = init_days,
      fill = 0,
      align = "right"
    ) >= init_days

    # (ii) and with a sum of `init_WDD`
    tmp1b <- zoo::rollsum(
      wdd_start[["values"]][[1L]],
      k = init_days,
      fill = 0,
      align = "right"
    ) >= init_WDD

    ids_start <- 1L + which(tmp1a & tmp1b)
    tmp <- length(ids_start)
    if (tmp > 0L && ids_start[tmp] > N_days) {
      ids_start[tmp] <- ids_start[tmp] - 1L
      ids_start <- unique(ids_start)
    }
  }

  # List of end/stop days due to DDD
  if (stop_DDD <= 0L) {
    ids_stop_DDD <- which(ddd_stop[["values"]][[1L]] > 0)

  } else {
    # (i) after `stop_days_DDD` with DDD
    tmp2a <- zoo::rollsum(
      ddd_stop[["values"]][[1L]] > 0,
      k = stop_days_DDD,
      fill = 0,
      align = "right"
    ) >= stop_days_DDD

    # (ii) and with a sum of `stop_DDD`
    tmp2b <- zoo::rollsum(
      ddd_stop[["values"]][[1L]],
      k = stop_days_DDD,
      fill = 0,
      align = "right"
    ) >= stop_DDD

    ids_stop_DDD <- 1L + which(tmp2a & tmp2b)
    tmp <- length(ids_stop_DDD)
    if (tmp > 0L && ids_stop_DDD[tmp] > N_days) {
      ids_stop_DDD[tmp] <- ids_stop_DDD[tmp] - 1L
      ids_stop_DDD <- unique(ids_stop_DDD)
    }
  }


  # List of end/stop days due to (absence of) TDD
  if (stop_TDD <= 0L && stop_days_TDD < 1L) {
    ids_stop_TDD <- which(tdd_nostop[["values"]][[1L]] <= tol)

  } else {
    # (i) after `stop_days_TDD` with TDD
    tmp3a <- zoo::rollsum(
      tdd_nostop[["values"]][[1L]] <= tol,
      k = stop_days_TDD,
      fill = 0,
      align = "right"
    ) >= stop_days_TDD

    # (ii) and with a sum of `stop_TDD`
    tmp3b <- zoo::rollsum(
      tdd_nostop[["values"]][[1L]],
      k = stop_days_TDD,
      fill = 0,
      align = "right"
    ) <= stop_TDD + tol

    ids_stop_TDD <- 1L + which(tmp3a & tmp3b)
    tmp <- length(ids_stop_TDD)
    if (tmp > 0L && ids_stop_TDD[tmp] > N_days) {
      ids_stop_TDD[tmp] <- ids_stop_TDD[tmp] - 1L
      ids_stop_TDD <- unique(ids_stop_TDD)
    }
  }


  # Combine all stopping days and add day after last simulated day as end day
  ids_stop <- unique(sort(c(ids_stop_DDD, ids_stop_TDD)))
  ids_stop[length(ids_stop) + 1L] <- 1L + length(ddd_stop[["values"]][[1L]])


  # List start/end of all suitable periods
  periods <- list()

  k0 <- 1L
  for (k1 in seq_along(ids_start)) {
    # Identify start day and locate earliest stop day
    tmp1 <- ids_start[k1]
    periods[[k0]] <- c(
      start = tmp1,
      end = min(ids_stop[ids_stop >= tmp1])
    )
    k0 <- k0 + 1L
  }

  periods <- do.call(rbind, periods)


  # Recruitment potential: sum of WDD within suitable soil depths
  ts_years <- unique(wdd_recruit[["time"]][, "Year"])
  jan0 <- as.Date(paste0(ts_years[[1L]] - 1L, "-12-31"))

  res <- array(
    data = 0,
    dim = c(length(ts_years), 6L + as.integer(include_year)),
    dimnames = list(NULL,
      c(
        if (include_year) "Year",
        paste0(
          rep(c("Spring", "Fall"), each = 3L),
          "Recruitment_",
          rep(c("maxWDD", "DOY", "DurationDays"), times = 2L)
        )
      )
    )
  )

  if (include_year) {
    res[, "Year"] <- ts_years
  }

  # Loop over years
  for (k1 in seq_along(ts_years)) {
    doy_mid_lyr <- doy_mid + rSW2utils::isLeapYear(ts_years[k1])

    # Identify which periods start or end during current year
    ids_yr <- which(wdd_recruit[["time"]][, "Year"] == ts_years[k1])
    ids_periods <- which(
      periods[, "start"] %in% ids_yr | periods[, "end"] %in% ids_yr
    )

    # Loop over periods in current year
    for (k2 in seq_along(ids_periods)) {
      # Identify start/end of current period
      id_mid_yr <- ids_yr[[1L]] + doy_mid_lyr - 1L
      lims <- c(
        max(ids_yr[[1L]], periods[ids_periods[k2], "start"]),
        min(ids_yr[length(ids_yr)], periods[ids_periods[k2], "end"])
      )

      # Identify maximum (cumulative) WDD (and starting DOY) of
      # periods in current year
      if (lims[[1L]] < id_mid_yr && lims[[2L]] >= id_mid_yr) {
        # Current period crosses mid-year date
        ids1 <- seq(from = lims[[1L]], to = id_mid_yr - 1L)
        ids2 <- seq(from = id_mid_yr, to = lims[[2]])
        tmp <- c(
          sum(wdd_recruit[["values"]][[1]][ids1]),
          sum(wdd_recruit[["values"]][[1]][ids2])
        )

        if (tmp[[1L]] > res[k1, "SpringRecruitment_maxWDD"]) {
          res[k1, "SpringRecruitment_maxWDD"] <- tmp[[1L]]
          res[k1, "SpringRecruitment_DOY"] <-
            as.POSIXlt(jan0 + lims[[1L]])$yday + 1L
          res[k1, "SpringRecruitment_DurationDays"] <- length(ids1)
        }

        if (tmp[[2L]] > res[k1, "FallRecruitment_maxWDD"]) {
          res[k1, "FallRecruitment_maxWDD"] <- tmp[[2L]]
          res[k1, "FallRecruitment_DOY"] <-
            as.POSIXlt(jan0 + id_mid_yr)$yday + 1L
          res[k1, "FallRecruitment_DurationDays"] <- length(ids2)
        }

      } else {
        ids <- seq(from = lims[[1L]], to = lims[[2L]])
        tmp <- sum(wdd_recruit[["values"]][[1L]][ids])

        if (all(lims < id_mid_yr)) {
          # Current period is completely before mid-year date
          if (tmp > res[k1, "SpringRecruitment_maxWDD"]) {
            # Current spring period is larger than previous ones -> replace
            res[k1, "SpringRecruitment_maxWDD"] <- tmp
            res[k1, "SpringRecruitment_DOY"] <-
              as.POSIXlt(jan0 + lims[[1L]])$yday + 1L
            res[k1, "SpringRecruitment_DurationDays"] <- length(ids)
          }

        } else if (tmp > res[k1, "FallRecruitment_maxWDD"]) {
          # Current period is completely after mid-year date
          # Current fall period is larger than previous ones -> replace
          res[k1, "FallRecruitment_maxWDD"] <- tmp
          res[k1, "FallRecruitment_DOY"] <-
            as.POSIXlt(jan0 + lims[[1L]])$yday + 1L
          res[k1, "FallRecruitment_DurationDays"] <- length(ids)
        }
      }
    }
  }

  res[res == 0] <- NA

  if (out == "ts_years") {
    res
  } else if (out == "raw") {
    list(
      res = res,
      periods = periods,
      wdd_recruit = wdd_recruit,
      wdd_start = wdd_start,
      ddd_stop = ddd_stop,
      tdd_nostop = tdd_nostop
    )
  }
}


# max WDD across recruitment events before/after mid-summer (July 15): where
# recruitment potential is accumulated WDD at 5-20 cm during intervals which
# start after 3-day periods with WDD > 0 that sum to >= 15 WDD in 0-5 cm and
# end either after 3-day periods with DDD > 0 that sum to >= 15 DDD in 0-20 cm
# or after 3-day periods with TDD == 0
metric_RecruitmentIndex_v4 <- function(
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

  init_depth_range_cm <- c(0, 5)
  recruitment_depth_range_cm <- c(5, 20)
  stop_depth_range_cm <- c(0, 20)

  # Check soil depths
  check_soillayer_availability(
    soil_depths_cm = soils[["depth_cm"]],
    used_depth_range_cm = init_depth_range_cm,
    strict = TRUE,
    type = "warn"
  )
  check_soillayer_availability(
    soil_depths_cm = soils[["depth_cm"]],
    used_depth_range_cm = recruitment_depth_range_cm,
    strict = c(TRUE, FALSE),
    type = "warn"
  )
  check_soillayer_availability(
    soil_depths_cm = soils[["depth_cm"]],
    used_depth_range_cm = stop_depth_range_cm,
    strict = c(TRUE, FALSE),
    type = "warn"
  )


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

    res[[k1]] <- t(calc_RecruitmentIndex_v3(
      sim_data = sim_data,
      soils = soils,
      out = out,
      hemisphere_NS = "N",
      recruitment_depth_range_cm = recruitment_depth_range_cm,
      Temp_limit_C = 5,
      Wet_SWP_limit_MPa = -1.5,
      Dry_SWP_limit_MPa = -3,
      init_WDD = 15,
      init_days = 3,
      init_depth_range_cm = init_depth_range_cm,
      stop_DDD = 15,
      stop_days_DDD = 3,
      stop_depth_range_cm = stop_depth_range_cm,
      stop_TDD = 0,
      stop_days_TDD = 3
    ))
  }

  res
}



# max WDD across recruitment events before/after mid-summer (July 15): where
# recruitment potential is accumulated WDD at 10-20 cm during intervals which
# start after 3-day periods with WDD > 0 that sum to >= 15 WDD in 0-10 cm and
# end either after 3-day periods with DDD > 0 that sum to >= 15 DDD in 0-20 cm
# or after 3-day periods with TDD == 0
metric_RecruitmentIndex_v5 <- function(
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

  init_depth_range_cm <- c(0, 10)
  recruitment_depth_range_cm <- c(10, 20)
  stop_depth_range_cm <- c(0, 20)

  # Check soil depths
  check_soillayer_availability(
    soil_depths_cm = soils[["depth_cm"]],
    used_depth_range_cm = init_depth_range_cm,
    strict = TRUE,
    type = "warn"
  )
  check_soillayer_availability(
    soil_depths_cm = soils[["depth_cm"]],
    used_depth_range_cm = recruitment_depth_range_cm,
    strict = c(TRUE, FALSE),
    type = "warn"
  )
  check_soillayer_availability(
    soil_depths_cm = soils[["depth_cm"]],
    used_depth_range_cm = stop_depth_range_cm,
    strict = c(TRUE, FALSE),
    type = "warn"
  )


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

    res[[k1]] <- t(calc_RecruitmentIndex_v3(
      sim_data = sim_data,
      soils = soils,
      out = out,
      hemisphere_NS = "N",
      recruitment_depth_range_cm = recruitment_depth_range_cm,
      Temp_limit_C = 5,
      Wet_SWP_limit_MPa = -1.5,
      Dry_SWP_limit_MPa = -3,
      init_WDD = 15,
      init_days = 3,
      init_depth_range_cm = init_depth_range_cm,
      stop_DDD = 15,
      stop_days_DDD = 3,
      stop_depth_range_cm = stop_depth_range_cm,
      stop_TDD = 0,
      stop_days_TDD = 3
    ))
  }

  res
}
