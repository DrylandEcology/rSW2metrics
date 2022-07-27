
#--- Plant recruitment index:
calc_RecruitmentIndex_v2 <- function(...) {
  calc_RecruitmentIndex_v3(..., tol = 0)
}

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
    ids_start <- which(wdd_start[["values"]][[1]] > 0)

  } else {
    # (i) after `init_days` with WDD
    tmp1a <- zoo::rollsum(
      wdd_start[["values"]][[1]] > 0,
      k = init_days,
      fill = 0,
      align = "right"
    ) >= init_days

    # (ii) and with a sum of `init_WDD`
    tmp1b <- zoo::rollsum(
      wdd_start[["values"]][[1]],
      k = init_days,
      fill = 0,
      align = "right"
    ) >= init_WDD

    ids_start <- 1 + which(tmp1a & tmp1b)
    tmp <- length(ids_start)
    if (tmp > 0 && ids_start[tmp] > N_days) {
      ids_start[tmp] <- ids_start[tmp] - 1
      ids_start <- unique(ids_start)
    }
  }

  # List of end/stop days due to DDD
  if (stop_DDD <= 0) {
    ids_stop_DDD <- which(ddd_stop[["values"]][[1]] > 0)

  } else {
    # (i) after `stop_days_DDD` with DDD
    tmp2a <- zoo::rollsum(
      ddd_stop[["values"]][[1]] > 0,
      k = stop_days_DDD,
      fill = 0,
      align = "right"
    ) >= stop_days_DDD

    # (ii) and with a sum of `stop_DDD`
    tmp2b <- zoo::rollsum(
      ddd_stop[["values"]][[1]],
      k = stop_days_DDD,
      fill = 0,
      align = "right"
    ) >= stop_DDD

    ids_stop_DDD <- 1 + which(tmp2a & tmp2b)
    tmp <- length(ids_stop_DDD)
    if (tmp > 0 && ids_stop_DDD[tmp] > N_days) {
      ids_stop_DDD[tmp] <- ids_stop_DDD[tmp] - 1
      ids_stop_DDD <- unique(ids_stop_DDD)
    }
  }


  # List of end/stop days due to (absence of) TDD
  if (stop_TDD <= 0 && stop_days_TDD < 1) {
    ids_stop_TDD <- which(tdd_nostop[["values"]][[1]] <= tol)

  } else {
    # (i) after `stop_days_TDD` with TDD
    tmp3a <- zoo::rollsum(
      tdd_nostop[["values"]][[1]] <= tol,
      k = stop_days_TDD,
      fill = 0,
      align = "right"
    ) >= stop_days_TDD

    # (ii) and with a sum of `stop_TDD`
    tmp3b <- zoo::rollsum(
      tdd_nostop[["values"]][[1]],
      k = stop_days_TDD,
      fill = 0,
      align = "right"
    ) <= stop_TDD + tol

    ids_stop_TDD <- 1 + which(tmp3a & tmp3b)
    tmp <- length(ids_stop_TDD)
    if (tmp > 0 && ids_stop_TDD[tmp] > N_days) {
      ids_stop_TDD[tmp] <- ids_stop_TDD[tmp] - 1
      ids_stop_TDD <- unique(ids_stop_TDD)
    }
  }


  # Combine all stopping days and add day after last simulated day as end day
  ids_stop <- unique(sort(c(ids_stop_DDD, ids_stop_TDD)))
  ids_stop[length(ids_stop) + 1] <- 1 + length(ddd_stop[["values"]][[1]])


  # List start/end of all suitable periods
  periods <- list()

  k0 <- 1
  for (k1 in seq_along(ids_start)) {
    # Identify start day and locate earliest stop day
    tmp1 <- ids_start[k1]
    periods[[k0]] <- c(
      start = tmp1,
      end = min(ids_stop[ids_stop >= tmp1])
    )
    k0 <- k0 + 1
  }

  periods <- do.call(rbind, periods)


  # Recruitment potential: sum of WDD within suitable soil depths
  ts_years <- unique(wdd_recruit[["time"]][, "Year"])
  jan0 <- as.Date(paste0(ts_years[[1]] - 1, "-12-31"))

  res <- array(
    data = 0,
    dim = c(length(ts_years), 6 + as.integer(include_year)),
    dimnames = list(NULL,
      c(
        if (include_year) "Year",
        paste0(
          rep(c("Spring", "Fall"), each = 3),
          "Recruitment_",
          rep(c("maxWDD", "DOY", "DurationDays"), times = 2)
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
      id_mid_yr <- ids_yr[[1]] + doy_mid_lyr - 1
      lims <- c(
        max(ids_yr[[1]], periods[ids_periods[k2], "start"]),
        min(ids_yr[length(ids_yr)], periods[ids_periods[k2], "end"])
      )

      # Identify maximum (cumulative) WDD (and starting DOY) of
      # periods in current year
      if (lims[[1]] < id_mid_yr && lims[[2]] >= id_mid_yr) {
        # Current period crosses mid-year date
        ids1 <- seq(from = lims[[1]], to = id_mid_yr - 1)
        ids2 <- seq(from = id_mid_yr, to = lims[[2]])
        tmp <- c(
          sum(wdd_recruit[["values"]][[1]][ids1]),
          sum(wdd_recruit[["values"]][[1]][ids2])
        )

        if (tmp[[1]] > res[k1, "SpringRecruitment_maxWDD"]) {
          res[k1, "SpringRecruitment_maxWDD"] <- tmp[[1]]
          # nolint start: extraction_operator_linter.
          res[k1, "SpringRecruitment_DOY"] <-
            as.POSIXlt(jan0 + lims[[1]])$yday + 1
          # nolint end
          res[k1, "SpringRecruitment_DurationDays"] <- length(ids1)
        }

        if (tmp[[2]] > res[k1, "FallRecruitment_maxWDD"]) {
          res[k1, "FallRecruitment_maxWDD"] <- tmp[[2]]
          # nolint start: extraction_operator_linter.
          res[k1, "FallRecruitment_DOY"] <-
            as.POSIXlt(jan0 + id_mid_yr)$yday + 1
          # nolint end
          res[k1, "FallRecruitment_DurationDays"] <- length(ids2)
        }

      } else {
        ids <- seq(from = lims[[1]], to = lims[[2]])
        tmp <- sum(wdd_recruit[["values"]][[1]][ids])

        if (all(lims < id_mid_yr)) {
          # Current period is completely before mid-year date
          if (tmp > res[k1, "SpringRecruitment_maxWDD"]) {
            # Current spring period is larger than previous ones -> replace
            res[k1, "SpringRecruitment_maxWDD"] <- tmp
            # nolint start: extraction_operator_linter.
            res[k1, "SpringRecruitment_DOY"] <-
              as.POSIXlt(jan0 + lims[[1]])$yday + 1
            # nolint end
            res[k1, "SpringRecruitment_DurationDays"] <- length(ids)
          }

        } else {
          # Current period is completely after mid-year date
          if (tmp > res[k1, "FallRecruitment_maxWDD"]) {
            # Current fall period is larger than previous ones -> replace
            res[k1, "FallRecruitment_maxWDD"] <- tmp
            # nolint start: extraction_operator_linter.
            res[k1, "FallRecruitment_DOY"] <-
              as.POSIXlt(jan0 + lims[[1]])$yday + 1
            # nolint end
            res[k1, "FallRecruitment_DurationDays"] <- length(ids)
          }
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
      )
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
      )
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
