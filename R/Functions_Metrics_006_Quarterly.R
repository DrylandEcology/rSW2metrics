#------ Quarterly ------
# nolint start
# Q1 = Jan-Mar; Q2 = Apr-Jun; Q3 = Jul-Sep; Q4 = Oct-Dec
# nolint end



#--- Quarterly precipitation amount [mm] and mean air temperature [C]
metric_Climate_quarterly <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  include_year = FALSE,
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    sim_data <- collect_sw2_sim_data(
      path = path,
      name_sw2_run = name_sw2_run,
      id_scen = id_scen_used[k1],
      years = list_years_scen_used[[k1]],
      output_sets = list(
        climate = list(
          sw2_tp = "Day",
          sw2_outs = c("TEMP", "PRECIP"),
          sw2_vars = c(`tmean-C` = "avg_C", `ppt-mm` = "ppt"),
          varnames_are_fixed = TRUE
        )
      )
    )

    time <- data.frame(
      sim_data[[1]][["time"]],
      Quarter = paste0(
        "Q",
        1 + (sim_data[[1]][["time"]][, "Month"] - 1) %/% 3
      )
    )

    tmp_res <- list(
      tmean_C = tapply(
        sim_data[[1]][["values"]][["tmean-C"]],
        INDEX = list(
          Quarter = time[, "Quarter"],
          Year = time[, "Year"]
        ),
        FUN = mean
      ),
      ppt_mm = tapply(
        10 * sim_data[[1]][["values"]][["ppt-mm"]],
        INDEX = list(
          Quarter = time[, "Quarter"],
          Year = time[, "Year"]
        ),
        FUN = sum
      )
    )

    tmp_res <- do.call(rbind, tmp_res)
    rownames(tmp_res) <- paste0(
      rep(names(sim_data[[1]][["values"]]), each = 4), "_",
      rownames(tmp_res)
    )

    res[[k1]] <- tmp_res
  }

  res
}


# Quarterly mean SWA of daily sums across soil layers:
# soils must have "depth_cm", "sand_frac", "clay_frac", and "gravel_content"
get_SWA_multilayer_quarterly <- function(
  path, name_sw2_run, id_scen_used,
  list_years_scen_used,
  include_year = FALSE,
  soils,
  list_used_depth_range_cm = NULL,
  SWP_limit_MPa = -Inf,
  ...
) {
  tag_depth_ranges <- rep(
    unlist(lapply(
      list_used_depth_range_cm,
      function(x) paste0(x[1], "to", x[2], "cm")
    )),
    each = 4
  )

  res <- list()

  for (k1 in seq_along(id_scen_used)) {
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
        )
      )
    )

    list_swa_daily <- lapply(
      list_used_depth_range_cm,
      function(range) {
        calc_SWA_mm(
          sim_swc_daily = sim_data[["swc_daily"]],
          soils = soils,
          SWP_limit_MPa = SWP_limit_MPa,
          used_depth_range_cm = range,
          method = "across_profile"
        )
      }
    )

    swa_time <- data.frame(
      list_swa_daily[[1]][["time"]],
      Quarter = paste0(
        "Q",
        1 + (list_swa_daily[[1]][["time"]][, "Month"] - 1) %/% 3
      )
    )

    tmp_res <- lapply(
      list_swa_daily,
      function(x) {
        tapply(
          x[["values"]][[1]],
          INDEX = list(
            Quarter = swa_time[, "Quarter"],
            Year = swa_time[, "Year"]
          ),
          FUN = mean
        )
      }
    )

    tmp_res <- do.call(rbind, tmp_res)
    rownames(tmp_res) <- paste0(tag_depth_ranges, "_", rownames(tmp_res))

    res[[k1]] <- tmp_res
  }

  res
}


# Soil water availability (SWA) as daily sum across
# 0-20 cm and 20-100 cm soil depth of
# soil water content held at a potential > -3.9 MPa
#--- Quarterly means of daily sums of SWA
metric_SWAat0to020to100cm39bar_quarterly <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  soils,
  include_year = FALSE,
  ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = c("depth_cm", "sand_frac", "clay_frac", "gravel_content")
  ))

  list_used_depth_range_cm <- list(c(0, 20), c(20, 100))
  SWP_limit_MPa <- -3.9

  lapply(
    list_used_depth_range_cm,
    function(range) {
      check_soillayer_availability(
        soil_depths_cm = soils[["depth_cm"]],
        used_depth_range_cm = range,
        strict = TRUE,
        type = "warn"
      )
    }
  )

  get_SWA_multilayer_quarterly(
    path, name_sw2_run, id_scen_used,
    list_years_scen_used = list_years_scen_used,
    soils = soils,
    list_used_depth_range_cm = list_used_depth_range_cm,
    SWP_limit_MPa = SWP_limit_MPa,
    include_year = include_year
  )
}



#--- TDD = Total degree days [C x day] = MDD(op = `>`, limit_MPa = -Inf)
#--- Quarterly sum of daily TDD
metric_TDDat5C_quarterly <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  soils, ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
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
      )
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

    # Aggregate daily TDD to quarterly sum
    res[[k1]] <- tapply(
      tdd_daily[["values"]][["mdd"]],
      INDEX = list(
        Quarter = paste0("Q", 1 + (tdd_daily[["time"]][, "Month"] - 1) %/% 3),
        Year = tdd_daily[["time"]][, "Year"]
      ),
      FUN = sum
    )
  }

  res
}



#--- WDD = Wet degree days [C x day] = MDD(op = `>`, limit_MPa = -1.5 MPa)
#--- Quarterly sum of daily WDD
get_WDD_quarterly <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  soils,
  used_depth_range_cm = NULL,
  Temp_limit_C = 5,
  SWP_limit_MPa = -1.5
) {
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

    wdd_daily <- calc_MDD_daily(
      sim_data = sim_data,
      soils = soils,
      used_depth_range_cm = used_depth_range_cm,
      t_periods = list(op = `>`, limit = Temp_limit_C),
      sm_periods = list(op = `>`, limit = SWP_limit_MPa)
    )

    # Aggregate daily WDD to quarterly sums
    res[[k1]] <- tapply(
      wdd_daily[["values"]][["mdd"]],
      INDEX = list(
        Quarter = paste0("Q", 1 + (wdd_daily[["time"]][, "Month"] - 1) %/% 3),
        Year = wdd_daily[["time"]][, "Year"]
      ),
      FUN = sum
    )
  }

  res
}


metric_WDDat5C0to020cm15bar_quarterly <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  soils, ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = "depth_cm"
  ))

  get_WDD_quarterly(
    path, name_sw2_run, id_scen_used, list_years_scen_used,
    out = "ts_years",
    soils,
    used_depth_range_cm = c(0, 20),
    Temp_limit_C = 5,
    SWP_limit_MPa = -1.5
  )
}


metric_WDDat5C0to100cm15bar_quarterly <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  soils, ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = "depth_cm"
  ))

  get_WDD_quarterly(
    path, name_sw2_run, id_scen_used, list_years_scen_used,
    out = "ts_years",
    soils,
    used_depth_range_cm = c(0, 100),
    Temp_limit_C = 5,
    SWP_limit_MPa = -1.5
  )
}



#--- DDD = Dry degree days [C x day] = MDD(op = `<`, limit_MPa = -3.0 MPa)
#--- Quarterly sum of daily DDD
get_DDD_quarterly <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  soils,
  used_depth_range_cm = NULL,
  Temp_limit_C = 5,
  SWP_limit_MPa = -3
) {
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

    wdd_daily <- calc_MDD_daily(
      sim_data = sim_data,
      soils = soils,
      used_depth_range_cm = used_depth_range_cm,
      t_periods = list(op = `>`, limit = Temp_limit_C),
      sm_periods = list(op = `<`, limit = SWP_limit_MPa)
    )

    # Aggregate daily DDD to quarterly sums
    res[[k1]] <- tapply(
      wdd_daily[["values"]][["mdd"]],
      INDEX = list(
        Quarter = paste0("Q", 1 + (wdd_daily[["time"]][, "Month"] - 1) %/% 3),
        Year = wdd_daily[["time"]][, "Year"]
      ),
      FUN = sum
    )
  }

  res
}


metric_DDDat5C0to020cm30bar_quarterly <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  soils, ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = "depth_cm"
  ))

  get_DDD_quarterly(
    path, name_sw2_run, id_scen_used, list_years_scen_used,
    out = "ts_years",
    soils,
    used_depth_range_cm = c(0, 20),
    Temp_limit_C = 5,
    SWP_limit_MPa = -3
  )
}


metric_DDDat5C0to100cm30bar_quarterly <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  soils, ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = "depth_cm"
  ))

  get_DDD_quarterly(
    path, name_sw2_run, id_scen_used, list_years_scen_used,
    out = "ts_years",
    soils,
    used_depth_range_cm = c(0, 100),
    Temp_limit_C = 5,
    SWP_limit_MPa = -3
  )
}
