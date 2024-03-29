#--- Fractional land cover (re-calculated)
metric_land_cover_v1 <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  zipped_runs = FALSE,
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
        wd = list(
          sw2_tp = "Day",
          sw2_outs = c("PRECIP", "TEMP", "TEMP"),
          sw2_vars = c(ppt = "ppt", tmax = "max_C", tmin = "min_C"),
          varnames_are_fixed = TRUE
        )
      ),
      zipped_runs = zipped_runs
    )

    tmp_meteo <- cbind(
      Year = sim_data[["wd"]][["time"]][, "Year"],
      DOY = sim_data[["wd"]][["time"]][, "Day"],
      Tmax_C = sim_data[["wd"]][["values"]][["tmax"]],
      Tmin_C = sim_data[["wd"]][["values"]][["tmin"]],
      PPT_cm = sim_data[["wd"]][["values"]][["ppt"]]
    )

    if (anyNA(tmp_meteo)) {
      ids <- sim_data[["wd"]][["time"]][, "mode"] == "sim_keep"
      warning(
        "`metric_land_cover_v1(): ",
        "simulated time period ",
        paste0(range(sim_data[["wd"]][["time"]][ids, "Year"]), collapse = "-"),
        " does not completely include requested years ",
        paste0(range(sim_data[["wd"]][["time"]][, "Year"]), collapse = "-"),
        "; land cover (based on climate conditions) will be valid only ",
        "for simulated subset instead of full requested time period."
      )
      tmp_meteo <- tmp_meteo[ids, , drop = FALSE]
    }

    clim <- rSOILWAT2::calc_SiteClimate(
      weatherList = rSOILWAT2::dbW_dataframe_to_weatherData(tmp_meteo),
      do_C4vars = TRUE
    )

    cov <- rSOILWAT2::estimate_PotNatVeg_composition(
      MAP_mm = 10 * clim[["MAP_cm"]],
      MAT_C = clim[["MAT_C"]],
      mean_monthly_ppt_mm = 10 * clim[["meanMonthlyPPTcm"]],
      mean_monthly_Temp_C = clim[["meanMonthlyTempC"]],
      dailyC4vars = clim[["dailyC4vars"]]
    )[["Rel_Abundance_L1"]]

    tmp_var <- c(
      "SW_BAREGROUND", "SW_TREES", "SW_SHRUB", "SW_FORBS", "SW_GRASS"
    )

    res[[k1]] <- matrix(
      data = cov[tmp_var],
      nrow = 5,
      ncol = length(unique(sim_data[["wd"]][["time"]][, "Year"])),
      dimnames = list(
        c(
          "fCover_BareGround",
          "fCover_tree", "fCover_shrub", "fCover_forbs", "fCover_grass"
        ),
        NULL
      )
    )

  }

  res
}


#--- Fractional land cover (available with rSOILWAT2 v3.1.2/SOILWAT v5.2.0)
metric_land_cover_v2 <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  zipped_runs = FALSE,
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
        cover = list(
          sw2_tp = "Year",
          sw2_outs = "BIOMASS",
          sw2_vars = c(
            "fCover_BareGround",
            "fCover_tree", "fCover_shrub", "fCover_forbs", "fCover_grass"
          ),
          varnames_are_fixed = TRUE
        )
      ),
      zipped_runs = zipped_runs
    )

    res[[k1]] <- t(do.call(cbind, sim_data[["cover"]][["values"]]))
  }

  res
}


#--- Monthly and annual vegetation biomass
# available with rSOILWAT2 v3.1.2/SOILWAT v5.2.0
get_veg_biomass_v2 <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  include_year = FALSE,
  timestep = c("yearly", "monthly"),
  zipped_runs = FALSE,
  ...
) {
  timestep <- match.arg(timestep)

  tmp_var <- c("total", "tree", "shrub", "forbs", "grass")
  tmp_veg <- c(
    paste0("Biomass_", tmp_var),
    "Biomass_litter",
    paste0("Biolive_", tmp_var)
  )

  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    sim_data <- collect_sw2_sim_data(
      path = path,
      name_sw2_run = name_sw2_run,
      id_scen = id_scen_used[k1],
      years = list_years_scen_used[[k1]],
      output_sets = list(
        val = list(
          sw2_tp = switch(timestep, yearly = "Year", monthly = "Month"),
          sw2_outs = "BIOMASS",
          sw2_vars = tmp_veg,
          varnames_are_fixed = TRUE
        )
      ),
      zipped_runs = zipped_runs
    )

    res[[k1]] <- format_values_to_matrix(
      x = unname(sim_data[["val"]][["values"]]),
      ts_years = sim_data[["val"]][["time"]][, "Year"],
      timestep = timestep,
      out_label = tmp_veg
    )
  }

  res
}

metric_veg_biomass_annual_v2 <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  include_year = FALSE,
  zipped_runs = FALSE,
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  get_veg_biomass_v2(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    include_year = include_year,
    timestep = "yearly",
    zipped_runs = zipped_runs,
    ...
  )
}

metric_veg_biomass_monthly_v2 <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  include_year = FALSE,
  zipped_runs = FALSE,
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  get_veg_biomass_v2(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    include_year = include_year,
    timestep = "monthly",
    zipped_runs = zipped_runs,
    ...
  )
}



#--- Monthly and annual vegetation biomass
# available with rSOILWAT2 v1.7.0/SOILWAT v3.5.0
# and before rSOILWAT2 v3.1.2/SOILWAT v5.2.0)
get_veg_biomass_v1 <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  include_year = FALSE,
  timestep = c("yearly", "monthly"),
  zipped_runs = FALSE,
  ...
) {
  timestep <- match.arg(timestep)

  tmp_var <- c("total", "tree", "shrub", "forbs", "grass")
  tmp_veg_metrics <- c(
    paste0("Biomass_", tmp_var),
    "Biomass_litter",
    paste0("Biolive_", tmp_var)
  )

  tmp_var2 <- c("Total", "Tree", "Shrub", "Forb", "Grass")
  tmp_veg_tbm <- paste0(tmp_var2, "Biomass")
  tmp_veg_lbm <- paste0(tmp_var2, "Biolive")
  tmp_veg_sw2old <- c(tmp_veg_tbm, tmp_veg_lbm)

  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    sim_data <- collect_sw2_sim_data(
      path = path,
      name_sw2_run = name_sw2_run,
      id_scen = id_scen_used[k1],
      years = list_years_scen_used[[k1]],
      output_sets = list(
        val = list(
          sw2_tp = switch(timestep, yearly = "Year", monthly = "Month"),
          sw2_outs = "CO2EFFECTS",
          sw2_vars = tmp_veg_sw2old,
          varnames_are_fixed = TRUE
        )
      ),
      zipped_runs = zipped_runs,
      fail = FALSE
    )

    res[[k1]] <- format_values_to_matrix(
      x = unname(c(
        sim_data[["val"]][["values"]][tmp_veg_tbm],
        Biomass_litter = list(rep(NA, nrow(sim_data[["val"]][["time"]]))),
        sim_data[["val"]][["values"]][tmp_veg_lbm]
      )),
      ts_years = sim_data[["val"]][["time"]][, "Year"],
      timestep = timestep,
      out_label = tmp_veg_metrics
    )
  }

  res
}


metric_veg_biomass_annual_v1 <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  include_year = FALSE,
  zipped_runs = FALSE,
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  get_veg_biomass_v1(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    include_year = include_year,
    timestep = "yearly",
    zipped_runs = zipped_runs,
    ...
  )
}

metric_veg_biomass_monthly_v1 <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  include_year = FALSE,
  zipped_runs = FALSE,
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  get_veg_biomass_v1(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    include_year = include_year,
    timestep = "monthly",
    zipped_runs = zipped_runs,
    ...
  )
}
