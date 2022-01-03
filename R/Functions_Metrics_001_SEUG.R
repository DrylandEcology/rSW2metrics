
#--- Specific aggregation functions ------------------------------------------

# Temperature--Seasonal--Mean	Degrees C	Mean daily Temperature
metric_TemperatureMean_Seasonal <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  group_by_month, first_month_of_year,
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  calc_univariate_from_sw2(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    sw2_tp = "Day",
    sw2_out = "TEMP",
    sw2_var = "avg_C",
    fun_across_time = "mean"
  )
}


# Minimum temperature--Seasonal--Degrees C	Lowest daily maximum temperature
metric_TemperatureMin_Seasonal <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  group_by_month, first_month_of_year,
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  calc_univariate_from_sw2(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    sw2_tp = "Day",
    sw2_out = "TEMP",
    sw2_var = "min_C",
    fun_across_time = "min"
  )
}

# Maximum temperature--Seasonal--Degrees C	Lowest daily minimum temperature
metric_TemperatureMax_Seasonal <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  group_by_month, first_month_of_year,
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  calc_univariate_from_sw2(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    sw2_tp = "Day",
    sw2_out = "TEMP",
    sw2_var = "max_C",
    fun_across_time = "max"
  )
}

# Days below -1C--Seasonal--Count	Days	Count of days below -1C
metric_FrostDays_Seasonal <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  group_by_month, first_month_of_year,
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  calc_univariate_from_sw2(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    sw2_tp = "Day",
    sw2_out = "TEMP",
    sw2_var = "min_C",
    fun_across_time = function(x, limit,
  ...) sum(x < limit),
    limit = -1
  )
}



# Precipitation--Seasonal--Sum	cm	Sum of daily Precipitation
metric_PPT_Seasonal <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  group_by_month, first_month_of_year,
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  calc_univariate_from_sw2(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    sw2_tp = "Month",
    sw2_out = "PRECIP",
    sw2_var = "ppt",
    fun_across_time = "sum"
  )
}


# Potential evapotranspiration (PET)--Seasonal--Sum	cm	Sum of daily PET
metric_PET_Seasonal <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  group_by_month, first_month_of_year,
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  calc_univariate_from_sw2(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    sw2_tp = "Month",
    sw2_out = "PET",
    sw2_var = "pet_cm",
    fun_across_time = "sum"
  )
}


# Transpiration--Seasonal--Sum	cm
# Sum of daily total Transpiration from all soil layers
metric_Transpiration_Seasonal <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  group_by_month, first_month_of_year,
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  calc_univariate_from_sw2(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    sw2_tp = "Month",
    sw2_out = "TRANSP",
    sw2_var = "transp_total",
    varnames_are_fixed = FALSE,
    fun_across_vars = "sum",
    fun_across_time = "sum"
  )
}


# Evaporation--Seasonal--Sum	cm
# Sum of daily evaporation from soil, litter, canopy, surface water, and
# snowpack
metric_Evaporation_Seasonal <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  group_by_month, first_month_of_year,
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  # Evaporation from bare-soil
  tmp_Esoil <- calc_univariate_from_sw2(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    sw2_tp = "Month",
    sw2_out = "EVAPSOIL",
    sw2_var = "Lyr",
    varnames_are_fixed = FALSE,
    fun_across_vars = "sum",
    fun_across_time = "sum"
  )

  # Evaporation from
  #  (i) water intercepted by vegetation and by litter
  #  (ii) (open, ponded) surface water
  #  (iii) as sublimation from snow
  tmp_Eother <- calc_univariate_from_sw2(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    sw2_tp = "Month",
    sw2_out = c("EVAPSURFACE", "PRECIP"),
    sw2_var = c("evap_total", "snowloss"),
    varnames_are_fixed = TRUE,
    fun_across_vars = "sum",
    fun_across_time = "sum"
  )

  lapply(
    seq_along(id_scen_used),
    function(k) tmp_Esoil[[k]] + tmp_Eother[[k]]
  )
}



#--- VWC ------
# Soil volumetric water content (VWC)- whole/part profile
# --Seasonal--Mean	m ^ 3 / m^ 3
# Mean daily soil volumetric water content
get_VWC_Seasonal <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  group_by_month, first_month_of_year,
  soils, used_depth_range_cm,
  ...
) {

  widths_cm <- calc_soillayer_weights(soils[["depth_cm"]], used_depth_range_cm)

  warning("`get_VWC_Seasonal()` returns matric-VWC!")

  calc_univariate_from_sw2(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    req_ts = TRUE,
    sw2_tp = "Month",
    sw2_out = "VWCMATRIC",
    sw2_var = "Lyr",
    varnames_are_fixed = FALSE,
    fun_across_vars = function(x, ...) {
      weighted_mean_across_soillayers(x, w = widths_cm)
    },
    fun_across_time = "mean"
  )
}

metric_VWC_Seasonal_wholeprofile <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  group_by_month, first_month_of_year,
  soils,
  ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = "depth_cm"
  ))

  get_VWC_Seasonal(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    soils = soils,
    used_depth_range_cm = NULL
  )
}

metric_VWC_Seasonal_top50cm <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  group_by_month, first_month_of_year,
  soils,
  ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = "depth_cm"
  ))

  get_VWC_Seasonal(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    soils = soils,
    used_depth_range_cm = c(0, 50)
  )
}


#--- SWP ------
# SWP--Monthly--MPa	Mean of daily VWC converted to SWP
metric_SWP_SoilLayers_MeanMonthly <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "across_years",
  soils,
  ...
) {
  stopifnot(requireNamespace("reshape2", quietly = TRUE))
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = c("depth_cm", "sand_frac", "clay_frac")
  ))

  warning("`metric_SWP_SoilLayers_MeanMonthly()` uses matric-VWC!")

  vwc <- calc_multivariate_from_sw2(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = seq_len(12),
    first_month_of_year = 1L,
    req_ts = FALSE,
    sw2_tp = "Month",
    sw2_outs = "VWCMATRIC",
    sw2_vars = "Lyr",
    varnames_are_fixed = FALSE,
    fun_across_time = function(x) {
      nsl <- ncol(x)
      if (!is.null(nsl) && nsl > 1) {
        apply(x, MARGIN = 2, FUN = mean)
      } else {
        mean(x)
      }
    }
  )

  id_slyrs <- which(!is.na(soils[["depth_cm"]]))

  tmp <- lapply(
    X = vwc,
    FUN = function(x) {
      tmp <- array(NA, dim = dim(x), dimnames = dimnames(x))
      for (k in seq_len(dim(x)[3])) {
        tmp[, , k] <- rSOILWAT2::VWCtoSWP(
          vwc = x[, , k],
          sand = soils[["sand_frac"]][id_slyrs],
          clay = soils[["clay_frac"]][id_slyrs]
        )
      }
      tmp
    }
  )

  # Create compound group labels
  # TODO: fix so that req_ts = TRUE works
  nlyrs <- ncol(tmp[[1]])
  group_labels <- paste0(
    list_subannual_timesteps()[["monthly"]],
    "_Layer", rep(seq_len(nlyrs), each = 12)
  )

  # Convert to group x year format
  lapply(
    tmp,
    function(x) {
      # TODO: fix so that req_ts = TRUE works
      tmp <- reshape2::acast(
        data = reshape2::melt(x),
        Var2 + Var1 ~ Var3
      )
      rownames(tmp) <- group_labels
      tmp
    }
  )
}


#--- SWA ------
# Soil water availability (SWA)- whole/part profile--Seasonal--Mean	total cm
# Mean daily SWA (moisture above -3.9MPa)
#
# see `calc_SWA_mm()` for explanation of calculation
#
get_SWA_Seasonal <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  group_by_month, first_month_of_year,
  soils, used_depth_range_cm,
  crit_SWP_MPa,
  ...
) {

  widths_cm <- calc_soillayer_weights(soils[["depth_cm"]], used_depth_range_cm)

  id_slyrs <- which(!is.na(widths_cm))
  # Calculate SWC threshold (corrected for coarse fragments)
  # SWC <-> VWC exists only for the matric component
  base_SWC_cm <- rSOILWAT2::SWPtoVWC(
    swp = crit_SWP_MPa,
    sand = soils[["sand_frac"]][id_slyrs],
    clay = soils[["clay_frac"]][id_slyrs]
  ) * widths_cm[id_slyrs] * (1 - soils[["gravel_content"]][id_slyrs])

  calc_univariate_from_sw2(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    sw2_tp = "Month",
    sw2_out = "SWCBULK",
    sw2_var = "Lyr",
    varnames_are_fixed = FALSE,
    fun_across_vars = function(x, ...) {
      sum(pmax(0., x[id_slyrs] - base_SWC_cm))
    },
    fun_across_time = "mean"
  )
}



metric_SWA_Seasonal_wholeprofile <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  group_by_month, first_month_of_year,
  soils,
  ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = c("depth_cm", "sand_frac", "clay_frac", "gravel_content")
  ))

  get_SWA_Seasonal(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    soils = soils,
    used_depth_range_cm = NULL,
    crit_SWP_MPa = -3.9
  )
}

metric_SWA_Seasonal_top50cm <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  group_by_month, first_month_of_year,
  soils,
  ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = c("depth_cm", "sand_frac", "clay_frac", "gravel_content")
  ))

  get_SWA_Seasonal(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    soils = soils,
    used_depth_range_cm = c(0, 50),
    crit_SWP_MPa = -3.9
  )
}


#--- Wet soil days ------
# Wet soil days - whole/part profile--Seasonal--Count	Days
# Count of days with wet soil,
# defined as ANY soil layer being wet (> -1.5 MPa)
get_WetSoilDays_Seasonal <- function(
  path, name_sw2_run,
  id_scen_used, list_years_scen_used,
  group_by_month, first_month_of_year,
  soils, used_depth_range_cm,
  crit_SWP_MPa,
  ...
) {

  widths_cm <- calc_soillayer_weights(soils[["depth_cm"]], used_depth_range_cm)
  id_slyrs <- which(!is.na(widths_cm))

  calc_univariate_from_sw2(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    sw2_tp = "Day",
    sw2_out = "SWPMATRIC",
    sw2_var = "Lyr",
    varnames_are_fixed = FALSE,
    # SOILWAT2 outputs SWP in units of [-bar = -10 * MPa]
    fun_across_vars = function(x, ...) any(x[id_slyrs] <= -10 * crit_SWP_MPa),
    fun_across_time = "sum"
  )
}

metric_WetSoilDays_Seasonal_wholeprofile <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  group_by_month, first_month_of_year,
  soils,
  ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = "depth_cm"
  ))

  get_WetSoilDays_Seasonal(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    soils = soils,
    used_depth_range_cm = NULL,
    crit_SWP_MPa = -1.5
  )
}

metric_WetSoilDays_Seasonal_top50cm <- function(
  path, name_sw2_run,
  id_scen_used, list_years_scen_used,
  out = "ts_years",
  group_by_month, first_month_of_year,
  soils,
  ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = "depth_cm"
  ))

  get_WetSoilDays_Seasonal(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    soils = soils,
    used_depth_range_cm = c(0, 50),
    crit_SWP_MPa = -1.5
  )
}


#--- Dry soil days ------
# Dry soil days - whole/part profile--Seasonal--Count	Days
# Count of days with dry soil, defined as ALL soil layers being < -3.9 Mpa
get_DrySoilDays_Seasonal <- function(
  path, name_sw2_run,
  id_scen_used, list_years_scen_used,
  group_by_month, first_month_of_year,
  soils, used_depth_range_cm,
  crit_SWP_MPa,
  ...
) {

  widths_cm <- calc_soillayer_weights(soils[["depth_cm"]], used_depth_range_cm)
  id_slyrs <- which(!is.na(widths_cm))

  calc_univariate_from_sw2(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    sw2_tp = "Day",
    sw2_out = "SWPMATRIC",
    sw2_var = "Lyr",
    varnames_are_fixed = FALSE,
    # SOILWAT2 outputs SWP in units of [-bar = -10 * MPa]
    fun_across_vars = function(x, ...) all(x[id_slyrs] > -10 * crit_SWP_MPa),
    fun_across_time = "sum"
  )
}

metric_DrySoilDays_Seasonal_wholeprofile <- function(
  path, name_sw2_run,
  id_scen_used, list_years_scen_used,
  out = "ts_years",
  group_by_month, first_month_of_year,
  soils,  ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = "depth_cm"
  ))

  get_DrySoilDays_Seasonal(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    soils = soils,
    used_depth_range_cm = NULL,
    crit_SWP_MPa = -3.9
  )
}

metric_DrySoilDays_Seasonal_top50cm <- function(
  path, name_sw2_run,
  id_scen_used, list_years_scen_used,
  out = "ts_years",
  group_by_month, first_month_of_year,
  soils,
  ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = "depth_cm"
  ))

  get_DrySoilDays_Seasonal(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    soils = soils,
    used_depth_range_cm = c(0, 50),
    crit_SWP_MPa = -3.9
  )
}


#--- SWA during not dry soil days [i.e., remove SWA == 0] ------
# SWA during not dry soil days - whole/part profile--Seasonal--Mean	total cm
# Mean SWA during days with ANY soil layer > -3.9
get_NonDrySWA_Seasonal <- function(
  path, name_sw2_run,
  id_scen_used, list_years_scen_used,
  group_by_month, first_month_of_year,
  soils, used_depth_range_cm,
  crit_SWP_MPa,
  ...
) {

  widths_cm <- calc_soillayer_weights(soils[["depth_cm"]], used_depth_range_cm)

  id_slyrs <- which(!is.na(widths_cm))
  # Calculate SWC threshold (corrected for coarse fragments)
  # SWC <-> VWC exists only for the matric component
  base_SWC_cm <- rSOILWAT2::SWPtoVWC(
    swp = crit_SWP_MPa,
    sand = soils[["sand_frac"]][id_slyrs],
    clay = soils[["clay_frac"]][id_slyrs]
  ) * widths_cm[id_slyrs] * (1 - soils[["gravel_content"]][id_slyrs])

  calc_univariate_from_sw2(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    sw2_tp = "Month",
    sw2_out = "SWCBULK",
    sw2_var = "Lyr",
    varnames_are_fixed = FALSE,
    fun_across_vars = function(x, ...) {
      sum(pmax(0., x[id_slyrs] - base_SWC_cm))
    },
    fun_across_time = function(x, ...) mean(x[x > 0])
  )
}



metric_NonDrySWA_Seasonal_wholeprofile <- function(
  path, name_sw2_run,
  id_scen_used, list_years_scen_used,
  out = "ts_years",
  group_by_month, first_month_of_year,
  soils,
  ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = c("depth_cm", "sand_frac", "clay_frac")
  ))

  get_NonDrySWA_Seasonal(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    soils = soils,
    used_depth_range_cm = NULL,
    crit_SWP_MPa = -3.9
  )
}

metric_NonDrySWA_Seasonal_top50cm <- function(
  path, name_sw2_run,
  id_scen_used, list_years_scen_used,
  out = "ts_years",
  group_by_month, first_month_of_year,
  soils,
  ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = c("depth_cm", "sand_frac", "clay_frac")
  ))

  get_NonDrySWA_Seasonal(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    soils = soils,
    used_depth_range_cm = c(0, 50),
    crit_SWP_MPa = -3.9
  )
}



#--- Extreme short-term dry stress ------
# Extreme short-term dry stress - whole/part profile
# --Seasonal--Mean	Degrees C
# Mean max. temperature of the X hottest days
# when All soil layers are < -1.5 MPa
get_ExtremeShortTermDryStress_Seasonal <- function(
  path, name_sw2_run,
  id_scen_used, list_years_scen_used,
  group_by_month, first_month_of_year,
  soils, used_depth_range_cm,
  crit_SWP_MPa, n_extremes,
  ...
) {

  id_slyrs <- determine_used_soillayers(
    soil_depths_cm = soils[["depth_cm"]],
    used_depth_range_cm = used_depth_range_cm
  )

  calc_multivariate_from_sw2(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    sw2_tp = "Day",
    sw2_outs = c("SWPMATRIC", "TEMP"),
    sw2_vars = c("Lyr", "max_C"),
    varnames_are_fixed = c(FALSE, TRUE),
    funs_across_each_var = list(
      # SOILWAT2 outputs SWP in units of [-bar = -10 * MPa]
      function(x, ...) all(x[id_slyrs] > -10 * crit_SWP_MPa),
      NULL
    ),
    fun_across_time = function(x, ...) {
      # We need at least `n_extremes` dry days
      if (sum(x[, 1]) >= n_extremes) {
        tmp <- x[x[, 1], 2] # max temperature on dry days
        stmp <- sort(tmp, decreasing = TRUE)
        mean(stmp[seq_len(n_extremes)])
      } else {
        NA
      }
    }
  )
}

metric_ExtremeShortTermDryStress_Seasonal_wholeprofile <- function(
  path, name_sw2_run,
  id_scen_used, list_years_scen_used,
  out = "ts_years",
  group_by_month, first_month_of_year,
  soils,
  ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = "depth_cm"
  ))

  get_ExtremeShortTermDryStress_Seasonal(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    soils = soils,
    used_depth_range_cm = NULL,
    crit_SWP_MPa = -1.5,
    n_extremes = 10
  )
}

metric_ExtremeShortTermDryStress_Seasonal_top50cm <- function(
  path, name_sw2_run,
  id_scen_used, list_years_scen_used,
  out = "ts_years",
  group_by_month, first_month_of_year,
  soils,
  ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = "depth_cm"
  ))

  get_ExtremeShortTermDryStress_Seasonal(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = group_by_month,
    first_month_of_year = first_month_of_year,
    soils = soils,
    used_depth_range_cm = c(0, 50),
    crit_SWP_MPa = -1.5,
    n_extremes = 10
  )
}



#--- Semi-dry duration ------
# Semi-Dry soil duration length - whole/part profile--Annual--Mean	Days
# Annual mean length of continuous days with ALL soil layers < -1.5 Mpa
get_SemiDryDuration_Annual <- function(
  path, name_sw2_run,
  id_scen_used, list_years_scen_used,
  first_month_of_year,
  soils, used_depth_range_cm,
  crit_SWP_MPa,
  ...
) {

  id_slyrs <- determine_used_soillayers(
    soil_depths_cm = soils[["depth_cm"]],
    used_depth_range_cm = used_depth_range_cm
  )

  calc_univariate_from_sw2(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = rep(0, 12),
    first_month_of_year = first_month_of_year,
    sw2_tp = "Day",
    sw2_out = "SWPMATRIC",
    sw2_var = "Lyr",
    varnames_are_fixed = FALSE,
    # SOILWAT2 outputs SWP in units of [-bar = -10 * MPa]
    fun_across_vars = function(x, ...) all(x[id_slyrs] > -10 * crit_SWP_MPa),
    fun_across_time = function(x, ...) {
      tmp <- rle(x)
      if (any(tmp[["values"]])) {
        mean(tmp[["lengths"]][tmp[["values"]]])
      } else {
        0
      }
    }
  )

}

metric_SemiDryDuration_Annual_wholeprofile <- function(
  path, name_sw2_run,
  id_scen_used, list_years_scen_used,
  out = "ts_years",
  first_month_of_year,
  soils,
  ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = "depth_cm"
  ))

  get_SemiDryDuration_Annual(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    first_month_of_year = first_month_of_year,
    soils = soils,
    used_depth_range_cm = NULL,
    crit_SWP_MPa = -1.5
  )
}

metric_SemiDryDuration_Annual_top50cm <- function(
  path, name_sw2_run,
  id_scen_used, list_years_scen_used,
  out = "ts_years",
  first_month_of_year,
  soils,
  ...
) {
  stopifnot(check_metric_arguments(
    out = match.arg(out),
    req_soil_vars = "depth_cm"
  ))

  get_SemiDryDuration_Annual(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    first_month_of_year = first_month_of_year,
    soils = soils,
    used_depth_range_cm = c(0, 50),
    crit_SWP_MPa = -1.5
  )
}



#--- T-P correlations ------

# Monthly T-P correlation--Annual--Correlation
# Correlation (pearson's) between monthly precipitation and
# average temperature across one (seasonal) year
metric_CorTP_Annual <- function(
  path, name_sw2_run,
  id_scen_used, list_years_scen_used,
  out = "ts_years",
  first_month_of_year,
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  calc_multivariate_from_sw2(
    path, name_sw2_run,
    id_scen_used = id_scen_used,
    list_years_scen_used = list_years_scen_used,
    group_by_month = rep(0, 12),
    first_month_of_year = first_month_of_year,
    sw2_tp = "Month",
    sw2_outs = c("TEMP", "PRECIP"),
    sw2_vars = c("avg_C", "ppt"),
    fun_across_time = function(x, ...) cor(x[, 1], x[, 2])
  )
}
