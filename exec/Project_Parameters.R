#------ Describe project: XXX
do_full <- exists("args") && isTRUE(args[["do_full"]])

dir_prj <- ".."

if (do_full) {
  dir_rSFSW2 <- file.path(
    dir_prj, "..", "..",
    "1_SOILWAT2_Simulations",
    "YOURPROJECTNAME_SOILWAT2_simulations"
  )

} else {
  dir_rSFSW2 <- file.path(
    dir_prj, "..", "..",
    "1_SOILWAT2_Simulations",
    "YOURPROJECTNAME_SOILWAT2_simulations"
  )
}

dir_sw2_output <- file.path(dir_rSFSW2, "3_Runs")
has_rSOILWAT2_inputs <- TRUE

make_short_run_names <- rSW2metrics::shorten_run_names

N_exp <- 1
N_scen <- 7
id_scen_used <- c(1, 5:7)


#--- Output path
if (do_full) {
  dir_out <- file.path(dir_prj, "Outputs")
} else {
  dir_out <- file.path(dir_prj, "Outputs_tests")
}


#--- Timeframe over which metrics are calculated
years_historical <- 1979:2020
years_future_projection <- 2006:2099

years_timeseries_by_scen <- c(
  list(years_historical),
  lapply(
    id_scen_used[-1],
    function(k) years_future_projection
  )
)


#--- Across-year aggregations of metrics
# Two options for across-year summaries
#  1) Request across-year summaries for time-series metric,
#     i.e., pass options `-add_aggs_across_yrs` and `-ts` on the command line
#  2) Request a climatology metric, i.e., a metric that ends in "Clim"

# Years over values are summarized
years_aggs_by_scen <- c(
  list(list(hist = years_historical)),
  lapply(
    id_scen_used[-1],
    function(k) list(nearterm = 2020:2059, longterm = 2060:2099)
  )
)

# Function that produces across-year summaries
fun_aggs_across_yrs <- rSW2metrics::mean_cv_trend


#--- Seasonal index by month
#   1 = Winter = DJF, 2 = Spring = MAM, 3 = Summer = JJA, 4 = Fall = SON
season_by_month <- c(rep(1, 2), rep(2:4, each = 3), 1)
first_month_of_year <- 12 # First season (winter) starts in December


#--- Soil parameters
Nmax_soillayers <- 13
# see `list_soil_variables()` for possible values
used_soil_variables <- c("depth", "sand", "clay", "gravel")


#--- Parameters for output type "SW2toTable"
# used by `metric_SW2toTable_daily()`
dir_out_SW2toTable <- file.path(
  dir_prj,
  basename(dir_rSFSW2),
  paste0(
    format(Sys.Date(), "%Y%m%d"),
    "_SOILWAT2_OutputShared__",
    basename(dir_rSFSW2)
  )
)

# Format of files written to disk: "rds" (default) or "csv"
format_share_SW2toTable <- "rds"

# Groups of variables to include in output
#   - "all" (default, includes all the following)
#   - "meteo", "snow", "radiation", "waterbalance", "evapotranspiration",
#     "soiltemperature", "VWC", "SWP"
outputs_SW2toTable <- "all"
