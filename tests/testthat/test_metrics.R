
dir_test_data <- file.path("..", "test_data")
test_that("Test data availability", {
  expect_true(dir.exists(dir_test_data), info = getwd())
})


# Aggregation function for rSOILWAT2 input/output for each simulated site
foo_metrics <- function(
  fun,
  fun_args,
  run_rSFSW2_names,
  is_soils_input,
  N_sites
) {
  lapply(
    seq_len(N_sites),
    function(s) {
      tmp <- suppressWarnings(process_values_one_site(
        fun = fun,
        fun_args = fun_args,
        name_sw2_run = run_rSFSW2_names[s],
        is_soils_input = is_soils_input,
        soil_variables = list_soil_variables()
      ))
      format_metric_1sim(x = tmp, id = s)
    }
  )
}


test_that("Check metrics", {
  skip_on_ci()

  #--- List all metric functions
  fun_metrics <- list_all_metrics()


  #------ Timing (only if interactively used)
  do_timing <- interactive() && !testthat::is_testing()
  if (do_timing) {
    print("do timing")
    time_metrics <- vector("numeric", length = length(fun_metrics))
  }


  #--- Create an example rSOILWAT2 simulation run with several scenarios
  prjpars <- list()
  prjpars[["fun_aggs_across_yrs"]] <- mean_cv_trend

  prjpars[["dir_sw2_output"]] <- tempdir()

  prjpars[["N_scen"]] <- 3
  prjpars[["id_scen_used"]] <- seq_len(prjpars[["N_scen"]])

  years_sim_historical <- 1980:2010
  years_sim_future_projection <- 2006:2099

  years_sim_timeseries_by_scen <- c(
    list(years_sim_historical),
    lapply(
      prjpars[["id_scen_used"]][-1],
      function(k) years_sim_future_projection
    )
  )


  # Put together rSOILWAT2 simulations and save to disk
  # Site = 1: rSOILWAT2 example data
  # Site = 2: as site 1, but with one only soil layer

  N_sites <- 2
  run_rSFSW2_names <- paste0("rSW2metrics_rSOILWAT2testrun", seq_len(N_sites))
  dir_runs_rSFSW2 <- file.path(prjpars[["dir_sw2_output"]], run_rSFSW2_names)
  tmp <- sapply(
    dir_runs_rSFSW2,
    dir.create,
    recursive = TRUE,
    showWarnings = FALSE
  )

  for (s in seq_len(N_sites)) {
    swRunScenariosData <- list()

    sw2_in_template <- rSOILWAT2::sw_exampleData

    if (s == 2) {
      # Trim soils to one layer
      tmp <- rSOILWAT2::swSoils_Layers(sw2_in_template)[1, , drop = FALSE]
      vtmp <- grep(
        "(EvapBareSoil_frac)|(transp[[:alpha:]])",
        colnames(tmp)
      )
      tmp[, vtmp] <- 1
      rSOILWAT2::swSoils_Layers(sw2_in_template) <- tmp
    }

    wgen_coeffs <- rSOILWAT2::dbW_estimate_WGen_coefs(
      weatherData = rSOILWAT2::get_WeatherHistory(sw2_in_template)
    )

    for (sc in prjpars[["id_scen_used"]]) {
      sw2_in <- sw2_in_template

      # Simulation time
      rSOILWAT2::swWeather_FirstYearHistorical(sw2_in) <- -1
      years <- years_sim_timeseries_by_scen[[sc]]
      rSOILWAT2::swYears_StartYear(sw2_in) <- 0
      rSOILWAT2::swYears_EndYear(sw2_in) <- years[length(years)]
      rSOILWAT2::swYears_StartYear(sw2_in) <- years[1]

      # Weather generator
      set.seed(127 + sc)
      rSOILWAT2::swWeather_UseMarkov(sw2_in) <- TRUE
      rSOILWAT2::swMarkov_Prob(sw2_in) <- wgen_coeffs[["mkv_doy"]]
      rSOILWAT2::swMarkov_Conv(sw2_in) <- wgen_coeffs[["mkv_woy"]]

      # CO2 concentration scenario
      co2_nametag <- "RCP85"
      co2_data <- rSOILWAT2::lookup_annual_CO2a(
        start = rSOILWAT2::swYears_StartYear(sw2_in),
        end = rSOILWAT2::swYears_EndYear(sw2_in),
        name_co2 = co2_nametag
      )
      rSOILWAT2::swCarbon_Scenario(sw2_in) <- co2_nametag
      rSOILWAT2::swCarbon_CO2ppm(sw2_in) <- data.matrix(co2_data)


      if (sc > 1) {
        # Climate scenarios: 2 C warming + 50% reduction in June-Aug precip
        tmp <- sc / prjpars[["N_scen"]]
        rSOILWAT2::swWeather_MonScalingParams(sw2_in)[6:8, "PPT"] <- 0.5 * tmp
        rSOILWAT2::swWeather_MonScalingParams(sw2_in)[, c("MaxT", "MinT")] <-
          2 * tmp
      }

      swRunScenariosData[[sc]] <- sw2_in
      runDataSC <- rSOILWAT2::sw_exec(inputData = swRunScenariosData[[sc]])

      save(
        runDataSC,
        file = file.path(
          dir_runs_rSFSW2[s],
          paste0("sw_output_sc", sc, ".RData")
        )
      )
    }

    save(
      swRunScenariosData,
      file = file.path(dir_runs_rSFSW2[s], "sw_input.RData")
    )
  }

  #--- Calculate metrics for example simulation and compare with previous output
  years_metrics_historical <- 1990:2010
  stopifnot(years_metrics_historical %in% years_sim_historical)
  years_metrics_future_projection <- 2050:2090
  stopifnot(years_metrics_future_projection %in% years_sim_future_projection)

  prjpars[["years_timeseries_by_scen"]] <- c(
    list(years_metrics_historical),
    lapply(
      prjpars[["id_scen_used"]][-1],
      function(k) years_metrics_future_projection
    )
  )

  prjpars[["years_aggs_by_scen"]] <- c(
    list(list(hist = years_metrics_historical)),
    lapply(
      prjpars[["id_scen_used"]][-1],
      function(k) list(nearterm = 2020:2059, longterm = 2060:2099)
    )
  )

  #   1 = Winter = DJF, 2 = Spring = MAM, 3 = Summer = JJA, 4 = Fall = SON
  prjpars[["season_by_month"]] <- c(rep(1, 2), rep(2:4, each = 3), 1)
  # First season (winter) starts in December
  prjpars[["first_month_of_year"]] <- 12


  args_template <- list(
    path = prjpars[["dir_sw2_output"]],
    id_scen_used = prjpars[["id_scen_used"]],
    group_by_month = prjpars[["season_by_month"]],
    first_month_of_year = prjpars[["first_month_of_year"]],
    dir_out_SW2toTable = tempdir()
  )




  #------ Loop over metrics and aggregate
  for (k1 in seq_along(fun_metrics)) {
    is_out_ts <- has_fun_ts_as_output(fun_metrics[[k1]])

    fun_args <- c(
      args_template,
      list_years_scen_used = if (is_out_ts) {
        list(prjpars[["years_timeseries_by_scen"]])
      } else {
        list(prjpars[["years_aggs_by_scen"]])
      }
    )


    # Call aggregation function for rSOILWAT2 input/output for each site `s`
    if (!do_timing) {
      res <- foo_metrics(
        fun = fun_metrics[k1],
        fun_args = fun_args,
        run_rSFSW2_names = run_rSFSW2_names,
        is_soils_input = has_fun_soils_as_arg(fun_metrics[k1]),
        N_sites = N_sites
      )

    } else {
      time_metrics[k1] <- system.time(
        res <- foo_metrics(
          fun = fun_metrics[k1],
          fun_args = fun_args,
          run_rSFSW2_names = run_rSFSW2_names,
          is_soils_input = has_fun_soils_as_arg(fun_metrics[k1]),
          N_sites = N_sites
        )
      )["elapsed"]
    }


    N_yrs_expected <- sum(lengths(fun_args[["list_years_scen_used"]]))
    expect_equal(
      sapply(res, ncol) - 2,
      rep(N_yrs_expected, length(res))
    )


    values_all_sites <- format_metric_Nsim(
      x = do.call(rbind, res),
      names = run_rSFSW2_names,
      prjpars = prjpars,
      do_collect_inputs = FALSE,
      fun_name = fun_metrics[k1],
      is_out_ts = is_out_ts
    )

    expect_false(anyNA(colnames(values_all_sites)))

    output <- if (is_out_ts) {
      aggs_across_years(
        values_all_sites,
        fun = prjpars[["fun_aggs_across_yrs"]],
        list_years = prjpars[["years_timeseries_by_scen"]],
        id_scens = prjpars[["id_scen_used"]],
        combine = TRUE
      )
    } else {
      values_all_sites
    }

    if (FALSE) {
      # `testthat::expect_snapshot_value()` doesn't properly work
      # for our situation as of v3.0.1:
      # - style "deparse" leads to errors such 'could not find function "-"'
      # - if a new metric is changed or added to the package,
      #   then all tests that are sorted alphabetically later will fail,
      #   likely because
      #   * all snapshots are stored in the same huge file and
      #   * differences are not resolve correctly
      # - the function produces for style = "serialize" a snapshot of c. 12 MB
      #   while saving individual "rds" consumes in total only 3.3 MB
      expect_snapshot_value(x = output, style = "serialize")

    } else {
      ftest_output <- file.path(
        dir_test_data,
        paste0("ref_", fun_metrics[k1], ".rds")
      )

      if (file.exists(ftest_output)) {
        ref_output <- readRDS(ftest_output)
        expect_equal(output, ref_output, label = fun_metrics[k1])

      } else {
        succeed(
          message = paste("New reference stored for", shQuote(fun_metrics[k1]))
        )

        saveRDS(output, file = ftest_output, compress = "xz")
      }
    }
  }


  #------ Report on timing (only if interactively used)
  if (do_timing) {
    ttime <- data.frame(metric = fun_metrics, time = time_metrics)
    print(ttime[order(ttime$time, decreasing = TRUE), ])
  }


  #------ Cleanup
  unlink(prjpars[["dir_sw2_output"]])
})
