
dir_test_data <- file.path("..", "test_data")
test_that("Test data availability", {
  expect_true(dir.exists(dir_test_data), info = getwd())
})


#--- rSOILWAT2 versions with reference output files available
# NOTE: may need to update references for new major/minor releases of rSOILWAT2
#   * set `create_new_reference_output` to TRUE
#     (and re-set back to FALSE when completed)
create_new_reference_output <- FALSE

# NA = detect currently installed version
#   * basis rSOILWAT2 v5.0.4
#   * rSOILWAT2 versions leading to significant differences in metrics
#     (last patch for each minor version):
#     v5.1.3, v5.2.0, v6.0.x
used_rSOILWAT2_version <- NA


# Aggregation function for rSOILWAT2 input/output for each simulated site
foo_metrics <- function(
  fun,
  fun_args,
  run_rSFSW2_names,
  is_soils_input,
  is_swrc_input,
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
        soil_variables = list_soil_variables(),
        is_swrc_input = is_swrc_input
      ))
      format_metric_1sim(x = tmp, id = s)
    }
  )
}


test_that("Check metrics", {
  skip_on_ci()

  #--- List all metric functions ------
  fun_metrics <- list_all_metrics()


  # List of metrics that call outdated `calc_univariate_from_sw2()`
  # or `calc_multivariate_from_sw2()`
  # (instead of up-to-date `collect_sw2_sim_data()`) and cannot handle:
  #  * varying simulation periods
  #  * non-simulated requested years
  old_fun_metrics <- c(
    "metric_SWA_Seasonal_top50cm",
    "metric_SWP_SoilLayers_MeanMonthly",
    "metric_DrySoilDays_Seasonal_wholeprofile",
    "metric_DrySoilDays_Seasonal_top50cm",
    "metric_PPT_Seasonal",
    "metric_PET_Seasonal",
    "metric_VWC_Seasonal_wholeprofile",
    "metric_SemiDryDuration_Annual_top50cm",
    "metric_SemiDryDuration_Annual_wholeprofile",
    "metric_WetSoilDays_Seasonal_wholeprofile",
    "metric_TemperatureMin_Seasonal",
    "metric_ExtremeShortTermDryStress_Seasonal_top50cm",
    "metric_WetSoilDays_Seasonal_top50cm",
    "metric_NonDrySWA_Seasonal_wholeprofile",
    "metric_Evaporation_Seasonal",
    "metric_VWC_Seasonal_top50cm",
    "metric_Transpiration_Seasonal",
    "metric_TemperatureMax_Seasonal",
    "metric_SWA_Seasonal_wholeprofile",
    "metric_NonDrySWA_Seasonal_top50cm",
    "metric_ExtremeShortTermDryStress_Seasonal_wholeprofile",
    "metric_CorTP_Annual",
    "metric_TemperatureMean_Seasonal",
    "metric_FrostDays_Seasonal"
  )


  #------ Timing (only if interactively used)
  do_timing <- interactive() && !testthat::is_testing()


  #--- Create an example rSOILWAT2 simulation run with several scenarios
  prjpars <- list()
  prjpars[["fun_aggs_across_yrs"]] <- mean_cv_trend

  prjpars[["dir_sw2_output"]] <- tempdir()

  prjpars[["N_scen"]] <- 3
  prjpars[["id_scen_used"]] <- seq_len(prjpars[["N_scen"]])


  #--- Simulation time periods
  years_sim_historical <- 1980:2010
  years_sim_future_projection <- 2006:2099

  years_sim_timeseries_by_scen <- c(
    list(years_sim_historical),
    lapply(
      prjpars[["id_scen_used"]][-1],
      function(k) years_sim_future_projection
    )
  )

  #--- Metric time periods
  years_metrics_historical <- 1990:2010 # -> discard 1980:1989
  years_metrics_future_projection <- 2050:2090 # -> discard 2006:2049, 2091:2099
  stopifnot(
    sum(years_metrics_historical %in% years_sim_historical) > 0,
    sum(years_metrics_future_projection %in% years_sim_future_projection) > 0
  )

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


  #--- Seasonal metrics
  #   1 = Winter = DJF, 2 = Spring = MAM, 3 = Summer = JJA, 4 = Fall = SON
  prjpars[["season_by_month"]] <- c(rep(1, 2), rep(2:4, each = 3), 1)
  # First season (winter) starts in December
  prjpars[["first_month_of_year"]] <- 12





  #------ Put together rSOILWAT2 simulations and save to disk ------
  # Site = 1: rSOILWAT2 example data
  # Site = 2: as site 1, but with one only soil layer
  # Site = 3: as site 1, but simulation starts/ends 20/10 years later

  N_sites <- 3
  run_rSFSW2_names <- paste0("rSW2metrics_rSOILWAT2testrun", seq_len(N_sites))
  dir_runs_rSFSW2 <- file.path(prjpars[["dir_sw2_output"]], run_rSFSW2_names)
  tmp <- lapply(
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
      years <- if (s == 3) {
        if (sc == 1) {
          # start 20 years later but end 10 years later
          # --> 1990:1999 requested but not simulated; discard 2011:2020
          tmp <- 20 + years_sim_timeseries_by_scen[[sc]]
          tmp[1:(length(tmp) - 10)]
        } else {
          # start 30 years later and end 20 years earlier
          # --> 2080:2090 requested but not simulated; discard 2036:2049
          tmp <- 30 + years_sim_timeseries_by_scen[[sc]]
          tmp[1:(length(tmp) - 50)]
        }
      } else {
        years_sim_timeseries_by_scen[[sc]]
      }

      if (getNamespaceVersion("rSOILWAT2") < as.numeric_version("6.0.0")) {
        rSOILWAT2::swWeather_FirstYearHistorical(sw2_in) <- -1
      }
      rSOILWAT2::swYears_StartYear(sw2_in) <- 0
      rSOILWAT2::swYears_EndYear(sw2_in) <- years[length(years)]
      rSOILWAT2::swYears_StartYear(sw2_in) <- years[[1]]

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

      if (is.na(used_rSOILWAT2_version)) {
        used_rSOILWAT2_version <- rSOILWAT2::get_version(runDataSC)
      }

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
  args_template <- list(
    path = prjpars[["dir_sw2_output"]],
    id_scen_used = prjpars[["id_scen_used"]],
    group_by_month = prjpars[["season_by_month"]],
    first_month_of_year = prjpars[["first_month_of_year"]],
    dir_out_SW2toTable = tempdir(),
    # We use `mean` for across-year summaries for historical reasons;
    # this would be `prjpars[["fun_aggs_across_yrs"]]` in production
    fun_aggs_across_yrs = mean
  )



  #------ Loop over unzipped and zipped versions of runs -------
  for (kzip in c(FALSE, TRUE)) {

    #------ Zip folders ------
    if (kzip) {
      used_run_rSFSW2_names <- paste0(run_rSFSW2_names, ".zip")

      # See rSFSW2/tools/SFSW2_project_zip3runs.R
      for (ks in seq_len(N_sites)) {
        fname_run <- file.path(
          prjpars[["dir_sw2_output"]],
          run_rSFSW2_names[ks]
        )
        fname_zip <- file.path(
          prjpars[["dir_sw2_output"]],
          used_run_rSFSW2_names[ks]
        )

        ret <- utils::zip(
          zipfile = fname_zip,
          files = fname_run,
          flags = "-jrTq0"
        )

        if (ret == 0 && file.exists(fname_zip)) {
          unlink(fname_run, recursive = TRUE)
        } else {
          stop("Zipping of simulation output failed.")
        }
      }

    } else {
      used_run_rSFSW2_names <- run_rSFSW2_names
    }


    #------ Prepare timing information ------
    if (do_timing) {
      time_metrics <- vector("numeric", length = length(fun_metrics))
    }


    #------ Loop over metrics and aggregate ------
    for (k1 in seq_along(fun_metrics)) {
      is_out_ts <- has_fun_ts_as_output(fun_metrics[[k1]])

      fun_args <- c(
        args_template,
        list_years_scen_used = if (is_out_ts) {
          list(prjpars[["years_timeseries_by_scen"]])
        } else {
          list(prjpars[["years_aggs_by_scen"]])
        },
        zipped_runs = kzip
      )

      N_sites_used <- if (fun_metrics[[k1]] %in% old_fun_metrics) 2 else N_sites
      ids_used_runs <- seq_len(N_sites_used)


      #--- Call aggregation function for rSOILWAT2 input/output for each site
      if (do_timing) {
        time_metrics[k1] <- system.time(
          # nolint start: implicit_assignment_linter.
          res <- foo_metrics(
            fun = fun_metrics[k1],
            fun_args = fun_args,
            run_rSFSW2_names = used_run_rSFSW2_names[ids_used_runs],
            is_soils_input = has_fun_soils_as_arg(fun_metrics[k1]),
            is_swrc_input = has_fun_swrc_as_arg(fun_metrics[k1]),
            N_sites = N_sites_used
          )
          # nolint end: implicit_assignment_linter.
        )[["elapsed"]]

      } else {
        res <- foo_metrics(
          fun = fun_metrics[k1],
          fun_args = fun_args,
          run_rSFSW2_names = used_run_rSFSW2_names[ids_used_runs],
          is_soils_input = has_fun_soils_as_arg(fun_metrics[k1]),
          is_swrc_input = has_fun_swrc_as_arg(fun_metrics[k1]),
          N_sites = N_sites_used
        )
      }


      #--- Check that output contains columns for each requested year x scenario
      N_yrs_expected <- sum(lengths(fun_args[["list_years_scen_used"]]))
      expect_identical(
        vapply(res, ncol, FUN.VALUE = NA_integer_) - 2L,
        rep(N_yrs_expected, length(res))
      )


      values_all_sites <- format_metric_Nsim(
        x = do.call(rbind, res),
        names = used_run_rSFSW2_names,
        prjpars = prjpars,
        do_collect_inputs = FALSE,
        fun_name = fun_metrics[k1],
        is_out_ts = is_out_ts
      )

      #--- Check that formatted output has column names
      expect_false(anyNA(colnames(values_all_sites)))


      #--- Check consistency of the one output time step
      submetric_timesteps <- unique(
        identify_submetric_timesteps(values_all_sites[, "group"])
      )

      tmp <- submetric_timesteps != "yearly"
      if (any(tmp)) {
        # Check that time step occurs as pattern in function name
        # (if not annual)
        expect_true(
          grepl(
            !!submetric_timesteps[tmp],
            !!fun_metrics[k1],
            ignore.case = TRUE
          )
        )

      } else {
        # Check that no subannual timestep occurs in annual function name
        # Avoid exceptions
        tmp <- any(vapply(
          "seasonality",
          FUN = grepl,
          x = fun_metrics[k1],
          ignore.case = TRUE,
          FUN.VALUE = NA
        ))

        if (!tmp) {
          for (ts_suba in names(list_subannual_timesteps())) {
            expect_false(
              grepl(!!ts_suba, !!fun_metrics[k1], ignore.case = TRUE)
            )
          }
        }
      }

      # Check that there is exactly one time step
      expect_length(!!c(fun_metrics[k1], submetric_timesteps), n = 2)


      #--- Calculate aggregations across years
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


      #--- Check that output is a data.frame
      # with character vectors (for `site` and `group`) and
      # with numeric/logical vectors (for metric values/SW2toTable)
      expect_equal(
        vapply(
          output,
          function(x) {
            !is.list(x) &&
              is.vector(x) &&
              typeof(x) %in% c("character", "integer", "double", "logical")
          },
          FUN.VALUE = NA
        ),
        rep(TRUE, ncol(output)),
        ignore_attr = "names"
      )



      #--- * Check output against stored copy of previous output ------
      # note: previous values depend on the (minor) version of rSOILWAT2
      vused <- paste(
        strsplit(
          used_rSOILWAT2_version,
          split = ".",
          fixed = TRUE
        )[[1]][1:2],
        collapse = "."
      )

      do_create_new_reference_output <- create_new_reference_output

      #--- Locate previous output
      # Output derived with rSOILWAT2 at the current version or at the
      # most recent previous and available version,
      # but not at a later version than current
      ftest_outputs <- list.files(
        path = dir_test_data,
        pattern = paste0("ref_", fun_metrics[k1], ".rds"),
        recursive = TRUE,
        full.names = TRUE
      )

      vhas <- sub("v", "", basename(dirname(ftest_outputs)), fixed = TRUE)
      compvs <- which(as.numeric_version(vhas) <= as.numeric_version(vused))

      ftest_output <- ftest_outputs[compvs[length(compvs)]]
      has_test_output <- length(ftest_output) == 1L

      vtag <- basename(dirname(ftest_output))


      #--- Compare output against previous output
      if (has_test_output) {
        # Fail test if previous output is different:
        #  !create_new_reference_output OR
        #  (create_new_reference_output AND previous output is current version)

        # Save new file if previous output is different:
        #  (create_new_reference_output AND previous output is old version)

        ref_output <- readRDS(ftest_output)

        # Check versions
        has_outdated_version <-
          as.numeric_version(sub("v", "", vtag, fixed = TRUE)) <
          as.numeric_version(vused)


        ids <- if (FALSE && vtag == "v5.0" && nrow(output) > nrow(ref_output)) {
          # previously, reference for v5.0 contained values for sites 1 and 2
          output[["site"]] %in% run_rSFSW2_names[1:2]
        } else {
          seq_len(nrow(output))
        }


        if (create_new_reference_output && has_outdated_version) {
          # Since we may save output as new file,
          # skip expectation if previous output is different
          do_test <- isTRUE(
            all.equal(
              target = output[ids, ],
              current = ref_output,
              tolerance = sqrt(.Machine[["double.eps"]])
            )
          )

          do_create_new_reference_output <- !do_test

        } else {
          # Test expectation in any case because we will not save a new file
          do_test <- TRUE
          do_create_new_reference_output <- FALSE
        }


        if (do_test) {
          #--- Check character columns: "site", "group"
          var_str <- c("site", "group")

          expect_equal(
            output[ids, var_str, drop = FALSE],
            expected = ref_output[, var_str, drop = FALSE],
            label = fun_metrics[k1],
            tolerance = sqrt(.Machine[["double.eps"]])
          )


          #--- Check (numeric) results (but by "group" instead of year)
          var_res <- colnames(output)[!(colnames(output) %in% var_str)]

          res_output_by_group <- as.data.frame(
            t(output[ids, var_res, drop = FALSE])
          )
          colnames(res_output_by_group) <- output[["group"]]

          has_var_res <- setequal(
            var_res,
            colnames(ref_output)[!(colnames(ref_output) %in% var_str)]
          )

          expect_true(has_var_res)

          if (has_var_res) {
            ref_output_by_group <- as.data.frame(
              t(ref_output[, var_res, drop = FALSE])
            )
            colnames(ref_output_by_group) <- ref_output[["group"]]

            expect_equal(
              res_output_by_group,
              expected = ref_output_by_group,
              label = fun_metrics[k1],
              tolerance = sqrt(.Machine[["double.eps"]])
            )
          }
        }
      }


      #--- Create new reference output or warn
      if (do_create_new_reference_output) {
        vtag_used <- paste0("v", vused)

        ftest_new_output <- file.path(
          dir_test_data,
          vtag_used,
          paste0("ref_", fun_metrics[k1], ".rds")
        )

        if (isTRUE(file.exists(ftest_new_output))) {
          fail(
            paste(
              "Reference of", shQuote(fun_metrics[k1]),
              "for", vtag_used,
              "already exists and is different from output."
            )
          )

        } else {
          msg <- paste(
            "New reference of", shQuote(fun_metrics[k1]),
            "for", vtag_used,
            "stored on disk because",
            if (has_test_output) {
              paste0(
                "output is different from previous reference for ",
                vtag,
                "."
              )
            } else {
              "previous reference does not exist."
            }
          )

          succeed(msg)

          dir.create(
            dirname(ftest_new_output),
            recursive = TRUE,
            showWarnings = FALSE
          )
          saveRDS(output, file = ftest_new_output, compress = "xz")
        }

      } else if (!has_test_output) {
        fail(
          paste0(
            shQuote(fun_metrics[k1]),
            ": no comparisons against previous values ",
            "because reference values could not be found or ",
            "created (see `create_new_reference_output`)."
          )
        )
      }
    }


    #------ Report on timing (only if interactively used) ------
    if (do_timing) {
      ttime <- data.frame(metric = fun_metrics, time = time_metrics)
      cat(
        "Timing of metrics based on simulations organized in ",
        if (kzip) "zipped archives" else "folders",
        ":",
        sep = "",
        fill = TRUE
      )
      print(ttime[order(ttime[["time"]], decreasing = TRUE), ])
    }
  }




  #------ Cleanup ------
  unlink(prjpars[["dir_sw2_output"]])
})
