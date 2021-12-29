

ref_extraction_arguments <- function() {
  data.frame(
    # command line options
    options = c(
      "-o", "-fun", "-fparam",
      "-mode", "-ntests",
      "-ncores", "-cllog",
      "-add_aggs_across_yrs"
    ),
    # argument names created from command line options
    args = c(
      "tag_filename", "fun_name", "filename_params",
      "do_full", "ntests",
      "ncores", "cl_log",
      "add_aggs_across_yrs"
    ),
    stringsAsFactors = FALSE
  )
}

#' Process arguments of calling script
#'
#' @param x A vector of character strings.
#'   The return value of \code{\link[base]{commandArgs}}.
#'
#' @return A named list with the processed arguments.
#'
#' @export
process_arguments <- function(x) {
  res <- list()

  tmp <- strsplit(x, split = "=", fixed = TRUE)
  tmp <- unlist(sapply(tmp, function(x) if (length(x) == 2) x else c(x[1], NA)))
  args <- matrix(
    data = unlist(tmp),
    ncol = 2,
    byrow = TRUE
  )

  tmp <- args[!(args[, 1] %in% ref_extraction_arguments()[["options"]]), 1]
  if (length(tmp) > 0) {
    warning(
      "Arguments ",
      paste0(shQuote(tmp), collapse = ", "),
      " are not implemented."
    )
  }


  # file name for the output (without extension)
  id <- args[, 1] %in% "-o"
  if (any(id)) {
    res[["tag_filename"]] <- args[id, 2]
  } else {
    stop("Output filename (option `-o`) is missing.")
  }

  # function name to calculate the values (e.g. "get_CorTP_annual")
  id <- args[, 1] %in% "-fun"
  if (any(id)) {
    res[["fun_name"]] <- args[id, 2]
  } else {
    stop("Function name (option `-fun`) is missing.")
  }


  # file name for the project parameters
  id <- args[, 1] %in% "-fparam"
  if (any(id)) {
    res[["filename_params"]] <- args[id, 2]
  } else {
    stop("Name of file with project parameters (option `-fparam`) is missing.")
  }


  # test/production mode
  #   FALSE/"full"/NA/no flag = full dataset
  #   TRUE/"test" = test mode (subset of runs)
  id <- args[, 1] %in% "-mode"
  if (any(id)) {
    res[["do_full"]] <-
      !("test" %in% args[id, 2]) ||
      is.na(args[id, 2]) ||
      isFALSE(as.logical(args[id, 2]))
  } else {
    res[["do_full"]] <- TRUE
  }

  # Number of test runs if `do_full`
  id <- args[, 1] %in% "-ntests"
  if (any(id)) {
    res[["ntests"]] <- as.integer(args[id, 2])
  } else {
    res[["ntests"]] <- 100L
  }

  # Size of parallel cluster
  id <- args[, 1] %in% "-ncores"
  if (any(id)) {
    res[["ncores"]] <- as.integer(args[id, 2])
  } else {
    res[["ncores"]] <- 1L
  }

  # Log activity on cluster?
  id <- args[, 1] %in% "-cllog"
  if (any(id)) {
    res[["cl_log"]] <-
      is.na(args[id, 2]) || isTRUE(as.logical(args[id, 2]))
  } else {
    res[["cl_log"]] <- FALSE
  }


  # Determine whether function output is
  # annual time series (ts) or one value across years
  res[["is_out_ts"]] <- has_fun_ts_as_output(res[["fun_name"]])


  # Check whether to add aggregations across years to output (if `is_out_ts`)
  # as produced by `fun_aggs_across_yrs()`
  id <- args[, 1] %in% "-add_aggs_across_yrs"
  if (any(id)) {
    res[["add_aggs_across_yrs"]] <-
      is.na(args[id, 2]) || isTRUE(as.logical(args[id, 2]))
  } else {
    res[["add_aggs_across_yrs"]] <- FALSE
  }

  res
}

check_extraction_arguments <- function(x) {
  hasnot_args <- !sapply(
    ref_extraction_arguments()[["args"]],
    exists,
    where = x
  )

  if (any(hasnot_args)) {
    stop(
      "The following required extraction arguments are missing: ",
      paste0(shQuote(names(hasnot_args)[hasnot_args]), collapse = ", ")
    )
  }


  stopifnot(!is.na(x[["tag_filename"]]), nzchar(x[["tag_filename"]]))

  stopifnot(is.function(get0(x[["fun_name"]])))

  if (!file.exists(x[["filename_params"]])) {
    stop(
      "Project parameter file ", shQuote(x[["filename_params"]]),
      " cannot be located."
    )
  }

  stopifnot(is.logical(x[["do_full"]]), !is.na(x[["do_full"]]))

  if (!x[["do_full"]]) {
    stopifnot(
      is.integer(x[["ntests"]]),
      is.finite(x[["ntests"]]),
      x[["ntests"]] > 0
    )
  }

  stopifnot(
    is.integer(x[["ncores"]]),
    is.finite(x[["ncores"]]),
    x[["ncores"]] > 0
  )

  stopifnot(is.logical(x[["cl_log"]]), !is.na(x[["cl_log"]]))

  stopifnot(is.logical(x[["is_out_ts"]]), !is.na(x[["is_out_ts"]]))

  stopifnot(
    is.logical(x[["add_aggs_across_yrs"]]),
    !is.na(x[["add_aggs_across_yrs"]])
  )

  invisible(TRUE)
}


required_project_parameters <- function() {
  c(
    "dir_sw2_output", "dir_out", "has_rSOILWAT2_inputs",
    "make_short_run_names",
    "N_exp", "N_scen", "id_scen_used",
    "years_timeseries_by_scen", "years_aggs_by_scen",
    "season_by_month", "first_month_of_year",
    "Nmax_soillayers",
    "used_soil_variables"
  )
}

check_project_parameters <- function(x, args) {
  #--- Add missing parameter (and warn)
  # e.g., if this is run with an earlier version of "Project_Parameters.R"
  if (!exists("has_rSOILWAT2_inputs", where = x)) {
    warning(
      "Project parameter 'has_rSOILWAT2_inputs' is missing; ",
      "using default value 'TRUE'."
    )

    x[["has_rSOILWAT2_inputs"]] <- TRUE
  }

  if (!exists("make_short_run_names", where = x)) {
    warning(
      "Project parameter 'make_short_run_names' is missing; ",
      "using default value 'rSW2metrics::shorten_run_names()'."
    )

    x[["make_short_run_names"]] <- rSW2metrics::shorten_run_names
  }


  #---
  hasnot_params <- !sapply(
    required_project_parameters(),
    exists,
    where = x
  )

  if (any(hasnot_params)) {
    stop(
      "The following required project parameters are missing: ",
      paste0(shQuote(names(hasnot_params)[hasnot_params]), collapse = ", ")
    )
  }

  if (!dir.exists(x[["dir_sw2_output"]])) {
    stop(
      "Cannot locate simulation output at ",
      shQuote(normalizePath(x[["dir_sw2_output"]], mustWork = FALSE))
    )
  }

  if (args[["add_aggs_across_yrs"]] && args[["is_out_ts"]]) {
    stopifnot(
      exists("fun_aggs_across_yrs", where = x),
      is.function(x[["fun_aggs_across_yrs"]]) ||
        is.function(get0(x[["fun_aggs_across_yrs"]]))
    )
  }

  invisible(TRUE)
}




#' Main function that organizes the extraction of metrics of a simulation
#' project and stores them on disk
#'
#' @param args A named list with name - value pairs of arguments;
#'   a list as produced by \code{\link{process_arguments}}.
#'
#' @export
extract_metrics <- function(args) {

  #------ Check extraction parameters
  check_extraction_arguments(args)


  #------ Load project parameters
  prjpars <- new.env()
  sys.source(
    file = args[["filename_params"]],
    envir = prjpars
  )

  check_project_parameters(prjpars, args)


  #------ Soil information
  tmpi <- prjpars[["used_soil_variables"]]
  tmpa <- list_soil_variables()
  ids <- match(tmpi, tmpa, nomatch = 0)
  soil_variables <- tmpi[ids > 0]
  names(soil_variables) <- names(tmpa)[ids]

  is_soils_input <- has_fun_soils_as_arg(args[["fun_name"]])

  fname_prepared_soildata <- file.path(
    prjpars[["dir_out"]],
    paste0("input_soillayers_", soil_variables, ".rds")
  )

  has_prepared_soils <-
    is_soils_input &&
    all(file.exists(fname_prepared_soildata))

  if (is_soils_input && !has_prepared_soils) {
    if (!prjpars[["has_rSOILWAT2_inputs"]]) {
      stop(
        shQuote(args[["fun_name"]]), " requires soils as input; ",
        "however, neither pre-extracted soil data nor simulation inputs ",
        "are available."
      )
    }
  }

  do_collect_inputs <- is_fun_collecting_inputs(args[["fun_name"]])



  #------ Directories
  dir.create(prjpars[["dir_out"]], recursive = TRUE, showWarnings = FALSE)
  stopifnot(dir.exists(prjpars[["dir_out"]]))

  filename_output <- file.path(
    prjpars[["dir_out"]],
    paste0(args[["tag_filename"]], ".rds")
  )

  # Test ability to write to this location
  filename_output_test <- sub(".rds", "_test.rds", filename_output)
  tmp <- try(saveRDS("test", file = filename_output_test), silent = TRUE)
  if (inherits(tmp, "try-error")) {
    stop(
      "We cannot write to the designated output directory: ",
      shQuote(normalizePath(dirname(filename_output_test), mustWork = FALSE))
    )
  } else {
    unlink(filename_output_test)
  }



  #------ Loop through runs calling function for each run ####
  # (rSFSW2) "run" = combination of sites and experimental treatments
  # if prjpars[["N_exp"]] == 1, then runs = sites
  # Each (rSFSW2) "run" contains one (SOILWAT2) run for each climate scenario

  dir_runs_rSFSW2 <- list.files(prjpars[["dir_sw2_output"]], full.names = TRUE)
  stopifnot(length(dir_runs_rSFSW2) > 0)
  run_rSFSW2_names <- basename(dir_runs_rSFSW2)

  # (Shortened) run identifier
  tag_run_rSFSW2_names <- if (prjpars[["N_exp"]] == 1) {
    prjpars[["make_short_run_names"]](run_rSFSW2_names)
  } else {
    run_rSFSW2_names
  }


  # Index across runs
  indexes <- if (args[["do_full"]]) {
    seq_along(run_rSFSW2_names)
  } else {
    seq_len(min(args[["ntests"]], length(run_rSFSW2_names)))
  }

  # Check cores
  tmp <- parallel::detectCores() - 1
  if (is.na(tmp)) tmp <- args[["ncores"]]
  args[["ncores"]] <- as.integer(min(args[["ncores"]], tmp))


  # Let user know what is happening
  t_start <- Sys.time()

  cat(
    "-----------------------------------------------------------------------",
    fill = TRUE
  )
  cat(
    "'rSW2metrics' version", getNamespaceVersion("rSW2metrics"),
    "at", format(t_start),
    fill = TRUE
  )
  cat(
    "This is a", if (args[["do_full"]]) "production call" else "test",
    "with", length(indexes), "sites.",
    fill = TRUE
  )
  cat("Metric:", shQuote(args[["fun_name"]]), fill = TRUE)
  cat("Output name =", shQuote(args[["tag_filename"]]), fill = TRUE)
  if (is_soils_input) {
    cat(
      shQuote(args[["fun_name"]]),
      "requires soil information as input:",
      if (has_prepared_soils) {
        "pre-extracted soil data will be used."
      } else {
        "soil data will be extracted individually from simulation inputs."
      },
      fill = TRUE
    )
  }
  cat(
    shQuote(args[["fun_name"]]),
    if (do_collect_inputs) {
      "collects input data (e.g., soils)."
    } else {
      if (args[["is_out_ts"]]) {
        "produces annual time-series."
      } else {
        "produces aggregations across years."
      }
    },
    fill = TRUE
  )
  if (args[["is_out_ts"]] && args[["add_aggs_across_yrs"]]) {
    cat("We are adding aggregations across years to final output.", fill = TRUE)
  }

  cat("Processing on", args[["ncores"]], "cores ...", fill = TRUE)



  ######################################################################
  ##### Soils Info (if necessary)  ------------------------------  #####
  ######################################################################

  if (is_soils_input) {
    if (has_prepared_soils) {
      soils <- lapply(fname_prepared_soildata, readRDS)
      names(soils) <- names(soil_variables)

      if (length(soils) > 1) {
        for (k in seq_along(soils)) {
          # Check that we have soils for every simulation runs
          is_run_wo_soils <- !(tag_run_rSFSW2_names %in% soils[[k]][, "site"])
          if (any(is_run_wo_soils)) {
            stop(
              "We don't have soil data for simulation run(s): ",
              paste0(
                shQuote(tag_run_rSFSW2_names[is_run_wo_soils]),
                collapse = ","
              )
            )
          }
        }
      }

    } else {
      warning(
        "Function ", shQuote(args[["fun_name"]]), " requires soils as input; ",
        "this is most efficient if soils have been collected with function ",
        "'collect_input_soillayers_xxx()'."
      )
    }
  }


  ######################################################################
  ##### Get data!                  ------------------------------  #####
  ######################################################################

  #--- Prepare function arguments
  if (do_collect_inputs) {
    fun_args <- list(
      path = prjpars[["dir_sw2_output"]],
      Nmax_soillayers = prjpars[["Nmax_soillayers"]]
    )

  } else {
    fun_args <- list(
      path = prjpars[["dir_sw2_output"]],
      id_scen_used = prjpars[["id_scen_used"]],
      list_years_scen_used = if (args[["is_out_ts"]]) {
        prjpars[["years_timeseries_by_scen"]]
      } else {
        prjpars[["years_aggs_by_scen"]]
      },
      group_by_month = prjpars[["season_by_month"]],
      first_month_of_year = prjpars[["first_month_of_year"]]
    )


    #--- Arguments for alternative output type via `metric_SW2toTable_daily()`
    tmp_args <- c(
      "dir_out_SW2toTable", "format_share_SW2toTable",
      "share_soillayer_ids",
      "outputs_SW2toTable"
    )

    # note: because `prjpars` is an environment,
    # we cannot use `[` and need to loop each with `[[`
    for (k in seq_along(tmp_args)) {
      if (tmp_args[k] %in% names(prjpars)) {
        fun_args[[tmp_args[k]]] <- prjpars[[tmp_args[k]]]
      }
    }

    # use `dir_out` if `dir_out_SW2toTable` is not defined
    if (!("dir_out_SW2toTable" %in% names(prjpars))) {
      warning(
        "Output will be written to `dir_out` ",
        "instead of expected but absent `dir_out_SW2toTable`."
      )
      fun_args[["dir_out_SW2toTable"]] <- prjpars[["dir_out"]]
    }
  }



  # Prepare cluster
  if (args[["ncores"]] > 1) {
    tmp_args <- list(spec = args[["ncores"]])
    if (args[["cl_log"]]) {
      tmp_args[["outfile"]] <- file.path(
        prjpars[["dir_out"]],
        paste0(
          "log_rSW2metrics_",
          args[["tag_filename"]], "_",
          format(Sys.time(), "%Y%m%d-%H%M%S"),
          ".txt"
        )
      )
    }

    cl <- do.call(what = parallel::makeCluster, args = tmp_args)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl), add = TRUE)

  } else {
    foreach::registerDoSEQ()
  }


  # Do the extractions
  s <- NULL # define `s` to avoid "Undefined global functions or variables"

  values_all_sites <- foreach::foreach(
    s = indexes,
    .combine = rbind,
    .errorhandling = "stop",
    .packages = "rSW2metrics"
  ) %dopar% {

    has_sw2_output <- check_all_output_available_of_run(
      path_to_run = file.path(fun_args[["path"]], run_rSFSW2_names[s]),
      N_scen = prjpars[["N_scen"]],
      check_input = prjpars[["has_rSOILWAT2_inputs"]]
    )

    if (has_sw2_output) {
      res <- process_values_one_site(
        fun = args[["fun_name"]],
        fun_args = fun_args,
        name_sw2_run = run_rSFSW2_names[s],
        name_sw2_run_soils = tag_run_rSFSW2_names[s],
        is_soils_input = is_soils_input,
        soils = if (exists("soils")) soils,
        soil_variables = soil_variables
      )

      format_metric_1sim(x = res, id = s)
    }
  }


  #------ Format output
  values_all_sites <- format_metric_Nsim(
    x = values_all_sites,
    names = tag_run_rSFSW2_names,
    prjpars = prjpars,
    do_collect_inputs = do_collect_inputs,
    fun_name = args[["fun_name"]],
    is_out_ts = args[["is_out_ts"]]
  )

  #------ Add aggregations across years (if requested and we have yearly values)
  if (args[["add_aggs_across_yrs"]] && args[["is_out_ts"]]) {
    values_all_sites <- aggs_across_years(
      values_all_sites,
      fun = prjpars[["fun_aggs_across_yrs"]],
      list_years = prjpars[["years_timeseries_by_scen"]],
      id_scens = prjpars[["id_scen_used"]],
      combine = TRUE
    )
  }


  #------ Save to disk
  tmp <- try(
    saveRDS(
      values_all_sites,
      file = filename_output
    )
  )

  if (inherits(tmp, "try-error")) {
    tmp_filename_output <- file.path(getwd(), basename(filename_output))
    cat(
      "Saving output to",
      shQuote(normalizePath(filename_output, mustWork = FALSE)), "failed;",
      "attempting to write instead to",
      shQuote(normalizePath(tmp_filename_output, mustWork = FALSE)),
      fill = TRUE
    )
    saveRDS(values_all_sites, file = tmp_filename_output)
  }


  # Conclude -------------------------------------------------------------------
  t_end <- Sys.time()
  cat(
    "Completed extraction for",
    length(unique(values_all_sites[, "site"])), "sites",
    "at", format(t_end), "in", format(round(t_end - t_start, 2)),
    fill = TRUE
  )
  tmp <- warnings()
  if (is.list(tmp) && length(tmp) > 0) {
    print(summary(tmp))
  }
  cat(
    "-----------------------------------------------------------------------",
    fill = TRUE
  )

  invisible(TRUE)
}



#' Extract the requested metrics from one simulation
#' @noRd
process_values_one_site <- function(
  fun,
  fun_args,
  name_sw2_run,
  name_sw2_run_soils = NULL,
  is_soils_input = FALSE,
  soils = NULL,
  soil_variables = NULL
) {
  # Add run-specific arguments
  used_args <- c(
    fun_args,
    name_sw2_run = name_sw2_run
  )


  if (is_soils_input) {
    used_args[["soils"]] <- prepare_soils_for_site(
      path = fun_args[["path"]],
      name_sw2_run = name_sw2_run,
      name_sw2_run_soils = name_sw2_run_soils,
      soils = soils,
      soil_variables = soil_variables
    )
  }

  # Call aggregation function for rSOILWAT2 input/output
  do.call(what = fun, args = used_args)
}


#' Transform result into wide format suitable for spreadsheet concatenation
#' @noRd
format_metric_1sim <- function(x, id) {
  if (is.list(x)) {
    # List with one element per climate scenario
    # Transform result into
    # [n groups (+ soil layers) x (years by scenarios)] 2-dim object
    ngs <- nrow(x[[1]])

    if (is.null(ngs)) {
      c(id, 0, do.call(c, x))

    } else {
      ngls <- rownames(x[[1]])
      if (is.null(ngls)) {
        ngls <- if (ngs > 1) seq_len(ngs) else 0
      }

      data.frame(
        site = id,
        group = ngls,
        do.call(cbind, x)
      )
    }

  } else {
    #--- do_collect_inputs
    # 1- or 2-dimensional object
    # Transform result into [1 x output columns] object
    ngs <- nrow(x)

    if (is.null(ngs) || ngs == 1) {
      c(id, 0, x)

    } else {
      c(id, 0, as.vector(t(x)))
    }

  }
}


#' Format concatenated spreadsheet with identifiers and column names
#'
#' @noRd
format_metric_Nsim <- function(
  x, names, prjpars,
  do_collect_inputs = FALSE,
  fun_name = "",
  is_out_ts = TRUE
) {
  #--- We need a data.frame to add run identifiers (character strings)
  # No/one-group aggregations may return a matrix
  if (!is.data.frame(x)) {
    x <- as.data.frame(x)
  }


  #--- Add run identifiers
  x[, 1] <- names[x[, 1]]


  #--- Add column names
  if (do_collect_inputs) {
    tmp <- sub("collect_input_soillayers_", "", fun_name)
    col_var_names <- paste0(tmp, "_L", seq_len(ncol(x) - 2))

  } else {

    if (is_out_ts) {
      stopifnot(
        c("id_scen_used", "years_timeseries_by_scen") %in% names(prjpars)
      )

      tmp <- lapply(
        seq_along(prjpars[["id_scen_used"]]),
        function(k1) {
          paste0(
            "sc",
            prjpars[["id_scen_used"]][k1],
            "_",
            prjpars[["years_timeseries_by_scen"]][[k1]]
          )
        }
      )

    } else {
      stopifnot(
        c("id_scen_used", "years_aggs_by_scen") %in% names(prjpars)
      )

      tmp <- lapply(
        seq_along(prjpars[["id_scen_used"]]),
        function(k1) {
          paste0(
            "sc",
            prjpars[["id_scen_used"]][k1],
            "_",
            names(prjpars[["years_aggs_by_scen"]][[k1]])
          )
        }
      )
    }

    col_var_names <- unlist(tmp)
  }

  cn_header <- c("site", "group")
  colnames(x) <- c(cn_header, col_var_names)

  if (ncol(x) != length(cn_header) + length(col_var_names)) {
    warnings(
      "Number of columns of data.frame and calculated column names ",
      "mismatch: this could be due to a mismatch in ",
      "requested years and years returned."
    )
  }


  # Remove row names
  rownames(x) <- NULL

  x
}



#' Interface to formatted output of a metric for one simulation
#' @noRd
formatted_metric_1sim <- function(
  metric_foo_name,
  foo_args = list(
    path = NULL,
    name_sw2_run = NULL,
    id_scen_used = NULL,
    list_years_scen_used = NULL,
    soils = NULL,
    out = NULL
  ),
  do_collect_inputs = FALSE
) {

  req_arg_names <- c(
    "path", "name_sw2_run",
    "id_scen_used", "list_years_scen_used",
    "soils",
    "out"
  )
  has_arg_names <- req_arg_names %in% names(foo_args)

  if (any(!has_arg_names)) {
    stop(
      "`foo_args` is missing the named element(s): ",
      paste0(shQuote(req_arg_names[!has_arg_names]), collapse = ", ")
    )
  }

  res <- do.call(what = metric_foo_name, args = foo_args)

  is_out_ts <- identical(foo_args[["out"]], "ts_years")

  if (is_out_ts) {
    prjpars <- list(id_scen_used = foo_args[["id_scen_used"]])
    if (is_out_ts) {
      prjpars[["years_timeseries_by_scen"]] <-
        foo_args[["list_years_scen_used"]]

    } else {
      prjpars[["years_aggs_by_scen"]] <- foo_args[["list_years_scen_used"]]
    }

    format_metric_Nsim(
      x = list(format_metric_1sim(res, id = 1)),
      names = foo_args[["name_sw2_run"]],
      prjpars = prjpars,
      do_collect_inputs = do_collect_inputs,
      fun_name = metric_foo_name,
      is_out_ts = is_out_ts
    )

  } else if (foo_args[["out"]] == "raw") {
    res
  }
}
