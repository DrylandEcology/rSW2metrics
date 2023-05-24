
#--- Metrics load simulations and calculate a specific response ------
required_metrics_arguments <- c(
  "path",
  "name_sw2_run",
  "zipped_runs",
  "id_scen_used",
  "list_years_scen_used",
  "out",
  "..."
)

test_that("Check parameters of outside-facing metrics", {
   funs_check <- list_all_metrics()

  for (fun in funs_check) {
    ff <- formals(fun)

    tmp <- !(required_metrics_arguments %in% names(ff))
    expect_identical(
      required_metrics_arguments[tmp],
      character(0),
      label = paste(shQuote(fun), "is missing required arguments:")
    )

    expect_true(
      all(eval(ff[["out"]]) %in% c("across_years", "ts_years", "raw"))
    )
  }
})


#--- Input collectors prepare simulation inputs for later use ------
required_inputcollectors_arguments <- c(
  "path",
  "name_sw2_run",
  "zipped_runs",
  "..."
)

test_that("Check parameters of input collectors", {
  funs_check <- list_all_input_collectors()

  for (fun in funs_check) {
    ff <- formals(fun)

    tmp <- !(required_inputcollectors_arguments %in% names(ff))
    expect_identical(
      required_inputcollectors_arguments[tmp],
      character(0),
      label = paste(shQuote(fun), "is missing required arguments:")
    )
  }
})


#--- `get_*()` functions load simulations and calculate a response ------
required_get_arguments <- c(
  "path",
  "name_sw2_run",
  "zipped_runs"
)

test_that("check parameters of get_* functions", {
  # List all `get_*()`
  tmp <- ls(getNamespace("rSW2metrics"))
  tmp <- tmp[startsWith(tmp, "get_")]
  ids <- vapply(tmp, function(x) is.function(get0(x)), FUN.VALUE = NA)
  funs_check <- tmp[ids]

  for (fun in funs_check) {
    ff <- formals(fun)

    tmp <- !(required_get_arguments %in% names(ff))
    expect_identical(
      required_get_arguments[tmp],
      character(0),
      label = paste(shQuote(fun), "is missing required arguments:")
    )
  }
})



#--- `calc_*()` functions calculate a response ------
excluded_calc_arguments <- required_get_arguments
excluded_calcfuns <- c(
  "calc_multivariate_from_sw2",
  "calc_univariate_from_sw2"
)

test_that("check parameters of calc_* functions", {
  # List all `calc_*()`
  tmp <- ls(getNamespace("rSW2metrics"))
  tmp <- tmp[startsWith(tmp, "calc_")]
  tmp <- setdiff(tmp, excluded_calcfuns)
  ids <- vapply(tmp, function(x) is.function(get0(x)), FUN.VALUE = NA)
  funs_check <- tmp[ids]

  for (fun in funs_check) {
    ff <- formals(fun)

    tmp <- excluded_calc_arguments %in% names(ff)
    expect_identical(
      excluded_calc_arguments[tmp],
      character(0),
      label = paste(shQuote(fun), "has excluded arguments:")
    )
  }
})
