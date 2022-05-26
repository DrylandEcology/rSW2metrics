
#--- Metrics load simulations and calculate a specific response ------
required_metrics_arguments <- c(
  "path",
  "name_sw2_run",
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
    tmp_args <- required_metrics_arguments[tmp]
    expect_equal(
      tmp_args, character(0),
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
  "..."
)

test_that("Check parameters of input collectors", {
  funs_check <- list_all_input_collectors()

  for (fun in funs_check) {
    ff <- formals(fun)

    tmp <- !(required_inputcollectors_arguments %in% names(ff))
    tmp_args <- required_inputcollectors_arguments[tmp]
    expect_equal(
      tmp_args,
      character(0),
      label = paste(shQuote(fun), "is missing required arguments:")
    )
  }
})


#--- `get_*()` functions load simulations and calculate a response ------
required_get_arguments <- c(
  "path",
  "name_sw2_run"
)

test_that("check parameters of get_* functions", {
  # List all `get_*()`
  tmp <- ls(getNamespace("rSW2metrics"))
  tmp <- tmp[grepl("^get_", tmp)]
  funs_check <- tmp[sapply(tmp, function(x) is.function(get0(x)))]

  for (fun in funs_check) {
    ff <- formals(fun)

    tmp <- !(required_get_arguments %in% names(ff))
    tmp_args <- required_get_arguments[tmp]
    expect_equal(
      tmp_args,
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
  tmp <- tmp[grepl("^calc_", tmp)]
  tmp <- setdiff(tmp, excluded_calcfuns)
  funs_check <- tmp[sapply(tmp, function(x) is.function(get0(x)))]

  for (fun in funs_check) {
    ff <- formals(fun)

    tmp <- excluded_calc_arguments %in% names(ff)
    tmp_args <- excluded_calc_arguments[tmp]
    expect_equal(
      tmp_args,
      character(0),
      label = paste(shQuote(fun), "has excluded arguments:")
    )
  }
})
