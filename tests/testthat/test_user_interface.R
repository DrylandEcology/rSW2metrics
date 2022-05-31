

test_that("Check command-line options", {
  # Test inputs
  args_must_have <- c(
    "-o=AI_annual",
    "-fun=metric_AI",
    paste0(
      "-fparam=",
      system.file("exec", "Project_Parameters.R", package = "rSW2metrics")
    )
  )


  #--- Check that all works if must-have options are present
  expect_silent(tmp <- process_arguments(args_must_have))
  expect_true(check_extraction_arguments(tmp))


  #--- Check for error if a must-have option is missing
  for (k in seq_along(args_must_have)) {
    expect_error(process_arguments(args_must_have[-k]))
  }


  #--- Check for warning if incorrect option
  expect_warning(process_arguments(c(args_must_have, "-notimplemented")))


  #--- Check mode: `-mode=[{TRUE,test}|{FALSE,full,any,}]`
  tmp <- process_arguments(c(args_must_have, "-mode=TRUE"))
  expect_true(check_extraction_arguments(tmp))
  expect_false(tmp[["do_full"]])

  tmp <- process_arguments(c(args_must_have, "-mode=test"))
  expect_false(tmp[["do_full"]])


  tmp <- process_arguments(c(args_must_have, "-mode=FALSE"))
  expect_true(check_extraction_arguments(tmp))
  expect_true(tmp[["do_full"]])

  tmp <- process_arguments(c(args_must_have, "-mode=full"))
  expect_true(tmp[["do_full"]])

  tmp <- process_arguments(c(args_must_have, "-mode=NA"))
  expect_true(tmp[["do_full"]])

  tmp <- process_arguments(c(args_must_have, "-mode=any"))
  expect_true(tmp[["do_full"]])

  tmp <- process_arguments(c(args_must_have, "-mode="))
  expect_true(tmp[["do_full"]])

  tmp <- process_arguments(args_must_have)
  expect_true(tmp[["do_full"]])


  #--- Check number of test runs
  tmp <- process_arguments(c(args_must_have, "-ntests=23"))
  expect_true(check_extraction_arguments(tmp))
  expect_equal(tmp[["ntests"]], 23)


  #--- Check sequence of runs
  tmp <- process_arguments(args_must_have)
  expect_equal(tmp[["runids"]], -1)

  tmp <- process_arguments(c(args_must_have, "-mode=test"))
  expect_true(check_extraction_arguments(tmp))
  expect_equal(tmp[["runids"]], seq_len(tmp[["ntests"]]))

  tmp <- process_arguments(c(args_must_have, "-runids=10:20"))
  expect_true(check_extraction_arguments(tmp))
  expect_equal(tmp[["runids"]], 10:20)

  expect_error(process_arguments(c(args_must_have, "-runids=10")))
  expect_error(process_arguments(c(args_must_have, "-runids=-1:20")))
  expect_error(suppressWarnings(
    process_arguments(c(args_must_have, "-runids=NA:NA"))
  ))


  #--- Check size of parallel cluster
  tmp <- process_arguments(c(args_must_have, "-ncores=23"))
  expect_true(check_extraction_arguments(tmp))
  expect_equal(tmp[["ncores"]], 23)


  #--- Check log activity on cluster: `-cllog=[{TRUE,}|{FALSE,any}]`
  tmp <- process_arguments(c(args_must_have, "-cllog=TRUE"))
  expect_true(check_extraction_arguments(tmp))
  expect_true(tmp[["cl_log"]])

  tmp <- process_arguments(c(args_must_have, "-cllog="))
  expect_true(tmp[["cl_log"]])

  tmp <- process_arguments(c(args_must_have, "-cllog"))
  expect_true(tmp[["cl_log"]])


  tmp <- process_arguments(c(args_must_have, "-cllog=FALSE"))
  expect_true(check_extraction_arguments(tmp))
  expect_false(tmp[["cl_log"]])

  tmp <- process_arguments(c(args_must_have, "-cllog=NA"))
  expect_false(tmp[["cl_log"]])

  tmp <- process_arguments(c(args_must_have, "-cllog=any"))
  expect_false(tmp[["cl_log"]])


  #--- Across-year summaries: `-add_aggs_across_yrs=[{TRUE,}|{FALSE,any}]`
  tmp <- process_arguments(c(args_must_have, "-add_aggs_across_yrs=TRUE"))
  expect_true(check_extraction_arguments(tmp))
  expect_true(tmp[["add_aggs_across_yrs"]])

  tmp <- process_arguments(c(args_must_have, "-add_aggs_across_yrs="))
  expect_true(tmp[["add_aggs_across_yrs"]])

  tmp <- process_arguments(c(args_must_have, "-add_aggs_across_yrs"))
  expect_true(tmp[["add_aggs_across_yrs"]])


  tmp <- process_arguments(c(args_must_have, "-add_aggs_across_yrs=FALSE"))
  expect_true(check_extraction_arguments(tmp))
  expect_false(tmp[["add_aggs_across_yrs"]])

  tmp <- process_arguments(c(args_must_have, "-add_aggs_across_yrs=NA"))
  expect_false(tmp[["add_aggs_across_yrs"]])

  tmp <- process_arguments(c(args_must_have, "-add_aggs_across_yrs=any"))
  expect_false(tmp[["add_aggs_across_yrs"]])
})
