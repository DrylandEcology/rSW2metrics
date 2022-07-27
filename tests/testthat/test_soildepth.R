

test_that("Soil layer widths", {
  soil_depths_cm <- list(
    NULL,
    5,
    10,
    c(5, 10, 30),
    c(5, 10, 30, 50, 100, 200),
    c(30, 50, 100, 200),
    c(30, 50, 100, 200, NA, NA)
  )

  n_slyrs_has <- 4

  #--- Use complete depth range
  # Including: no soils --> `numeric(0)`
  for (k in seq_along(soil_depths_cm)) {
    expect_length(
      calc_soillayer_weights(
        soil_depths_cm = soil_depths_cm[[k]],
        used_depth_range_cm = NULL
      ),
      length(soil_depths_cm[[k]])
    )
  }

  expect_identical(
    calc_soillayer_weights(
      soil_depths_cm = c(10, 20, 30),
      used_depth_range_cm = c(5, 30)
    ),
    c(10, 10, 10)
  )

  expect_identical(
    calc_soillayer_weights(
      soil_depths_cm = c(10, 20, 30),
      used_depth_range_cm = c(5, 100)
    ),
    c(10, 10, 10)
  )

  expect_identical(
    calc_soillayer_weights(
      soil_depths_cm = c(5, 20, 30),
      used_depth_range_cm = c(5, 100)
    ),
    c(NA, 15, 10)
  )

  expect_identical(
    calc_soillayer_weights(
      soil_depths_cm = c(5, 20, 30, 40, 50),
      used_depth_range_cm = c(5, 30)
    ),
    c(NA, 15, 10, NA, NA)
  )

  expect_identical(
    calc_soillayer_weights(
      soil_depths_cm = c(5, 20, 30, NA, NA),
      used_depth_range_cm = c(5, 30)
    ),
    c(NA, 15, 10, NA, NA)
  )

  expect_identical(
    suppressWarnings(calc_soillayer_weights(
      soil_depths_cm = c(5, 20, 30, 40, 50),
      used_depth_range_cm = c(5, 30),
      n_slyrs_has = n_slyrs_has
    )),
    c(NA, 15, 10, NA)
  )
})

test_that("Soil layer positions", {
  soil_depths_cm <- list(
    NULL,
    5,
    10,
    c(5, 10, 30),
    c(5, 10, 30, 50, 100, 200),
    c(30, 50, 100, 200),
    c(30, 50, 100, 200, NA, NA)
  )

  n_slyrs_has <- 4

  #--- Use complete depth range
  # Including: no soils --> `numeric(0)`
  for (k in seq_along(soil_depths_cm)) {
    expect_identical(
      determine_used_soillayers(
        soil_depths_cm = soil_depths_cm[[k]],
        used_depth_range_cm = NULL
      ),
      seq_len(sum(!is.na(soil_depths_cm[[k]])))
    )
  }

  expect_identical(
    determine_used_soillayers(
      soil_depths_cm = c(10, 20, 30),
      used_depth_range_cm = c(5, 30)
    ),
    1:3
  )

  expect_identical(
    determine_used_soillayers(
      soil_depths_cm = c(10, 20, 30),
      used_depth_range_cm = c(5, 100)
    ),
    1:3
  )

  expect_identical(
    determine_used_soillayers(
      soil_depths_cm = c(5, 20, 30),
      used_depth_range_cm = c(5, 100)
    ),
    2:3
  )

  expect_identical(
    determine_used_soillayers(
      soil_depths_cm = c(5, 20, 30, 40, 50),
      used_depth_range_cm = c(5, 30)
    ),
    2:3
  )

  expect_identical(
    determine_used_soillayers(
      soil_depths_cm = c(5, 20, 30, NA, NA),
      used_depth_range_cm = c(5, 30)
    ),
    2:3
  )

  expect_identical(
    determine_used_soillayers(
      soil_depths_cm = c(5, 20, 30, 40, 50),
      used_depth_range_cm = c(5, 30),
      n_slyrs_has = n_slyrs_has
    ),
    2:3
  )

  expect_error(
    determine_used_soillayers(
      soil_depths_cm = c(5, 20, 30, 40, 50),
      used_depth_range_cm = c(5, 50),
      n_slyrs_has = n_slyrs_has
    )
  )
})


test_that("Soil layer availability", {

  expect_warning(
    check_soillayer_availability(
      soil_depths_cm = c(10, 20, 30),
      used_depth_range_cm = c(5, 30),
      strict = c(TRUE, FALSE),
      type = "warn"
    )
  )

  expect_warning(
    check_soillayer_availability(
      soil_depths_cm = c(10, 20, 30),
      used_depth_range_cm = c(5, 30),
      strict = TRUE,
      type = "warn"
    )
  )

  expect_error(
    check_soillayer_availability(
      soil_depths_cm = c(10, 20, 30),
      used_depth_range_cm = c(5, 30),
      strict = TRUE,
      type = "error"
    )
  )


  expect_silent(
    check_soillayer_availability(
      soil_depths_cm = c(10, 20, 30),
      used_depth_range_cm = c(5, 30),
      strict = c(FALSE, TRUE),
      type = "warn"
    )
  )

  expect_silent(
    check_soillayer_availability(
      soil_depths_cm = c(10, 20, 30),
      used_depth_range_cm = c(5, 30),
      strict = FALSE,
      type = "warn"
    )
  )
})
