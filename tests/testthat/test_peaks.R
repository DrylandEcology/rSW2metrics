

test_that("Peaks", {
  ## No or less than two plateaus -> central candidate from among all values
  expect_identical(central_candidate(1), 1)
  expect_identical(central_candidate(NULL), NULL)
  expect_identical(central_candidate(1:2), 1L)
  expect_identical(central_candidate(1:3), 2L)
  expect_identical(central_candidate(1:4), 2L)
  expect_identical(central_candidate(c(1, 3, 5)), 3)
  expect_identical(central_candidate(c(1, 3, 5, 7, 9, 17:19)), 7)

  # return type is equal to type of input
  expect_identical(central_candidate(c(1, 20:29)), 24) # numeric
  expect_identical(central_candidate(c(1L, 20:29)), 24L) # integer

  ## More than one plateau -> central candidate from longest plateau
  expect_identical(central_candidate(c(1, 20:30, 40:60)), 50)
  expect_identical(central_candidate(c(1, 20:30, 40:61)), 50)
  expect_identical(central_candidate(40:60), 50L)
  expect_identical(central_candidate(c(1, 20:30, 40:60, 70:80, 100:110)), 50)
  expect_identical(central_candidate(c(1, 20:30, 40:60, 70:90)), 80)
})
