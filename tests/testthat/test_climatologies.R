

test_that("Across-year aggregations", {
  #--- Define test inputs ------
  nt <- 2
  N <- nt * 5
  nc <- 4

  #--- Input
  #  * vector, matrix or data.frame
  #  * may contain missing values
  #  * may have column names
  list_X <- list(
    # Input: vector
    x1 = NA,
    x2 = 1,
    x3 = c(NA, NA, 3:N),
    x4a = seq_len(N),
    # Input: matrix
    x4b = as.matrix(seq_len(N)),
    x6a = matrix(seq_len(nc * N), ncol = nc),
    x6b = matrix(
      seq_len(nc * N),
      ncol = nc,
      dimnames = list(NULL, paste0("Layer", seq_len(nc)))
    ),
    x7 = {
      tmp <- matrix(seq_len(nc * N), ncol = nc)
      tmp[3, 1] <- NA
      tmp
    },
    # Input: data.frame
    x8 = as.data.frame(matrix(seq_len(nc * N), ncol = nc))
  )

  list_names_X <- lapply(list_X, colnames)


  #--- INDEX identifies unique time steps for across-aggregations
  list_INDEX <- list(
    uniq = function(x) seq_len(NROW(x)),
    agg = function(x) {
      l <- NROW(x)
      rep_len(seq_len(ceiling(l / nt)), length.out = l)
    }
  )


  #---- FUN must return a vector
  #  * length one or longer
  #  * returned named or unnamed values
  #  * must accept ... (e.g., to accept `na.rm`)
  list_FUN <- list(
    mean1 = mean,
    mean2 = function(x, ...) mean,
    mean3 = function(x, ...) c(mean = mean(x, ...)),
    q1 = function(x, ...) quantile(x, prob = 0.5, na.rm = TRUE),
    q2 = function(x, ...) quantile(x, prob = c(0.05, 0.5, 0.95), na.rm = TRUE),
    mix1 = function(x, ...) c(n = length(x), med = median(x))
  )

  list_n_FUN <- lapply(list_FUN, function(f) length(f(1)))
  list_names_FUN <- lapply(list_FUN, function(f) names(f(1)))



  #--- Test and loop over argument combinations
  for (k1 in seq_along(list_X)) {
    for (k2 in seq_along(list_INDEX)) {
      for (k3 in seq_along(list_FUN)) {

        res <- calc_climatology(
          X = list_X[[k1]],
          INDEX = list_INDEX[[k2]](list_X[[k1]]),
          FUN = list_FUN[[k3]]
        )

        # Expect that output rows represent unique `INDEX` values
        expect_identical(
          NROW(res),
          length(unique(list_INDEX[[k2]](list_X[[k1]])))
        )

        # Expect that columns represent input x function output
        expect_identical(
          NCOL(res),
          NCOL(list_X[[k1]]) * list_n_FUN[[k3]]
        )

        # Expect that output contains function output names
        expect_true(
          all(
            vapply(
              list_names_FUN[[k3]],
              function(x) any(grepl(x, x = colnames(res))),
              FUN.VALUE = NA
            )
          )
        )

        # Expect that output contains input column names (if any were present)
        expect_true(
          all(
            vapply(
              list_names_X[[k1]],
              function(x) is.null(x) || any(grepl(x, x = colnames(res))),
              FUN.VALUE = NA
            )
          )
        )

        # Expect that output has column names
        # (unless both single-column and function without names)
        expect_true(
          !is.null(colnames(res)) ||
            NCOL(res) == 1 && is.null(list_names_FUN[[k3]])
        )

        # Expect that output is a matrix or data.frame
        expect_s3_class(res, c("matrix", "data.frame"))
      }
    }
  }
})
