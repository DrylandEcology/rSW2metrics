

test_that("Value formatter", {
  #--- Define test inputs ------
  list_nyrs <- c(1L, 5L)
  list_xtype <- c(
    "vector", "list_of_vector1", "list_of_vectorV", "data.frame", "matrix"
  )
  list_xtime <- c("ts", "clim")
  list_labeltypes <- c("xnames", "labels")
  list_timesteps <- c("yearly", "monthly", "daily")

  start_year <- 2001L
  N_vars <- 2L
  ns_vars <- paste0("variable", seq_len(N_vars))

  #--- Test and loop over argument combinations
  for (timestep in list_timesteps) {
    for (nyrs in list_nyrs) {
      ts_years <- start_year - 1L + seq_len(nyrs)

      if (timestep == "daily") {
        ts_daily <- as.POSIXlt(
          rSW2utils::days_in_years(min(ts_years), max(ts_years))
        )
      }

      for (xtime in list_xtime) {
        xyears <- switch(
          EXPR = xtime,
          ts = if (timestep == "daily") {
            as.integer(1900L + ts_daily$year)
          } else {
            rep(
              ts_years,
              each = switch(timestep, yearly = 1L, monthly = 12L)
            )
          },
          clim = NA
        )

        for (xtype in list_xtype) {
          N_data <- if (xtime == "ts") {
            switch(
              EXPR = timestep,
              yearly = nyrs,
              monthly = nyrs * 12L,
              daily = length(xyears)
            )
          } else {
            switch(
              EXPR = timestep,
              yearly = 1L,
              monthly = 12L,
              daily = 366L
            )
          }

          tmpx <- seq_len(N_data)
          tmpx2 <- lapply(seq_len(N_vars), function(k) tmpx)

          x <- switch(
            EXPR = xtype,
            vector = tmpx,
            list_of_vector1 = list(tmpx),
            list_of_vectorV = tmpx2,
            data.frame = as.data.frame(tmpx2),
            matrix = matrix(unlist(tmpx2), ncol = N_vars)
          )

          for (labeltype in list_labeltypes) {
            if (labeltype == "xnames") {
              if (xtype %in% c("list_of_vector1", "list_of_vectorV")) {
                names(x) <- ns_vars[seq_along(x)]
              } else if (xtype %in% c("data.frame", "matrix")) {
                colnames(x) <- ns_vars[seq_len(ncol(x))]
              }
              xlabels <- NULL

            } else if (labeltype == "labels") {
              xlabels <- if (identical(xtype, "vector")) {
                ns_vars[[1]]
              } else if (xtype %in% c("list_of_vector1", "list_of_vectorV")) {
                names(x) <- NULL
                ns_vars[seq_along(x)]
              } else if (xtype %in% c("data.frame", "matrix")) {
                colnames(x) <- NULL
                ns_vars[seq_len(ncol(x))]
              }
            }

            res <- format_values_to_matrix(
              x = x,
              ts_years = xyears,
              timestep = timestep,
              out_label = xlabels
            )

            # nolint start: expect_s3_class_linter.
            expect_true(inherits(res, c("data.frame", "matrix")))
            # nolint end

            expect_identical(
              !!unique(unname(identify_submetric_timesteps(rownames(res)))),
              !!timestep
            )
            expect_identical(!!ncol(res), !!length(unique(xyears)))
          }
        }
      }
    }
  }
})
