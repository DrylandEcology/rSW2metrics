

test_that("Simulated vs requested time", {
  #--- Define test inputs ------
  yrs1 <- 1950L:2100L # simulated years
  yrs2 <- 1900L:2200L # request starts earlier & ends later than yrs1
  yrs3 <- 1900L:2050L # request starts earlier & ends during than yrs1
  yrs4 <- 2000L:2200L # request starts during & ends later than yrs1

  tp_vars <- c("Year", "Month", "Day")

  xty <- data.frame(Year = yrs1)
  rownames(xty) <- NULL

  xtm <- data.frame(
    Year = rep(yrs1, each = 12L),
    Month = seq_len(12)
  )
  rownames(xtm) <- NULL

  ts_days <- as.POSIXlt(seq(
    from = ISOdate(min(yrs1), 1L, 1L, tz = "UTC"),
    to = ISOdate(max(yrs1), 12L, 31L, tz = "UTC"),
    by = "1 day"
  ))

  xtd <- data.frame(
    Year = 1900L + ts_days$year,
    Month = 1L + ts_days$mon,
    Day = 1L + ts_days$yday
  )
  rownames(xtd) <- NULL

  yrs_w_badleap <- c(1957L, 2021L)
  ids <- which(xtd[, "Year"] %in% yrs_w_badleap & xtd[, "Day"] == 365L)
  xtd2 <- xtd[0L, , drop = FALSE]
  for (k in seq_along(ids)) {
    tmp0 <- if (k == 1L) {
      xtd[1L:ids[k], , drop = FALSE]
    } else {
      xtd[(ids[k - 1L] + 1L):ids[k], , drop = FALSE]
    }
    tmp <- xtd[ids[k], , drop = FALSE]
    tmp[, "Day"] <- 366L
    xtd2 <- rbind(xtd2, tmp0, tmp)
  }
  xtd2 <- rbind(
    xtd2,
    xtd[min(nrow(xtd), ids[k] + 1L):nrow(xtd), , drop = FALSE]
  )
  rownames(xtd2) <- NULL


  #--- Tests without requested years ------
  for (k1 in seq_len(4L)) {
    xtt <- switch(k1, xty, xtm, xtd, xtd2)
    tp <- switch(k1, "Year", "Month", "Day", "Day")
    vids <- seq_len(min(k1, 3L))

    tmp <- suppressWarnings(
      determine_sw2_sim_time(xt = xtt, sw2_tp = tp)
    )

    # Expect: output equal to simulated time steps
    expect_identical(tmp[, !!tp_vars[vids]], xtt[, !!tp_vars[vids]])

    # Expect: only mode is "sim_keep"
    expect_setequal(tmp[, "mode"], "sim_keep")

    if (k1 == 4L) {
      expect_false(identical(tmp[, !!tp_vars], xtd[, !!tp_vars]))
    }
  }


  #--- Test with requested years ------
  for (k2 in seq_len(4L)) {
    ryrs <- switch(k2, yrs1, yrs2, yrs3, yrs4)

    for (k1 in seq_len(4L)) {
      xtt <- switch(k1, xty, xtm, xtd, xtd2)
      tp <- switch(k1, "Year", "Month", "Day", "Day")
      vids <- seq_len(min(k1, 3L))

      tmp <- suppressWarnings(
        determine_sw2_sim_time(xt = xtt, req_years = ryrs, sw2_tp = tp)
      )

      # Expect: all requested years are in output
      expect_true(all(ryrs %in% tmp[, "Year"]))

      # Expect: simulated time steps and "sim_*" portion of output are equal
      ttt <- tmp[tmp[["mode"]] %in% c("sim_keep", "sim_discard"), vids]
      rownames(ttt) <- NULL
      expect_identical(ttt, xtt[, vids])

      # Expect: simulated years do not show up in "nosim" output
      expect_false(
        any(xtt[, "Year"] %in% tmp[tmp[["mode"]] == "nosim", "Year"])
      )

      # Expectation about nosim, sim_keep, and sim_discard
      tmprle <- rle(tmp[["mode"]])

      if (k2 == 1L) {
        # Requested years are equal to simulated years
        expect_identical(tmprle[["values"]], "sim_keep")
        expect_identical(tmprle[["lengths"]], nrow(tmp))

      } else if (k2 == 2L) {
        # Requested years start earlier and end later than simulated years
        expect_identical(tmprle[["values"]], c("nosim", "sim_keep", "nosim"))

      } else if (k2 == 3L) {
        # Request years start earlier than and end during simulated years
        expect_identical(
          tmprle[["values"]],
          c("nosim", "sim_keep", "sim_discard")
        )

      } else if (k2 == 4L) {
        # Request years start during and end later than simulated years
        expect_identical(
          tmprle[["values"]],
          c("sim_discard", "sim_keep", "nosim")
        )
      }
    }
  }
})
