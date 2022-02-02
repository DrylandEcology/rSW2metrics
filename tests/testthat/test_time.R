

test_that("Simulated vs requested time", {
  #--- Define test inputs ------
  yrs1 <- 1950:2100 # simulated years
  yrs2 <- 1900:2200 # request starts earlier & ends later than yrs1
  yrs3 <- 1900:2050 # request starts earlier & ends during than yrs1
  yrs4 <- 2000:2200 # request starts during & ends later than yrs1

  tp_vars <- c("Year", "Month", "Day")

  xty <- data.frame(Year = yrs1)
  rownames(xty) <- NULL

  xtm <- data.frame(
    Year = rep(yrs1, each = 12),
    Month = seq_len(12)
  )
  rownames(xtm) <- NULL

  ts_days <- as.POSIXlt(seq(
    from = ISOdate(min(yrs1), 1, 1, tz = "UTC"),
    to = ISOdate(max(yrs1), 12, 31, tz = "UTC"),
    by = "1 day"
  ))
  xtd <- data.frame(
    Year = 1900 + ts_days$year,
    Month = 1 + ts_days$mon,
    Day = 1 + ts_days$yday
  )
  rownames(xtd) <- NULL

  yrs_w_badleap <- c(1957, 2021)
  ids <- which(xtd[, "Year"] %in% yrs_w_badleap & xtd[, "Day"] == 365)
  xtd2 <- xtd[0, , drop = FALSE]
  for (k in seq_along(ids)) {
    tmp0 <- if (k == 1) {
      xtd[1:ids[k], , drop = FALSE]
    } else {
      xtd[(ids[k - 1] + 1):ids[k], , drop = FALSE]
    }
    tmp <- xtd[ids[k], , drop = FALSE]
    tmp[, "Day"] <- 366
    xtd2 <- rbind(xtd2, tmp0, tmp)
  }
  xtd2 <- rbind(xtd2, xtd[min(nrow(xtd), ids[k] + 1):nrow(xtd), , drop = FALSE])
  rownames(xtd2) <- NULL


  #--- Tests without requested years ------
  for (k1 in seq_len(4)) {
    xtt <- switch(k1, xty, xtm, xtd, xtd2)
    tp <- switch(k1, "Year", "Month", "Day", "Day")
    vids <- 1:min(k1, 3)

    tmp <- suppressWarnings(
      determine_sw2_sim_time(xt = xtt, sw2_tp = tp)
    )

    # Expect: output equal to simulated time steps
    expect_equal(tmp[, !!tp_vars[vids]], xtt[, !!tp_vars[vids]])

    # Expect: only mode is "sim_keep"
    expect_setequal(tmp[, "mode"], "sim_keep")

    if (k1 == 4) {
      expect_false(identical(tmp[, !!tp_vars], xtd[, !!tp_vars]))
    }
  }


  #--- Test with requested years ------
  for (k2 in seq_len(4)) {
    ryrs <- switch(k2, yrs1, yrs2, yrs3, yrs4)

    for (k1 in seq_len(4)) {
      xtt <- switch(k1, xty, xtm, xtd, xtd2)
      tp <- switch(k1, "Year", "Month", "Day", "Day")
      vids <- 1:min(k1, 3)

      tmp <- suppressWarnings(
        determine_sw2_sim_time(xt = xtt, req_years = ryrs, sw2_tp = tp)
      )

      # Expect: all requested years are in output
      expect_true(all(ryrs %in% tmp[, "Year"]))

      # Expect: simulated time steps and "sim_*" portion of output are equal
      ttt <- tmp[tmp$mode %in% c("sim_keep", "sim_discard"), vids]
      rownames(ttt) <- NULL
      expect_equal(ttt, xtt[, vids])

      # Expect: simulated years do not show up in "nosim" output
      expect_true(all(!xtt[, "Year"] %in% tmp[tmp$mode == "nosim", "Year"]))

      # Expectation about nosim, sim_keep, and sim_discard
      tmprle <- rle(tmp$mode)

      if (k2 == 1) {
        # Requested years are equal to simulated years
        expect_equal(tmprle[["values"]], "sim_keep")
        expect_equal(tmprle[["lengths"]], nrow(tmp))

      } else if (k2 == 2) {
        # Requested years start earlier and end later than simulated years
        expect_equal(tmprle[["values"]], c("nosim", "sim_keep", "nosim"))

      } else if (k2 == 3) {
        # Request years start earlier than and end during simulated years
        expect_equal(tmprle[["values"]], c("nosim", "sim_keep", "sim_discard"))

      } else if (k2 == 4) {
        # Request years start during and end later than simulated years
        expect_equal(tmprle[["values"]], c("sim_discard", "sim_keep", "nosim"))
      }
    }
  }
})
