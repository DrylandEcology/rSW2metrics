

calc_peaks_v1 <- function(
  x,
  bufferx_size = 0,
  peak_type = c("value", "volume"),
  days_window = 91,
  peak2_drop_factor = 2 / 3,
  peak2_increase_factor = 1 / 6,
  peak2_min_factor = 1 / 3
) {
  # Identify DOY of candidate peaks
  # (peaks are at least a half-window apart)
  pids <- identify_peaks(x, days_window)

  if (length(pids) > 0) {
    # Calculate peak size
    psize <- peak_size_v1(pids, peak_type, values = x)

    pvals <- switch(
      EXPR = peak_type,
      value = psize,
      volume = x[pids]
    )

    # Primary peak: largest size
    tmp <- which.max(psize)
    p1 <- c(pids[tmp], pvals[tmp])
    # Secondary peaks are at least `peak2_min_factor * p1[[2]]` tall
    is2 <- pids != p1[[1]] & pvals >= peak2_min_factor * p1[[2]]

    # peak 2
    p2 <- rep(NA, 2)

    if (sum(is2) > 0) {
      pids2 <- pids[is2]
      ppos <- order(psize[is2], decreasing = TRUE)

      # Loop over secondary peak candidates (descending values)
      k <- 1
      while (k <= length(pids2)) {
        tmp_p2 <- c(pids2[ppos[k]], pvals[is2][ppos[k]])

        # Days in-between peak 1 and candidate peak 2
        ids_inbetween <- sort(tmp_p2[[1]]:p1[[1]])

        if (length(ids_inbetween) > 0) {
          # Sufficient drop?
          ids_dropped <- x[ids_inbetween] < peak2_drop_factor * p1[[2]]
          has_dropped <- any(ids_dropped)

          if (has_dropped) {
            # Sufficient increase?
            increase <- tmp_p2[[2]] - min(x[ids_inbetween[ids_dropped]])
            has_increased <- increase >= peak2_increase_factor * p1[[2]]

            # Is candidate our peak 2?
            if (has_increased) {
              p2 <- tmp_p2
              break
            }
          }
        }

        k <- k + 1
      }
    }
    c(p1, p2)
  } else {
    rep(NA, 4)
  }
}


calc_peaks_v2 <- function(
  x,
  bufferx_size = 0,
  peak_type = c("value", "volume"),
  days_window = 91,
  peaks_drop_factor = 2 / 3,
  peaks_increase_factor = 1 / 6,
  peaksize_min_factor = 1 / 3,
  N_peaks_max_reported = 4
) {
  vars <- c(
    "Peak_N",
    paste0(
      "Peak",
      rep(seq_len(4), each = 2),
      rep(c("_DOY", "_rvol"), times = N_peaks_max_reported)
    )
  )
  res <- rep(NA, length(vars))
  names(res) <- vars

  #--- Identify DOY of candidate peaks
  # (peaks are at least a half-window apart)
  pids <- identify_peaks(x, days_window)
  res[["Peak_N"]] <- length(pids)

  if (res[["Peak_N"]] > 0) {
    #--- Peak value, relative volume, and size (as determined by `peak_type`)
    tmp <- cbind(
      ID = seq_along(pids),
      DOY = pids,
      value = x[pids],
      volume = peak_size_v2(
        pids,
        peak_type = "volume",
        values = x,
        window = days_window
      ) / sum(x, na.rm = TRUE)
    )
    peaks <- cbind(
      tmp,
      meas = switch(
        EXPR = peak_type,
        value = tmp[, "value"],
        volume = tmp[, "volume"]
      )
    )

    # ID of largest peak by size
    idmax <- peaks[which.max(peaks[, "meas"]), "ID"]
    tmp <- peaks[, "ID"] == idmax
    maxpeak_size <- peaks[tmp, "meas"]
    maxpeak_value <- peaks[tmp, "value"]

    # Retain sufficiently sized candidate peaks
    tmp <- peaks[, "meas"] >= peaksize_min_factor * maxpeak_size
    peaks <- peaks[tmp, , drop = FALSE]

    res[["Peak_N"]] <- nrow(peaks)

    if (res[["Peak_N"]] > 0) {
      # Growing list of identified/confirmed peak IDs
      ids_good <- idmax

      #--- Peaks need sufficient drop and increase from neighboring peaks
      # (loop over peaks in descending size)
      while (nrow(peaks) > 0 && length(ids_good) < nrow(peaks)) {
        # Largest sized peak among candidate peaks
        cid <- peaks[-ids_good, "ID"][which.max(peaks[-ids_good, "meas"])]

        tmp <- peaks[, "ID"] == cid
        cdoy <- peaks[tmp, "DOY"]
        cvalue <- peaks[tmp, "value"]

        tmp <- cid + c(-1, 1)
        ids_neighs <- tmp[tmp %in% peaks[, "ID"]]

        # Sufficiently large drop and increase?
        is_good <- rep(FALSE, length(ids_neighs))

        # Loop over neighboring peaks and check if candidate is a good peak
        for (k in seq_along(ids_neighs)) {

          # Days in-between current candidate peak cid and neighboring peak
          ids_inbetween <- sort(seq.int(
            from = cdoy,
            to = peaks[peaks[, "ID"] == ids_neighs[k], "DOY"]
          ))

          if (length(ids_inbetween) > 0) {
            # Sufficient drop?
            ids_dropped <-
              x[ids_inbetween] < peaks_drop_factor * maxpeak_value
            has_dropped <- any(ids_dropped)

            if (has_dropped) {
              # And sufficient increase?
              increase <- cvalue - min(x[ids_inbetween[ids_dropped]])

              # Is candidate a good peak in relation to neighbor k?
              is_good[k] <-
                increase >= peaks_increase_factor * maxpeak_value
            }
          }
        }

        if (all(is_good)) {
          # Candidate is a good peak -> add to list of good peaks
          ids_good <- c(ids_good, cid)
        } else {
          # Candidate is not a suitable peak -> remove from peak data.frame
          peaks <- peaks[peaks[, "ID"] != cid, , drop = FALSE]
        }
      }


      #--- Update peak volume
      # (volume may have changed due to removal of unsuitable candidate peaks)
      peaks[, "volume"] <- peak_size_v2(
        peaks[, "DOY"],
        peak_type = "volume",
        values = x,
        window = days_window
      ) / sum(x, na.rm = TRUE)

      #--- Copy peak information to results
      res[["Peak_N"]] <- nrow(peaks)

      tmp <- seq_len(min(N_peaks_max_reported, res[["Peak_N"]]))
      res[paste0("Peak", tmp, "_DOY")] <- peaks[tmp, "DOY"]
      res[paste0("Peak", tmp, "_rvol")] <- peaks[tmp, "volume"]
    }
  }

  res
}


#--- DOY of two maxima of a smoothed transpiration
calc_transp_peaks_v1 <- function(
  x,
  time,
  # Identify peak size by volume (valley - peak - valley) or by value at peak
  peak_type = c("value", "volume"),
  # Smoothing width of daily transpiration (should be odd)
  days_smoothing = 15,
  # Window for determining secondary candidate peaks (should be odd)
  days_window = 91,
  # Secondary peak should be separated by a valley that
  # dropped below `peak2_drop_factor` * main peak size
  peak2_drop_factor = 2 / 3,
  # Secondary peak should rise above the valley
  # by at least `peak2_drop_factor` * main peak size
  peak2_increase_factor = 1 / 6,
  # Secondary peak should be at least `peak2_min_factor` * main peak size
  peak2_min_factor = 1 / 3
) {
  stopifnot(requireNamespace("zoo", quietly = TRUE))
  stopifnot(days_window %% 2L == 1L) # check that window is odd

  tsmoothed <- if (days_smoothing > 1) {
    zoo::rollapply(
      x,
      width = days_smoothing,
      FUN = mean,
      na.rm = TRUE,
      align = "center",
      partial = TRUE,
      fill = NA
    )
  } else {
    x
  }

  # Peak 1 = annual maximum peak (doy, value)
  # Peak 2 = largest local maximum if exists, i.e., separated from peak 1
  #   - by drop of >= 1/3 of max and
  #   - increase >= 1/6 of max
  ts_years <- unique(time)
  peaks <- matrix(NA, nrow = length(ts_years), ncol = 4)
  bufferx_size <- (days_window + 1) / 2

  for (k1 in seq_along(ts_years)) {
    # Add NA-buffer of size `bufferx_size` around values for current year
    ids_vals <- which(time == ts_years[k1])
    lims_bufft <- bufferx_size + c(1, length(ids_vals))
    tmpx <- rep(NA, length(ids_vals) + 2 * bufferx_size)
    tmpx[lims_bufft[[1]]:lims_bufft[[2]]] <- tsmoothed[ids_vals]

    tmp <- calc_peaks_v1(
      x = tmpx,
      bufferx_size = bufferx_size,
      peak_type = peak_type,
      days_window = days_window,
      peak2_drop_factor = peak2_drop_factor,
      peak2_increase_factor = peak2_increase_factor,
      peak2_min_factor = peak2_min_factor
    )

    # Subtract buffer from DOYs
    tmp[c(1, 3)] <- tmp[c(1, 3)] - bufferx_size

    peaks[k1, ] <- tmp
  }

  # Create data.frame formatted as if returned by `aggregate`
  structure(
    list(ts_years, peaks),
    class = "data.frame",
    .Names = c("Year", "x"),
    row.names = seq_along(ts_years)
  )
}


#--- Count, DOY, and relative volume of smoothed transpiration peaks
calc_transp_peaks_v2 <- function(
  x,
  time,
  # Maximum number of peaks for which to return values
  N_peaks_max_reported = 4,
  # Identify peak size by volume (valley - peak - valley) or by value at peak
  peak_type = c("volume", "value"),
  # Smoothing width of daily transpiration (should be odd)
  days_smoothing = 15,
  # Window for determining candidate peaks (should be odd)
  days_window = 91,
  # Peak values should be separated by a valley that
  # dropped below `peaks_drop_factor` * maximum peak value
  peaks_drop_factor = 2 / 3,
  # Peak values should rise above the valley
  # by at least `peaks_drop_factor` * maximum peak value
  peaks_increase_factor = 1 / 6,
  # Peaks should be at least `peaksize_min_factor` * maximum peak size
  peaksize_min_factor = 1 / 4
) {
  stopifnot(requireNamespace("zoo", quietly = TRUE))
  stopifnot(days_window %% 2L == 1L) # check that window is odd

  tsmoothed <- if (days_smoothing > 1) {
    zoo::rollapply(
      x,
      width = days_smoothing,
      FUN = mean,
      na.rm = TRUE,
      align = "center",
      partial = TRUE,
      fill = NA
    )
  } else {
    x
  }

  ts_years <- unique(time)
  peaks <- matrix(
    NA,
    nrow = length(ts_years),
    ncol = 1 + 2 * N_peaks_max_reported
  )
  bufferx_size <- (days_window + 1) / 2

  for (k1 in seq_along(ts_years)) {
    # Add NA-buffer of size `bufferx_size` around values for current year
    ids_vals <- which(time == ts_years[k1])
    lims_bufft <- bufferx_size + c(1, length(ids_vals))
    tmpx <- rep(NA, length(ids_vals) + 2 * bufferx_size)
    tmpx[lims_bufft[[1]]:lims_bufft[[2]]] <- tsmoothed[ids_vals]

    tmp <- calc_peaks_v2(
      x = tmpx,
      bufferx_size = bufferx_size,
      peak_type = peak_type,
      days_window = days_window,
      peaks_drop_factor = peaks_drop_factor,
      peaks_increase_factor = peaks_increase_factor,
      peaksize_min_factor = peaksize_min_factor,
      N_peaks_max_reported = N_peaks_max_reported
    )

    # Subtract buffer from DOYs
    ids <- grep("_DOY", names(tmp))
    tmp[ids] <- tmp[ids] - bufferx_size

    peaks[k1, ] <- tmp
  }

  colnames(peaks) <- names(tmp)

  # Create data.frame formatted as if returned by `aggregate`
  structure(
    list(ts_years, peaks),
    class = "data.frame",
    .Names = c("Year", "x"),
    row.names = seq_along(ts_years)
  )
}



calc_TranspirationSeasonality_v1 <- function(
  sim_T_daily_by_lyr,
  # Identify peak size by volume (valley - peak - valley) or by value at peak
  peak_type = c("value", "volume"),
  # DOY of `transp_timing_probs` quantiles of cumulative transpiration
  transp_timing_probs = c(0.1, 0.5, 0.9),
  # Smoothing width of daily transpiration (should be odd)
  days_smoothing = 15,
  # Window for determining secondary candidate peaks (should be odd)
  days_window = 91,
  # Secondary peak should be separated by a valley that
  # dropped below `peak2_drop_factor` * main peak size
  peak2_drop_factor = 2 / 3,
  # Secondary peak should rise above the valley
  # by at least `peak2_drop_factor` * main peak size
  peak2_increase_factor = 1 / 6,
  # Secondary peak should be at least `peak2_min_factor` * main peak size
  peak2_min_factor = 1 / 3
) {
  transp <- 10 * rowSums(sim_T_daily_by_lyr[["values"]][[1]])

  # DOY of 10%, 50%, and 90% percentile of cumulative transpiration
  # Annual total transpiration (mm)
  xtmp1 <- calc_transp_seasonality(
    x = transp,
    time = sim_T_daily_by_lyr[["time"]][, "Year"],
    probs = transp_timing_probs
  )

  tmp1 <- t(xtmp1[["x"]])
  rownames(tmp1) <- c(
    "Transpiration_mm",
    paste0("Tcumulative_", round(100 * transp_timing_probs), "perc_DOY")
  )

  # DOY of two maxima of a smoothed transpiration
  xtmp2 <- calc_transp_peaks_v1(
    x = transp,
    time = sim_T_daily_by_lyr[["time"]][, "Year"],
    peak_type = peak_type,
    days_smoothing = days_smoothing,
    days_window = days_window,
    peak2_drop_factor = peak2_drop_factor,
    peak2_increase_factor = peak2_increase_factor,
    peak2_min_factor = peak2_min_factor
  )

  tmp2 <- t(xtmp2[["x"]][, c(1, 3), drop = FALSE])
  rownames(tmp2) <- paste0("Transpiration_peak", 1:2, "_DOY")

  rbind(tmp1, tmp2)
}



calc_TranspirationSeasonality_v2 <- function(
  sim_T_daily_by_lyr,
  # DOY of `transp_timing_probs` quantiles of cumulative transpiration
  transp_timing_probs = c(0.1, 0.5, 0.9),
  # Maximum number of peaks for which to return values
  N_peaks_max_reported = 4,
  # Identify peak size by volume (valley - peak - valley) or by value at peak
  peak_type = c("volume", "value"),
  # Smoothing width of daily transpiration (should be odd)
  days_smoothing = 15,
  # Window for determining candidate peaks (should be odd)
  days_window = 91,
  # Peak values should be separated by a valley that
  # dropped below `peaks_drop_factor` * maximum peak value
  peaks_drop_factor = 2 / 3,
  # Peak values should rise above the valley
  # by at least `peaks_drop_factor` * maximum peak value
  peaks_increase_factor = 1 / 6,
  # Peaks should be at least `peaksize_min_factor` * maximum peak size
  peaksize_min_factor = 1 / 4,
  ...
) {
  transp <- 10 * rowSums(sim_T_daily_by_lyr[["values"]][[1]])

  # DOY of 10%, 50%, and 90% percentile of cumulative transpiration
  # Annual total transpiration (mm)
  xtmp1 <- calc_transp_seasonality(
    x = transp,
    time = sim_T_daily_by_lyr[["time"]][, "Year"],
    probs = transp_timing_probs
  )

  tmp1 <- t(xtmp1[["x"]])
  rownames(tmp1) <- c(
    "Transpiration_mm",
    paste0("Tcumulative_", round(100 * transp_timing_probs), "perc_DOY")
  )

  # Count, DOY, and relative volume of smoothed transpiration peaks
  xtmp2 <- calc_transp_peaks_v2(
    x = transp,
    time = sim_T_daily_by_lyr[["time"]][, "Year"],
    N_peaks_max_reported = N_peaks_max_reported,
    peak_type = peak_type,
    days_smoothing = days_smoothing,
    days_window = days_window,
    peaks_drop_factor = peaks_drop_factor,
    peaks_increase_factor = peaks_increase_factor,
    peaksize_min_factor = peaksize_min_factor
  )

  tmp2 <- t(xtmp2[["x"]])
  rownames(tmp2) <- paste0("Transpiration_", rownames(tmp2))

  rbind(tmp1, tmp2)
}


calc_TranspirationSeasonality_v3 <- function(
  sim_T_daily_by_lyr,
  # DOY of `transp_timing_probs` quantiles of cumulative transpiration
  transp_timing_probs = c(0.1, 0.5, 0.9),
  ...
) {
  transp <- 10 * rowSums(sim_T_daily_by_lyr[["values"]][[1]])

  # DOY of 10%, 50%, and 90% percentile of cumulative transpiration
  # Annual total transpiration (mm)
  xtmp1 <- calc_transp_seasonality(
    x = transp,
    time = sim_T_daily_by_lyr[["time"]][, "Year"],
    probs = transp_timing_probs
  )

  tmp1 <- t(xtmp1[["x"]])
  rownames(tmp1) <- c(
    "Transpiration_mm",
    paste0("Tcumulative_", round(100 * transp_timing_probs), "perc_DOY")
  )

  tmp1
}



calc_TranspirationPeaks_v3 <- function(
  sim_T_daily_by_lyr,
  # Maximum number of peaks for which to return values
  N_peaks_max_reported = 4,
  # Identify peak size by volume (valley - peak - valley) or by value at peak
  peak_type = c("volume", "value"),
  # Smoothing width of daily transpiration (should be odd)
  days_smoothing = 15,
  # Window for determining candidate peaks (should be odd)
  days_window = 91,
  # Peak values should be separated by a valley that
  # dropped below `peaks_drop_factor` * maximum peak value
  peaks_drop_factor = 2 / 3,
  # Peak values should rise above the valley
  # by at least `peaks_drop_factor` * maximum peak value
  peaks_increase_factor = 1 / 6,
  # Peaks should be at least `peaksize_min_factor` * maximum peak size
  peaksize_min_factor = 1 / 4,
  ...
) {
  transp <- 10 * rowSums(sim_T_daily_by_lyr[["values"]][[1]])

  # Count, DOY, and relative volume of smoothed transpiration peaks
  xtmp2 <- calc_transp_peaks_v2(
    x = transp,
    time = sim_T_daily_by_lyr[["time"]][, "Year"],
    N_peaks_max_reported = N_peaks_max_reported,
    peak_type = peak_type,
    days_smoothing = days_smoothing,
    days_window = days_window,
    peaks_drop_factor = peaks_drop_factor,
    peaks_increase_factor = peaks_increase_factor,
    peaksize_min_factor = peaksize_min_factor
  )

  tmp2 <- t(xtmp2[["x"]])
  rownames(tmp2) <- paste0("Transpiration_", rownames(tmp2))

  tmp2
}



metric_TranspirationSeasonality_v1 <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    sim_data <- collect_sw2_sim_data(
      path = path,
      name_sw2_run = name_sw2_run,
      id_scen = id_scen_used[k1],
      years = list_years_scen_used[[k1]],
      output_sets = list(
        T_by_lyr = list(
          sw2_tp = "Day",
          sw2_outs = "TRANSP",
          sw2_vars = "transp_total_Lyr",
          varnames_are_fixed = FALSE
        )
      )
    )

    res[[k1]] <- calc_TranspirationSeasonality_v1(
      sim_T_daily_by_lyr = sim_data[["T_by_lyr"]],
      peak_type = "volume",
      transp_timing_probs = c(0.1, 0.5, 0.9),
      days_smoothing = 15,
      days_window = 91,
      peak2_drop_factor = 2 / 3,
      peak2_increase_factor = 1 / 6,
      peak2_min_factor = 1 / 3
    )
  }

  res
}

metric_TranspirationSeasonality_v2 <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))
  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    sim_data <- collect_sw2_sim_data(
      path = path,
      name_sw2_run = name_sw2_run,
      id_scen = id_scen_used[k1],
      years = list_years_scen_used[[k1]],
      output_sets = list(
        T_by_lyr = list(
          sw2_tp = "Day",
          sw2_outs = "TRANSP",
          sw2_vars = "transp_total_Lyr",
          varnames_are_fixed = FALSE
        )
      )
    )

    res[[k1]] <- calc_TranspirationSeasonality_v1(
      sim_T_daily_by_lyr = sim_data[["T_by_lyr"]],
      peak_type = "value",
      transp_timing_probs = c(0.1, 0.5, 0.9),
      days_smoothing = 15,
      days_window = 91,
      peak2_drop_factor = 2 / 3,
      peak2_increase_factor = 1 / 6,
      peak2_min_factor = 1 / 3
    )
  }

  res
}


metric_TranspirationSeasonality_v3 <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    sim_data <- collect_sw2_sim_data(
      path = path,
      name_sw2_run = name_sw2_run,
      id_scen = id_scen_used[k1],
      years = list_years_scen_used[[k1]],
      output_sets = list(
        T_by_lyr = list(
          sw2_tp = "Day",
          sw2_outs = "TRANSP",
          sw2_vars = "transp_total_Lyr",
          varnames_are_fixed = FALSE
        )
      )
    )

    res[[k1]] <- calc_TranspirationSeasonality_v1(
      sim_T_daily_by_lyr = sim_data[["T_by_lyr"]],
      peak_type = "value",
      transp_timing_probs = c(0.1, 0.5, 0.9),
      days_smoothing = 31,
      days_window = 91,
      peak2_drop_factor = 2 / 3,
      peak2_increase_factor = 1 / 6,
      peak2_min_factor = 1 / 3
    )
  }

  res
}



metric_TranspirationSeasonality_v4 <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    sim_data <- collect_sw2_sim_data(
      path = path,
      name_sw2_run = name_sw2_run,
      id_scen = id_scen_used[k1],
      years = list_years_scen_used[[k1]],
      output_sets = list(
        T_by_lyr = list(
          sw2_tp = "Day",
          sw2_outs = "TRANSP",
          sw2_vars = "transp_total_Lyr",
          varnames_are_fixed = FALSE
        )
      )
    )

    res[[k1]] <- calc_TranspirationSeasonality_v2(
      sim_T_daily_by_lyr = sim_data[["T_by_lyr"]],
      transp_timing_probs = c(0.1, 0.5, 0.9),
      N_peaks_max_reported = 4L,
      peak_type = "value",
      days_smoothing = 15,
      days_window = 91,
      peaks_drop_factor = 2 / 3,
      peaks_increase_factor = 1 / 6,
      peaksize_min_factor = 1 / 3
    )
  }

  res
}

metric_TranspirationSeasonality_v5 <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    sim_data <- collect_sw2_sim_data(
      path = path,
      name_sw2_run = name_sw2_run,
      id_scen = id_scen_used[k1],
      years = list_years_scen_used[[k1]],
      output_sets = list(
        T_by_lyr = list(
          sw2_tp = "Day",
          sw2_outs = "TRANSP",
          sw2_vars = "transp_total_Lyr",
          varnames_are_fixed = FALSE
        )
      )
    )

    res[[k1]] <- calc_TranspirationSeasonality_v2(
      sim_T_daily_by_lyr = sim_data[["T_by_lyr"]],
      transp_timing_probs = c(0.1, 0.5, 0.9),
      N_peaks_max_reported = 4L,
      peak_type = "volume",
      days_smoothing = 15,
      days_window = 91,
      peaks_drop_factor = 2 / 3,
      peaks_increase_factor = 1 / 6,
      peaksize_min_factor = 1 / 4
    )
  }

  res
}



# Identical to `metric_TranspirationSeasonality_v5`,
# but separated quantiles from peaks
metric_TranspirationSeasonality_v6 <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    sim_data <- collect_sw2_sim_data(
      path = path,
      name_sw2_run = name_sw2_run,
      id_scen = id_scen_used[k1],
      years = list_years_scen_used[[k1]],
      output_sets = list(
        T_by_lyr = list(
          sw2_tp = "Day",
          sw2_outs = "TRANSP",
          sw2_vars = "transp_total_Lyr",
          varnames_are_fixed = FALSE
        )
      )
    )

    res[[k1]] <- calc_TranspirationSeasonality_v3(
      sim_T_daily_by_lyr = sim_data[["T_by_lyr"]],
      transp_timing_probs = c(0.1, 0.5, 0.9)
    )
  }

  res
}


metric_TranspirationPeaks_v6 <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "ts_years",
  ...
) {
  stopifnot(check_metric_arguments(out = match.arg(out)))

  res <- list()

  for (k1 in seq_along(id_scen_used)) {
    sim_data <- collect_sw2_sim_data(
      path = path,
      name_sw2_run = name_sw2_run,
      id_scen = id_scen_used[k1],
      years = list_years_scen_used[[k1]],
      output_sets = list(
        T_by_lyr = list(
          sw2_tp = "Day",
          sw2_outs = "TRANSP",
          sw2_vars = "transp_total_Lyr",
          varnames_are_fixed = FALSE
        )
      )
    )

    res[[k1]] <- calc_TranspirationPeaks_v3(
      sim_T_daily_by_lyr = sim_data[["T_by_lyr"]],
      N_peaks_max_reported = 4L,
      peak_type = "volume",
      days_smoothing = 15,
      days_window = 91,
      peaks_drop_factor = 2 / 3,
      peaks_increase_factor = 1 / 6,
      peaksize_min_factor = 1 / 4
    )
  }

  res
}
