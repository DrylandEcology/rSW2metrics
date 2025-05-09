
#--- Functions to check metrics visually ---------------------------------------

#' Plot of metrics for visual checks
#'
#' Type of data to plot:
#'   * Time-series: annual, monthly, or daily
#'   * \var{Intra}-annual, seasonal time-series:
#'     monthly or daily means across years
#'   * Density distribution of values or across-year summaries
#'   * Maps of values or across-year summaries
#'
#' where across-year summaries are statistics such as a
#' mean, trend, coefficient of variation or standard deviation across years.
#'
#' Data formats:
#'   * output from any of the `metrics_xxx()` functions
#'       * different sites or locations are coded by the column "site"
#'       * the column "group" codes different metrics or
#'         sub-annual (monthly and daily) time steps or
#'         soil layers of a metric
#'       * annual time series are stored in columns as `scenario_calendaryear`
#'       * across-year summaries are stored either
#'           * in columns named `fun_scenario_year-year` if
#'             calculated from annual time series
#'           * in columns named `scenario_periodname`
#'             where `periodname` are named elements of `years_aggs_by_scen`
#'             and different across-year summaries are encoded by column "group"
#'             if calculated by a climatology metric
#'
#'   * output from `metric_SW2toTable_daily()`
#'       * columns represent variables
#'       * rows represent Calendar days
#'       * sites are stored in different files
#'
#' @param metrics The metrics to be checked. This is either
#'   * a vector of character strings with the path(s) to metric files
#'     (that will be loaded), or
#'   * a named list of data.frames that hold (already loaded) metric values
#'     where names of list elements are the metric names.
#' @param dir_out A character string. Resulting figures are stored at this path.
#'
#' @param include_metrics A character string. If not `NULL`
#' @param exclude_metrics A character string. If not `NA`
#' @param exclude_submetrics A character string. If not `NA`
#'
#' @param site_coords A two-dimensional numeric object
#'   with spatial coordinates of point locations or
#'   a point or polygon object of class `sf` where
#'   rows represent sites.
#'   Spatial maps of metrics will be produced if not `NULL`.
#' @param site_groups A numeric or character string vector.
#'   Group assignment to each site,
#'   i.e., length must correspond to number of sites.
#'   Figures are produced separately for each group of sites if not `NULL`,
#'   otherwise, one group is set up that includes all sites.
#' @param scenario_ids An integer vector.
#'   The scenarios to plot. If `NULL` then all available scenarios are plotted.
#'
#' @section Notes:
#'   Sites are matched by position (but site names are not checked), i.e.,
#'     * case `metric_SW2toTable_daily()`:
#'       files (that contain site names) are sorted alphabetically;
#'       this order is silently assumed by `site_coords` and `site_groups`.
#'     * case `metric_xxx()`:
#'       the order of sites as provided by column "sites" is silently assumed
#'       by `site_coords` and `site_groups`.
#'
#' @export
check_metrics_visually_v0 <- function(
  dir_metrics,
  dir_out,

  include_metrics = NULL,
  exclude_metrics = NA,
  exclude_submetrics = NA,

  site_coords = NULL,
  site_groups = NULL,

  scenario_ids = NULL
) {

  if (!dir.exists(dir_out)) {
    dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
  }

  #--- Locate metrics files ------
  fname_metrics <- unlist(lapply(dir_metrics, list.files, full.names = TRUE))


  #--- Determine overall characteristics of metric files ------
  is_rds <- all(grepl(".rds\\>", fname_metrics))
  if (!is_rds) stopifnot(grepl(".csv\\>", ftmp))
  out_format <- if (is_rds) "rds" else "csv"

  x <- read_metric_file_v0(fname_metrics[1], out_format = out_format)
  is_SW2toTable <- all(!(c("site", "group") %in% colnames(x)))

  if (is_SW2toTable) {
    stopifnot(c("Year", "DOY") %in% colnames(x))
    n_sites <- length(fname_metrics)

  } else {
    n_sites <- length(unique(x[, "site"]))
  }

  stopifnot(
    is.null(site_coords) || nrow(site_coords) == n_sites,
    is.null(site_groups) || length(site_groups) == n_sites
  )

  if (is.null(site_groups)) {
    site_groups <- rep(1, n_sites)
  }

  n_groups <- length(unique(site_groups))


  #--- Determine scenarios ------
  if (is_SW2toTable) {
    #--- case SW2toTable: scenarios are encoded in file names
    tmp <- regmatches(
      basename(fname_metrics),
      regexpr("_sc(\\d+)", basename(fname_metrics))
    )

    scen_by_file <- as.integer(sub("_sc", "", tmp))
    scen_has <- unique(scen_by_file)

    if (length(scen_has) == 0) {
      stop("File names do not encode scenarios with pattern '_scX'.")
    }

  } else {
    #--- case regular metrics: scenarios are encoded in column names
    cn_vars <- grep(
      "\\bsc(\\d+)_(\\d{4})\\b(?![[:graph:]])",
      colnames(x),
      perl = TRUE,
      value = TRUE
    )

    if (length(cn_vars) == 0) {
      stop("No suitable columns with the format 'scX_YYYY'.")
    }

    # Scenarios and years
    tmp <- matrix(
      as.integer(unlist(
        strsplit(sub("sc", "", cn_vars), split = "_", fixed = TRUE)
      )),
      ncol = 2,
      byrow = TRUE
    )

    scen_has <- unique(tmp[, 1])
    years_by_scen <- split(tmp[, 2], tmp[, 1])
  }


  if (is.null(scenario_ids)) {
    scenario_ids <- scen_has
  } else {
    stopifnot(scenario_ids %in% scen_has)
    scenario_ids <- sort(scenario_ids)
  }




  #--- Determine metrics/variables ------
  if (is_SW2toTable) {
    #--- case SW2toTable: column names are variables
    # loop over all files because soil layers may vary
    tmp_vars0 <- NULL
    years_by_scen <- NULL
    ksc <- 1L

    for (k in seq_along(fname_metrics)) {
      x <- read_metric_file_v0(fname_metrics[k], out_format = out_format)

      tmp_vars0 <- unique(c(
        tmp_vars0,
        grep("Sim_|Input_", colnames(x), value = TRUE)
      ))

      if (ksc == scen_by_file[k]) {
        years_by_scen[[ksc]] <- unique(x[, "Year"])
        ksc <- ksc + 1L
      }
    }

    names(years_by_scen) <- scen_has
    tmp_vars1 <- tmp_vars0


  } else {
    #--- case regular metric: file names are metrics
    tmp_vars0 <- fname_metrics
    tmp_vars1 <- basename(fname_metrics)
   }


  # Only keep metrics that match a pattern in `include_metrics`
  ids_keep <- if (!is.null(include_metrics)) {
    unique(unlist(lapply(
      include_metrics,
      function(x) grep(x, tmp_vars1)
    )))
  }
  if (length(ids_keep) > 0) {
    tmp_vars1 <- tmp_vars1[ids_keep]
    tmp_vars0 <- tmp_vars0[ids_keep]
  }

  # Remove metrics that match a pattern in `exclude_metrics`
  ids_remove <- if (!anyNA(exclude_metrics)) {
    unique(unlist(lapply(
      exclude_metrics,
      function(x) grep(x, tmp_vars1)
    )))
  }
  if (length(ids_remove) > 0) {
    tmp_vars0 <- tmp_vars0[-ids_remove]
  }


  # Finalize list of metrics and file names
  if (is_SW2toTable) {
    tag_metrics <- sort(tmp_vars0)

  } else {
    fname_metrics <- tmp_vars0[order(basename(tmp_vars0))]
    tag_metrics <- sub(".rds", "", basename(tmp_vars0))
  }


  stopifnot(length(years_by_scen) == scen_has)



  #--- Loop over scenarios and spatial groups ------
  for (k1g in seq_along(site_groups)) {
    for (k2sc in seq_along(scenario_ids)) {
      id_scen <- which(scen_has == scenario_ids[k2sc])
      years <- years_by_scen[[id_scen]]


      #--- * Plot SW2toTable ------
      if (is_SW2toTable) {
        #--- case SW2toTable
        #  * site_groups --> subset of files
        #  * scenarios --> subset of files
        #  * sites == files --> load all relevant files, then plot

        fname_used <- fname_metrics[
          site_groups == site_groups[k1g] &
            scen_by_file == scenario_ids[k2sc]
        ]

        #--- Prepare containers
        res_annual <- array(
          dim = c(length(fname_used), length(years), length(tag_metrics)),
          dimnames = list(basename(fname_used), years, tag_metrics)
        )

        res_daily_clim <- array(
          dim = c(length(fname_used), 366, length(tag_metrics)),
          dimnames = list(basename(fname_used), seq_len(366), tag_metrics)
        )


        #--- Read data (loop over files)
        for (k in seq_along(fname_used)) {
          x <- read_metric_file_v0(fname_used[k], out_format = out_format)

          tmp_vars_used <- intersect(colnames(x), tag_metrics)

          ids_yrs <- x[, "Year"] %in% years

          # Calculate mean annual time series from daily values time step
          tmp <- as.matrix(
            aggregate(
              x[ids_yrs, tmp_vars_used, drop = FALSE],
              by = list(Year = x[ids_yrs, "Year"]),
              mean,
              na.rm = TRUE
            )
          )

          ids <- match(years, tmp[, "Year"], nomatch = 0)
          res_annual[k, ids > 0, tmp_vars_used] <- tmp[ids, -1, drop = FALSE]


          # Calculate across-year mean daily values
          tmp <- as.matrix(
            aggregate(
              x[ids_yrs, tmp_vars_used, drop = FALSE],
              by = list(x[ids_yrs, "DOY"]),
              mean,
              na.rm = TRUE
            )
          )[, -1, drop = FALSE]

          res_daily_clim[k, seq_len(nrow(tmp)), tmp_vars_used] <- tmp
        }

        # Calculate across-year mean annual values
        res_annual_clim <- apply(res_annual, c(1, 3), mean, na.rm = TRUE)



        #--- Plot (loop over metrics/variables)



      }

      #--- * Plot regular metrics ------
      if (!is_SW2toTable) {
        #--- case regular metrics: sites within a file --> load and plot per file

        #--- Read data and plot (loop over files)
        for (k in seq_along(fname_metrics)) {
          x <- read_metric_file_v0(fname_metrics[k], out_format = out_format)
          tag_submetrics <- unique(x[, "group"])

          ids_remove <- if (!anyNA(exclude_submetrics)) {
            unique(unlist(lapply(
              exclude_submetrics,
              function(x) grep(x, tag_submetrics)
            )))
          }
          if (length(ids_remove) > 0) {
            tag_submetrics <- tag_submetrics[-ids_remove]
          }


          #--- Is any submetric a set of intra-annual time-steps?
          # quarterly, monthly, daily

          #--- Is any submetric a set of soil layers?
        }
      }
    }
  }

}


read_metric_file_v0 <- function(file, out_format = c("rds", "csv")) {
  out_format <- match.arg(out_format)

  switch(
    EXPR = out_format,
    csv = utils::read.csv(file, check.names = FALSE),
    rds = readRDS(file)
  )
}

read_metric_file <- function(file, out_format = c("rds", "csv")) {
  out_format <- match.arg(out_format)

  switch(
    EXPR = out_format,
    csv = utils::read.csv(file, check.names = FALSE),
    rds = readRDS(file)
  )
}


check_metrics_visually_v1 <- function(
  metrics,
  dir_out,

  include_metrics = NULL,
  exclude_metrics = NA,
  exclude_submetrics = NA,

  site_coords = NULL,
  site_groups = NULL,

  scenario_ids = NULL
) {

  if (!dir.exists(dir_out)) {
    dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
  }

  #--- Locate metrics ------

  #--- Is the metrics argument a list with values or a vector of paths
  if (is.list(metrics) && all(sapply(metrics, is.data.frame))) {
    type_input <- "values"
  } else if (is.character(metrics)) {
    type_input <- "fnames"
    fname_metrics <- unlist(lapply(metrics, list.files, full.names = TRUE))
  } else {
    stop("Argument `metrics` is mis-specified.")
  }


  #--- Overall characteristics of metrics ------
  x <- switch(
    EXPR = type_input,
    values = metrics[[1]],
    fnames = read_metric_file(which.min(file.size(fname_metrics)))
  )

  n_sites <- length(unique(x[, "site"]))

  stopifnot(
    c("site", "group") %in% colnames(x),
    is.null(site_coords) || nrow(site_coords) == n_sites,
    is.null(site_groups) || length(site_groups) == n_sites
  )

  if (is.null(site_groups)) {
    site_groups <- rep(1, n_sites)
  }

  n_groups <- length(unique(site_groups))



  #--- Determine scenarios ------
  # scenarios are encoded in column names
  cn_vars <- grep(
    "\\bsc(\\d+)_(\\d{4})\\b(?![[:graph:]])",
    colnames(x),
    perl = TRUE,
    value = TRUE
  )

  if (length(cn_vars) == 0) {
    stop("No suitable columns with the format 'scX_YYYY'.")
  }

  # Scenarios and years
  tmp <- matrix(
    as.integer(unlist(
      strsplit(sub("sc", "", cn_vars), split = "_", fixed = TRUE)
    )),
    ncol = 2,
    byrow = TRUE
  )

  scen_has <- unique(tmp[, 1])
  years_by_scen <- split(tmp[, 2], tmp[, 1])


  if (is.null(scenario_ids)) {
    scenario_ids <- scen_has
  } else {
    stopifnot(scenario_ids %in% scen_has)
    scenario_ids <- sort(scenario_ids)
  }





  #--- Determine metrics/variables ------
  # file names are metrics
  tmp_vars0 <- fname_metrics
  tmp_vars1 <- basename(fname_metrics)


  # Only keep metrics that match a pattern in `include_metrics`
  ids_keep <- if (!is.null(include_metrics)) {
    unique(unlist(lapply(
      include_metrics,
      function(x) grep(x, tmp_vars1)
    )))
  }
  if (length(ids_keep) > 0) {
    tmp_vars1 <- tmp_vars1[ids_keep]
    tmp_vars0 <- tmp_vars0[ids_keep]
  }

  # Remove metrics that match a pattern in `exclude_metrics`
  ids_remove <- if (!anyNA(exclude_metrics)) {
    unique(unlist(lapply(
      exclude_metrics,
      function(x) grep(x, tmp_vars1)
    )))
  }
  if (length(ids_remove) > 0) {
    tmp_vars0 <- tmp_vars0[-ids_remove]
  }


  # Finalize list of metrics and file names
  fname_metrics <- tmp_vars0[order(basename(tmp_vars0))]
  tag_metrics <- sub(".rds", "", basename(tmp_vars0))



  #--- Loop over metrics ------
  for (km in seq_along(fname_metrics)) {
    # Read data file of metric
    x <- readRDS(fname_metrics[km])
    x <- x[!is.na(x[, "group"]), , drop = FALSE]

    tag_submetrics <- unique(x[, "group"])


    # Remove submetrics/variables contained in the metric file
    ids_remove <- if (!anyNA(exclude_submetrics)) {
      unique(unlist(lapply(
        exclude_submetrics,
        function(x) grep(x, tag_submetrics)
      )))
    }
    if (length(ids_remove) > 0) {
      tag_submetrics <- tag_submetrics[-ids_remove]
    }


    #--- Is any submetric a set of intra-annual time-steps?
    # quarterly, monthly, daily
    ts_info <- identify_metric_timestep(tag_submetrics)

    print(tag_metrics[km])
    print(ts_info[["submetrics2u"]])
    print(ts_info[["timestep"]])
    print("___________________")


  }


  # - timeseries: each site vs. quantiles
  # - annual means
  # - mean daily
  #
  # - maps
  # - means, cv or sd, trend across years
  #
  # - input
  # - csv/rds from sharing individual runs
  # - rds produced by rSW2metrics
  # - netCDFs/GeoTIFFs
  #
if (TRUE) {
    #--- TODO: Is any submetric a set of soil layers?


    n_submetrics <- length(tag_submetrics)

    # panel columns:
    #   1) lines: sub-annual clim, sub-annual density, annual ts, annual density
    #   2) maps: annual mean, trend, cv (if suitable), sd

    # panel rows: submetrics


    #--- Loop over scenarios and spatial groups ------
    for (kg in seq_along(site_groups)) {
      for (ksc in seq_along(scenario_ids)) {
        id_scen <- which(scen_has == scenario_ids[ksc])
        years <- years_by_scen[[id_scen]]




        tmp_rows <-
          x[, "group"] %in% tag_submetrics[k4v]
        tmp_cols <- paste0("sc", id_scen, "_", years)

        plot_ts_panel(
          x[, tmp_cols, drop = FALSE],
          ylab = tag_submetrics[k4v]
        )

      }
    }
  }

}



#' Time-series line or band plots
#'
#' @param x A numeric two-dimensional object.
#'   Columns represent time steps and rows represent sites.
plot_ts_panel <- function(
  x,
  time_values = NULL,
  xlab = "Years",
  ylab = NULL,
  title = NULL,
  type = c("depends", "aggregated", "individual"),
  N_limit_individuals = 100
) {
  type <- match.arg(type)

  tmp_sa <- utils::stack(as.data.frame(t(x)))

  if (is.null(time_values)) {
    time_values <- as.integer(sapply(
      strsplit(as.character(colnames(x)), split = "_", fixed = TRUE),
      `[`,
      j = 2
    ))
  }

  stopifnot(ncol(x) == length(time_values))
  tmp_sa[, "Time"] <- rep(time_values, nrow(x))

  if (
    type == "aggregated" ||
      (type == "depends" && nrow(x) > N_limit_individuals)
  ) {
    # Plot band across sites
    tmp_say <- aggregate(
      x = tmp_sa[, "values"],
      by = list(Time = tmp_sa[, "Time"]),
      FUN = function(x) {
        quantile(
          x = x[is.finite(x)],
          probs = c(0, 0.025, 0.25, 0.75, 0.975, 1)
        )
      }
    )

    tmp_say <- cbind(tmp_say["Time"], tmp_say[["x"]])

    tmp <- ggplot2::ggplot(tmp_say) +
      ggplot2::aes(Time) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = `0%`, ymax = `100%`),
        alpha = 0.5,
        fill = "darkseagreen3",
        color = "transparent"
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = `2.5%`, ymax = `97.5%`),
        alpha = 0.5,
        fill = "darkblue",
        color = "transparent"
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = `25%`, ymax = `75%`),
        alpha = 0.5,
        fill = "darkred",
        color = "transparent"
      )

  } else {
    # Plot each site as a line
    tmp <- ggplot2::ggplot(tmp_sa) +
      ggplot2::aes(Time, values, color = ind) +
      ggplot2::geom_line(show.legend = FALSE) +
      ggplot2::scale_color_viridis_d(alpha = 0.5)
  }

  tmp <- tmp +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = title
    ) +
    egg::theme_article()

  # Add overall smoothed trend by a cubic-spline
  ids <- is.finite(tmp_sa[, "values"]) & is.finite(tmp_sa[, "Time"])
  tmp_sa2 <- tmp_sa[ids, , drop = FALSE]
  tmp_trend <- try(mgcv::gam(values ~ s(Time, bs = "cs"), data = tmp_sa2))

  if (inherits(tmp_trend, "try-error")) {
    tmp_trend <- try(stats::lm(
      values ~ splines::bs(Time, 3),
      data = tmp_sa2
    ))
  }

  spv <- if (inherits(tmp_trend, "try-error")) {
    1
  } else {
    tmp_trends <- try(summary(tmp_trend), silent = TRUE)
    if (inherits(tmp_trends, "try-error")) 1 else tmp_trends[["s.pv"]]
  }

  if (spv < 0.05) {
    xt <- seq(
      min(tmp_sa[["Time"]]),
      max(tmp_sa[["Time"]]),
      length.out = 101
    )
    py <- predict(tmp_trend, newdata = data.frame(Time = xt))

    tmp <- tmp + ggplot2::geom_line(
      data = data.frame(x = xt, y = py),
      ggplot2::aes(x, y),
      lwd = 2,
      color = "black",
      show.legend = FALSE
    )
  }

  tmp
}
