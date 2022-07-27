#------ CLIMATOLOGIES ------

#------ CLIMATOLOGIES FROM TIME-SERIES METRICS ------


#' Calculate temporal aggregations across annual values
#'
#' @param x A two-dimensional object where rows represent units/cases and
#'   (at least some of the) columns represent
#'   annual (numerical) values (with column names as \code{scX_YYYY}
#'   where \var{X} is the scenario identifier 1, 2, ... and
#'   \var{YYYY} is a calendar year).
#' @param fun A function that has \var{x} as argument, can accept
#'   \var{na.rm} as additional argument, e.g., via \var{...} (see examples),
#'   and returns a named vector.
#' @param list_years A list with integer vectors. Each element represents a
#'   continuous sequence of years over which aggregations are to be calculated.
#' @param id_scens An integer vector of scenario identifiers or \code{NULL};
#'   see details.
#' @param combine A logical value. If \code{TRUE}, then the data columns
#'   \code{scX_YYYY} are appended to the returned object.
#'
#' @section Details:
#' Arguments \code{list_years} and \code{id_scens} specify two modes of
#' aggregations: \itemize{
#'   \item Both arguments, i.e., \code{list_years} and \code{id_scens},
#'     are specified. There must be one element in \code{list_years} for each
#'     value of \code{id_scens}.
#'   \item Only \code{list_years} is provided as argument. Each element of
#'     \code{list_years} is applied to each scenario available in \code{x}.
#' }
#'
#' @section Details:
#' Years requested by \code{list_years} but not available in \code{x} are
#' silently ignored!
#'
#' @section Notes:
#' The package \pkg{rSW2metrics} offers two pathways to across-year summaries:
#' 1. `aggs_across_years()` calculates across-year summaries
#'    from output of a time-series metric; this can be calculated after
#'    extractions or requested with
#'    command-line options `-add_aggs_across_yrs` and `-ts`.
#' 1. Metrics that directly return across-year summaries,
#'    i.e., climatologies;
#'    those are internally calculated by `calc_climatology()`.
#'
#' Both pathways utilize the user-defined function `fun_aggs_across_yrs()`
#' from \var{"Project_Parameters.R}.
#'
#' @examples
#' x <- data.frame(
#'   site = paste0("Site_", 1:3),
#'   matrix(
#'     sample(3 * 2 * 6),
#'     nrow = 3,
#'     dimnames = list(NULL, paste0("sc", rep(1:2, each = 6), "_", 2001:2006))
#'   )
#' )
#'
#' # Temporal summary for two time periods for each available scenario
#' # Note that requested years 2007 to 2010 are silently ignored
#' rSW2metrics::aggs_across_years(
#'   x,
#'   fun = rSW2metrics::mean_cv,
#'   list_years = list(2001:2006, 2005, 2004:2010),
#'   combine = FALSE
#' )
#'
#' # Temporal summary for one time period per requested scenario
#' # Note that some requested years and some scenarios are not available
#' rSW2metrics::aggs_across_years(
#'   x,
#'   fun = rSW2metrics::mean_cv,
#'   list_years = list(2001:2006, 2004:2006, 1990:2010),
#'   id_scens = 1:3,
#'   combine = FALSE
#' )
#'
#' # Count years used in temporal summaries
#' rSW2metrics::aggs_across_years(
#'   x,
#'   fun = function(x, ...) c(N = length(x)),
#'   list_years = list(2001:2006, 2004:2006, 1990:2010),
#'   id_scens = 1:3,
#'   combine = FALSE
#' )
#'
#' @export
aggs_across_years <- function(
  x,
  fun,
  list_years,
  id_scens = NULL,
  combine = TRUE
) {
  #--- Check arguments
  fun_across_years <- match.fun(fun)
  varnames_fun <- names(fun_across_years(1))

  stopifnot(
    inherits(list_years, "list"),
    vapply(list_years, is.numeric, FUN.VALUE = NA),
    !grepl("_", varnames_fun, fixed = TRUE),
    length(varnames_fun) > 0
  )

  if (!is.null(id_scens)) {
    stopifnot(length(id_scens) == length(list_years))
  }


  #--- Prepare column names
  cn_vals <- colnames(x)

  pattern_vars <- "\\bsc(\\d+)_(\\d{4})\\b(?![[:graph:]])"
  cn_vars <- grep(
    pattern_vars,
    cn_vals,
    perl = TRUE,
    value = TRUE
  )

  if (length(cn_vars) == 0) {
    stop("No suitable columns with the format 'scX_YYYY'.")
  }

  # If no id_scens provided, then apply list_years to each available id_scen
  if (is.null(id_scens)) {
    tmp <- vapply(
      strsplit(cn_vars, split = "_", fixed = TRUE),
      `[`,
      j = 1,
      FUN.VALUE = NA_character_
    )

    id_scens <- unique(as.integer(sub("sc", "", tmp, fixed = TRUE)))

    list_years <- lapply(seq_along(id_scens), function(x) list_years)

  } else {
    list_years <- lapply(list_years, function(x) list(x))
  }

  stopifnot(length(id_scens) == length(list_years))


  #--- Prepare container
  tags_sc_aggs <- lapply(
    seq_along(id_scens),
    function(k1) {
      unname(vapply(
        list_years[[k1]],
        function(yrs) {
          paste0("sc", id_scens[k1], "_", yrs[[1]], "-", yrs[length(yrs)])
        },
        FUN.VALUE = NA_character_
      ))
    }
  )

  rn_aggs <- paste0(
    varnames_fun,
    "_",
    rep(unlist(tags_sc_aggs), each = length(varnames_fun))
  )

  if (any(rn_aggs %in% colnames(x))) {
    warning(
      "Argument 'x' contains (some) requested across-year aggregated columns: ",
      toString(shQuote(rn_aggs[rn_aggs %in% colnames(x)]))
    )
  }

  x_aggs <- array(
    data = NA,
    dim = c(length(rn_aggs), nrow(x)),
    dimnames = list(rn_aggs, NULL)
  )


  #--- Calculate aggregations for each scenario
  for (k1 in seq_along(id_scens)) {

    for (k2 in seq_along(list_years[[k1]])) {
      tmp1 <- paste0(varnames_fun, "_", tags_sc_aggs[[k1]][k2])
      tag_ts <- paste0("sc", id_scens[k1], "_", list_years[[k1]][[k2]])
      tmp2 <- intersect(tag_ts, cn_vars)

      if (length(tmp2) > 0) {
        x_aggs[tmp1, ] <- apply(
          X = x[, tmp2, drop = FALSE],
          MARGIN = 1,
          FUN = fun_across_years,
          na.rm = TRUE
        )
      }
    }
  }

  #--- Combine with data
  tmp <- grep(
    pattern_vars,
    cn_vals,
    perl = TRUE,
    invert = TRUE,
    value = TRUE
  )
  cn_header <- setdiff(tmp, rn_aggs)

  if (combine) {
    data.frame(
      x[, cn_header, drop = FALSE],
      t(x_aggs),
      x[, cn_vars, drop = FALSE],
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

  } else {
    data.frame(
      x[, cn_header, drop = FALSE],
      t(x_aggs),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
}



#' Coefficient of variation
#' @noRd
cv <- function(x, ...) {
  mx <- mean(x)
  if (isTRUE(abs(mx) > sqrt(.Machine[["double.eps"]]))) {
    sd(x) / mx
  } else {
    NA
  }
}

#' Sen Slope of Mann-Kendall Trend Test
#' @noRd
sen_slope <- function(x, ...) {
  stopifnot(requireNamespace("modifiedmk", quietly = TRUE))
  if (sum(is.finite(x)) > 3) {
    unname(modifiedmk::mkttest(x)[["Sen's slope"]])
  } else {
    NA
  }
}


#' Useful functions for aggregate statistics of values across years
#'
#' @param x A numeric vector.
#' @param na.rm A logical value
#' @param ... Additional parameters passed to the function(s).
#'
#' @name fun_across_years
NULL

#' @rdname fun_across_years
#' @export
mean_cv <- function(x, na.rm = TRUE, ...) {
  if (na.rm) x <- x[is.finite(x)]

  c(mean = mean(x), cv = cv(x))
}

#' @rdname fun_across_years
#' @export
mean_cv_trend <- function(x, na.rm = TRUE, ...) {
  if (na.rm) x <- x[is.finite(x)]

  c(mean = mean(x), cv = cv(x), trend = sen_slope(x))
}


#' @rdname fun_across_years
#' @export
mean_cv_mmk <- function(x, na.rm = TRUE, ...) {
  x <- unname(x)

  if (na.rm) {
    x <- x[is.finite(x)]
  }

  mx <- mean(x)
  mmk <- if (sum(is.finite(x)) > 3) {
    # Modified Mann-Kendall Test For Serially Correlated Data Using
    # the Yue and Wang (2004) Variance Correction Approach
    # The Hamed & Rao 1998 variance correction approach was used, e.g., by
    # Zhai, J., S. K. Mondal, T.
    # Fischer, Y. Wang, B. Su, J. Huang, H. Tao, G. Wang, W. Ullah, and Md. J.
    # Uddin. 2020. Future drought characteristics through a multi-model ensemble
    # from CMIP6 over South Asia. Atmospheric Research 246:105111.
    # https://doi.org/10.1016/j.atmosres.2020.105111.
    tmp <- try(modifiedmk::mmky(x), silent = TRUE)
    do_unmod <- inherits(tmp, "try-error")

    if (!do_unmod) {
      mtmp <- unname(tmp[c("Sen's slope", "new P-value")])
      do_unmod <- !all(is.finite(mtmp))
    }

    if (do_unmod) {
      mtmp <- unname(modifiedmk::mkttest(x)[c("Sen's slope", "P-value")])
    }

    mtmp

  } else {
    NA
  }

  c(
    mean = mx,
    cv = if (isTRUE(abs(mx) > sqrt(.Machine[["double.eps"]]))) {
      sd(x) / mx
    } else {
      NA
    },
    senslope = mmk[[1]],
    mmkp = mmk[[2]]
  )
}



#' @rdname fun_across_years
#' @export
mean_sd_cv_mmk <- function(x, na.rm = TRUE, ...) {
  x <- unname(x)

  if (na.rm) {
    x <- x[is.finite(x)]
  }

  mx <- mean(x)

  sdx <- sd(x)

  mmk <- if (sum(is.finite(x)) > 3) {
    # Modified Mann-Kendall Test For Serially Correlated Data Using
    # the Yue and Wang (2004) Variance Correction Approach
    # The Hamed & Rao 1998 variance correction approach was used, e.g., by
    # Zhai, J., S. K. Mondal, T.
    # Fischer, Y. Wang, B. Su, J. Huang, H. Tao, G. Wang, W. Ullah, and Md. J.
    # Uddin. 2020. Future drought characteristics through a multi-model ensemble
    # from CMIP6 over South Asia. Atmospheric Research 246:105111.
    # https://doi.org/10.1016/j.atmosres.2020.105111.
    tmp <- try(modifiedmk::mmky(x), silent = TRUE)
    do_unmod <- inherits(tmp, "try-error")

    if (!do_unmod) {
      mtmp <- unname(tmp[c("Sen's slope", "new P-value")])
      do_unmod <- !all(is.finite(mtmp))
    }

    if (do_unmod) {
      mtmp <- unname(modifiedmk::mkttest(x)[c("Sen's slope", "P-value")])
    }

    mtmp

  } else {
    NA
  }

  c(
    mean = mx,
    sd = sdx,
    cv = if (isTRUE(abs(mx) > sqrt(.Machine[["double.eps"]]))) {
      sdx / mx
    } else {
      NA
    },
    senslope = mmk[[1]],
    mmkp = mmk[[2]]
  )
}




#------ CLIMATOLOGIES FOR ACROSS-YEAR METRICS ------

calc_climatology_1var <- function(
    X,
  INDEX,
  FUN,
  ...,
  n_fun = NULL
) {
  stopifnot(NCOL(X) == 1)

  if (is.null(n_fun)) {
    n_fun <- length(FUN(1, ...))
  }

  matrix(
    unname(unlist(
      tapply(X, INDEX = INDEX, FUN = FUN, ..., simplify = FALSE)
    )),
    ncol = n_fun,
    byrow = TRUE
  )
}


#' Calculate across-year summaries
#'
#' @param X A numeric vector, matrix or data.frame. Rows represent time steps.
#' @param INDEX A numeric vector. Time steps of `X`.
#' @param FUN A function or a name of a function.
#'   The function must accept `...` and returns a named or unnamed vector.
#' @param ... Additional parameters passed to `FUN` such as `na.rm`.
#'
#' @return A numeric data.frame where
#'   rows represent unique values of `INDEX` and
#'   columns represent combinations of columns of `x` and `FUN` output.
#'   Columns names contain combinations of names of `x` and `FUN` if
#'   available.
#'
#' @inheritSection aggs_across_years Notes
#'
#' @examples
#' calc_climatology(
#'   1:100,
#'   INDEX = rep(1:10, each = 10),
#'   FUN = function(x, ...) {
#'     tmp <- unname(quantile(x, probs = c(0.05, 0.5, 0.95), ...))
#'     c(low = tmp[[1]], med = tmp[[2]], high = tmp[[3]])
#'   },
#'   type = 1
#' )
#'
#' @export
calc_climatology <- function(X, INDEX, FUN, ...) {
  if (!is.list(X)) {
    X <- if (NCOL(X) == 1) list(X) else as.data.frame(X)
  }

  FUN <- match.fun(FUN)

  # Deal with column names
  res_template <- FUN(1, ...)
  n_fun <- length(res_template)
  ns_fun <- names(res_template)

  ns_x <- names(X)

  ns_out1 <- if (length(X) > 1 || !is.null(ns_x)) {
    rep(
      if (!is.null(ns_x)) ns_x else paste0("L", seq_along(X)),
      each = n_fun
    )
  }

  ns_out <- if (is.null(ns_out1)) {
    ns_fun
  } else if (is.null(ns_fun)) {
    ns_out1
  } else {
    paste0(ns_out1, "_", ns_fun)
  }

  stats::setNames(
    as.data.frame(
      lapply(
        X,
        function(x) {
          do.call(
            calc_climatology_1var,
            c(
              list(
                X = x,
                INDEX = INDEX,
                FUN = FUN,
                n_fun = n_fun
              ),
              list(...)
            )
          )
        }
      )
    ),
    ns_out
  )
}
