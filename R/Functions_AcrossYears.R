
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
#' @section Notes:
#' Years requested by \code{list_years} but not available in \code{x} are
#' silently ignored!
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
    sapply(list_years, is.numeric),
    !grepl("_", varnames_fun),
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
    tmp <- sapply(
      strsplit(cn_vars, split = "_", fixed = TRUE),
      `[`,
      j = 1
    )

    id_scens <- unique(as.integer(sub("sc", "", tmp)))

    list_years <- lapply(seq_along(id_scens), function(x) list_years)

  } else {
    list_years <- lapply(list_years, function(x) list(x))
  }

  stopifnot(length(id_scens) == length(list_years))


  #--- Prepare container
  tags_sc_aggs <- lapply(
    seq_along(id_scens),
    function(k1) {
      unname(sapply(
        list_years[[k1]],
        function(yrs) {
          paste0("sc", id_scens[k1], "_", yrs[1], "-", yrs[length(yrs)])
        }
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
      paste0(shQuote(rn_aggs[rn_aggs %in% colnames(x)]), collapse = ", ")
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
  if (isTRUE(abs(mx) > sqrt(.Machine$double.eps))) {
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
    unname(modifiedmk::mkttest(x)["Sen's slope"])
  } else {
    NA
  }
}


#' Calculate aggregate statistics of values across years
#'
#' @param x A numeric vector.
#' @param na.rm A logical value
#'
#' @name fun_across_years
NULL

#' @rdname fun_across_years
#' @export
mean_cv <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[is.finite(x)]

  c(mean = mean(x), cv = cv(x))
}

#' @rdname fun_across_years
#' @export
mean_cv_trend <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[is.finite(x)]

  c(mean = mean(x), cv = cv(x), trend = sen_slope(x))
}


#' @rdname fun_across_years
#' @export
mean_cv_mmk <- function(x, na.rm = TRUE) {
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
      do_unmod <- any(!is.finite(mtmp))
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
    cv = if (isTRUE(abs(mx) > sqrt(.Machine$double.eps))) {
      sd(x) / mx
    } else {
      NA
    },
    senslope = mmk[1],
    mmkp = mmk[2]
  )
}
