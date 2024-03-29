% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions_AcrossYears.R
\name{calc_climatology}
\alias{calc_climatology}
\title{Calculate across-year summaries}
\usage{
calc_climatology(X, INDEX, FUN, ...)
}
\arguments{
\item{X}{A numeric vector, matrix or data.frame. Rows represent time steps.}

\item{INDEX}{A numeric vector. Time steps of \code{X}.}

\item{FUN}{A function or a name of a function.
The function must accept \code{...} and returns a named or unnamed vector.}

\item{...}{Additional parameters passed to \code{FUN} such as \code{na.rm}.}
}
\value{
A numeric data.frame where
rows represent unique values of \code{INDEX} and
columns represent combinations of columns of \code{x} and \code{FUN} output.
Columns names contain combinations of names of \code{x} and \code{FUN} if
available.
}
\description{
Calculate across-year summaries
}
\section{Notes}{

The package \pkg{rSW2metrics} offers two pathways to across-year summaries:
\enumerate{
\item \code{aggs_across_years()} calculates across-year summaries
from output of a time-series metric; this can be calculated after
extractions or requested with
command-line options \code{-add_aggs_across_yrs} and \code{-ts}.
\item Metrics that directly return across-year summaries,
i.e., climatologies;
those are internally calculated by \code{calc_climatology()}.
}

Both pathways utilize the user-defined function \code{fun_aggs_across_yrs()}
from \var{"Project_Parameters.R}.
}

\examples{
calc_climatology(
  1:100,
  INDEX = rep(1:10, each = 10),
  FUN = function(x, ...) {
    tmp <- unname(quantile(x, probs = c(0.05, 0.5, 0.95), ...))
    c(low = tmp[[1]], med = tmp[[2]], high = tmp[[3]])
  },
  type = 1
)

}
