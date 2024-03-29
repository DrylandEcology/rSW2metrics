% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Extractions_Main.R
\name{process_arguments}
\alias{process_arguments}
\title{Process arguments of calling script}
\usage{
process_arguments(x)
}
\arguments{
\item{x}{A vector of character strings.
The return value of \code{\link[base]{commandArgs}}.}
}
\value{
A named list with the processed arguments.
}
\description{
Process arguments of calling script
}
\section{Details}{

The available command-line options are
\itemize{
\item \code{-o=output_filename}: output file name (without extension)
\item \code{-fun=fun_metric}: name of a metric function
\item \code{-fparam=param_filename}: file name of the R script with parameters; see
\code{system.file("exec", "Project_Parameters.R", package = "rSW2metrics")}
for a template
\item \verb{-mode=[\{TRUE,test\}|\{FALSE,full,any,\}]}: option for running a test;
if \code{TRUE} or \code{test}, then the metric function is extracting only from
the first \code{ntests} runs;
all available runs are extracted (default)
if option is missing or contains any other value
\item \code{-ntests=x}: number of runs used if in test mode (\code{-mode}) with a default
of \code{x=100}
\item \code{-runids=r1:r2}: sequence of runs defined by first \code{r1} and last \code{r2} run
(from available runs) from which metrics are extracted;
\itemize{
\item if missing and in full mode (\code{-mode}),
then all available runs are used (default);
\item if missing and in test mode, then option \code{-ntests} is used;
\item if in test mode and both \code{-ntests} and \code{-runids} are present, then
\code{-runids} takes precedence
}
\item \code{-ncores=x}: size of parallel (socket) cluster used to extract
\code{fun_metric} from runs (default \code{x=1})
\item \verb{-cllog=[\{TRUE,\}|\{FALSE,any\}]}: logging activity on cluster to disk
(default \code{FALSE})
\item \verb{-add_aggs_across_yrs=[\{TRUE,\}|\{FALSE,any\}]}: across-year summaries
(defined by \code{fun_aggs_across_yrs()} from \code{fparam}) added to output
if option is present and \code{fun_metric} produces time-series output
}
}

\examples{
process_arguments(
  c(
   "-o=AI_annual",
   "-fun=metric_AI",
   "-fparam=Project_Parameters.R",
   "-add_aggs_across_yrs",
   "-ncores=2"
  )
)

}
