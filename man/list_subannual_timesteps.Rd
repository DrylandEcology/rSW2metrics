% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions_Selfdescribing.R
\name{list_subannual_timesteps}
\alias{list_subannual_timesteps}
\title{List possible sub-annual time step identifiers}
\usage{
list_subannual_timesteps()
}
\value{
A list with the implemented sub-annual time step identifiers
\itemize{
\item seasonal: \var{\dQuote{seasonX}} where X can be 1 to a value in \code{1:12}
\item quarterly: \var{\dQuote{QX}} where X in \code{1:4}
\item monthly: \var{\dQuote{monX}} where X in \code{01:12}
\item daily: \var{\dQuote{doyX}} where X in \code{001:366}
}
}
\description{
Sub-annual time steps are organized via the column \dQuote{group}.
}
