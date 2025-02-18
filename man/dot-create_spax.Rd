% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00-utils-spax_creation.R
\name{.create_spax}
\alias{.create_spax}
\title{Create a spax object with validation}
\usage{
.create_spax(
  accessibility = NULL,
  type = NULL,
  parameters = list(),
  facilities = NULL,
  iterations = NULL,
  variations = NULL,
  call = NULL,
  snap = FALSE
)
}
\arguments{
\item{accessibility}{SpatRaster containing accessibility scores}

\item{type}{Character string specifying model type}

\item{parameters}{List of model parameters}

\item{facilities}{data.frame containing facility-level results (optional)}

\item{iterations}{List containing iteration info (optional)}

\item{variations}{List containing variation results (optional)}

\item{call}{The original function call}

\item{snap}{Logical; if TRUE skip validation (default = FALSE)}
}
\value{
A spax object
}
\description{
Main function for creating spax objects, performing validation first.
For internal use by model functions.
}
\keyword{internal}
