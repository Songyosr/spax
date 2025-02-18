% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01-decay_functions.R
\name{.help_decay_inverse}
\alias{.help_decay_inverse}
\title{Helper function for inverse distance weight computation}
\usage{
.help_decay_inverse(distance, c = 1, ...)
}
\arguments{
\item{distance}{Numeric vector of distances}

\item{c}{Constant to prevent division by zero}

\item{...}{Additional parameters (not used in inverse decay)}
}
\value{
Numeric vector of weights
}
\description{
Helper function for inverse distance weight computation
}
\keyword{internal}
