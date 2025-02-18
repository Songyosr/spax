% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03-spax_e2sfca.R
\name{compute_access}
\alias{compute_access}
\title{Calculate spatial accessibility using weighted surfaces}
\usage{
compute_access(
  demand,
  supply,
  demand_weights,
  access_weights,
  id_col = NULL,
  supply_cols = NULL,
  indicator_names = NULL,
  snap = FALSE
)
}
\arguments{
\item{demand}{SpatRaster representing spatial distribution of demand}

\item{supply}{vector, matrix, or data.frame containing supply capacity values.
If using an sf object, please use st_drop_geometry() first.}

\item{demand_weights}{Multi-layer SpatRaster of demand-side weights}

\item{access_weights}{Multi-layer SpatRaster of accessibility-side weights}

\item{id_col}{Character; column name for facility IDs if supply is a data.frame}

\item{supply_cols}{Character vector; names of supply columns if supply is a data.frame}

\item{indicator_names}{Character vector; custom names for output accessibility layers}

\item{snap}{Logical; if TRUE enable fast computation mode (default = FALSE)}
}
\value{
SpatRaster of accessibility scores
}
\description{
General-purpose function to calculate spatial accessibility by combining
demand-side and supply-side weighted distributions. Supports multiple
supply measures and flexible weight specifications.
}
