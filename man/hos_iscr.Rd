% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{hos_iscr}
\alias{hos_iscr}
\title{Hospital Travel Time Isochrones (Raster Format)}
\format{
A RasterStack with 77 layers and the following specifications:
\describe{
  \item{Dimensions}{509 rows × 647 columns × 77 layers}
  \item{Resolution}{520.4038 × 520.4038 meters}
  \item{Extent}{505646.5, 842347.8, 620843.7, 885729.2 (xmin, xmax, ymin, ymax)}
  \item{CRS}{WGS 84 / UTM zone 47N (EPSG:32647)}
  \item{Values}{Travel time in minutes}
  \item{Layer Names}{Match facility IDs in hc12_hos (e.g., "c172", "c173")}
}
}
\source{
Computed using OSRM (Open Source Routing Machine) with OpenStreetMap data.
Hospital locations from \code{\link{hc12_hos}}.
}
\usage{
hos_iscr
}
\description{
A RasterStack containing travel time isochrones for 77 hospitals in Thailand's Region 12.
Each layer represents the travel time (in minutes) to reach one hospital, computed using
the OSRM (Open Source Routing Machine) routing engine.
}
\details{
Travel times were calculated using the following time-break structure:
\itemize{
  \item 1-minute intervals for first 10 minutes
  \item 2-minute intervals from 12 to 30 minutes
  \item 5-minute intervals from 35 to 60 minutes
  \item 15-minute intervals from 75 to 180 minutes
}

The isochrones were generated using hospital locations from \code{\link{hc12_hos}}.
}
\references{
Luxen, D., & Vetter, C. (2011). Real-time routing with OpenStreetMap data.
In Proceedings of the 19th ACM SIGSPATIAL International Conference on
Advances in Geographic Information Systems (pp. 513-516).
}
\keyword{datasets}
