% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{hos_iscvec}
\alias{hos_iscvec}
\title{Hospital Travel Time Isochrones (Vector Format)}
\format{
An sf object with 2027 features and 5 fields:
\describe{
  \item{id}{Numeric ID for each isochrone band}
  \item{isomin}{Minimum travel time in minutes for the band}
  \item{isomax}{Maximum travel time in minutes for the band}
  \item{location_id}{Hospital identifier matching \code{\link{hc12_hos}}}
  \item{iso_mean}{Mean travel time for the band ((isomin + isomax)/2)}
  \item{geometry}{MULTIPOLYGON geometry in UTM zone 47N}
}
}
\source{
Computed using OSRM with OpenStreetMap data.
Hospital locations from \code{\link{hc12_hos}}.
}
\usage{
hos_iscvec
}
\description{
A spatial dataset containing travel time isochrone polygons for 77 hospitals in
Thailand's Region 12. This vector format provides the original isochrone bands
from which \code{\link{hos_iscr}} was derived.
}
\details{
This vector dataset is provided to demonstrate the isochrone generation process
and conversion to raster format. The raster version (\code{\link{hos_iscr}}) is
recommended for accessibility analysis within the spax package.
}
\references{
Luxen, D., & Vetter, C. (2011). Real-time routing with OpenStreetMap data.
In Proceedings of the 19th ACM SIGSPATIAL International Conference on
Advances in Geographic Information Systems (pp. 513-516).
}
\keyword{datasets}
