% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{phc_iscr}
\alias{phc_iscr}
\title{Primary Health Center Travel Time Isochrones}
\format{
A RasterStack with 819 layers and the following specifications:
\describe{
  \item{Dimensions}{509 rows × 647 columns × 819 layers}
  \item{Resolution}{520.4038 × 520.4038 meters}
  \item{Extent}{505646.5, 842347.8, 620843.7, 885729.2 (xmin, xmax, ymin, ymax)}
  \item{CRS}{WGS 84 / UTM zone 47N (EPSG:32647)}
  \item{Values}{Travel time in minutes (limited to 60 minutes)}
  \item{Layer Names}{Match facility IDs in hc12_phc (e.g., "c001", "c002")}
}
}
\source{
Computed using OSRM with OpenStreetMap data.
PHC locations from \code{\link{hc12_phc}}.
}
\usage{
phc_iscr
}
\description{
A RasterStack containing travel time isochrones for 819 Primary Health Centers (PHCs)
in Thailand's Region 12, limited to 60-minute travel time to reflect local service areas.
}
\details{
Travel times were calculated using the following time-break structure optimized for
local service areas:
\itemize{
  \item 1-minute intervals for first 10 minutes
  \item 2-minute intervals from 12 to 30 minutes
  \item 5-minute intervals from 35 to 60 minutes
}

The 60-minute limit reflects the local service nature of PHCs and optimizes
computational efficiency. Isochrones were generated using PHC locations from
\code{\link{hc12_phc}}.
}
\references{
Luxen, D., & Vetter, C. (2011). Real-time routing with OpenStreetMap data.
In Proceedings of the 19th ACM SIGSPATIAL International Conference on
Advances in Geographic Information Systems (pp. 513-516).
}
\keyword{datasets}
