% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01-gather_functions.R
\name{gather_demand}
\alias{gather_demand}
\title{Calculate demand captured by each service site}
\usage{
gather_demand(demand, weights)
}
\arguments{
\item{demand}{SpatRaster representing spatial distribution of demand}

\item{weights}{Multi-layer SpatRaster where:
- Each layer represents one service site
- Values are probability weights (0-1) from distance decay
- Layer names should match site IDs}
}
\value{
data.frame with:
        - location_id: identifier matching weight layer names
        - potential_demand: weighted sum of demand for each site
}
\description{
Specialized version of gather_weighted() for accessibility analysis.
Calculates the potential demand for each service site by applying pre-computed
spatial weights to a demand raster.
}
\examples{
# Load example data - already lazy-loaded
# u5pd # Under-5 population density raster
# hos_iscr # Hospital isochrones

# Convert population to SpatRaster
pop_terra <- terra::rast(u5pd)

# Calculate probability weights using gaussian decay
weights <- calc_decay(
  terra::rast(hos_iscr),
  method = "gaussian",
  sigma = 30
) |>
  calc_normalize(method = "semi") # Normalize to ensure proper probabilities

# Calculate potential demand for each hospital
hospital_demands <- gather_demand(pop_terra, weights)
head(hospital_demands)
}
