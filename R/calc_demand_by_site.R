#' Calculate demand captured by each service site
#'
#' Calculates the potential demand for each service site by applying pre-computed
#' spatial weights to a demand raster. Each layer in the weights represents one
#' site's probability-based catchment area (derived from distance decay).
#'
#' @param demand SpatRaster representing spatial distribution of demand
#' @param weights Multi-layer SpatRaster where:
#'        - Each layer represents one service site
#'        - Values are probability weights (0-1) from distance decay
#'        - Layer names should match site IDs
#' @return data.frame with:
#'         - location_id: identifier matching weight layer names
#'         - potential_demand: weighted sum of demand for each site
#' @examples
#' # With pre-computed gaussian distance decay weights:
#' weights <- calc_decay(distance_raster, method = "gaussian", sigma = 30)
#' site_demands <- calc_demand_by_site(population_density, weights)
#' @export
calc_demand_by_site <- function(demand, weights) {
  # Input validation
  if (!inherits(demand, "SpatRaster")) {
    stop("demand must be a SpatRaster object")
  }
  if (!inherits(weights, "SpatRaster")) {
    stop("weights must be a SpatRaster object")
  }

  # Validate weights are probabilities
  w_range <- range(values(weights), na.rm = TRUE)
  if (w_range[1] < 0 || w_range[2] > 1) {
    stop("weights must contain probability values between 0 and 1")
  }

  # Check resolution compatibility
  if (!all(res(demand) == res(weights))) {
    stop("demand and weights must have the same resolution")
  }

  # Check extent compatibility
  if (!all(ext(demand) == ext(weights))) {
    stop("demand and weights must have the same extent")
  }

  # Calculate weighted sums
  potential_demand <- global(weights, 'sum',
                             weights = demand,
                             na.rm = TRUE) |>
    rownames_to_column(var = "location_id") |>
    rename('potential_demand' = weighted_sum)

  return(potential_demand)
}
