#' Aggregate weighted values from a raster
#'
#' @description
#' General-purpose function to aggregate values from a raster using a stack of weight
#' rasters. Each layer in the weights represents a different aggregation unit.
#'
#' @param values SpatRaster representing values to be aggregated
#' @param weights Multi-layer SpatRaster where:
#'        - Each layer represents one aggregation unit
#'        - Values are weights (typically 0-1) for aggregation
#'        - Layer names should match unit IDs
#' @return data.frame with:
#'         - unit_id: identifier matching weight layer names
#'         - weighted_sum: aggregated value for each unit
#' @export
weighted_gather <- function(values, weights) {
  # Input validation
  if (!inherits(values, "SpatRaster")) {
    stop("values must be a SpatRaster object")
  }
  if (!inherits(weights, "SpatRaster")) {
    stop("weights must be a SpatRaster object")
  }

  # Check resolution compatibility
  if (!all(res(values) == res(weights))) {
    stop("values and weights must have the same resolution")
  }

  # Check extent compatibility
  if (!all(ext(values) == ext(weights))) {
    stop("values and weights must have the same extent")
  }

  # Calculate weighted sums
  weighted_sums <- global(weights, 'sum',
                          weights = values,
                          na.rm = TRUE) |>
    rownames_to_column(var = "unit_id") |>
    rename('weighted_sum' = weighted_sum)

  return(weighted_sums)
}

#' Calculate demand captured by each service site
#'
#' @description
#' Specialized version of weighted_gather() for accessibility analysis.
#' Calculates the potential demand for each service site by applying pre-computed
#' spatial weights to a demand raster.
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
#' weights <- compute_weights(distance_raster, method = "gaussian", sigma = 30)
#' site_demands <- gather_demand(population_density, weights)
#' @export
gather_demand <- function(demand, weights) {
  # Validate weights are probabilities
  w_range <- range(values(weights), na.rm = TRUE)
  if (w_range[1] < 0 || w_range[2] > 1) {
    stop("weights must contain probability values between 0 and 1")
  }

  # Use weighted_gather and rename columns appropriately
  result <- weighted_gather(demand, weights) |>
    rename(
      location_id = unit_id,
      potential_demand = weighted_sum
    )

  return(result)
}
