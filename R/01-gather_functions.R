#' Validate inputs for gather_weighted
#' @param values SpatRaster of values
#' @param weights SpatRaster of weights
#' @param na.rm Logical for NA handling
#' @keywords internal
.chck_gather_weighted <- function(values, weights, na.rm) {
  # Input type validation
  if (!inherits(values, "SpatRaster")) {
    stop("values must be a SpatRaster object")
  }
  if (!inherits(weights, "SpatRaster")) {
    stop("weights must be a SpatRaster object")
  }

  # Resolution compatibility
  if (!all(res(values) == res(weights))) {
    stop("values and weights must have the same resolution")
  }

  # Extent compatibility
  if (!all(ext(values) == ext(weights))) {
    stop("values and weights must have the same extent")
  }

  # na.rm validation
  if (!is.logical(na.rm)) {
    stop("na.rm must be logical")
  }

  invisible(TRUE)
}
#' Core computation for gather_weighted
#' @keywords internal
.gather_weighted_core <- function(values, weights, na.rm = TRUE) {
  weighted_sums <- global(weights, "sum",
    weights = values,
    na.rm = na.rm
  )

  return(weighted_sums)
}

#' Format output for gather_weighted
#' @keywords internal
.help_format_gather <- function(weighted_sums) {
  weighted_sums |>
    tibble::rownames_to_column(var = "unit_id")
}
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
#' @param na.rm Logical; if TRUE, remove NA values from computation (default = TRUE)
#' @param snap Logical; if TRUE, avoid input validation. Used for performance optimization
#'        in internal functions.
#' @return data.frame with:
#'         - unit_id: identifier matching weight layer names
#'         - weighted_sum: aggregated value for each unit
#' @examples
#' # pacakge data is already lazy-loaded
#'
#' # Convert population raster to terra format
#' pop_terra <- terra::rast(u5pd)
#'
#' # Calculate distance decay weights using hospital isochrones
#' weights <- calc_decay(
#'   terra::rast(hos_iscr),
#'   method = "gaussian",
#'   sigma = 30
#' )
#'
#' # Calculate potential demand for each hospital
#' potential_demand <- gather_weighted(pop_terra, weights)
#' head(potential_demand)
#'
#' # With NA handling - simulate some missing data
#' pop_with_na <- pop_terra
#' pop_with_na[1:100] <- NA
#' demand_na <- gather_weighted(pop_with_na, weights, na.rm = TRUE)
#' head(demand_na)
#' @export
gather_weighted <- function(values, weights, na.rm = TRUE, snap = FALSE) {
  # Validation
  if (!snap) {
    .chck_gather_weighted(values, weights, na.rm)
  }

  # Core computation
  weighted_sums <- .gather_weighted_core(values, weights, na.rm)

  # Format output (skip for snap mode)
  if (!snap) {
    weighted_sums <- .help_format_gather(weighted_sums)
  }

  return(weighted_sums)
}

#' Calculate demand captured by each service site
#'
#' @description
#' Specialized version of gather_weighted() for accessibility analysis.
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
#' # Load example data - already lazy-loaded
#' # u5pd # Under-5 population density raster
#' # hos_iscr # Hospital isochrones
#'
#' # Convert population to SpatRaster
#' pop_terra <- terra::rast(u5pd)
#'
#' # Calculate probability weights using gaussian decay
#' weights <- calc_decay(
#'   terra::rast(hos_iscr),
#'   method = "gaussian",
#'   sigma = 30
#' ) |>
#'   calc_normalize(method = "semi") # Normalize to ensure proper probabilities
#'
#' # Calculate potential demand for each hospital
#' hospital_demands <- gather_demand(pop_terra, weights)
#' head(hospital_demands)
#' @export
gather_demand <- function(demand, weights) {
  # Validate weights are probabilities
  w_range <- range(values(weights), na.rm = TRUE)
  if (w_range[1] < 0 || w_range[2] > 1) {
    stop("weights must contain probability values between 0 and 1")
  }

  # Use gather_weighted and rename columns appropriately
  result <- gather_weighted(demand, weights) |>
    dplyr::rename(
      location_id = unit_id,
      potential_demand = weighted_sum
    )

  return(result)
}
