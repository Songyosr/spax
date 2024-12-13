#' Calculate spatial accessibility using weighted surfaces
#'
#' @description
#' General-purpose function to calculate spatial accessibility by combining
#' demand-side and supply-side weighted distributions. Supports multiple
#' supply measures and flexible weight specifications.
#'
#' @param demand SpatRaster representing spatial distribution of demand
#' @param supply vector, matrix, or data.frame containing supply capacity values.
#'        If using an sf object, please use st_drop_geometry() first.
#' @param demand_weights Multi-layer SpatRaster of demand-side weights
#' @param access_weights Multi-layer SpatRaster of accessibility-side weights
#' @param id_col Character; column name for facility IDs if supply is a data.frame
#' @param supply_cols Character vector; names of supply columns if supply is a data.frame
#' @param indicator_names Character vector; custom names for output accessibility layers.
#'        If NULL, will use "A_" prefix with supply column names.
#' @param full_output Logical; whether to return intermediate calculations
#' @return SpatRaster of accessibility scores or list with full results if full_output=TRUE
#' @seealso
#' \code{\link{compute_2sfca}} for specific two-step floating catchment area implementation
#' \code{\link{compute_weights}} for generating weight surfaces
#' \code{\link{gather_demand}} for demand calculation
#' \code{\link{spread_access}} for accessibility distribution
#' @examples
#' \dontrun{
#' # Basic usage with supply columns
#' measure_access(demand, supply, weights,
#'               supply_cols = c("doctors", "nurses"))
#'
#' # With custom indicator names
#' measure_access(demand, supply, weights,
#'               supply_cols = c("doctors", "nurses"),
#'               indicator_names = c("physician_access", "nurse_access"))
#' }
#' @export
measure_access <- function(demand, supply, demand_weights, access_weights,
                           id_col = NULL, supply_cols = NULL,
                           indicator_names = NULL,
                           full_output = FALSE) {

  # Input validation
  if (!inherits(demand, "SpatRaster") ||
      !inherits(demand_weights, "SpatRaster") ||
      !inherits(access_weights, "SpatRaster")) {
    stop("demand and weight arguments must be SpatRaster objects")
  }

  # Check for sf object
  if (inherits(supply, "sf")) {
    stop("Supply data is an sf object. Please use st_drop_geometry() first to convert to a regular data frame")
  }

  # Calculate demand by site using gather_demand
  demand_by_site <- gather_demand(demand, demand_weights)

  # Process supply data if it's a data.frame
  if (is.data.frame(supply)) {
    if (is.null(id_col) || is.null(supply_cols)) {
      stop("id_col and supply_cols must be specified for data.frame input")
    }

    # Match and reorder to weight layers
    weight_ids <- names(demand_weights)
    supply <- supply[match(weight_ids, supply[[id_col]]), supply_cols]
  }

  # Calculate ratios
  ratios <- sweep(as.matrix(supply), 1, demand_by_site$potential_demand, "/")

  # Determine layer names
  if (is.null(indicator_names)) {
    if (!is.null(supply_cols)) {
      indicator_names <- paste0("A_", supply_cols)
      message("Using supply column names as accessibility indicators: ",
              paste(indicator_names, collapse = ", "))
    }
  } else {
    if (length(indicator_names) != ncol(ratios)) {
      stop("Length of indicator_names must match number of supply measures")
    }
  }

  # Calculate accessibility scores
  result <- spread_access(ratios, access_weights, full_output = full_output)

  # Apply names if they exist
  if (!is.null(indicator_names)) {
    if (full_output) {
      names(result$access_scores) <- indicator_names
    } else {
      names(result) <- indicator_names
    }
  }

  return(result)
}

#' Calculate Two-Step Floating Catchment Area (2SFCA) accessibility scores
#'
#' @inheritParams measure_access
#' @param distance SpatRaster stack of travel times/distances to facilities
#' @param decay_params List of parameters for decay function
#' @param demand_normalize Character specifying normalization method: "identity", "standard", or "semi"
#' @return SpatRaster of 2SFCA accessibility scores or list with full results
#' @examples
#' # Create sample data
#' library(terra)
#'
#' # Create a simple demand raster
#' r <- rast(nrows=10, ncols=10, xmin=0, xmax=10, ymin=0, ymax=10)
#' values(r) <- runif(ncell(r)) * 100  # Random population values
#' demand <- r
#'
#' # Create sample supply data
#' supply <- data.frame(
#'   location_id = c("A", "B", "C"),
#'   doctors = c(10, 15, 20),
#'   nurses = c(20, 25, 30)
#' )
#'
#' # Create distance raster stack
#' distance <- rast(replicate(3, r))  # 3 layers for 3 facilities
#' values(distance) <- runif(ncell(distance) * nlyr(distance)) * 10  # Random distances
#' names(distance) <- supply$location_id
#'
#' # Calculate accessibility
#' result <- compute_2sfca(
#'   demand = demand,
#'   supply = supply,
#'   distance = distance,
#'   supply_cols = c("doctors", "nurses"),
#'   decay_params = list(method = "gaussian", sigma = 30),
#'   demand_normalize = "identity",
#'   id_col = "location_id"
#' )
#'
#' @export
compute_2sfca <- function(demand, supply, distance,
                          decay_params = list(method = "gaussian", sigma = 30),
                          demand_normalize = "identity",
                          id_col = NULL, supply_cols = NULL,
                          indicator_names = NULL,
                          full_output = FALSE) {

  # Check for sf object
  if (inherits(supply, "sf")) {
    stop("Supply data is an sf object. Please use st_drop_geometry() first to convert to a regular data frame")
  }

  # Compute weights
  weights <- do.call(compute_weights, c(list(distance = distance), decay_params))

  # Process demand weights based on normalization method
  demand_weights <- switch(demand_normalize,
                           "identity" = weights,
                           "standard" = normalize_weights(weights, method = "normalize"),
                           "semi" = normalize_weights(weights, method = "semi_normalize"),
                           stop("Invalid demand_normalize method. Use 'identity', 'standard', or 'semi'"))

  # Access weights remain unnormalized
  access_weights <- weights

  # Use general measure_access function
  return(measure_access(demand = demand,
                        supply = supply,
                        demand_weights = demand_weights,
                        access_weights = access_weights,
                        id_col = id_col,
                        supply_cols = supply_cols,
                        indicator_names = indicator_names,
                        full_output = full_output))
}
