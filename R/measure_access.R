#' Calculate spatial accessibility using weighted surfaces
#'
#' @description
#' General-purpose function to calculate spatial accessibility by combining
#' demand-side and supply-side weighted distributions. Supports multiple
#' supply measures and flexible weight specifications.
#'
#' @param demand SpatRaster representing spatial distribution of demand
#' @param supply vector, matrix, or data.frame containing supply capacity values
#' @param demand_weights Multi-layer SpatRaster of demand-side weights
#' @param access_weights Multi-layer SpatRaster of accessibility-side weights
#' @param id_col Character; column name for facility IDs if supply is a data.frame
#' @param supply_cols Character vector; names of supply columns if supply is a data.frame
#' @param full_output Logical; whether to return intermediate calculations
#' @return SpatRaster of accessibility scores or list with full results if full_output=TRUE
#' @seealso
#' \code{\link{compute_2sfca}} for specific two-step floating catchment area implementation
#' \code{\link{compute_weights}} for generating weight surfaces
#' \code{\link{gather_demand}} for demand calculation
#' \code{\link{spread_access}} for accessibility distribution
#' @export
measure_access <- function(demand, supply, demand_weights, access_weights,
                           id_col = NULL, supply_cols = NULL,
                           full_output = FALSE) {

  # Input validation
  if (!inherits(demand, "SpatRaster") ||
      !inherits(demand_weights, "SpatRaster") ||
      !inherits(access_weights, "SpatRaster")) {
    stop("demand and weight arguments must be SpatRaster objects")
  }

  # Calculate demand by site using weighted_gather/gather_demand
  demand_by_site <- gather_demand(demand, demand_weights)

  # Process supply data
  if (is.data.frame(supply)) {
    if (is.null(id_col) || is.null(supply_cols)) {
      stop("id_col and supply_cols must be specified for data.frame input")
    }
    if (!id_col %in% names(supply) || !all(supply_cols %in% names(supply))) {
      stop("Specified columns not found in supply data.frame")
    }

    # Match and reorder to weight layers
    weight_ids <- names(demand_weights)
    if (!all(weight_ids %in% supply[[id_col]])) {
      stop("Some weight layer names not found in supply ID column")
    }

    supply <- supply[match(weight_ids, supply[[id_col]]), supply_cols]
  }

  # Calculate ratios
  ratios <- sweep(as.matrix(supply), 1, demand_by_site$potential_demand, "/")

  # Calculate and return accessibility scores
  return(spread_access(ratios, access_weights, full_output = full_output))
}

#' Calculate Two-Step Floating Catchment Area (2SFCA) accessibility scores
#'
#' @description
#' Specialized implementation of spatial accessibility calculation using the
#' Two-Step Floating Catchment Area method. Handles multiple supply indicators
#' and supports enhanced distance decay functions.
#'
#' @inheritParams measure_access
#' @param distance SpatRaster stack of travel times/distances to facilities
#' @param decay_params List of parameters passed directly to compute_weights():
#'        - method: Decay function type
#'        - sigma: Decay parameter value
#'        - Additional parameters as needed
#' @param demand_normalize Character or function specifying demand normalization method:
#'        - "identity": No normalization (default)
#'        - "standard": Full normalization to sum to 1
#'        - "semi": Semi-normalization (only if sum > 1)
#'        - Function: Custom normalization function
#' @return SpatRaster of 2SFCA accessibility scores or list with full results
#' @seealso
#' \code{\link{measure_access}} for general accessibility calculation
#' \code{\link{compute_weights}} for distance decay options
#' \code{\link{normalize_weights}} for normalization methods
#' @examples
#' # Basic usage with default parameters
#' result <- compute_2sfca(demand, supply, distance)
#'
#' # Custom decay parameters
#' decay_params <- list(
#'   method = "gaussian",
#'   sigma = 45,
#'   max_distance = 120
#' )
#' result <- compute_2sfca(demand, supply, distance,
#'                        decay_params = decay_params)
#'
#' # Using semi-normalization for demand weights
#' result <- compute_2sfca(demand, supply, distance,
#'                        demand_normalize = "semi")
#'
#' # Custom normalization function
#' custom_norm <- function(x) {
#'   normalize_weights(x, method = "competing")
#' }
#' result <- compute_2sfca(demand, supply, distance,
#'                        demand_normalize = custom_norm)
#' @export
compute_2sfca <- function(demand, supply, distance,
                          decay_params = list(method = "gaussian", sigma = 30),
                          demand_normalize = "identity",
                          id_col = NULL, supply_cols = NULL,
                          full_output = FALSE) {

  # Compute weights using all provided decay parameters
  weights <- do.call(compute_weights, c(list(distance = distance), decay_params))

  # Process demand weights based on normalization method
  demand_weights <- if (is.function(demand_normalize)) {
    # Use custom normalization function
    demand_normalize(weights)
  } else {
    # Use built-in normalization methods
    method <- match.arg(demand_normalize,
                        c("identity", "standard", "semi"))

    switch(method,
           "identity" = weights,
           "standard" = normalize_weights(weights, method = "standard"),
           "semi" = normalize_weights(weights, method = "semi"))
  }

  # Access weights remain unnormalized
  access_weights <- weights

  # Use general measure_access function
  return(measure_access(demand = demand,
                        supply = supply,
                        demand_weights = demand_weights,
                        access_weights = access_weights,
                        id_col = id_col,
                        supply_cols = supply_cols,
                        full_output = full_output))
}
