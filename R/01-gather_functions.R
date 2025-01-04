#' Validate inputs for gather_weighted
#' @param values SpatRaster of values
#' @param weights SpatRaster of weights
#' @param na.rm Logical for NA handling
#' @keywords internal
.chck_gather_weighted <- function(values, weights, na.rm) {
  # Input type validation
  .assert_class(values, "SpatRaster", "values")
  .assert_class(weights, "SpatRaster", "weights")

  # Check raster alignment (resolution, extent and CRS)
  .assert_raster_alignment(values, weights, "values", "weights")

  # na.rm validation
  .assert_class(na.rm, "logical", "na.rm")

  invisible(TRUE)
}

#' Core computation for gather_weighted
#' @keywords internal
.gather_weighted_core <- function(values, weights, na.rm = TRUE) {
  weighted_sums <- terra::global(weights, "sum",
    weights = values,
    na.rm = na.rm
  )
  # return in vector format
  return(weighted_sums[[1]])
}

#' Format output for gather_weighted
#' @keywords internal
.help_format_gather <- function(weighted_sums, weights, values = NULL) {
  if (is.null(values)) {
    # Single layer case
    data.frame(
      unit_id = names(weights),
      weighted_sum = weighted_sums
    )
  } else {
    # Multi layer case
    result_df <- as.data.frame(weighted_sums)
    names(result_df) <- paste0(names(values), "_weighted_sum")
    result_df$unit_id <- names(weights)
    result_df[c("unit_id", names(result_df)[-ncol(result_df)])]
  }
}

#' Aggregate weighted values from a raster
#'
#' @description
#' General-purpose function to aggregate values from a raster (single or multi-layer) using
#' a stack of weight rasters. Each layer in the weights represents a different aggregation unit.
#'
#' @param values SpatRaster representing values to be aggregated. Can be single or multi-layer.
#'        If multi-layer, each layer represents a different realization/scenario.
#' @param weights Multi-layer SpatRaster where:
#'        - Each layer represents one aggregation unit
#'        - Values are weights (typically 0-1) for aggregation
#'        - Layer names should match unit IDs
#' @param na.rm Logical; if TRUE, remove NA values from computation (default = TRUE)
#' @param simplify Logical; if TRUE, returns vector (single layer) or matrix (multi-layer).
#'        If FALSE, returns data.frame in wide format. (default = FALSE)
#' @param snap Logical; if TRUE, avoid input validation and formatting. Used for performance
#'        optimization in internal functions.
#' @return Depending on input and simplify parameter:
#'         - If simplify=TRUE and single layer: named vector
#'         - If simplify=TRUE and multi-layer: matrix with rownames=units, colnames=scenarios
#'         - If simplify=FALSE: data.frame in wide format with unit_id column
#' @examples
#' # Load necessary library
#' library(terra)
#'
#' # Create test data
#' values <- rast(matrix(1:9, 3, 3)) # Single layer
#' weights <- c(
#'   rast(matrix(runif(9), 3, 3)),
#'   rast(matrix(runif(9), 3, 3))
#' )
#' names(weights) <- c("unit1", "unit2")
#'
#' # Single layer example
#' result1 <- gather_weighted(values, weights, simplify = TRUE)
#' result1_df <- gather_weighted(values, weights)
#'
#' # Multi-layer example
#' values_multi <- c(values, values * 2) # Two scenarios
#' names(values_multi) <- c("sim1", "sim2")
#' result2 <- gather_weighted(values_multi, weights, simplify = TRUE) # Returns matrix
#' result2_df <- gather_weighted(values_multi, weights)
#' @export
gather_weighted <- function(values, weights, na.rm = TRUE, simplify = FALSE, snap = FALSE) {
  # Validation
  if (!snap) {
    .chck_gather_weighted(values, weights, na.rm)
  }

  # Single layer fast track
  if (nlyr(values) == 1) {
    result <- .gather_weighted_core(values, weights, na.rm)

    # Fast return for snap mode
    if (snap) {
      return(result)
    }

    # Format output
    if (simplify) {
      return(setNames(result, names(weights)))
    } else {
      return(.help_format_gather(result, weights))
    }
  }

  # Multi-layer processing using vapply for direct matrix output
  results <- vapply(1:nlyr(values), function(i) {
    .gather_weighted_core(values[[i]], weights, na.rm)
  }, numeric(nlyr(weights)))

  # Fast return for snap mode
  if (snap) {
    return(results)
  }

  # Format output
  if (simplify) {
    rownames(results) <- names(weights)
    colnames(results) <- names(values)
    return(results)
  } else {
    return(.help_format_gather(results, weights, values))
  }
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
