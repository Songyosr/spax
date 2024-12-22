#' Validate inputs for spread_weighted
#' @param values Numeric vector, matrix or data.frame of values
#' @param weights Multi-layer SpatRaster of weights
#' @param value_cols Column names if values is data.frame
#' @keywords internal
.chck_spread_weighted <- function(values, weights, value_cols = NULL) {
  # Check weights
  if (!inherits(weights, "SpatRaster")) {
    stop("weights must be a SpatRaster object")
  }

  # Handle sf objects first
  if (inherits(values, "sf")) {
    stop("Input is an sf object. Please use st_drop_geometry() first to convert to a regular data frame")
  }

  # Handle different input types
  if (is.vector(values)) {
    if (length(values) != nlyr(weights)) {
      stop("Length of values must match number of weight layers")
    }
    return(TRUE)
  }

  if (is.matrix(values)) {
    if (nrow(values) != nlyr(weights)) {
      stop("Number of rows in values must match number of weight layers")
    }
    return(TRUE)
  }

  if (is.data.frame(values)) {
    if (is.null(value_cols)) {
      stop("value_cols must be specified when values is a data.frame")
    }
    if (!all(value_cols %in% names(values))) {
      stop("Not all value_cols found in data.frame")
    }
    if (nrow(values) != nlyr(weights)) {
      stop("Number of rows in values must match number of weight layers")
    }
    return(TRUE)
  }

  stop("values must be a vector, matrix, or data.frame")
}

#' Core spreading computation for single value vector
#' @param values Numeric vector of values
#' @param weights Multi-layer SpatRaster of weights
#' @param full_output Whether to return intermediate calculations
#' @return SpatRaster or list depending on full_output
#' @keywords internal
.spread_core <- function(values, weights, full_output = FALSE) {
  if (length(values) != nlyr(weights)) {
    stop("Number of values must match number of weight layers")
  }

  unit_distribution <- weights * values
  total_distribution <- terra::app(unit_distribution, fun = sum, na.rm = TRUE)

  if (full_output) {
    return(list(
      total_distribution = total_distribution,
      unit_distribution = unit_distribution
    ))
  } else {
    return(total_distribution)
  }
}

#' Distribute values across weighted surfaces
#'
#' @description
#' General-purpose function to distribute values across space using a stack of weight
#' rasters. Each input value is spread according to its corresponding weight layer.
#'
#' @param values Numeric vector, matrix, or data.frame of values to distribute:
#'        - Vector: Must match number of weight layers
#'        - Matrix: Rows match weight layers, columns are different measures
#'        - Data.frame: Same as matrix but requires value_cols parameter
#' @param weights Multi-layer SpatRaster where each layer represents one unit's
#'        weighted distribution surface
#' @param value_cols Character vector of column names if values is a data.frame
#' @param full_output Logical; whether to return intermediate calculations (default FALSE)
#' @param name_prefix Character string to prepend to output names (default NULL)
#' @param snap Logical; if TRUE, avoid input validation for performance (default FALSE)
#' @return If full_output = TRUE, returns list containing:
#'         - total_distribution: SpatRaster stack of total distribution per measure
#'         - unit_distribution: List of unit-specific distribution SpatRasters
#'         If full_output = FALSE, returns only total_distribution SpatRaster
#' @examples
#' \dontrun{
#' # Example 1: Basic vector input using package data
#' # Spread doctor counts across space using distance-decay weights
#' weights <- calc_decay(hos_iscr, method = "gaussian", sigma = 30)
#' doc_distribution <- spread_weighted(hc12_hos$s_doc, weights)
#'
#' # Example 2: Multiple supply measures
#' supply_df <- hc12_hos[c("s_doc", "s_nurse")]
#' distributions <- spread_weighted(supply_df, weights,
#'   value_cols = c("s_doc", "s_nurse"),
#'   parallel = TRUE
#' )
#'
#' # Example 3: With full output for detailed analysis
#' result <- spread_weighted(hc12_hos$s_doc, weights,
#'   full_output = TRUE
#' )
#' plot(result$total_distribution, main = "Total Distribution")
#' plot(result$unit_distribution[[1]], main = "First Unit Distribution")
#' }
#' @export
spread_weighted <- function(values, weights, value_cols = NULL,
                            full_output = FALSE, name_prefix = NULL,
                            snap = FALSE) {
  # Validate all inputs unless in snap mode
  if (!snap) {
    .chck_spread_weighted(values, weights, value_cols)
  }

  # Handle vector input directly for maximum performance
  if (is.vector(values)) {
    return(.spread_core(values, weights, full_output))
  }

  # Process data.frame or matrix input
  if (is.data.frame(values)) {
    values_mat <- as.matrix(values[value_cols])
    col_names <- value_cols
  } else if (is.matrix(values)) {
    values_mat <- values
    col_names <- colnames(values_mat)
    if (is.null(col_names)) {
      col_names <- paste0("value", 1:ncol(values_mat))
    }
  }

  # Process each column
  results <- apply(values_mat, 2, function(v) {
    .spread_core(v, weights, full_output)
  }, simplify = FALSE)

  # Add names and prefix if provided
  if (!is.null(name_prefix)) {
    col_names <- paste0(name_prefix, col_names)
  }
  names(results) <- col_names

  # Format output
  if (full_output) {
    total_distribution <- rast(lapply(results, function(x) x$total_distribution))
    names(total_distribution) <- col_names

    return(list(
      total_distribution = total_distribution,
      unit_distribution = lapply(results, function(x) x$unit_distribution)
    ))
  } else {
    total_distribution <- rast(results)
    names(total_distribution) <- col_names
    return(total_distribution)
  }
}
