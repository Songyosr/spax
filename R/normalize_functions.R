#' Weight Normalization Functions
#'
#' A collection of functions for normalizing weights in spatial analysis.
#' Functions are vectorized and support numeric vectors, matrices, and SpatRaster inputs.
#'
#' Normalize weights using different methods
#'
#' @param x Numeric vector, matrix, or SpatRaster of weights
#' @param method Character string specifying normalization method or a custom function:
#'        "identity", "normalize", "semi_normalize", or a function(x, ...)
#' @param ... Additional arguments passed to custom normalization function
#' @return Object of the same class as input containing normalized weights
#' @export
normalize <- function(x, method = "normalize", ...) {
  # Check for empty input
  if (length(x) == 0) {
    stop("Input weights cannot be empty")
  }

  # If method is a function, use it directly
  if (is.function(method)) {
    return(method(x, ...))
  }

  # Otherwise, use predefined methods
  norm_weights <- switch(method,
                         "identity" = calc_identity(x),
                         "normalize" = calc_normalize(x),
                         "semi_normalize" = calc_semi_normalize(x),
                         stop("Invalid normalization method specified")
  )

  return(norm_weights)
}

#' Identity weight function
#'
#' Returns weights unchanged
#' @param x Numeric vector, matrix, or SpatRaster of weights
#' @return Same object as input
#' @keywords internal
calc_identity <- function(x) {
  if (length(x) == 0) stop("Input weights cannot be empty")
  x
}

#' Normalize weights
#'
#' Normalizes weights by dividing by their sum. For SpatRaster inputs,
#' normalization is performed cell-wise across layers.
#'
#' @param x Numeric vector, matrix, or SpatRaster of weights
#' @return Normalized weights that sum to 1 (per cell for SpatRaster)
#' @keywords internal
calc_normalize <- function(x) {
  if (length(x) == 0) stop("Input weights cannot be empty")

  if (inherits(x, "SpatRaster")) {
    # For SpatRaster, sum across layers for each cell
    x_sum <- sum(x, na.rm = TRUE)
    # Use ifel to handle NA values and division by zero
    return(ifel(x_sum > 0, x / x_sum, 0))
  } else {
    # For numeric vectors/matrices
    x_sum <- sum(x, na.rm = TRUE)
    if (x_sum > 0) {
      return(x / x_sum)
    } else {
      return(x * 0)  # Return zeros with same dimensions
    }
  }
}

#' Semi-normalize weights
#'
#' Normalizes weights only if they sum to more than 1.
#' For SpatRaster inputs, normalization is performed cell-wise across layers.
#'
#' @param x Numeric vector, matrix, or SpatRaster of weights
#' @return Semi-normalized weights
#' @keywords internal
calc_semi_normalize <- function(x) {
  if (length(x) == 0) stop("Input weights cannot be empty")

  if (inherits(x, "SpatRaster")) {
    # For SpatRaster, sum across layers for each cell
    x_sum <- sum(x, na.rm = TRUE)
    # Use ifel for conditional normalization
    return(ifel(x_sum > 1, x / x_sum, x))
  } else {
    # For numeric vectors/matrices
    x_sum <- sum(x, na.rm = TRUE)
    if (x_sum > 1) {
      return(x / x_sum)
    } else {
      return(x)
    }
  }
}

#' Calculate competing weights
#'
#' Calculates competing weights based on a reference value.
#' Useful for scenarios where weights should be relative to a maximum or reference.
#'
#' @param x Numeric vector, matrix, or SpatRaster of weights
#' @param ref_value Reference value for competition (default is max value)
#' @return Competing weights relative to reference value
#' @export
calc_competing <- function(x, ref_value = NULL) {
  if (length(x) == 0) stop("Input weights cannot be empty")

  if (is.null(ref_value)) {
    if (inherits(x, "SpatRaster")) {
      ref_value <- max(x, na.rm = TRUE)
    } else {
      ref_value <- max(x, na.rm = TRUE)
    }
  }

  if (inherits(x, "SpatRaster")) {
    return(ifel(ref_value > 0, x / ref_value, 0))
  } else {
    if (ref_value > 0) {
      return(x / ref_value)
    } else {
      return(x * 0)
    }
  }
}

# Example usage:
# library(terra)
#
# # With numeric vector
# weights <- c(0.5, 1.0, 1.5, 2.0)
# norm_weights <- normalize(weights, method = "normalize")
# semi_norm_weights <- normalize(weights, method = "semi_normalize")
#
# # With SpatRaster
# r <- rast(nrows=10, ncols=10)
# values(r) <- runif(100)
# norm_raster <- normalize(r, method = "normalize")
#
# # Custom normalization function
# custom_norm <- function(x, threshold = 1) {
#   if (inherits(x, "SpatRaster")) {
#     x_sum <- sum(x, na.rm = TRUE)
#     return(ifel(x_sum > threshold, x / x_sum, x))
#   } else {
#     x_sum <- sum(x, na.rm = TRUE)
#     if (x_sum > threshold) return(x / x_sum) else return(x)
#   }
# }
# custom_weights <- normalize(weights, method = custom_norm, threshold = 2)
