#' Compute Spatial Distance Decay Weights
#'
#' @description
#' Computes weights using different spatial decay functions with optional attractiveness scaling.
#' Supports built-in methods and custom decay functions.
#'
#' @param distance Numeric vector, matrix, or SpatRaster of distances
#' @param method Character string specifying the decay function or a custom function:
#'        "gaussian", "exponential", "power", "inverse", "binary", or function(distance, ...)
#' @param sigma Parameter controlling the rate of decay
#' @param attractiveness Numeric vector of attractiveness values, length must match number of layers
#' @param alpha Numeric parameter controlling attractiveness sensitivity (default = 1)
#' @param ... Additional parameters passed to custom decay functions
#' @return Object of the same class as input containing decay weights
#' @export
compute_weights <- function(distance, method = "gaussian", sigma = NULL,
                            attractiveness = NULL, alpha = 1, ...) {
  # Input validation
  if (!is.null(attractiveness)) {
    if (!is.numeric(attractiveness)) {
      stop("attractiveness must be a numeric vector")
    }
    if (inherits(distance, "SpatRaster") &&
        length(attractiveness) != nlyr(distance)) {
      stop("Length of attractiveness vector must match number of distance layers")
    }
    if (any(attractiveness < 0, na.rm = TRUE)) {
      stop("attractiveness values must be non-negative")
    }
    if (!is.numeric(alpha)) {
      stop("alpha must be numeric")
    }
  }

  # Set default sigma if not provided
  if (is.null(sigma)) {
    sigma <- switch(method,
                    "gaussian" = 11.64884,
                    "exponential" = 0.1,
                    "power" = 2,
                    "binary" = 97.5,
                    sigma  # Keep provided sigma for custom functions
    )
  }

  # Handle custom function
  if (is.function(method)) {
    weights <- method(distance, sigma = sigma, ...)
  } else {
    # Select and apply the appropriate decay function
    weights <- switch(method,
                      "gaussian" = .gaussian_weights(distance, sigma, ...),
                      "exponential" = .exponential_weights(distance, sigma, ...),
                      "power" = .power_weights(distance, sigma, ...),
                      "inverse" = .inverse_weights(distance, ...),
                      "binary" = .binary_weights(distance, sigma, ...),
                      stop("Invalid method specified")
    )
  }

  # Apply attractiveness scaling if provided
  if (!is.null(attractiveness)) {
    if (inherits(distance, "SpatRaster")) {
      # For SpatRaster, multiply each layer by corresponding attractiveness
      for (i in seq_along(attractiveness)) {
        weights[[i]] <- weights[[i]] * (attractiveness[i]^alpha)
      }
    } else {
      # For matrix/vector, multiply by attractiveness directly
      weights <- weights * (attractiveness^alpha)
    }
  }

  return(weights)
}
#' Gaussian decay weight computation
#'
#' @param distance Numeric vector of distances
#' @param sigma Standard deviation parameter controlling spread
#' @param ... Additional parameters (not used in gaussian decay)
#' @return Numeric vector of weights
#' @keywords internal
.gaussian_weights <- function(distance, sigma = 11.64884, ...) {
  return(exp(-(distance^2) / (2 * sigma^2)))
}

#' Exponential decay weight computation
#'
#' @param distance Numeric vector of distances
#' @param sigma Rate parameter controlling steepness of decay
#' @param ... Additional parameters (not used in exponential decay)
#' @return Numeric vector of weights
#' @keywords internal
.exponential_weights <- function(distance, sigma, ...) {
  return(exp(-sigma * distance))
}

#' Power decay weight computation
#'
#' @param distance Numeric vector of distances
#' @param sigma Exponent parameter controlling rate of decay
#' @param ... Additional parameters (not used in power decay)
#' @return Numeric vector of weights
#' @keywords internal
.power_weights <- function(distance, sigma, ...) {
  return(distance^(-sigma))
}

#' Inverse distance weight computation
#'
#' @param distance Numeric vector of distances
#' @param c Constant to prevent division by zero
#' @param ... Additional parameters (not used in inverse decay)
#' @return Numeric vector of weights
#' @keywords internal
.inverse_weights <- function(distance, c = 1, ...) {
  return(1 / (distance + c))
}

#' Binary decay weight computation
#'
#' @param distance Numeric vector of distances
#' @param sigma Distance threshold
#' @param ... Additional parameters (not used in binary decay)
#' @return Numeric vector of weights (1 if distance <= threshold, 0 otherwise)
#' @keywords internal
.binary_weights <- function(distance, sigma = 97.5, ...) {
  weight <- +(distance <= sigma)
  weight[is.na(weight)] <- 0
  return(weight)
}
