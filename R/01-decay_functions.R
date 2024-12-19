#' Calculate Spatial Distance Decay Weights
#'
#' @description
#' Calculate weights using different spatial decay functions or a custom function.
#' Supports built-in methods and custom decay functions.
#'
#' @param distance Numeric vector, matrix, or SpatRaster of distances
#' @param method Character string specifying the decay function or a custom function:
#'        "gaussian", "exponential", "power", "inverse", "binary", or function(distance, ...)
#' @param sigma Parameter controlling the rate of decay
#' @param ... Additional parameters passed to custom decay functions
#' @return Object of the same class as input containing decay weights
#' @export
calc_decay <- function(distance, method = "gaussian", sigma = NULL, ...) {
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
    return(method(distance, sigma = sigma, ...))
  }

  # Select and apply the appropriate decay function
  weights <- switch(method,
                    "gaussian" = .gaussian_weights(distance, sigma, ...),
                    "exponential" = .exponential_weights(distance, sigma, ...),
                    "power" = .power_weights(distance, sigma, ...),
                    "inverse" = .inverse_weights(distance, ...),
                    "binary" = .binary_weights(distance, sigma, ...),
                    stop("Invalid method specified")
  )
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
