#' Calculate Spatial Decay Weights
#'
#' @description
#' Calculates weights using different spatial decay functions or a custom function.
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
                    "gaussian" = calc_gaussian_decay(distance, sigma, ...),
                    "exponential" = calc_exponential_decay(distance, sigma, ...),
                    "power" = calc_power_decay(distance, sigma, ...),
                    "inverse" = calc_inverse_decay(distance, ...),
                    "binary" = calc_binary_decay(distance, sigma, ...),
                    stop("Invalid method specified")
  )

  return(weights)
}

#' Calculate Gaussian Decay Weights
#'
#' @param distance Numeric vector of distances
#' @param sigma Standard deviation parameter controlling spread
#' @param ... Additional parameters (not used in gaussian decay)
#' @return Numeric vector of weights
#' @keywords internal
calc_gaussian_decay <- function(distance, sigma = 11.64884, ...) {
  return(exp(-(distance^2) / (2 * sigma^2)))
}

#' Calculate Exponential Decay Weights
#'
#' @param distance Numeric vector of distances
#' @param sigma Rate parameter controlling steepness of decay
#' @param ... Additional parameters (not used in exponential decay)
#' @return Numeric vector of weights
#' @keywords internal
calc_exponential_decay <- function(distance, sigma, ...) {
  return(exp(-sigma * distance))
}

#' Calculate Power Decay Weights
#'
#' @param distance Numeric vector of distances
#' @param sigma Exponent parameter controlling rate of decay
#' @param ... Additional parameters (not used in power decay)
#' @return Numeric vector of weights
#' @keywords internal
calc_power_decay <- function(distance, sigma, ...) {
  return(distance^(-sigma))
}

#' Calculate Inverse Distance Decay Weights
#'
#' @param distance Numeric vector of distances
#' @param c Constant to prevent division by zero
#' @param ... Additional parameters (not used in inverse decay)
#' @return Numeric vector of weights
#' @keywords internal
calc_inverse_decay <- function(distance, c = 1, ...) {
  return(1 / (distance + c))
}

#' Calculate Binary Decay Weights
#'
#' @param distance Numeric vector of distances
#' @param sigma Distance threshold
#' @param ... Additional parameters (not used in binary decay)
#' @return Numeric vector of weights (1 if distance <= threshold, 0 otherwise)
#' @keywords internal
calc_binary_decay <- function(distance, sigma = 97.5, ...) {
  weight <- +(distance <= sigma)
  weight[is.na(weight)] <- 0
  return(weight)
}
