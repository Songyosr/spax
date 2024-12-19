#' Calculate Spatial Distance Decay Weights
#'
#' @description
#' Calculate weights using different spatial decay functions or a custom function.
#' Supports built-in methods and custom decay functions. The decay weights represent
#' how the influence or interaction strength diminishes with distance.
#'
#' @param distance Numeric vector, matrix, or SpatRaster of distances
#' @param method Character string specifying the decay function or a custom function:
#'        "gaussian", "exponential", "power", "inverse", "binary", or function(distance, ...)
#' @param sigma Parameter controlling the rate of decay
#' @param snap Logical; if TRUE, skip input validation
#' @param ... Additional parameters passed to custom decay functions
#' @return Object of the same class as input containing decay weights
#' @examples
#' \dontrun{
#' library(terra)
#' library(spax)
#'
#' # Create a simple distance raster (10x10 grid)
#' r <- rast(nrows=10, ncols=10, xmin=0, xmax=10, ymin=0, ymax=10)
#' values(r) <- 1:100  # Distance values
#'
#' # 1. Basic usage with different methods ----
#' # Gaussian decay
#' w1 <- calc_decay(r, method="gaussian", sigma=30)
#'
#' # Exponential decay
#' w2 <- calc_decay(r, method="exponential", sigma=0.1)
#'
#' # Power decay
#' w3 <- calc_decay(r, method="power", sigma=2)
#'
#' # Binary threshold
#' w4 <- calc_decay(r, method="binary", sigma=50)
#'
#' # Plot to compare
#' plot(c(w1, w2, w3, w4))
#'
#' # 2. Custom decay function ----
#' # Create a custom decay function that combines gaussian and power
#' custom_decay <- function(distance, sigma=30, power=2, ...) {
#'   gaussian <- exp(-(distance^2) / (2 * sigma^2))
#'   power_decay <- distance^(-power)
#'   return(gaussian * power_decay)
#' }
#'
#' w5 <- calc_decay(r, method=custom_decay, sigma=30, power=1.5)
#'
#' # 3. Working with multiple facilities ----
#' # Create distance rasters for 3 facilities
#' distances <- rast(replicate(3, r))  # Stack of 3 identical rasters
#' names(distances) <- c("facility1", "facility2", "facility3")
#'
#' # Calculate decay weights for all facilities
#' weights <- calc_decay(distances, method="gaussian", sigma=30)
#'
#' # 4. Performance optimization ----
#' # Use snap=TRUE when calling repeatedly in performance-critical code
#' weights_fast <- calc_decay(distances, method="gaussian", sigma=30, snap=TRUE)
#'
#' # 5. Compare decay parameters ----
#' # Create a sequence of sigma values
#' sigmas <- c(10, 20, 30, 40, 50)
#'
#' # Calculate and plot weights for each sigma
#' weights_list <- lapply(sigmas, function(s) {
#'   calc_decay(r, method="gaussian", sigma=s)
#' })
#'
#' # Convert to SpatRaster stack and plot
#' weight_stack <- rast(weights_list)
#' names(weight_stack) <- paste0("sigma_", sigmas)
#' plot(weight_stack)
#' }
#' @export
calc_decay <- function(distance, method = "gaussian", sigma = NULL, snap = FALSE, ...) {

  # Handle custom function first
  if (is.function(method)) {
    # Get function arguments
    fn_args <- names(formals(method))

    # Build argument list starting with distance
    args <- list(distance = distance)

    # Add sigma if the function accepts it
    if ("sigma" %in% fn_args && !is.null(sigma)) {
      args$sigma <- sigma
    }

    # Add any additional arguments
    dots <- list(...)
    for (arg in names(dots)) {
      if (arg %in% fn_args) {
        args[[arg]] <- dots[[arg]]
      }
    }

    return(do.call(method, args))
  }

  # Input validation (unless in snap mode)
  .chck_decay(distance, method, sigma, snap)

  # Set default sigma if not provided
  if (is.null(sigma)) {
    sigma <- switch(method,
                    "gaussian" = 11.64884,
                    "exponential" = 0.1,
                    "power" = 2,
                    "binary" = 97.5
    )
  }

  # Select and apply the appropriate decay function
  switch(method,
         "gaussian" = .help_decay_gaussian(distance, sigma, ...),
         "exponential" = .help_decay_exponential(distance, sigma, ...),
         "power" = .help_decay_power(distance, sigma, ...),
         "inverse" = .help_decay_inverse(distance, ...),
         "binary" = .help_decay_binary(distance, sigma, ...),
         stop("Invalid method specified")
  )
}

#' Validate inputs for decay calculation
#' @param distance Numeric vector, matrix, or SpatRaster of distances
#' @param method Character string or function specifying decay method
#' @param sigma Numeric parameter controlling decay rate
#' @param snap Logical for validation skipping
#' @keywords internal
.chck_decay <- function(distance, method, sigma = NULL, snap = FALSE) {
  if (snap) return(TRUE)

  if (!inherits(distance, "SpatRaster") && !is.numeric(distance)) {
    stop("distance must be numeric or SpatRaster")
  }

  if (!is.function(method)) {
    if (!method %in% c("gaussian", "exponential", "power", "inverse", "binary")) {
      stop("Invalid method specified")
    }

    # Only validate sigma for built-in methods
    if (!is.null(sigma)) {
      if (!is.numeric(sigma) || length(sigma) != 1) {
        stop("sigma must be a single numeric value")
      }
      if (sigma <= 0) {
        stop("sigma must be positive")
      }
    }
  }

  return(TRUE)
}

#' Helper function for Gaussian decay weight computation
#' @param distance Numeric vector of distances
#' @param sigma Standard deviation parameter controlling spread
#' @param ... Additional parameters (not used in gaussian decay)
#' @return Numeric vector of weights
#' @keywords internal
.help_decay_gaussian <- function(distance, sigma = 11.64884, ...) {
  return(exp(-(distance^2) / (2 * sigma^2)))
}

#' Helper function for exponential decay weight computation
#' @param distance Numeric vector of distances
#' @param sigma Rate parameter controlling steepness of decay
#' @param ... Additional parameters (not used in exponential decay)
#' @return Numeric vector of weights
#' @keywords internal
.help_decay_exponential <- function(distance, sigma, ...) {
  return(exp(-sigma * distance))
}

#' Helper function for power decay weight computation
#' @param distance Numeric vector of distances
#' @param sigma Exponent parameter controlling rate of decay
#' @param ... Additional parameters (not used in power decay)
#' @return Numeric vector of weights
#' @keywords internal
.help_decay_power <- function(distance, sigma, ...) {
  return(distance^(-sigma))
}

#' Helper function for inverse distance weight computation
#' @param distance Numeric vector of distances
#' @param c Constant to prevent division by zero
#' @param ... Additional parameters (not used in inverse decay)
#' @return Numeric vector of weights
#' @keywords internal
.help_decay_inverse <- function(distance, c = 1, ...) {
  return(1 / (distance + c))
}

#' Helper function for binary decay weight computation
#' @param distance Numeric vector of distances
#' @param sigma Distance threshold
#' @param ... Additional parameters (not used in binary decay)
#' @return Numeric vector of weights
#' @keywords internal
.help_decay_binary <- function(distance, sigma = 97.5, ...) {
  weight <- +(distance <= sigma)
  weight[is.na(weight)] <- 0
  return(weight)
}

