#' Calculate Spatial Distance Decay Weights
#'
#' @description
#' Calculate weights using different spatial decay functions or a custom function.
#' Supports built-in methods and custom decay functions. The decay weights represent
#' how the influence or interaction strength diminishes with distance.
#'
#' @param distance Numeric vector, matrix, or SpatRaster of distances
#' @param method Character string specifying the decay function or a custom function:
#'        "gaussian", "exponential", "power", "inverse", "binary", or function(distance, ...);
#'        or a kernel declared with \code{\link{decay_kernel}}
#' @param sigma Parameter controlling the rate of decay. A single number, or a
#'        SpatRaster aligned to \code{distance} (1 layer, or one per layer) for a
#'        spatially varying catchment (variable-catchment FCA / V2SFCA). A
#'        declared custom kernel needs a single positive number within its
#'        bounds.
#' @param snap Logical; if TRUE, skip input validation
#' @param ... Additional parameters for custom decay functions, or the offset
#'        \code{c} of the "inverse" kernel. A declared custom kernel takes none:
#'        its fixed arguments belong to the declaration.
#' @return Object of the same class as input containing decay weights
#' @details
#' Built-in kernels for a distance \eqn{d}, and the \code{sigma} used when
#' none is given:
#' \itemize{
#'   \item \code{"gaussian"}: \eqn{\exp(-d^2 / (2\sigma^2))}{exp(-d^2 / (2 sigma^2))};
#'     default \eqn{\sigma = 11.64884}{sigma = 11.64884}.
#'   \item \code{"exponential"}: \eqn{\exp(-\sigma d)}{exp(-sigma d)}; default
#'     \eqn{\sigma = 0.1}{sigma = 0.1}.
#'   \item \code{"power"}: \eqn{d^{-\sigma}}{d^(-sigma)}; default
#'     \eqn{\sigma = 2}{sigma = 2}. The weight is \code{Inf} at distance 0.
#'   \item \code{"inverse"}: \eqn{1 / (d + c)}{1 / (d + c)}, with \code{c = 1}
#'     unless \code{c} is passed in \code{...}; \code{sigma} does not enter
#'     the weights (a given value is still validated).
#'   \item \code{"binary"}: 1 when \eqn{d \le \sigma}{d <= sigma}, otherwise 0;
#'     default \eqn{\sigma = 97.5}{sigma = 97.5}.
#' }
#' The defaults are historical values, not recommendations: set \code{sigma}
#' for the units and scale of your travel times.
#'
#' A kernel declared with \code{\link{decay_kernel}} can be given as
#' \code{method}. A built-in declaration behaves exactly like its name. A
#' declared custom kernel needs \code{sigma}, a single positive number within the
#' bounds of the declaration, and takes its fixed arguments from the declaration, so
#' \code{...} must be empty; a SpatRaster \code{sigma} is refused. It is
#' evaluated on the finite distances only: a missing distance has the weight
#' \code{NA} and an infinite distance (an absent edge) the weight 0, as with
#' the built-in kernels. A negative or non-finite weight at a finite distance is
#' an error. A vector or matrix keeps its shape, and a SpatRaster keeps its
#' geometry, layer names and missing cells; it is read into memory for a
#' declared kernel.
#' @examples
#' library(terra)
#'
#' # Distances on a small 10 x 10 grid
#' r <- rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10)
#' values(r) <- 1:100
#'
#' # Built-in decay functions
#' w <- c(
#'   calc_decay(r, method = "gaussian", sigma = 30),
#'   calc_decay(r, method = "exponential", sigma = 0.1),
#'   calc_decay(r, method = "power", sigma = 2),
#'   calc_decay(r, method = "binary", sigma = 50)
#' )
#' names(w) <- c("gaussian", "exponential", "power", "binary")
#' plot(w)
#'
#' # A custom decay function receives the distances and its own arguments
#' gaussian_power <- function(distance, sigma = 30, power = 2, ...) {
#'   exp(-(distance^2) / (2 * sigma^2)) * distance^(-power)
#' }
#' w_custom <- calc_decay(r, method = gaussian_power, sigma = 30, power = 1.5)
#'
#' # A declared kernel records its meaning, bounds and fixed arguments
#' plateau <- decay_kernel(
#'   function(distance, sigma, plateau = 5) {
#'     ifelse(distance <= plateau, 1, exp(-(distance - plateau)^2 / (2 * sigma^2)))
#'   },
#'   name = "plateau", parameter = "scale", args = list(plateau = 5)
#' )
#' w_plateau <- calc_decay(r, method = plateau, sigma = 10)
#'
#' # One layer per facility gives one layer of weights per facility
#' distances <- rast(replicate(3, r))
#' names(distances) <- c("facility1", "facility2", "facility3")
#' weights <- calc_decay(distances, method = "gaussian", sigma = 30)
#' @export
calc_decay <- function(distance, method = "gaussian", sigma = NULL, snap = FALSE, ...) {
  # A declared kernel (see decay_kernel()) comes first: a built-in declaration is
  # its method string, a custom one is evaluated on the finite distances only.
  if (inherits(method, "spax_decay_kernel")) {
    .validate_decay_kernel(method)
    if (!isTRUE(method$builtin)) {
      return(.calc_decay_declared(distance, method, sigma, snap, ...))
    }
    method <- method$name
  }

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

# The weights of a custom declared kernel for a numeric vector, matrix or
# SpatRaster. Only the finite distances are evaluated; an infinite distance gets
# 0 and a missing one NA.
.calc_decay_declared <- function(distance, kernel, sigma, snap, ...) {
  if (...length() > 0L) {
    stop("pass fixed arguments through decay_kernel(args = )")
  }
  if (inherits(sigma, "SpatRaster")) {
    stop("a variable catchment needs a built-in method or a plain function")
  }
  if (is.null(sigma)) {
    stop("sigma is required for a declared kernel")
  }
  if (!is.numeric(sigma) || length(sigma) != 1L || !is.finite(sigma) ||
      sigma < kernel$lower || sigma > kernel$upper) {
    stop("sigma must be a single number from ", format(kernel$lower), " to ",
         format(kernel$upper))
  }
  if (sigma <= 0) {
    stop(kernel$name, " decay requires positive sigma")
  }
  if (!snap && !inherits(distance, "SpatRaster") &&
      (!is.numeric(distance) || is.object(distance))) {
    stop("distance must be one of the following classes: numeric, matrix, SpatRaster")
  }
  sigma <- as.numeric(sigma)
  fun <- .decay_kernel_function(kernel)
  if (inherits(distance, "SpatRaster")) {
    values <- terra::values(distance, mat = TRUE)
    weights <- .eval_decay_kernel(kernel, values, sigma, fun)
    .check_declared_weights(values, weights, sigma)
    out <- terra::rast(distance)
    terra::values(out) <- weights
    return(out)
  }
  weights <- .eval_decay_kernel(kernel, distance, sigma, fun)
  .check_declared_weights(distance, weights, sigma)
  weights
}

# A declared kernel must give every finite distance a finite, nonnegative weight.
# The declaration only tests a grid, so other distances are checked here, where the
# weights are returned to the caller. A missing distance keeps NA and an infinite
# one (an absent edge) 0, which are not checked.
.check_declared_weights <- function(distance, weights, sigma) {
  bad <- which(is.finite(distance) & (!is.finite(weights) | weights < 0))
  if (length(bad)) {
    stop("the kernel returned a negative or non-finite weight at distance ",
         format(distance[bad[1L]]), " with sigma = ", format(sigma), call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate inputs for decay calculation
#' @param distance Numeric vector, matrix, or SpatRaster of distances
#' @param method Character string or function specifying decay method
#' @param sigma Numeric parameter controlling decay rate
#' @param snap Logical for validation skipping
#' @keywords internal
.chck_decay <- function(distance, method, sigma = NULL, snap = FALSE) {
  if (snap) {
    return(TRUE)
  }

  # Distance: a plain numeric (double or integer) vector or matrix, or a
  # SpatRaster. Classed numerics such as units objects are refused.
  if (!inherits(distance, "SpatRaster") &&
      (!is.numeric(distance) || is.object(distance))) {
    stop("distance must be one of the following classes: numeric, matrix, SpatRaster")
  }

  # Check method is either a character or function
  if (!is.function(method)) {
    if (!method %in% c("gaussian", "exponential", "power", "inverse", "binary")) {
      stop("Invalid method specified")
    }

    # Sigma validation if provided: a scalar, or a SpatRaster for a variable
    # catchment (spatially varying sigma), aligned to `distance`.
    if (!is.null(sigma)) {
      if (inherits(sigma, "SpatRaster")) {
        if (!inherits(distance, "SpatRaster")) {
          stop("a SpatRaster `sigma` (variable catchment) requires a SpatRaster `distance`")
        }
        .chck_raster_alignment(sigma, distance[[1]], "sigma", "distance")
        if (!terra::nlyr(sigma) %in% c(1L, terra::nlyr(distance))) {
          stop("`sigma` raster must have 1 layer or one layer per `distance` layer")
        }
        smin <- min(terra::global(sigma, "min", na.rm = TRUE)[[1]])
        if (is.finite(smin) && smin <= 0) {
          stop("`sigma` must be positive")
        }
      } else {
        .chck_is_numeric(sigma, "sigma")
        .chck_length(length(sigma), 1, "sigma")
        .chck_positive(sigma, allow_zero = FALSE, "sigma")
      }
    }
  }

  invisible(TRUE)
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
  weight <- 0 + (distance <= sigma)
  return(weight)
}
