#' Normalize Spatial Weights
#'
#' @description
#' Normalizes weights using different methods. Supports numeric vectors, matrices,
#' and SpatRaster inputs with vectorized operations.
#'
#' @param x Numeric vector, matrix, or SpatRaster of weights
#' @param method Character string specifying normalization method:
#'        - "standard": Divide by sum (default)
#'        - "semi": Normalize only if sum > 1
#'        - "reference": Normalize by reference value (default is max)
#'        - "identity": Return unchanged
#'        - Or a custom function(x, ...)
#' @param ref_value Optional reference value for "reference" method
#' @param ... Additional arguments passed to custom normalization function
#' @return Object of the same class as input containing normalized weights
#' @examples
#' # Standard normalization
#' w <- c(0.5, 1.0, 1.5, 2.0)
#' normalize_weights(w)
#'
#' # Semi-normalization
#' normalize_weights(w, method = "semi")
#'
#' # Reference normalization
#' normalize_weights(w, method = "reference", ref_value = 2)
#'
#' # With SpatRaster
#' r <- terra::rast(nrows=10, ncols=10)
#' terra::values(r) <- runif(100)
#' normalize_weights(r)
#' @export
normalize_weights <- function(x, method = "standard", ref_value = NULL, ...) {
  # Input validation
  if (length(x) == 0) {
    stop("Input weights cannot be empty")
  }

  # Handle custom function
  if (is.function(method)) {
    return(method(x, ...))
  }

  # Use predefined methods
  switch(method,
         "identity" = x,
         "standard" = .standard_normalize(x),
         "semi" = .semi_normalize(x),
         "reference" = .reference_normalize(x, ref_value),
         stop("Invalid normalization method specified")
  )
}

#' @keywords internal
.standard_normalize <- function(x) {
  if (inherits(x, "SpatRaster")) {
    x_sum <- sum(x, na.rm = TRUE)
    return(ifel(x_sum > 0, x / x_sum, 0))
  } else {
    x_sum <- sum(x, na.rm = TRUE)
    if (x_sum > 0) {
      return(x / x_sum)
    } else {
      return(x * 0)
    }
  }
}

#' @keywords internal
.semi_normalize <- function(x) {
  if (inherits(x, "SpatRaster")) {
    x_sum <- sum(x, na.rm = TRUE)
    return(ifel(x_sum > 1, x / x_sum, x))
  } else {
    x_sum <- sum(x, na.rm = TRUE)
    if (x_sum > 1) {
      return(x / x_sum)
    } else {
      return(x)
    }
  }
}

#' @keywords internal
.reference_normalize <- function(x, ref_value = NULL) {
  # Determine reference value if not provided
  if (is.null(ref_value)) {
    ref_value <- max(x, na.rm = TRUE)
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
