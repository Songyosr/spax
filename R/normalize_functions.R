#' Normalize Spatial Weights
#'
#' @description
#' Normalizes weights using different methods with support for outside options.
#'
#' @param x Numeric vector, matrix, or SpatRaster of weights
#' @param method Character string specifying normalization method:
#'
#'        - "standard": Divide by sum + a0 (default)
#'
#'        - "semi": Normalize only if sum + a0 > 1
#'
#'        - "reference": Normalize by reference value
#'
#'        - "identity": Return unchanged
#'
#'        - Or a custom function(x, ...)
#'
#' @param ref_value Optional reference value for "reference" method
#' @param a0 Non-negative numeric value representing outside option weight (default = 0) *Now working for only standard and semi method
#' @param ... Additional arguments passed to custom normalization function
#' @return Object of the same class as input containing normalized weights
#' @export
normalize_weights <- function(x, method = "standard", ref_value = NULL,
                              a0 = 0, ...) {
  # Input validation
  if (length(x) == 0) {
    stop("Input weights cannot be empty")
  }
  if (!is.numeric(a0) || length(a0) != 1 || a0 < 0) {
    stop("a0 must be a non-negative numeric value")
  }

  # Handle custom function
  if (is.function(method)) {
    return(method(x, a0 = a0, ...))
  }

  # Use predefined methods with a0
  switch(method,
         "identity" = x,
         "standard" = .standard_normalize(x, a0),
         "semi" = .semi_normalize(x, a0),
         "reference" = .reference_normalize(x, ref_value),
         stop("Invalid normalization method specified")
  )
}

#' @keywords internal
.standard_normalize <- function(x, a0 = 0) {
  if (inherits(x, "SpatRaster")) {
    x_sum <- sum(x, na.rm = TRUE)
    return(ifel(x_sum + a0 > 0, x / (x_sum + a0), 0))
  } else {
    x_sum <- sum(x, na.rm = TRUE)
    if (x_sum + a0 > 0) {
      return(x / (x_sum + a0))
    } else {
      return(x * 0)
    }
  }
}

#' @keywords internal
.semi_normalize <- function(x, a0 = 0) {
  if (inherits(x, "SpatRaster")) {
    x_sum <- sum(x, na.rm = TRUE)
    return(ifel(x_sum + a0 > 1, x / (x_sum + a0), x))
  } else {
    x_sum <- sum(x, na.rm = TRUE)
    if (x_sum + a0 > 1) {
      return(x / (x_sum + a0))
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
