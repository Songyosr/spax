#' @keywords internal
.normalize_core <- function(x, method, ref_value = NULL, a0 = 0, ...) {
  # Common computation patterns extracted
  if (inherits(x, "SpatRaster")) {
    x_sum <- sum(x, na.rm = TRUE)

    switch(method,
           "identity" = x,
           "standard" = ifel(x_sum + a0 > 0, x / (x_sum + a0), 0),
           "semi" = ifel(x_sum + a0 > 1, x / (x_sum + a0), x),
           "reference" = {
             if (is.null(ref_value)) ref_value <- max(x, na.rm = TRUE)
             ifel(ref_value > 0, x / ref_value, 0)
           })
  } else {
    x_sum <- sum(x, na.rm = TRUE)

    switch(method,
           "identity" = x,
           "standard" = if (x_sum + a0 > 0) x / (x_sum + a0) else x * 0,
           "semi" = if (x_sum + a0 > 1) x / (x_sum + a0) else x,
           "reference" = {
             if (is.null(ref_value)) ref_value <- max(x, na.rm = TRUE)
             if (ref_value > 0) x / ref_value else x * 0
           })
  }
}
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
#' @param snap Logical; if TRUE, avoid input validation. PS. snap is a performance optimization for internal use
#' @param ... Additional arguments passed to custom normalization function
#' @return Object of the same class as input containing normalized weights
#' @export
normalize_weights <- function(x, method = "standard", ref_value = NULL,
                              a0 = 0, snap = FALSE, ...) {
  # Fast path for internal use
  if (snap) {
    if (is.function(method)) {
      return(method(x, a0 = a0, ...))
    }
    return(.normalize_core(x, method, ref_value, a0))
  }

  # Regular path with validation
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

  # Validate method
  if (!method %in% c("identity", "standard", "semi", "reference")) {
    stop("Invalid normalization method specified")
  }

  # Use core computation
  .normalize_core(x, method, ref_value, a0)
}
