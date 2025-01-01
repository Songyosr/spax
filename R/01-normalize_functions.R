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
#' @param a0 Non-negative numeric value representing outside option weight (default = 0)
#' @param snap Logical; if TRUE, avoid input validation
#' @param ... Additional arguments passed to custom normalization function
#' @return Object of the same class as input containing normalized weights
#' @examples
#' \dontrun{
#' library(terra)
#'
#' # Example 1: Basic vector normalization
#' weights <- c(2, 3, 5)
#' calc_normalize(weights) # Returns c(0.2, 0.3, 0.5)
#'
#' # Example 2: Semi-normalization with vector
#' small_weights <- c(0.2, 0.3, 0.4)
#' calc_normalize(small_weights, method = "semi") # Returns original values
#'
#' large_weights <- c(0.5, 0.7, 0.9)
#' calc_normalize(large_weights, method = "semi") # Normalizes since sum > 1
#'
#' # Example 3: Working with raster data
#' r <- rast(nrows = 10, ncols = 10)
#' values(r) <- runif(100) # Random weights
#' norm_rast <- calc_normalize(r) # Values sum to 1
#'
#' # Example 4: Using outside option (a0)
#' weights_with_outside <- c(2, 3, 5)
#' calc_normalize(weights_with_outside, a0 = 10) # Includes outside option weight
#'
#' # Example 5: Reference normalization
#' values <- c(10, 15, 25)
#' calc_normalize(values, method = "reference", ref_value = 20)
#'
#' # Example 6: Custom normalization function
#' custom_norm <- function(x) {
#'   x / (max(x, na.rm = TRUE) + min(x, na.rm = TRUE))
#' }
#' calc_normalize(values, method = custom_norm)
#'
#' # Example 7: Multi-layer raster normalization
#' r_stack <- c(r, r * 2, r * 0.5) # Create 3-layer raster
#' names(r_stack) <- c("layer1", "layer2", "layer3")
#' norm_stack <- calc_normalize(r_stack) # Normalizes each cell across layers
#'
#' # Example 8: Handling NA values
#' weights_with_na <- c(2, NA, 5, 3)
#' calc_normalize(weights_with_na) # NA values are handled appropriately
#' }
#' @export
calc_normalize <- function(x, method = "standard", ref_value = NULL,
                           a0 = 0, snap = FALSE, ...) {
  # Fast path for internal use
  if (snap) {
    if (is.function(method)) {
      return(method(x, ...))
    }
    return(.normalize_core(x, method, ref_value, a0))
  }

  # Regular path with validation
  .chck_normalize(x, method, ref_value, a0)

  # Use core computation
  .normalize_core(x, method, ref_value, a0, ...)
}

#' @keywords internal
.chck_normalize <- function(x, method, ref_value = NULL, a0 = 0) {
  # Input existence check
  if (length(x) == 0) {
    stop("Input weights cannot be empty")
  }

  # Method validation - handle both string methods and custom functions
  if (!is.function(method)) {
    valid_methods <- c("identity", "standard", "semi", "reference")
    if (!method %in% valid_methods) {
      stop("Invalid normalization method specified")
    }

    # ref_value validation only for reference method
    if (method == "reference" && !is.null(ref_value)) {
      if (!is.numeric(ref_value) || length(ref_value) != 1) {
        stop("ref_value must be a single numeric value")
      }
    }
  }

  # a0 validation
  if (!is.numeric(a0) || length(a0) != 1 || a0 < 0) {
    stop("a0 must be a non-negative numeric value")
  }

  invisible(TRUE)
}

#' @keywords internal
.normalize_core <- function(x, method, ref_value = NULL, a0 = 0, ...) {
  # Handle custom function first
  if (is.function(method)) {
    return(method(x, ...))
  }

  # Handle SpatRaster input
  if (inherits(x, "SpatRaster")) {
    x_sum <- sum(x, na.rm = TRUE)

    result <- switch(method,
      "identity" = x,
      "standard" = terra::ifel(x_sum + a0 > 0, x / (x_sum + a0), NA_real_), # no service = NA
      "semi" = terra::ifel(x_sum + a0 > 1, x / (x_sum + a0), x),
      "reference" = {
        if (is.null(ref_value)) ref_value <- max(x, na.rm = TRUE)
        terra::ifel(ref_value > 0, x / ref_value, NA_real_) # Avoid division by zero
      }
    )

    # Preserve names if they exist
    if (!is.null(names(x))) {
      names(result) <- names(x)
    }

    return(result)
  }

  # Handle numeric input
  x_sum <- sum(x, na.rm = TRUE)

  switch(method,
    "identity" = x,
    "standard" = if (x_sum + a0 > 0) x / (x_sum + a0) else rep(NA_real_, length(x)),
    "semi" = if (x_sum + a0 > 1) x / (x_sum + a0) else x,
    "reference" = {
      if (is.null(ref_value)) ref_value <- max(x, na.rm = TRUE)
      if (ref_value > 0) x / ref_value else rep(NA_real_, length(x))
    }
  )
}
