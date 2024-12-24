#' Validate SpatRaster input
#' @param x SpatRaster or SpatRaster-coercible object to validate
#' @param template Optional SpatRaster to check compatibility
#' @param allow_null Logical; if TRUE, x can be NULL if template provided
#' @return Invisible TRUE if valid, error otherwise
#' @keywords internal
.chck_raster_input <- function(x, template = NULL, allow_null = FALSE) {
  # Handle NULL case
  if (is.null(x)) {
    if (!allow_null || is.null(template)) {
      stop("Input cannot be NULL")
    }
    return(invisible(TRUE))
  }

  # Check input type
  if (!inherits(x, "SpatRaster")) {
    stop("Input must be a SpatRaster object")
  }

  # Basic raster validation
  if (terra::nlyr(x) < 1) {
    stop("Input raster has no layers")
  }

  # Template compatibility if provided
  if (!is.null(template)) {
    if (!inherits(template, "SpatRaster")) {
      stop("Template must be a SpatRaster object")
    }

    # Check resolution match
    if (!all(terra::res(x) == terra::res(template))) {
      stop("Input and template must have same resolution")
    }

    # Check extent match
    if (!all(terra::ext(x) == terra::ext(template))) {
      stop("Input and template must have same extent")
    }

    # Check CRS match
    if (!terra::same.crs(x, template)) {
      stop("Input and template must have same CRS")
    }
  }

  invisible(TRUE)
}

#' Validate SpatVector input
#' @param x SpatVector to validate
#' @param value_col Character name of value column
#' @param template SpatRaster template for rasterization
#' @param allow_null Logical; if TRUE, value_col can be NULL
#' @return Invisible TRUE if valid, error otherwise
#' @keywords internal
.chck_vector_input <- function(x, value_col = NULL, template = NULL, allow_null = FALSE) {
  # Check input type
  if (!inherits(x, "SpatVector")) {
    stop("Input must be a SpatVector object")
  }

  # Validate value column if provided
  if (!is.null(value_col)) {
    if (!value_col %in% names(x)) {
      stop(sprintf("Column '%s' not found in input vector", value_col))
    }

    # Check column data
    values <- x[[value_col]]
    if (!is.numeric(values)) {
      stop(sprintf("Column '%s' must contain numeric values", value_col))
    }
  } else if (!allow_null) {
    stop("value_col must be provided")
  }

  # Template validation
  if (!is.null(template)) {
    if (!inherits(template, "SpatRaster")) {
      stop("Template must be a SpatRaster object")
    }

    # Check CRS compatibility
    if (!terra::same.crs(x, template)) {
      stop("Input and template must have same CRS")
    }

    # Check that vector intersects template extent
    if (!terra::intersect(terra::ext(x), terra::ext(template))) {
      stop("Input vector must intersect template extent")
    }
  }

  invisible(TRUE)
}

#' Validate PMF properties
#' @param x SpatRaster to check PMF properties
#' @param tolerance Numeric tolerance for sum to 1 check
#' @return Invisible TRUE if valid, error otherwise
#' @keywords internal
.chck_pmf_validity <- function(x, tolerance = 1e-10) {
  # Check for negative values
  min_val <- terra::global(x, "min", na.rm = TRUE)$min
  if (min_val < 0) {
    stop("PMF cannot contain negative values")
  }

  # Check sum to 1
  sum_val <- terra::global(x, "sum", na.rm = TRUE)$sum
  if (abs(sum_val - 1) > tolerance) {
    stop(sprintf("PMF must sum to 1 (current sum: %f)", sum_val))
  }

  invisible(TRUE)
}

#' Validate numeric values for likelihood computation
#' @param x Numeric vector to validate
#' @param allow_negative Logical; if TRUE, allow negative values
#' @param method Character string indicating computation method
#' @return Invisible TRUE if valid, error otherwise
#' @keywords internal
.chck_likelihood_values <- function(x, allow_negative = FALSE, method = "mean") {
  # Check for valid numeric values
  if (!is.numeric(x)) {
    stop("Values must be numeric")
  }

  # Check for negative values if not allowed
  if (!allow_negative && any(x < 0, na.rm = TRUE)) {
    stop("Values cannot be negative")
  }

  # Method-specific validation
  if (!method %in% c("mean", "sum", "custom")) {
    stop("Invalid method specified")
  }

  # Check for zero/NA issues
  if (method == "mean" && all(is.na(x))) {
    stop("All values are NA")
  }
  if (method == "sum" && sum(x, na.rm = TRUE) == 0) {
    stop("Sum of values is zero")
  }

  invisible(TRUE)
}
