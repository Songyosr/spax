#' Core computation for likelihood transformation
#' @keywords internal
.help_transform_likelihood_core <- function(x, method = "mean", ref_value = NULL) {
  if (method == "custom" && !is.null(ref_value)) {
    return(x / ref_value)
  }

  # Calculate reference value
  ref <- switch(method,
                "mean" = terra::global(x, "mean", na.rm = TRUE)[[1]],
                "sum" = terra::global(x, "sum", na.rm = TRUE)[[1]],
                stop("Invalid method specified")
  )

  return(x / ref)
}

#' Transform Input to Likelihood Surface
#'
#' @description
#' Creates a likelihood surface representing the relative intensity of values
#' compared to what would be expected under a reference distribution. This is
#' useful for Bayesian updating of spatial probability distributions.
#'
#' The likelihood surface shows where values are higher or lower than expected:
#' - Values > 1 indicate higher than expected
#' - Values = 1 indicate as expected
#' - Values < 1 indicate lower than expected
#'
#' @param x SpatRaster or SpatVector containing observed values
#' @param value_col Character; name of column if x is SpatVector
#' @param template SpatRaster; template for rasterization if x is SpatVector
#' @param method Character; method to compute reference value:
#'        - "mean": Compare to mean value (default)
#'        - "sum": Compare to sum
#'        - "custom": Compare to provided ref_value
#' @param ref_value Numeric; custom reference value if method = "custom"
#' @param standardize Logical; if TRUE return z-scores instead of ratios
#' @param snap Logical; if TRUE skip validation
#' @return SpatRaster of likelihood values
#'
#' @examples
#' \dontrun{
#' # From raster input
#' pop <- rast(u5pd)
#' lik1 <- transform_likelihood(pop)
#'
#' # From vector input
#' lik2 <- transform_likelihood(
#'   case_spatial,
#'   value_col = "cases",
#'   template = pop
#' )
#'
#' # With custom reference
#' lik3 <- transform_likelihood(
#'   pop,
#'   method = "custom",
#'   ref_value = 100
#' )
#' }
#' @export
transform_likelihood <- function(x, value_col = NULL, template = NULL,
                                 method = "mean", ref_value = NULL,
                                 standardize = FALSE, snap = FALSE) {
  # Input validation
  if (!snap) {
    if (inherits(x, "SpatRaster")) {
      .chck_raster_input(x, template)
    } else if (inherits(x, "SpatVector")) {
      .chck_vector_input(x, value_col, template)
    } else {
      stop("Input must be either SpatRaster or SpatVector")
    }

    # Method validation
    if (!method %in% c("mean", "sum", "custom")) {
      stop("Invalid method specified")
    }
    if (method == "custom" && is.null(ref_value)) {
      stop("ref_value must be provided when method = 'custom'")
    }
  }

  # Convert vector to raster if needed
  if (inherits(x, "SpatVector")) {
    x <- terra::rasterize(x, template, field = value_col)
  }

  # Core computation
  result <- if (standardize) {
    # Compute z-scores
    x_mean <- terra::global(x, "mean", na.rm = TRUE)[[1]]
    x_sd <- terra::global(x, "sd", na.rm = TRUE)[[1]]
    (x - x_mean) / x_sd
  } else {
    .help_transform_likelihood_core(x, method, ref_value)
  }

  return(result)
}
