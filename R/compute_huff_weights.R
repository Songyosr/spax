#' Compute Huff-like Spatial Interaction Weights
#'
#' @description
#' Computes probability weights based on Huff's spatial interaction model principles.
#' Combines distance decay with facility attractiveness and optional outside options.
#'
#' @param distance SpatRaster of distances/travel times to facilities
#' @param method Character string specifying the decay function:
#'        "gaussian", "exponential", "power", "inverse", "binary", or function(distance, ...)
#' @param sigma Parameter controlling rate of distance decay
#' @param attractiveness Numeric vector of facility attractiveness values
#' @param alpha Numeric parameter controlling attractiveness sensitivity (default = 1)
#' @param normalize_method Character string specifying normalization method:
#'        "standard", "semi", "reference", or custom function
#' @param a0 Numeric value for outside option weight (default = 0)
#' @param normalize_params List of additional parameters passed to normalize_weights()
#' @param ... Additional parameters passed to compute_weights()
#' @return SpatRaster stack of probability weights for each facility
#' @examples
#' \dontrun{
#' # Basic usage with gaussian decay
#' weights <- compute_huff_weights(
#'   distance = distance_raster,
#'   method = "gaussian",
#'   sigma = 30,
#'   attractiveness = facility_sizes
#' )
#'
#' # With outside option and semi-normalization
#' weights <- compute_huff_weights(
#'   distance = distance_raster,
#'   method = "gaussian",
#'   sigma = 30,
#'   attractiveness = facility_sizes,
#'   normalize_method = "semi",
#'   a0 = 0.2
#' )
#'
#' # With custom normalization parameters
#' weights <- compute_huff_weights(
#'   distance = distance_raster,
#'   method = "gaussian",
#'   sigma = 30,
#'   attractiveness = facility_sizes,
#'   normalize_method = "reference",
#'   normalize_params = list(ref_value = 0.5)
#' )
#' }
#' @export
compute_huff_weights <- function(distance,
                                 method = "gaussian",
                                 sigma = NULL,
                                 attractiveness = NULL,
                                 alpha = 1,
                                 normalize_method = "standard",
                                 a0 = 0,
                                 normalize_params = list(),
                                 ...) {
  # Input validation
  if (!inherits(distance, "SpatRaster")) {
    stop("distance must be a SpatRaster object")
  }

  if (is.null(attractiveness)) {
    attractiveness <- rep(1, nlyr(distance))
    warning("No attractiveness values provided. Using equal attractiveness for all facilities.")
  }

  # Compute initial weights with attractiveness scaling
  weights <- compute_weights(
    distance = distance,
    method = method,
    sigma = sigma,
    attractiveness = attractiveness,
    alpha = alpha,
    ...
  )

  # Combine normalization parameters
  norm_args <- c(
    list(
      x = weights,
      method = normalize_method,
      a0 = a0
    ),
    normalize_params
  )

  # Normalize to probabilities
  prob_weights <- do.call(normalize_weights, norm_args)

  # Set names if not present
  if (is.null(names(prob_weights))) {
    names(prob_weights) <- paste0("facility_", seq_len(nlyr(prob_weights)))
  }

  return(prob_weights)
}
