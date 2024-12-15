#' Compute Spatial Choice Probabilities
#'
#' @description
#' Converts spatial weights to choice probabilities. When used with attractiveness values,
#' implements Huff model logic. Without attractiveness, behaves like standard weight
#' normalization.
#'
#' @param weights Multi-layer SpatRaster of spatial weights (e.g., from compute_weights)
#' @param attractiveness Optional numeric vector of facility attractiveness values
#' @param alpha Numeric parameter for attractiveness sensitivity (default = 1)
#' @param a0 Non-negative numeric value representing outside option weight (default = 0)
#' @return SpatRaster stack of choice probabilities (layers sum to 1 at each location)
#' @examples
#' \dontrun{
#' # Calculate decay weights
#' w <- compute_weights(distance, method = "gaussian", sigma = 30)
#'
#' # Basic usage (equivalent to normalize_weights)
#' p1 <- compute_choice(w)
#'
#' # With Huff model attractiveness
#' attr <- c(100, 150, 80)  # for 3 facilities
#' p2 <- compute_choice(w, attractiveness = attr, alpha = 1.5)
#'
#' # With outside option
#' p3 <- compute_choice(w, attractiveness = attr, a0 = 0.1)
#' }
#' @export
compute_choice <- function(weights, attractiveness = NULL, alpha = 1, a0 = 0) {
  # Input validation
  if (!inherits(weights, "SpatRaster")) {
    stop("weights must be a SpatRaster object")
  }
  if (!is.null(attractiveness)) {
    if (!is.numeric(attractiveness) || any(attractiveness < 0, na.rm = TRUE)) {
      stop("attractiveness must be a non-negative numeric vector")
    }
    if (length(attractiveness) != nlyr(weights)) {
      stop("Length of attractiveness must match number of weight layers")
    }
    # Scale weights by attractiveness
    weights <- weights * (attractiveness^alpha)
  }

  # Normalize to get probabilities (sum to 1 at each location)
  probs <- normalize_weights(weights, method = "standard", a0 = a0)

  # Add names if not present
  if (is.null(names(probs))) {
    names(probs) <- paste0("facility_", seq_len(nlyr(probs)))
  }

  return(probs)
}
