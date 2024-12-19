#' Internal function for core choice computation
#' @keywords internal
.calc_choice_core <- function(weights, attractiveness, alpha, a0, snap) {
  if (!is.null(attractiveness)) {
    weights <- weights * (attractiveness^alpha)
  }
  return(calc_normalize(weights, method = "standard", a0 = a0, snap = snap))
}

#' Compute Spatial Choice Probabilities
#'
#' @description
#' Converts spatial weights to choice probabilities. When used with attractiveness values,
#' implements Huff model logic. Without attractiveness, behaves like standard weight
#' normalization.
#'
#' @param weights Multi-layer SpatRaster of spatial weights (e.g., from calc_decay)
#' @param attractiveness Optional numeric vector of facility attractiveness values
#' @param alpha Numeric parameter for attractiveness sensitivity (default = 1)
#' @param a0 Non-negative numeric value representing outside option weight (default = 0)
#' @param snap Logical; if TRUE, avoid input validation. PS. snap is a performance optimization for internal use
#' @return SpatRaster stack of choice probabilities (layers sum to 1 at each location)
#' @examples
#' \dontrun{
#' # Calculate decay weights
#' w <- calc_decay(distance, method = "gaussian", sigma = 30)
#'
#' # Basic usage (equivalent to calc_normalize)
#' p1 <- calc_choice(w)
#'
#' # With Huff model attractiveness
#' attr <- c(100, 150, 80)  # for 3 facilities
#' p2 <- calc_choice(w, attractiveness = attr, alpha = 1.5)
#'
#' # With outside option
#' p3 <- calc_choice(w, attractiveness = attr, a0 = 0.1)
#' }
#' @export
calc_choice <- function(weights, attractiveness = NULL, alpha = 1, a0 = 0, snap = FALSE) {
  # Fast path for internal use
  if (snap) {
    return(.calc_choice_core(weights, attractiveness, alpha, a0, snap))
  }

  # Regular path with validation
  if (!inherits(weights, "SpatRaster")) {
    stop("weights must be a SpatRaster object")
  }

  # Validate attractiveness if provided
  if (!is.null(attractiveness)) {
    if (!is.numeric(attractiveness) || any(attractiveness < 0, na.rm = TRUE)) {
      stop("attractiveness must be a non-negative numeric vector")
    }
    if (length(attractiveness) != nlyr(weights)) {
      stop("Length of attractiveness must match number of weight layers")
    }
  }

  # Use same core computation
  probs <- .calc_choice_core(weights, attractiveness, alpha, a0)

  # Add names for user-facing results
  if (is.null(names(probs))) {
    names(probs) <- paste0("facility_", seq_len(nlyr(probs)))
  }

  return(probs)
}
