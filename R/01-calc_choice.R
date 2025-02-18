#' Internal function for core choice cvalidation
#' @keywords internal
.chck_calc_choice <- function(weights, attractiveness = NULL, alpha = 1, a0 = 0) {
  # Check weights is SpatRaster
  .assert_class(weights, "SpatRaster", "weights")

  # Check attractiveness if provided
  if (!is.null(attractiveness)) {
    .assert_numeric(attractiveness, "attractiveness")
    .assert_positive(attractiveness, allow_zero = TRUE, "attractiveness")
    .assert_length(length(attractiveness), nlyr(weights), "attractiveness")
  }

  # Check alpha
  .assert_numeric(alpha, "alpha")
  .assert_length(length(alpha), 1, "alpha")

  # Check a0
  .assert_numeric(a0, "a0")
  .assert_length(length(a0), 1, "a0")
  .assert_positive(a0, allow_zero = TRUE, "a0")

  invisible(TRUE)
}

#' Internal function for core choice computation
#' @keywords internal
.help_calc_choice_core <- function(weights, attractiveness, alpha, a0) {
  if (!is.null(attractiveness)) {
    weights <- weights * (attractiveness^alpha)
  }

  # Use normalize core for consistent behavior
  return(.normalize_core(weights, method = "standard", a0 = a0))
}

# 3. Main function with proper documentation
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
#' @param snap Logical; if TRUE enables fast computation mode with:
#'        - Minimal validation
#'        - Essential preprocessing only
#'        - No name/attribute assignment
#'        Only use when inputs are known to be valid.
#' @return SpatRaster stack of choice probabilities (layers sum to 1 at each location)
#' @examples
#' library(terra)
#' # load data
#' distance_raster <- read_spax_example("hos_iscr.tif")
#'
#' # Calculate decay weights using gaussian decay
#' weights <- calc_decay(distance_raster, method = "gaussian", sigma = 30)
#'
#' # Basic usage (equivalent to calc_normalize)
#' p1 <- calc_choice(weights)
#'
#' # With Huff model using number of doctors as attractiveness
#' p2 <- calc_choice(weights,
#'   attractiveness = hc12_hos$s_doc, # Number of doctors
#'   alpha = 1.5
#' )
#'
#' # Plot to compare
#' par(mfrow = c(1, 2)) # Set up 2 panels side by side
#' plot(p1[[1]], main = "Basic Choice Prob.")
#' plot(p2[[1]], main = "Doctor-weighted Choice Prob.")
#'
#' # With outside option (a0)
#' p3 <- calc_choice(weights,
#'   attractiveness = hc12_hos$s_doc,
#'   a0 = 0.1
#' )
#' @export
calc_choice <- function(weights, attractiveness = NULL,
                        alpha = 1, a0 = 0, snap = FALSE) {
  # Fast path for internal use
  if (snap) {
    return(.help_calc_choice_core(weights, attractiveness, alpha, a0))
  }

  # Regular path with validation
  .chck_calc_choice(weights, attractiveness, alpha, a0)

  # Use same core computation
  probs <- .help_calc_choice_core(weights, attractiveness, alpha, a0)

  # Add names for user-facing results
  if (is.null(names(probs))) {
    names(probs) <- paste0("facility_", seq_len(nlyr(probs)))
  }

  return(probs)
}
