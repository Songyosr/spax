#' Internal function for core choice validation
#' @noRd
.chck_calc_choice <- function(weights, attractiveness = NULL, alpha = 1, a0 = 0) {
  # Check weights is a SpatRaster of non-negative values
  .chck_class(weights, "SpatRaster", "weights")
  .chck_nonnegative_weights(weights, "weights")

  # Check attractiveness if provided
  if (!is.null(attractiveness)) {
    .chck_is_numeric(attractiveness, "attractiveness")
    .chck_positive(attractiveness, allow_zero = TRUE, "attractiveness")
    .chck_length(length(attractiveness), nlyr(weights), "attractiveness")
  }

  # Check alpha
  .chck_is_numeric(alpha, "alpha")
  .chck_length(length(alpha), 1, "alpha")

  # Check a0
  .chck_is_numeric(a0, "a0")
  .chck_length(length(a0), 1, "a0")
  .chck_positive(a0, allow_zero = TRUE, "a0")

  invisible(TRUE)
}

#' Internal function for core choice computation
#' @noRd
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
#' @param weights Multi-layer SpatRaster of spatial weights (e.g., from [calc_decay()])
#' @param attractiveness Optional numeric vector of facility attractiveness values
#' @param alpha Numeric parameter for attractiveness sensitivity (default = 1)
#' @param a0 Non-negative numeric value representing outside option weight (default = 0)
#' @param snap Logical; if `TRUE`, a fast computation mode, only for inputs known
#'   to be valid, with:
#'   - Minimal validation
#'   - Essential preprocessing only
#' @return SpatRaster stack of choice probabilities (layers sum to 1 at each location)
#' @examples
#' library(terra)
#' # load data
#' distance_raster <- read_spax_example("cornwall_gp_travel.tif")
#'
#' # Calculate decay weights using gaussian decay (minutes)
#' weights <- calc_decay(distance_raster, method = "gaussian", sigma = 10)
#'
#' # Basic usage (equivalent to calc_normalize)
#' p1 <- calc_choice(weights)
#'
#' # With Huff model using GP full-time equivalents as attractiveness
#' p2 <- calc_choice(weights,
#'   attractiveness = cornwall_gp$gp_fte_extg, # GPs excluding trainees
#'   alpha = 1.5
#' )
#'
#' # Plot to compare, two panels side by side
#' old_par <- par(mfrow = c(1, 2))
#' plot(p1[[1]], main = "Basic Choice Prob.")
#' plot(p2[[1]], main = "GP-weighted Choice Prob.")
#' par(old_par)
#'
#' # With outside option (a0)
#' p3 <- calc_choice(weights,
#'   attractiveness = cornwall_gp$gp_fte_extg,
#'   a0 = 0.1
#' )
#' @export
#' @md
calc_choice <- function(weights, attractiveness = NULL,
                        alpha = 1, a0 = 0, snap = FALSE) {
  # Fast path for internal use
  if (snap) {
    return(.help_calc_choice_core(weights, attractiveness, alpha, a0))
  }

  # Regular path with validation
  .chck_calc_choice(weights, attractiveness, alpha, a0)

  # Use same core computation
  .help_calc_choice_core(weights, attractiveness, alpha, a0)
}
