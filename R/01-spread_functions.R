# Canonical spread implementation lives in R/01-spread_weighted.R
# (spread_weighted(), .spread_core(), .chck_spread_weighted()).
#
# This file previously held a second, shadowed copy of spread_weighted()/.spread_core()
# that won/lost only by source-load order; it also pulled in an undeclared `rlist`
# dependency and a `.spread_core` default that diverged from the canonical one.
# Those duplicates were removed in SPAX-001A; only spread_access() remains here.

#' Calculate spatial accessibility scores
#'
#' @description
#' Specialized version of [spread_weighted()] for accessibility analysis.
#' Distributes supply-to-demand ratios across space using probability-based
#' catchment areas to calculate accessibility scores.
#'
#' @param ratios Numeric vector, matrix, or data.frame of supply-to-demand ratios:
#'   * vector: must match the number of weight layers
#'   * matrix: rows match weight layers, columns are different measures
#'   * data.frame: as a matrix, with the columns named in `ratio_cols`
#' @param weights Multi-layer SpatRaster where each layer represents one site's
#'   probability-based catchment area
#' @param ratio_cols Character vector of column names if ratios is a data.frame
#' @param full_output Logical; whether to return intermediate calculations (default FALSE)
#' @param plot_prefix Character string to prepend to output names (default NULL)
#' @return If `full_output = TRUE`, a list with
#'   * `access_scores`: SpatRaster stack of accessibility scores per measure
#'   * `site_specific`: list of site-specific accessibility SpatRasters
#'
#'   If `full_output = FALSE`, the `access_scores` SpatRaster only.
#' @examples
#' library(terra)
#'
#' # Cornwall population grid, and Gaussian weights of the travel time to each
#' # GP practice
#' pop <- read_spax_example("cornwall_pop.tif")
#' travel <- read_spax_example("cornwall_gp_travel.tif")
#' weights <- calc_decay(travel, method = "gaussian", sigma = 10)
#'
#' # Step 1 of E2SFCA: the population each practice reaches, and its GP FTE per
#' # person reached, for GPs excluding GPs in training and for all GPs
#' gp <- sf::st_drop_geometry(cornwall_gp)
#' gp <- gp[match(names(weights), gp$practice), ]
#' reached <- gather_weighted(pop, weights, simplify = TRUE)
#' ratios <- data.frame(extg = gp$gp_fte_extg / reached, all = gp$gp_fte / reached)
#'
#' # Step 2: spread the ratios back over the grid with the same weights
#' access <- spread_access(ratios, weights, ratio_cols = c("extg", "all"),
#'                         plot_prefix = "gp_fte_")
#' access
#' plot(access)
#'
#' # With full_output, the score that each practice adds to each cell as well
#' detail <- spread_access(ratios$extg, weights, full_output = TRUE)
#' names(detail)
#' @export
#' @md
spread_access <- function(ratios, weights, ratio_cols = NULL,
                          full_output = FALSE, plot_prefix = NULL) {
  result <- spread_weighted(
    values = ratios,
    weights = weights,
    value_cols = ratio_cols,
    full_output = full_output
  )

  if (full_output) {
    names(result$total_distribution) <- paste0(plot_prefix, names(result$total_distribution))
    return(list(
      access_scores = result$total_distribution,
      site_specific = result$unit_distribution
    ))
  } else {
    names(result) <- paste0(plot_prefix, names(result))
    return(result)
  }
}
