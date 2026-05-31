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
#'        - Vector: Must match number of weight layers
#'        - Matrix: Rows match weight layers, columns are different measures
#'        - Data.frame: Same as matrix but requires ratio_cols parameter
#' @param weights Multi-layer SpatRaster where each layer represents one site's
#'        probability-based catchment area
#' @param ratio_cols Character vector of column names if ratios is a data.frame
#' @param full_output Logical; whether to return intermediate calculations (default FALSE)
#' @param plot_prefix Character string to prepend to output names (default NULL)
#' @return If full_output = TRUE, returns list containing:
#'         - access_scores: SpatRaster stack of accessibility scores per measure
#'         - site_specific: List of site-specific accessibility SpatRasters
#'         If full_output = FALSE, returns only access_scores SpatRaster
#' @export
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
