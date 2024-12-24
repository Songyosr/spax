#' Update Probability Mass Function with Likelihood Surface
#'
#' @description
#' Updates a spatial probability mass function (PMF) using a likelihood surface
#' following Bayesian principles. The function combines:
#' - Prior: Initial spatial PMF representing prior beliefs
#' - Likelihood: Surface showing relative intensity of observations
#' - Posterior: Updated PMF incorporating both prior and likelihood
#'
#' Values in the posterior will be higher where both prior probability
#' and likelihood are high, representing areas supported by both prior
#' beliefs and observed data.
#'
#' @param prior SpatRaster containing prior PMF
#' @param likelihood SpatRaster containing likelihood surface
#' @param normalize Logical; if TRUE ensure output sums to 1 (default TRUE)
#' @param density Logical; if TRUE return density surface instead of PMF
#' @param snap Logical; if TRUE skip validation
#' @return SpatRaster containing either:
#'         - Posterior PMF if density = FALSE
#'         - Posterior density if density = TRUE
#'
#' @examples
#' \dontrun{
#' # Create prior from population
#' prior <- transform_pmf(pop)
#'
#' # Create likelihood from cases
#' likelihood <- transform_likelihood(
#'   case_spatial,
#'   value_col = "cases",
#'   template = pop
#' )
#'
#' # Update prior with likelihood
#' posterior <- update_pmf(prior, likelihood)
#'
#' # Get density surface
#' density <- update_pmf(prior, likelihood, density = TRUE)
#' }
#' @export
update_pmf <- function(prior, likelihood, normalize = TRUE,
                       density = FALSE, snap = FALSE) {
  # Input validation
  if (!snap) {
    .chck_raster_input(prior)
    .chck_raster_input(likelihood, prior) # Use prior as template
    .chck_pmf_validity(prior)
  }

  # Calculate unnormalized posterior
  posterior <- prior * likelihood

  # Get total before normalization (needed for density)
  total <- terra::global(posterior, "sum", na.rm = TRUE)[[1]]

  # Normalize if requested
  if (normalize) {
    posterior <- transform_pmf(posterior, snap = TRUE)
  }

  # Convert to density if requested
  if (density) {
    posterior <- posterior * total
  }

  return(posterior)
}

#' Convenience wrapper for Bayesian spatial updating
#'
#' @description
#' Convenience function that handles the common case of updating a spatial
#' distribution with vector-based observations. Combines the functionality
#' of transform_pmf, transform_likelihood, and update_pmf into a single
#' workflow.
#'
#' @param x SpatVector containing observations
#' @param value_col Character name of column containing values
#' @param prior Optional SpatRaster prior PMF (if NULL, uses uniform prior)
#' @param template SpatRaster template (required if prior = NULL)
#' @param density Logical; if TRUE return density surface
#' @param standardize Logical; if TRUE standardize likelihood values
#' @return SpatRaster containing posterior PMF or density surface
#'
#' @examples
#' \dontrun{
#' # Basic usage with uniform prior
#' result1 <- rasterize_demand(
#'   case_spatial,
#'   value_col = "cases",
#'   template = pop
#' )
#'
#' # With population-based prior
#' prior <- transform_pmf(pop)
#' result2 <- rasterize_demand(
#'   case_spatial,
#'   value_col = "cases",
#'   prior = prior,
#'   density = TRUE
#' )
#' }
#' @export
rasterize_demand <- function(x, value_col, prior = NULL, template = NULL,
                             density = FALSE, standardize = FALSE) {
  # Input validation
  .chck_vector_input(x, value_col)
  if (is.null(prior) && is.null(template)) {
    stop("Either prior or template must be provided")
  }

  # Create prior if not provided
  if (is.null(prior)) {
    prior <- transform_pmf(x, value_col, template)
  } else {
    .chck_raster_input(prior)
    .chck_pmf_validity(prior)
    template <- prior
  }

  # Create likelihood surface
  likelihood <- transform_likelihood(
    x,
    value_col = value_col,
    template = template,
    standardize = standardize
  )

  # Update prior with likelihood
  posterior <- update_pmf(
    prior = prior,
    likelihood = likelihood,
    density = density
  )

  return(posterior)
}
