#' Sample Points from Probability Surface
#'
#' @description
#' A wrapper function around terra::spatSample() that implements a two-step sampling process:
#'
#' 1. Sample Size Determination:
#'    - Deterministic: Specify exact number of samples via n parameter
#'    - Probabilistic: Generate random sample size using probability distributions
#'      and population parameters (size, prob). Useful for Monte Carlo simulations
#'      modeling variability in occurrence rates (e.g., disease cases, species observations)
#'
#' 2. Spatial Sampling:
#'    - Uses terra::spatSample() to generate spatially distributed points
#'    - Sampling probability proportional to PMF values
#'    - Automatically converts density data to PMF if needed
#'
#' @param x SpatRaster representing spatial distribution (PMF or raw density values)
#' @param n Integer or NULL. Fixed number of samples for deterministic sampling.
#'        If specified, overrides probabilistic sampling parameters.
#' @param size Integer or NULL. Population size parameter for probability distributions.
#'        If NULL and input is density, will be calculated automatically from the data.
#' @param prob Numeric between 0 and 1. Probability parameter for sampling methods.
#'        Represents rate or probability of occurrence (e.g., disease prevalence,
#'        detection probability).
#' @param method Character string or function specifying how sample size is determined:
#'        - NULL: Uses fixed n parameter
#'        - "poisson": Random size with mean = size * prob
#'        - "binomial": Random size from binomial(size, prob)
#'        - "nbinom": Random size from negative binomial(size, prob)
#'        - custom function: Must return single numeric value
#' @param method_args List of additional parameters for sampling method
#' @param as_raster Logical. If TRUE, returns a SpatRaster (default)
#' @param as_points Logical. If TRUE, returns a SpatVector of points
#' @param as_df Logical. If TRUE, returns a data.frame
#' @param seed Integer for random seed (optional)
#' @param ... Additional arguments passed to spatSample()
#' @return Depending on output flags:
#'         - SpatRaster if as_raster = TRUE
#'         - SpatVector if as_points = TRUE
#'         - data.frame if as_df = TRUE
#'         - matrix if all are FALSE
#' @examples
#' # Example 1: Deterministic sampling - exactly 100 points
#' samples1 <- sample_pmf(pop_density, n = 100)
#'
#' # Example 2: Disease case simulation using population size
#' # Total population from density data, with disease prevalence
#' disease_cases <- sample_pmf(pop_density,
#'                            prob = 0.001,  # disease prevalence
#'                            method = "binomial")  # size calculated from data
#'
#' # Example 3: Species observation with pre-computed habitat PMF
#' # Note: habitat_pmf is already a probability mass function (sums to 1)
#' observations <- sample_pmf(habitat_pmf,
#'                           size = 1000,    # number of trials
#'                           prob = 0.3,     # success probability
#'                           method = "nbinom")
#' @export
sample_pmf <- function(x,
                       n = NULL,
                       size = NULL,
                       prob = NULL,
                       method = "binomial",
                       method_args = list(),
                       as_raster = TRUE,
                       as_points = FALSE,
                       as_df = FALSE,
                       seed = NULL,
                       ...) {
  # Input validation
  if (!inherits(x, "SpatRaster")) {
    stop("Input 'x' must be a SpatRaster object")
  }

  # Check if input is already a PMF
  sum_check <- global(x, "sum", na.rm = TRUE)$sum
  if (abs(sum_check - 1) > 1e-10) {
    # Convert to PMF and get total population
    pmf_result <- compute_pmf(x, return_total = TRUE)
    x <- pmf_result$pmf
    if (is.null(size)) {
      size <- pmf_result$total_pop
      message("Using total population ", round(size), " as size parameter")
    }
  } else if (is.null(size) && is.null(n)) {
    stop("When input is already a PMF, either n or size parameter must be provided")
  }

  # Set random seed if provided
  if (!is.null(seed)) set.seed(seed)

  # If n is provided, use deterministic sampling
  if (!is.null(n)) {
    if (!is.null(method) && method != "binomial") {
      warning("Fixed n provided - ignoring method and probability parameters")
    }
    sample_size <- n
  } else {
    # Probabilistic sampling
    sample_size <- if (is.character(method)) {
      switch(method,
             "poisson" = {
               if (is.null(prob)) stop("prob parameter required for Poisson")
               lambda <- size * prob
               rpois(1, lambda = lambda)
             },
             "binomial" = {
               if (is.null(prob)) stop("prob parameter required for binomial")
               rbinom(1, size = size, prob = prob)
             },
             "nbinom" = {
               if (is.null(prob)) stop("prob parameter required for negative binomial")
               rnbinom(1, size = size, prob = prob)
             },
             stop("Unknown method string. Use 'poisson', 'binomial', or 'nbinom'")
      )
    } else if (is.function(method)) {
      # Custom function should return a single number
      result <- do.call(method, method_args)
      if (!is.numeric(result) || length(result) != 1) {
        stop("Custom method must return a single numeric value for sample size")
      }
      result
    } else {
      stop("method must be either a string or a function")
    }
  }

  # Validate final sample size
  if (!is.numeric(sample_size) || length(sample_size) != 1 || sample_size <= 0) {
    stop("Computed sample size must be a positive number")
  }

  # Perform spatial sampling with direct output format
  spatSample(x,
             size = sample_size,
             method = "weights",
             as.raster = as_raster,
             as.points = as_points,
             as.df = as_df,
             ...)
}
