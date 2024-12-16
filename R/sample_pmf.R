#' Sample Points from Probability Surface with Multiple Realizations
#'
#' @description
#' Efficiently generates multiple realizations of point patterns from a probability
#' mass function (PMF) surface using a vectorized approach. Handles both deterministic
#' and probabilistic sample sizes.
#'
#' @param x SpatRaster representing spatial distribution (PMF or raw density values)
#' @param n Integer or NULL. Fixed number of samples for deterministic sampling.
#'        If specified, overrides probabilistic sampling parameters.
#' @param size Integer or NULL. Population size parameter for probability distributions.
#'        If NULL and input is density, will be calculated from data.
#' @param prob Numeric between 0 and 1. Probability parameter (e.g., disease prevalence)
#' @param method Character string specifying built-in method or custom function:
#'        - "poisson": Random size with mean = size * prob
#'        - "binomial": Random size from binomial(size, prob)
#'        - "nbinom": Random size from negative binomial(size, prob)
#'        - custom function: Must return vector of length iterations
#' @param ... Additional arguments passed to custom sampling function
#' @param iterations Integer. Number of realizations to generate
#' @param seed Integer for random seed (optional)
#' @param snap Logical. If TRUE, enters fast mode with minimal validation (default FALSE)
#' @return SpatRaster with one layer per realization
#' @export
sample_pmf <- function(x, n = NULL, size = NULL, prob = NULL,
                       method = "poisson", ..., iterations = 1,
                       seed = NULL, snap = FALSE) {
  # Essential input validation (always performed)
  if (!inherits(x, "SpatRaster")) {
    stop("Input 'x' must be a SpatRaster object")
  }

  if (!is.null(seed)) set.seed(seed)

  # Extended validation (skipped if snap = TRUE)
  if (!snap) {
    if (iterations < 1 || !is.numeric(iterations)) {
      stop("iterations must be a positive integer")
    }

    if (is.null(n) && is.character(method) && (is.null(size) || is.null(prob))) {
      stop("Either n or both size and prob must be specified for built-in methods")
    }

    if (!is.null(prob) && (prob < 0 || prob > 1)) {
      stop("prob must be between 0 and 1")
    }

    if (is.character(method) && !method %in% c("poisson", "binomial", "nbinom")) {
      stop("Unknown method string. Use 'poisson', 'binomial', 'nbinom', or provide a function")
    }
  }

  # Check if input is PMF, convert if needed
  sum_check <- global(x, "sum", na.rm = TRUE)$sum
  if (abs(sum_check - 1) > 1e-10) {
    pmf_result <- compute_pmf(x, return_total = TRUE)
    x <- pmf_result$pmf
    if (is.null(size)) {
      size <- pmf_result$total
      if (!snap) message("Using total population ", round(size), " as size parameter")
    }
  }

  # Determine sample sizes for all iterations
  n_samples <- if (!is.null(n)) {
    rep(n, iterations)
  } else if (is.function(method)) {
    # Call custom sampling function with provided arguments
    result <- do.call(method, list(...))
    if (!is.numeric(result)) {
      stop("Custom method must return numeric values")
    }
    if (length(result) != iterations) {
      stop(sprintf("Custom method must return %d values (one per iteration)", iterations))
    }
    result
  } else {
    # Built-in methods
    switch(method,
           "poisson" = rpois(iterations, lambda = size * prob),
           "binomial" = rbinom(iterations, size = size, prob = prob),
           "nbinom" = rnbinom(iterations, size = size, prob = prob)
    )
  }

  # Create empty multi-layer template
  result <- rast(x, nlyrs = iterations)
  values(result) <- 0

  # Do one large sampling for all iterations
  total_n <- sum(n_samples)
  if (total_n > 0) {
    # Sample cells
    cells <- spatSample(x, size = total_n,
                        method = "weights",
                        cells = TRUE)$cell

    # Create layer indices based on sample sizes
    layer_indices <- rep(1:iterations, times = n_samples)

    # Fill values
    idx <- cbind(cells, layer_indices)
    values(result)[idx] <- 1
  }

  # Add names if not in snap mode
  if (!snap && is.null(names(result))) {
    names(result) <- paste0("sim_", 1:iterations)
  }

  return(result)
}
