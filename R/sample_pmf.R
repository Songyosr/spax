#' Sample Points from Probability Surface with Multiple Realizations (Allowing Double Counting)
#'
#' @description
#' Efficiently generates multiple realizations of point patterns from a probability
#' mass function (PMF) surface using a vectorized approach. Allows multiple samples per cell.
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
#' @parem replace_0 Logical. If TRUE, replaces zero values with NA before sampling (default TRUE)
#' @param snap Logical. If TRUE, enters fast mode with minimal validation (default FALSE)
#' @return SpatRaster with one layer per realization, containing counts of samples per cell
#' @export
sample_pmf <- function(x, n = NULL, size = NULL, prob = NULL,
                       method = "poisson", ..., iterations = 1,
                       seed = NULL, replace_0 = TRUE,
                       snap = FALSE) {
  # Essential input validation (always performed)
  if (!inherits(x, "SpatRaster")) {
    stop("Input 'x' must be a SpatRaster object")
  }

  if (!is.null(seed)) set.seed(seed)

  # Extended validation (skipped if snap = TRUE)
  if (!snap) {
    if (iterations < 1 || !is.numeric(iterations) || iterations != as.integer(iterations)) {
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
    if (!is.numeric(n) || any(n < 0) || any(n != as.integer(n))) {
      stop("n must be a non-negative integer")
    }
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
    if (any(result < 0) || any(result != as.integer(result))) {
      stop("Custom method must return non-negative integer values")
    }
    result
  } else {
    # Built-in methods
    switch(method,
           "poisson" = {
             if (is.null(size)) stop("size must be specified for poisson method")
             rpois(iterations, lambda = size * prob)
           },
           "binomial" = {
             if (is.null(size)) stop("size must be specified for binomial method")
             rbinom(iterations, size = size, prob = prob)
           },
           "nbinom" = {
             if (is.null(size)) stop("size must be specified for nbinom method")
             rnbinom(iterations, size = size, prob = prob)
           }
    )
  }

  # Validate sample sizes
  if (any(n_samples < 0)) {
    stop("Sample sizes must be non-negative")
  }

  # Create empty multi-layer template for results
  result <- rast(x, nlyrs = iterations)
  # Initialize all values to zero
  values(result) <- 0

  # Total number of samples across all iterations
  total_n <- sum(n_samples)

  if (total_n > 0) {
    # Sample all points at once with replacement
    sampled_cells <- spatSample(x, size = total_n,
                                method = "weights",
                                cells = TRUE,
                                replace = TRUE)$cell

    # Generate layer indices corresponding to each sample
    layer_indices <- rep(1:iterations, times = n_samples)

    # Compute linear indices for (cell, layer) pairs
    ncells <- ncell(x)
    linear_index <- sampled_cells + (layer_indices - 1) * ncells

    # Tabulate counts for each (cell, layer) pair
    counts <- tabulate(linear_index, nbins = ncells * iterations)

    # Reshape counts into a matrix with ncells rows and iterations columns
    dim(counts) <- c(ncells, iterations)

    # Replace zero values with NA if requested
    if (replace_0) {
      counts[counts == 0] <- NA
    }

    # Assign counts to the result raster
    values(result) <- counts
  }

  # Assign layer names if not in snap mode
  if (!snap && is.null(names(result))) {
    names(result) <- paste0("sim_", 1:iterations)
  }

  return(result)
}
