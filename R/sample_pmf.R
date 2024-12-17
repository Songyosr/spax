#' Sample Points from Probability Surface with Multiple Realizations
#'
#' @description
#' Generates multiple realizations of point patterns from a probability mass function (PMF)
#' surface using a vectorized approach. The function supports both deterministic sampling
#' with fixed sample sizes and probabilistic sampling based on population parameters.
#'
#' @param x SpatRaster representing spatial distribution. Can be either:
#'   - A PMF where values sum to 1
#'   - Raw density values that will be converted to PMF (except in snap mode)
#' @param n Integer or NULL. Fixed number of samples for deterministic sampling.
#'        If specified, overrides probabilistic sampling parameters.
#' @param size Integer or NULL. Population size parameter for probability distributions.
#'        If NULL and input is density (not PMF), will be calculated from data.
#' @param prob Numeric between 0 and 1. Probability parameter for sampling methods
#'        (e.g., disease prevalence, detection probability).
#' @param method Character string or function specifying sampling method:
#'   - "poisson": Random size with mean = size * prob
#'   - "binomial": Random size from binomial(size, prob)
#'   - "nbinom": Random size from negative binomial(size, prob)
#'   - custom function: Must return vector of length 'iterations'
#' @param iterations Integer. Number of realizations to generate (default = 1)
#' @param seed Integer or NULL. Random seed for reproducibility
#' @param replace_0 Logical. If TRUE, replaces zero values with NA (default = TRUE)
#' @param snap Logical. If TRUE, enters fast mode with:
#'   - Minimal input validation
#'   - Requires input to already be PMF
#'   - Skips automatic PMF conversion
#'   - No layer naming
#'   Use for performance in iteration-heavy applications.
#' @param ... Additional arguments passed to custom sampling function
#'
#' @return SpatRaster with one layer per realization, containing counts of samples per cell.
#'         If replace_0 = TRUE, cells with zero counts contain NA.
#'
#' @details
#' The function supports two main sampling approaches:
#' 1. Deterministic: Specify exact number of samples via 'n'
#' 2. Probabilistic: Generate random sample sizes using built-in or custom distributions
#'
#' In snap mode, the function assumes inputs are valid and skips certain checks for
#' performance. The input must already be a proper PMF in this mode.
#'
#' @examples
#' \dontrun{
#' # Create sample population density
#' r <- terra::rast(nrows=10, ncols=10)
#' terra::values(r) <- runif(100) * 100
#'
#' # Example 1: Fixed sample size
#' samples1 <- sample_pmf(r, n = 50, iterations = 5)
#'
#' # Example 2: Disease case simulation
#' samples2 <- sample_pmf(r,
#'                       size = 10000,     # population size
#'                       prob = 0.001,     # disease prevalence
#'                       method = "poisson",
#'                       iterations = 100)  # Monte Carlo iterations
#'
#' # Example 3: Using snap mode with pre-computed PMF
#' pmf <- compute_pmf(r)
#' samples3 <- sample_pmf(pmf,
#'                       n = 1000,
#'                       iterations = 50,
#'                       snap = TRUE)
#' }
#'
#' @seealso
#' \code{\link{compute_pmf}} for converting density to PMF
#'
#' @export
sample_pmf <- function(x, n = NULL, size = NULL, prob = NULL,
                       method = "poisson", iterations = 1,
                       seed = NULL, replace_0 = TRUE,
                       snap = FALSE, ...) {

  # Input validation
  .validate_sample_pmf_inputs(x, n, size, prob, method, iterations, snap)

  # PMF checking and size determination
  if (!snap) {
    pmf_result <- .check_and_convert_pmf(x)
    x <- pmf_result$pmf
    if (is.null(size)) {
      size <- pmf_result$total
      message("Using total population ", round(size), " as size parameter")
    }
  } else {
    # Quick check in snap mode
    sum_check <- global(x, "sum", na.rm = TRUE)$sum
    if (abs(sum_check - 1) > 1e-10) {
      message("Input must be PMF in snap mode")
    }
  }

  # Generate samples with seed handling
  withr::with_seed(
    seed = seed,
    code = {
      # Compute sample sizes
      n_samples <- .compute_sample_sizes(n = n, size = size, prob = prob,
                                         method = method, iterations = iterations, ...)

      # Generate spatial samples
      result <- .generate_spatial_samples(x = x, n_samples = n_samples,
                                          iterations = iterations,
                                          replace_0 = replace_0,
                                          snap = snap)
    }
  )

  return(result)
}
#' Validate inputs for sample_pmf
#'
#' @param x SpatRaster input
#' @param n Sample size
#' @param size Population size
#' @param prob Probability parameter
#' @param method Sampling method
#' @param iterations Number of iterations
#' @param snap Logical for snap mode
#' @keywords internal
.validate_sample_pmf_inputs <- function(x, n, size, prob, method, iterations, snap) {
  # Essential validation (always run)
  if (!inherits(x, "SpatRaster")) {
    stop("Input 'x' must be a SpatRaster object")
  }

  # Extended validation (skip if snap=TRUE)
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
}

#' Check and convert input to PMF
#'
#' @param x SpatRaster input
#' @return List with PMF raster and total population
#' @keywords internal
.check_and_convert_pmf <- function(x) {
  # Check if input is PMF
  sum_check <- global(x, "sum", na.rm = TRUE)$sum

  if (abs(sum_check - 1) > 1e-10) {
    # Not a PMF, need to convert
    pmf_result <- compute_pmf(x, return_total = TRUE)
    return(pmf_result)  # Returns list with $pmf and $total
  } else {
    # Already a PMF, return as is with NULL total
    return(list(
      pmf = x,
      total = NULL
    ))
  }
}

#' Compute sample sizes for each iteration
#'
#' @param n Fixed sample size
#' @param size Population size
#' @param prob Probability parameter
#' @param method Sampling method
#' @param iterations Number of iterations
#' @param ... Additional arguments for custom methods
#' @return Vector of sample sizes
#' @keywords internal
.compute_sample_sizes <- function(n = NULL, size = NULL, prob = NULL,
                                  method = "poisson", iterations = 1, ...) {
  if (!is.null(n)) {
    if (!is.numeric(n) || any(n < 0) || any(n != as.integer(n))) {
      stop("n must be a non-negative integer")
    }
    return(rep(n, iterations))
  }

  if (is.function(method)) {
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
    return(result)
  }

  # Built-in methods
  switch(method,
         "poisson" = rpois(iterations, lambda = size * prob),
         "binomial" = rbinom(iterations, size = size, prob = prob),
         "nbinom" = rnbinom(iterations, size = size, prob = prob))
}

#' Generate spatial samples
#'
#' @param x PMF raster
#' @param n_samples Vector of sample sizes
#' @param iterations Number of iterations
#' @param replace_0 Logical for NA replacement
#' @param snap Logical for snap mode
#' @return Multi-layer SpatRaster of samples
#' @keywords internal
.generate_spatial_samples <- function(x, n_samples, iterations, replace_0 = TRUE, snap = FALSE) {
  # Create empty multi-layer template for results
  result <- rast(x, nlyrs = iterations)
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

#' Estimate new sample
#' @keywords internal
est_new_samples <- function(input_vector, evolve_prop) {
  # Round retained samples from previous iteration
  retained <- round(c(0, head(input_vector, -1)) * (1 - evolve_prop))

  # New samples needed = target - retained
  new_n <- pmax(input_vector - retained, 0)

  # Old samples to keep = target - new samples
  old_n <- input_vector - new_n

  list(new_n = new_n, old_n = old_n)
}

#' Generate evolving spatial samples across iterations
#' @keywords internal
.generate_evolve_samples <- function(x, n_samples, iterations,
                                     evolve_prop = 0.1,
                                     replace_0 = TRUE, snap = FALSE) {
  # Create empty multi-layer template for results
  result <- rast(x, nlyrs = iterations)
  values(result) <- 0

  # Early return if no samples needed
  if (sum(n_samples) == 0) {
    if (!snap && is.null(names(result))) {
      names(result) <- paste0("sim_", 1:iterations)
    }
    return(result)
  }

  # Calculate new and retained sample counts
  sample_est <- est_new_samples(n_samples, evolve_prop)
  new_n <- sample_est$new_n
  old_n <- sample_est$old_n

  total_new_n <- sum(new_n)
  total_n <- sum(n_samples)

  # Initialize storage for all cell indices
  all_cells <- rep(NA_real_, total_n)

  # Sample new points according to weights
  if (total_new_n > 0) {
    sampled_cells <- spatSample(x, size = total_new_n,
                                method = "weights",
                                cells = TRUE,
                                replace = TRUE)$cell
    # Assign new samples
    all_cells[1:total_new_n] <- sampled_cells
  }

  # 1. Indices for new samples
  layer_indices_1 <- rep(1:iterations, times = new_n)

  # 2. Indices for old (retained) samples
  layer_indices_2 <- rep(1:iterations, times = old_n)

  # Combine the two sets of indices
  layer_indices <- c(layer_indices_1, layer_indices_2)
  # Handle sample retention for iterations > 1
  if (iterations > 1) {
    for (i in 2:iterations) {
      # Identify unfilled positions for current iteration
      idx_current <- which(layer_indices == i & is.na(all_cells))
      if (length(idx_current) > 0) {
        # Get samples from previous iteration
        idx_previous <- which(layer_indices == (i - 1))
        retained_cells <- sample(all_cells[idx_previous],
                                 size = length(idx_current),
                                 replace = FALSE)
        all_cells[idx_current] <- retained_cells
      }
    }
  }

  # Compute linear indices and counts in one pass
  ncells <- ncell(x)
  linear_index <- all_cells + (layer_indices - 1) * ncells
  counts <- tabulate(linear_index, nbins = ncells * iterations)
  dim(counts) <- c(ncells, iterations)

  # Replace zeros with NA if requested
  if (replace_0) {
    counts[counts == 0] <- NA
  }

  # Assign counts to result raster
  values(result) <- counts

  # Assign layer names if not in snap mode
  if (!snap && is.null(names(result))) {
    names(result) <- paste0("sim_", 1:iterations)
  }

  return(result)
}
