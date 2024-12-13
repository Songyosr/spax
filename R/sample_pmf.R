#' Sample Points from Probability Surface
#'
#' @description
#' A wrapper function around terra::spatSample() that implements a two-step sampling process
#' with support for multiple iterations:
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
#'    - Supports multiple iterations for Monte Carlo simulation
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
#' @param iterations Integer. Number of samples to generate (default = 1)
#' @param output Character. Output format, one of:
#'        - "raster": Returns a SpatRaster stack with counts (default)
#'        - "points": Returns a list of SpatVectors
#'        - "matrix": Returns a list of coordinate matrices
#'        - "dataframe": Returns a list of data.frames
#' @param names Character vector of names for output layers (optional)
#' @param seed Integer for random seed (optional)
#' @param ... Additional arguments passed to spatSample()
#' @return Depending on output format:
#'         - "raster": Multi-layer SpatRaster with one layer per iteration
#'         - "points": List of SpatVector objects
#'         - "matrix": List of coordinate matrices
#'         - "dataframe": List of data.frames
#' @examples
#' \dontrun{
#' # Create sample data
#' r <- terra::rast(nrows=10, ncols=10)
#' terra::values(r) <- runif(100) * 100  # Random population values
#'
#' # Example 1: Multiple samples with fixed size
#' # Returns a 10-layer SpatRaster of point distributions
#' s1 <- sample_pmf(r, n = 50, iterations = 10)
#'
#' # Example 2: Disease case simulation with multiple scenarios
#' # Using Poisson distribution for case counts
#' s2 <- sample_pmf(r,
#'                  prob = 0.001,     # disease prevalence
#'                  method = "poisson",
#'                  iterations = 100,  # 100 Monte Carlo iterations
#'                  names = paste0("sim_", 1:100))
#'
#' # Example 3: Multiple samples with custom naming
#' # First convert to PMF
#' pmf <- compute_pmf(r)
#' s3 <- sample_pmf(pmf,
#'                  size = 1000,      # population size
#'                  prob = 0.05,      # occurrence probability
#'                  method = "binomial",
#'                  iterations = 50,
#'                  output = "points") # return as list of point vectors
#'
#' # Analyze results (if raster output)
#' if(inherits(s3, "SpatRaster")) {
#'   # Calculate summary statistics
#'   mean_cases <- terra::app(s3, fun = mean)
#'   sd_cases <- terra::app(s3, fun = sd)
#'   quantiles <- terra::app(s3, fun = quantile,
#'                          probs = c(0.025, 0.5, 0.975))
#' }
#' }
#' @export
sample_pmf <- function(x,
                       n = NULL,
                       size = NULL,
                       prob = NULL,
                       method = "binomial",
                       iterations = 1,
                       output = "raster",
                       names = NULL,
                       seed = NULL,
                       ...) {
  # Input validation
  if (!inherits(x, "SpatRaster")) {
    stop("Input 'x' must be a SpatRaster object")
  }

  if (iterations < 1 || !is.numeric(iterations)) {
    stop("iterations must be a positive integer")
  }

  output <- match.arg(output,
                      choices = c("raster", "points", "matrix", "dataframe"))

  # Check if input is already a PMF
  sum_check <- global(x, "sum", na.rm = TRUE)$sum
  if (abs(sum_check - 1) > 1e-10) {
    # Convert to PMF and get total population
    pmf_result <- compute_pmf(x, return_total = TRUE)
    x <- pmf_result$pmf
    if (is.null(size)) {
      size <- pmf_result$total
      message("Using total population ", round(size), " as size parameter")
    }
  } else if (is.null(size) && is.null(n)) {
    stop("When input is already a PMF, either n or size parameter must be provided")
  }

  # Set random seed if provided
  if (!is.null(seed)) set.seed(seed)

  # Generate all sample sizes at once if using probabilistic method
  sample_sizes <- if (!is.null(n)) {
    rep(n, iterations)
  } else {
    # Probabilistic sampling
    if (is.character(method)) {
      switch(method,
             "poisson" = {
               if (is.null(prob)) stop("prob parameter required for Poisson")
               lambda <- size * prob
               rpois(iterations, lambda = lambda)
             },
             "binomial" = {
               if (is.null(prob)) stop("prob parameter required for binomial")
               rbinom(iterations, size = size, prob = prob)
             },
             "nbinom" = {
               if (is.null(prob)) stop("prob parameter required for negative binomial")
               rnbinom(iterations, size = size, prob = prob)  # Now generates all at once
             },
             stop("Unknown method string. Use 'poisson', 'binomial', or 'nbinom'")
      )
    } else if (is.function(method)) {
      result <- do.call(method, method_args)
      if (!is.numeric(result)) {
        stop("Custom method must return numeric values")
      }
      if (length(result) != iterations) {
        stop(sprintf("Custom method must return %d values (one per iteration)", iterations))
      }
      result
    } else {
      stop("method must be either a string or a function")
    }
  }

  # Generate all point samples first
  point_samples <- lapply(seq_len(iterations), function(i) {
    spatSample(x,
               size = sample_sizes[i],
               method = "weights",
               as.points = TRUE,
               ...)
  })

  # Convert to requested format
  if (output == "raster") {
    # Rasterize all point samples at once
    results <- rast(lapply(point_samples, function(pts) {
      rasterize(pts, x, fun = "count")
    }))

    # Apply names if provided
    if (!is.null(names)) {
      if (length(names) != iterations) {
        warning("Length of names vector doesn't match number of iterations. Using default names.")
      } else {
        names(results) <- names
      }
    }
    return(results)

  } else if (output == "points") {
    if (!is.null(names) && length(names) == iterations) {
      names(point_samples) <- names
    }
    return(point_samples)

  } else if (output == "matrix") {
    result <- lapply(point_samples, crds)
    if (!is.null(names) && length(names) == iterations) {
      names(result) <- names
    }
    return(result)

  } else { # dataframe
    result <- lapply(point_samples, as.data.frame)
    if (!is.null(names) && length(names) == iterations) {
      names(result) <- names
    }
    return(result)
  }
}
