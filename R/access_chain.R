run_access_chain <- function(distance_raster, demand, supply, sigma,
                             lambda = 0.5,
                             convergence_type = c("utilization", "ratio"),
                             max_iter = 100,
                             tolerance = 1e-6,
                             debug = FALSE) {
  # Input validation
  convergence_type <- match.arg(convergence_type)
  if (lambda <= 0 || lambda > 1) stop("lambda must be between 0 and 1")
  if (!inherits(distance_raster, "SpatRaster")) stop("distance_raster must be a SpatRaster")
  if (!inherits(demand, "SpatRaster")) stop("demand must be a SpatRaster")
  if (length(supply) != nlyr(distance_raster)) {
    stop("Number of supply values must match number of distance raster layers")
  }

  if (debug) {
    cat("Configuration:\n")
    cat("- Number of facilities:", length(supply), "\n")
    cat("- Learning rate (lambda):", lambda, "\n")
    cat("- Convergence type:", convergence_type, "\n")
    cat("- Distance decay parameter (sigma):", sigma, "\n")
  }

  # Guard against zero supply
  if (any(supply == 0)) {
    warning("Zero supply facilities detected - removing from analysis")
    valid_facilities <- supply > 0
    supply <- supply[valid_facilities]
    distance_raster <- distance_raster[[valid_facilities]]
  }

  # Initial setup
  if (debug) cat("\nInitializing model components...\n")

  weights <- compute_weights(distance_raster, method = "gaussian", sigma = sigma)
  n_facilities <- length(supply)

  # Initialize tracking matrices
  util_matrix <- matrix(NA, nrow = max_iter, ncol = n_facilities)
  diff_matrix <- matrix(NA, nrow = max_iter, ncol = n_facilities)
  ratio_matrix <- matrix(NA, nrow = max_iter, ncol = n_facilities)

  # Initialize state variables
  current_attractiveness <- supply  # Start with supply as initial attractiveness
  current_util <- numeric(n_facilities)
  current_ratio <- supply

  # Initialize loop control variables
  iter <- 0
  max_diff <- Inf

  if (debug) cat("Starting main iteration loop...\n")

  while (iter < max_iter && max_diff > tolerance) {
    iter <- iter + 1
    if (debug) cat("\nIteration:", iter, "\n")

    # Step 1: Compute choice probabilities based on current attractiveness
    huff_probs <- compute_choice(weights, attractiveness = current_attractiveness)

    # Step 2: Calculate utilization probabilities and expected utilization
    util_probs <- huff_probs * weights
    new_util <- gather_demand(demand, util_probs)$potential_demand

    # Step 3: Compute supply-demand ratios
    new_ratio <- ifelse(new_util > 0, supply / new_util, 0)

    # Step 4: Update attractiveness with learning rate
    new_attractiveness <- (1 - lambda) * current_attractiveness +
      lambda * (supply / new_util)

    # Store history
    util_matrix[iter, ] <- new_util
    ratio_matrix[iter, ] <- new_ratio

    # Calculate differences based on convergence type
    if (convergence_type == "utilization") {
      if (iter > 1) {
        diff_matrix[iter,] <- abs(new_util - current_util)
        max_diff <- max(diff_matrix[iter,], na.rm = TRUE)
      }
    } else {  # ratio convergence
      if (iter > 1) {
        diff_matrix[iter,] <- abs(new_ratio - current_ratio)
        max_diff <- max(diff_matrix[iter,], na.rm = TRUE)
      }
    }

    if (debug) {
      cat("- Max difference:", round(max_diff, 6), "\n")
      cat("- Mean utilization:", round(mean(new_util), 2), "\n")
      cat("- Supply/demand ratios range:",
          round(range(new_ratio[new_ratio > 0], na.rm = TRUE), 3), "\n")
    }

    # Update current values
    current_attractiveness <- new_attractiveness
    current_util <- new_util
    current_ratio <- new_ratio
  }

  # Compute final accessibility surface
  accessibility <- spread_access(current_ratio, weights)

  # Trim matrices to actual iterations
  util_matrix <- util_matrix[1:iter,, drop = FALSE]
  diff_matrix <- diff_matrix[1:iter,, drop = FALSE]
  ratio_matrix <- ratio_matrix[1:iter,, drop = FALSE]

  if (debug) {
    cat("\nAlgorithm completed:\n")
    cat("- Total iterations:", iter, "\n")
    cat("- Final max difference:", round(max_diff, 6), "\n")
    cat("- Converged:", max_diff <= tolerance, "\n")
  }

  # Return results
  list(
    utilization = util_matrix,
    attractiveness = current_attractiveness,
    ratios = current_ratio,
    accessibility = accessibility,
    convergence = list(
      iterations = iter,
      converged = max_diff <= tolerance,
      differences = diff_matrix,
      ratios = ratio_matrix,
      type = convergence_type
    )
  )
}
