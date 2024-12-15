#' Perform single iteration of accessibility calculation
#'
#' @param supply Original supply capacity vector
#' @param supply_ratio Current supply-to-demand ratios
#' @param weights Multi-layer SpatRaster of spatial weights
#' @param demand SpatRaster of demand locations
#' @return List containing updated state information
#' @keywords internal
iterate_accessibility <- function(supply, supply_ratio, weights, demand) {
  # 1. Compute accessibility with current ratios
  accessibility <- spread_access(supply_ratio, weights)

  # 2. Compute choice probabilities based on accessibility
  huff_probs <- compute_choice(weights, attractiveness = supply_ratio)

  # 3. Get utilization probabilities and expected utilization
  util_probs <- huff_probs * weights
  new_util <- gather_demand(demand, util_probs)$potential_demand

  # 4. Update supply-to-demand ratios using original supply
  new_ratio <- supply / new_util

  list(
    supply_ratio = new_ratio,
    utilization = new_util,
    accessibility = accessibility
  )
}

#' Run iterative accessibility chain model until convergence
#'
#' Run iterative accessibility model until convergence
#'
#' @param distance_raster Multi-layer SpatRaster of distances
#' @param demand SpatRaster of demand locations
#' @param supply Vector of facility capacities
#' @param sigma Distance decay parameter
#' @param max_iter Maximum iterations (default 100)
#' @param tolerance Convergence tolerance (default 1e-6)
#' @return List containing final results and convergence information
#' @export
run_accessibility_model <- function(distance_raster, demand, supply, sigma,
                                    max_iter = 100, tolerance = 1e-6) {

  # Initial setup
  weights <- compute_weights(distance_raster, method = "gaussian", sigma = sigma)

  # Generate sequence of iterations
  results <- lapply(seq_len(max_iter), function(i) {
    if (i == 1) {
      # First iteration uses original supply as ratio
      return(iterate_accessibility(supply, supply, weights, demand))
    } else {
      # Get previous iteration result
      prev <- results[[i - 1]]

      # Check convergence
      if (!is.null(prev$utilization)) {
        new <- iterate_accessibility(supply, prev$supply_ratio, weights, demand)
        max_diff <- max(abs(new$utilization - prev$utilization))

        # If converged, add convergence info and return
        if (max_diff < tolerance) {
          new$converged <- TRUE
          new$iterations <- i
          return(new)
        }
      }

      # Continue iteration
      return(iterate_accessibility(supply, prev$supply_ratio, weights, demand))
    }
  })

  # Find the final result (either converged or last iteration)
  converged_idx <- Position(function(x) !is.null(x$converged) && x$converged, results)
  final_result <- if (!is.null(converged_idx)) {
    results[[converged_idx]]
  } else {
    last <- results[[max_iter]]
    last$converged <- FALSE
    last$iterations <- max_iter
    last
  }

  return(final_result)
}
