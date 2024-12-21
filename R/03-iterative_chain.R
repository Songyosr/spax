#' Process facility supply and distance data for accessibility calculations
#' @param supply Numeric vector of facility capacities
#' @param distance_raster Multi-layer SpatRaster of distances to facilities
#' @param names Optional character vector of facility names
#' @param snap Logical; if TRUE skip validation
#' @return Named list with validated and processed components
#' @keywords internal
.help_prep_facilities <- function(supply, distance_raster, names = NULL, snap = FALSE) {
  if (!snap) {
    # Basic validation
    if (!is.numeric(supply)) stop("supply must be numeric")
    if (!inherits(distance_raster, "SpatRaster")) {
      stop("distance_raster must be a SpatRaster")
    }
    if (length(supply) != nlyr(distance_raster)) {
      stop("supply length must match number of distance layers")
    }
  }

  # Create facility mapping
  zero_map <- supply == 0
  n_facilities <- length(supply)

  # Fast path if no zeros
  if (!any(zero_map)) {
    return(list(
      supply = supply,
      distances = distance_raster,
      zero_map = zero_map,
      names = names %||% seq_len(n_facilities)
    ))
  }

  # Extract valid facilities
  valid_idx <- !zero_map
  valid_supply <- supply[valid_idx]
  valid_distances <- distance_raster[[valid_idx]]

  # Handle names efficiently
  facility_names <- names %||% seq_len(n_facilities)
  valid_names <- facility_names[valid_idx]

  list(
    supply = valid_supply,
    distances = valid_distances,
    zero_map = zero_map,
    names = valid_names
  )
}

#' Reintegrate zero-supply facilities into results
#' @param results Numeric vector of results for valid facilities
#' @param zero_map Logical vector marking zero-supply facilities
#' @param fill Value to use for zero-supply facilities
#' @return Numeric vector with zero facilities reintegrated
#' @keywords internal
.help_add_0facilities <- function(results, zero_map, fill = 0) {
  n_total <- length(zero_map)
  output <- numeric(n_total)
  output[!zero_map] <- results
  output[zero_map] <- fill
  output
}


#' Fast version for snap mode - returns only utilization vector
#' @export
compute_iterative_fast <- function(supply, weights, demand,
                                   lambda = 0.5,
                                   max_iter = 100,
                                   tolerance = 1e-6,
                                   window_size = 5) {
  n_facilities <- length(supply)
  current_attractiveness <- supply
  current_util <- numeric(n_facilities)

  # Initialize rolling window
  diff_window <- numeric(window_size)
  window_idx <- 1
  window_filled <- FALSE

  # Main iteration loop
  iter <- 0
  while (iter < max_iter && (!window_filled || mean(diff_window) > tolerance)) {
    iter <- iter + 1

    # Fast computation path
    huff_probs <- calc_choice(weights, attractiveness = current_attractiveness, snap = TRUE)
    util_probs <- huff_probs * weights
    new_util <- gather_weighted(demand, util_probs, snap = TRUE)$weighted_sum
    new_ratio <- ifelse(new_util > 0, supply / new_util, 0)
    new_attractiveness <- (1 - lambda) * current_attractiveness + lambda * new_ratio

    # Fast convergence check
    if (iter > 1) {
      diff_window[window_idx] <- max(abs(new_util - current_util), na.rm = TRUE)
      window_idx <- (window_idx %% window_size) + 1
      if (iter >= window_size) window_filled <- TRUE
    }

    # Update current values
    current_attractiveness <- new_attractiveness
    current_util <- new_util
  }

  # Warning if not converged
  if (!window_filled || mean(diff_window) > tolerance) {
    warning("Fast iteration did not converge after ", iter, " iterations")
  }

  current_util
}

#' Full version with history tracking and detailed output
#' @export
compute_iterative <- function(supply, weights, demand,
                              lambda = 0.5,
                              max_iter = 100,
                              tolerance = 1e-6,
                              window_size = 5,
                              convergence_type = "utilization",
                              debug = FALSE) {
  n_facilities <- length(supply)

  # State array: [iterations, facilities, metrics]
  state <- array(0, dim = c(max_iter, n_facilities, 3))
  state[1, , 3] <- supply # Initial attractiveness

  # Initialize rolling window
  diff_window <- numeric(window_size)
  window_idx <- 1
  window_filled <- FALSE

  # Main iteration loop
  iter <- 1
  while (iter < max_iter && (!window_filled || mean(diff_window) > tolerance)) {
    current <- state[iter, , ]

    # Regular computation path with full tracking
    huff_probs <- calc_choice(weights, attractiveness = current[, 3], snap = TRUE)
    util_probs <- huff_probs * weights
    new_util <- gather_weighted(demand, util_probs, snap = TRUE)$weighted_sum

    # Update state array
    iter <- iter + 1
    state[iter, , 1] <- new_util # utilization
    state[iter, , 2] <- ifelse(new_util > 0, supply / new_util, 0) # ratio
    state[iter, , 3] <- (1 - lambda) * current[, 3] + lambda * state[iter, , 2]

    # Convergence check with type support
    if (iter > 1) {
      current_diff <- if (convergence_type == "utilization") {
        max(abs(state[iter, , 1] - state[iter - 1, , 1]), na.rm = TRUE)
      } else {
        max(abs(state[iter, , 2] - state[iter - 1, , 2]), na.rm = TRUE)
      }

      diff_window[window_idx] <- current_diff
      window_idx <- (window_idx %% window_size) + 1
      if (iter >= window_size) window_filled <- TRUE

      if (debug) {
        cat(
          "Iteration:", iter,
          "\n- Difference:", round(current_diff, 6),
          "\n- Rolling avg:", round(mean(diff_window), 6), "\n"
        )
      }
    }
  }

  # Trim and return full results
  state <- state[1:iter, , , drop = FALSE]
  list(
    iterations = iter,
    converged = window_filled && mean(diff_window) <= tolerance,
    state = state,
    util_probs = util_probs,
    convergence = list(
      diff_window = diff_window,
      window_filled = window_filled,
      final_average = mean(diff_window)
    )
  )
}

#' Iterative Floating Catchment Area (iFCA) Analysis
#'
#' @description
#' Computes spatial accessibility scores using an iterative floating catchment area method
#' that combines Huff-model based choice probabilities with distance decay effects. The method
#' iteratively adjusts facility attractiveness based on supply-demand ratios until convergence.
#'
#' The iFCA method extends traditional FCA approaches by:
#' \itemize{
#'   \item Incorporating choice behavior through a Huff-model framework
#'   \item Iteratively balancing supply and demand
#'   \item Using a learning rate to control convergence
#'   \item Supporting both utilization and ratio-based convergence checks
#' }
#'
#' @param distance_raster A multi-layer SpatRaster where each layer represents distances
#'        to one facility. All layers must share the same extent and resolution.
#' @param demand SpatRaster of demand distribution (e.g., population density).
#'        Must have same extent and resolution as distance_raster.
#' @param supply Numeric vector of facility capacities. Length must match number
#'        of layers in distance_raster. Zero values are allowed.
#' @param sigma Distance decay parameter controlling spatial interaction strength.
#'        Larger values indicate weaker distance decay effect.
#' @param lambda Learning rate between 0 and 1 controlling convergence speed.
#'        Lower values provide more stability but slower convergence.
#'        Default: 0.5
#' @param max_iter Maximum number of iterations to attempt. Default: 100
#' @param tolerance Convergence tolerance threshold. Iteration stops when the rolling
#'        average of differences falls below this value. Default: 1e-6
#' @param window_size Size of rolling window for convergence checking. Default: 5
#' @param convergence_type Character string specifying convergence metric:
#'        "utilization" (default) or "ratio"
#' @param snap Logical; if TRUE enables fast computation mode returning only
#'        utilization vector. Default: FALSE
#' @param debug Logical; if TRUE provides detailed convergence information.
#'        Default: FALSE
#'
#' @return
#' If snap = TRUE:
#'   Numeric vector of predicted utilization for all facilities
#'
#' If snap = FALSE:
#'   List containing:
#'   \describe{
#'     \item{utilization}{Numeric vector of predicted facility utilization}
#'     \item{ratios}{Numeric vector of final supply-demand ratios}
#'     \item{attractiveness}{Numeric vector of final facility attractiveness values}
#'     \item{accessibility}{SpatRaster of accessibility scores}
#'     \item{convergence}{List with convergence details:
#'       \itemize{
#'         \item iterations: Number of iterations run
#'         \item converged: Logical indicating if convergence achieved
#'         \item type: Convergence check method used
#'         \item final_average: Final rolling average of differences
#'         \item window_size: Size of rolling window used
#'       }
#'     }
#'     \item{history}{Array of historical state values if converged, NULL otherwise}
#'     \item{parameters}{List of parameter values used in computation}
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' result <- spax_ifca(
#'   distance_raster = dist_rast,
#'   demand = pop_rast,
#'   supply = facility_capacity,
#'   sigma = 30
#' )
#'
#' # Fast computation mode
#' util <- spax_ifca(
#'   distance_raster = dist_rast,
#'   demand = pop_rast,
#'   supply = facility_capacity,
#'   sigma = 30,
#'   snap = TRUE
#' )
#'
#' # With modified convergence settings
#' result <- spax_ifca(
#'   distance_raster = dist_rast,
#'   demand = pop_rast,
#'   supply = facility_capacity,
#'   sigma = 30,
#'   lambda = 0.3, # Slower but more stable convergence
#'   convergence_type = "ratio",
#'   tolerance = 1e-8, # Stricter convergence
#'   window_size = 10 # Longer rolling window
#' )
#'
#' # Plot accessibility surface
#' if (!is.null(result$accessibility)) {
#'   plot(result$accessibility)
#' }
#'
#' # Check convergence history
#' if (!is.null(result$history)) {
#'   plot(1:result$convergence$iterations,
#'     result$history[, , 1], # Plot utilization history
#'     type = "l",
#'     xlab = "Iteration",
#'     ylab = "Utilization"
#'   )
#' }
#' }
#'
#' @references
#' The iFCA method builds on:
#' \itemize{
#'   \item Huff's model of retail gravitation
#'   \item Two-Step Floating Catchment Area (2SFCA) method
#'   \item Enhanced 2SFCA (E2SFCA) with variable catchment sizes
#' }
#'
#' @seealso
#' \code{\link{calc_decay}} for decay function options
#'
#' @import terra
#' @importFrom stats complete.cases
#'
#' @export
spax_ifca <- function(distance_raster,
                      demand,
                      supply,
                      sigma,
                      lambda = 0.5,
                      max_iter = 100,
                      tolerance = 1e-6,
                      window_size = 5,
                      convergence_type = c("utilization", "ratio"),
                      snap = FALSE,
                      debug = FALSE) {
  convergence_type <- match.arg(convergence_type)

  # Process facilities and compute weights - needed for both paths
  processed <- .help_prep_facilities(supply, distance_raster, snap = snap)
  weights <- calc_decay(processed$distances,
    method = "gaussian",
    sigma = sigma,
    snap = snap
  )

  # Fast path - returns only utilization
  if (snap) {
    utilization <- compute_iterative_fast(
      supply = processed$supply,
      weights = weights,
      demand = demand,
      lambda = lambda,
      max_iter = max_iter,
      tolerance = tolerance,
      window_size = window_size
    )
    return(.help_add_0facilities(utilization, processed$zero_map))
  }

  # Full computation path
  results <- compute_iterative(
    supply = processed$supply,
    weights = weights,
    demand = demand,
    lambda = lambda,
    max_iter = max_iter,
    tolerance = tolerance,
    window_size = window_size,
    convergence_type = convergence_type,
    debug = debug
  )

  # Extract final values
  final_iter <- results$iterations
  final_state <- results$state[final_iter, , , drop = TRUE]

  # Compute final accessibility using the [valid] weights
  raw_ratios <- final_state[, 2]
  accessibility <- spread_weighted(raw_ratios, weights, snap = snap)
  util_prob_surface <- sum(results$util_probs, na.rm = TRUE)


  # Reintegrate zeros into final values
  utilization <- .help_add_0facilities(final_state[, 1], processed$zero_map)
  ratios <- .help_add_0facilities(final_state[, 2], processed$zero_map)
  attractiveness <- .help_add_0facilities(final_state[, 3], processed$zero_map)



  # Return full results
  list(
    utilization = utilization,
    ratios = ratios,
    attractiveness = attractiveness,
    accessibility = accessibility,
    util_prob_surface = util_prob_surface,
    convergence = list(
      iterations = results$iterations,
      converged = results$converged,
      type = convergence_type,
      final_average = results$convergence$final_average,
      window_size = window_size
    ),
    history = if (results$converged) results$state else NULL,
    parameters = list(
      sigma = sigma,
      lambda = lambda,
      tolerance = tolerance
    )
  )
}
