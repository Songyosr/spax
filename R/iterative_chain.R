#' Process facility supply and distance data for accessibility calculations
#' @param supply Numeric vector of facility capacities
#' @param distance_raster Multi-layer SpatRaster of distances to facilities
#' @param names Optional character vector of facility names
#' @param snap Logical; if TRUE skip validation
#' @return Named list with validated and processed components
#' @keywords internal
preprocess_facilities <- function(supply, distance_raster, names = NULL, snap = FALSE) {
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
reintegrate_facilities <- function(results, zero_map, fill = 0) {
  n_total <- length(zero_map)
  output <- numeric(n_total)
  output[!zero_map] <- results
  output[zero_map] <- fill
  output
}

#' Update facility names while preserving order
#' @param names Character vector of facility names
#' @param zero_map Logical vector marking zero-supply facilities
#' @return List with name mapping information
#' @keywords internal
update_facility_names <- function(names, zero_map) {
  valid_names <- names[!zero_map]
  list(
    original = names,
    valid = valid_names,
    mapping = match(valid_names, names)
  )
}

#' Core iteration function for accessibility chain computation
#' @param supply Numeric vector of valid facility supplies
#' @param weights SpatRaster of spatial weights
#' @param demand SpatRaster of demand
#' @param lambda Learning rate between 0 and 1
#' @param max_iter Maximum iterations
#' @param tolerance Convergence tolerance
#' @param debug Logical for debug output
#' @return List with results and convergence info
#' @keywords internal
run_iterative_core <- function(supply, weights, demand,
                               lambda = 0.5,
                               max_iter = 100,
                               tolerance = 1e-6,
                               window_size = 5,
                               convergence_type = "utilization",
                               debug = FALSE) {

  n_facilities <- length(supply)

  # Initialize state array: [iterations, facilities, metrics]
  # metrics: [1=utilization, 2=ratio, 3=attractiveness]
  state <- array(0, dim = c(max_iter, n_facilities, 3))

  # Initialize first iteration
  state[1, , 3] <- supply  # Initial attractiveness = supply

  # Initialize rolling window for convergence
  diff_window <- numeric(window_size)
  window_idx <- 1
  window_filled <- FALSE

  # Main iteration loop
  iter <- 1

  while (iter < max_iter && (!window_filled || mean(diff_window) > tolerance)) {
    current <- state[iter, , ]

    # Compute choice probabilities using current attractiveness
    huff_probs <- compute_choice(weights,
                                 attractiveness = current[, 3],
                                 snap = TRUE)

    # Calculate utilization
    util_probs <- huff_probs * weights
    new_util <- weighted_gather(demand, util_probs, snap = TRUE)$weighted_sum

    # Update state array with new values
    iter <- iter + 1
    state[iter, , 1] <- new_util  # utilization
    state[iter, , 2] <- ifelse(new_util > 0, supply / new_util, 0)  # ratio
    state[iter, , 3] <- (1 - lambda) * current[, 3] +
      lambda * state[iter, , 2]  # attractiveness

    # Update convergence check
    if (iter > 1) {
      # Calculate differences based on convergence type
      current_diff <- if (convergence_type == "utilization") {
        max(abs(state[iter, , 1] - state[iter-1, , 1]), na.rm = TRUE)
      } else {
        max(abs(state[iter, , 2] - state[iter-1, , 2]), na.rm = TRUE)
      }

      # Update rolling window
      diff_window[window_idx] <- current_diff
      window_idx <- (window_idx %% window_size) + 1
      if (iter >= window_size) window_filled <- TRUE

      if (debug) {
        cat("Iteration:", iter, "\n",
            "- Current difference:", round(current_diff, 6), "\n",
            "- Rolling average:", round(mean(diff_window), 6), "\n",
            "- Mean utilization:", round(mean(new_util), 2), "\n",
            "- Supply/demand ratios range:",
            round(range(state[iter, , 2][state[iter, , 2] > 0], na.rm = TRUE), 3), "\n")
      }
    }
  }

  # Trim unused iterations from state array
  state <- state[1:iter, , , drop = FALSE]

  list(
    iterations = iter,
    converged = window_filled && mean(diff_window) <= tolerance,
    state = state,
    convergence = list(
      diff_window = diff_window,
      window_filled = window_filled,
      final_average = mean(diff_window)
    ),
    final_utilization = state[iter, , 1],
    final_ratio = state[iter, , 2],
    final_attractiveness = state[iter, , 3]
  )
}

#' Iterative Spatial Accessibility Chain Analysis
#'
#' @description
#' This function performs an iterative spatial accessibility analysis that calculates
#' how demand is distributed across facilities while accounting for distance decay,
#' facility capacity, and spatial interactions.
#'
#' The core algorithm iteratively balances supply and demand by:
#' - Distributing potential demand based on distance and facility attractiveness
#' - Adjusting facility attractiveness through a learning mechanism
#' - Converging on a stable spatial accessibility pattern
#'
#' @param distance_raster A multi-layer SpatRaster where each layer represents
#'   distances from demand locations to a specific facility.
#'
#' @param demand A SpatRaster representing the spatial distribution of demand
#'   (e.g., population density).
#'
#' @param supply A numeric vector of facility capacities, with length matching
#'   the number of layers in `distance_raster`. Facilities with zero supply are
#'   automatically handled.
#'
#' @param sigma Numeric distance decay parameter controlling how quickly
#'   accessibility diminishes with increasing distance.
#'
#' @param lambda Learning rate (0-1) controlling the speed of convergence.
#'   Lower values provide more stability but slower adaptation.
#'   Default is 0.5.
#'
#' @param method Distance decay function method. Options include:
#'   - "gaussian" (default)
#'   - "exponential"
#'   - "power"
#'   - "binary"
#'
#' @param decay_params Additional parameters for the decay function
#'   (passed to \code{\link{compute_weights}}). Default is an empty list.
#'
#' @param convergence_type Method for assessing model convergence:
#'   - "utilization": Converges based on changes in facility utilization
#'   - "ratio": Converges based on changes in supply-demand ratios
#'   Default is "utilization".
#'
#' @param max_iter Maximum number of iterations before stopping. Default is 100.
#'
#' @param tolerance Convergence threshold. The algorithm stops when the average
#'   difference between iterations falls below this value. Default is 1e-6.
#'
#' @param window_size Number of recent iterations used to assess convergence.
#'   Default is 5.
#'
#' @param snap Logical. If TRUE, skips input validation for performance.
#'   Use with caution. Default is FALSE.
#'
#' @param debug Logical. If TRUE, prints detailed iteration information.
#'   Useful for diagnosing convergence issues. Default is FALSE.
#'
#' @return A list containing:
#'   \itemize{
#'     \item{accessibility: }{SpatRaster of final accessibility scores}
#'     \item{utilization: }{Numeric vector of final facility utilization}
#'     \item{ratio: }{Numeric vector of supply-demand ratios}
#'     \item{attractiveness: }{Numeric vector of final facility attractiveness}
#'     \item{convergence: }{List with convergence diagnostic information}
#'     \item{iterations: }{Number of iterations performed}
#'     \item{facility_names: }{Mapping of facility names}
#'   }
#'
#' @examples
#' \dontrun{
#' # Assuming you have prepared:
#' # - distance_raster: Multi-layer raster of distances
#' # - demand_raster: Population density raster
#' # - supply_vector: Facility capacities
#'
#' # Basic usage
#' results <- run_access_chain(
#'   distance_raster = dist_rast,
#'   demand = population_density,
#'   supply = c(100, 50, 75),
#'   sigma = 30
#' )
#'
#' # Advanced usage with custom parameters
#' results_custom <- run_access_chain(
#'   distance_raster = dist_rast,
#'   demand = population_density,
#'   supply = c(100, 0, 75),  # Handling zero-supply facilities
#'   sigma = 40,
#'   lambda = 0.3,
#'   method = "exponential",
#'   convergence_type = "ratio",
#'   max_iter = 200,
#'   tolerance = 1e-8,
#'   debug = TRUE
#' )
#'
#' # Visualize accessibility
#' plot(results$accessibility)
#' }
#'
#' @seealso
#' \code{\link{compute_weights}}, \code{\link{spread_access}}
#'
#' @export
run_access_chain <- function(distance_raster, demand, supply,
                             sigma, lambda = 0.5,
                             method = "gaussian", decay_params = list(),
                             convergence_type = "utilization",
                             max_iter = 100, tolerance = 1e-6, window_size = 5,
                             snap = FALSE, debug = FALSE) {

  # Preprocess facilities
  facilities <- preprocess_facilities(supply, distance_raster, snap = snap)

  # Compute weights
  weights <- do.call(compute_weights,
                     c(list(distance = facilities$distances,
                            method = method,
                            sigma = sigma),
                       decay_params))

  # Run iterative core
  core_result <- run_iterative_core(supply = facilities$supply,
                                    weights = weights,
                                    demand = demand,
                                    lambda = lambda,
                                    max_iter = max_iter,
                                    tolerance = tolerance,
                                    window_size = window_size,
                                    convergence_type = convergence_type,
                                    debug = debug)

  # Compute final accessibility
  final_ratio <- reintegrate_facilities(core_result$final_ratio,
                                        facilities$zero_map)
  accessibility <- weighted_spread(final_ratio, weights, snap = TRUE)

  # Update facility names
  facility_names <- update_facility_names(facilities$names, facilities$zero_map)

  # Prepare output
  list(
    accessibility = accessibility,
    utilization = reintegrate_facilities(core_result$final_utilization,
                                         facilities$zero_map),
    ratio = final_ratio,
    attractiveness = reintegrate_facilities(core_result$final_attractiveness,
                                            facilities$zero_map),
    convergence = core_result$convergence,
    iterations = core_result$iterations,
    facility_names = facility_names
  )
}
