#' Validate inputs for spax_ifca
#' @param distance_raster SpatRaster stack of travel times/distances to facilities
#' @param demand SpatRaster of demand (single layer or matching max_iter)
#' @param supply Numeric vector of facility capacities
#' @param decay_params List of parameters for decay function
#' @param lambda Learning rate between 0 and 1
#' @param max_iter Maximum number of iterations
#' @param tolerance Convergence tolerance threshold
#' @param window_size Size of rolling window for convergence
#' @return Invisibly returns TRUE if all validations pass.
#' @keywords internal
.chck_spax_ifca <- function(distance_raster, demand, supply, decay_params,
                            lambda, max_iter, tolerance, window_size) {
  # Input type validation
  .assert_class(distance_raster, "SpatRaster", "distance_raster")
  .assert_class(demand, "SpatRaster", "demand")
  .assert_numeric(supply, "supply")

  # Validate demand layers
  n_layers <- nlyr(demand)
  if (n_layers != 1 && n_layers != max_iter) {
    stop(sprintf(
      "demand must have either 1 layer or %d layers (matching max_iter)",
      max_iter
    ))
  }

  # Validate decay_params
  .assert_class(decay_params, "list", "decay_params")
  if (is.null(decay_params$method)) {
    stop("decay_params must include 'method'")
  }

  # Parameter range validation
  .assert_numeric(lambda, "lambda")
  .assert_range(lambda, 0, 1, "lambda")

  .assert_numeric(max_iter, "max_iter")
  .assert_integer(max_iter, "max_iter")
  .assert_positive(max_iter, allow_zero = FALSE, "max_iter")

  .assert_numeric(tolerance, "tolerance")
  .assert_positive(tolerance, allow_zero = FALSE, "tolerance")

  .assert_numeric(window_size, "window_size")
  .assert_integer(window_size, "window_size")
  .assert_positive(window_size, allow_zero = FALSE, "window_size")

  # Validate raster alignment
  .assert_raster_alignment(demand, distance_raster, "demand", "distance_raster")

  # Validate facility counts match
  .assert_lengths_match(
    nlyr(distance_raster), length(supply),
    "distance_raster layers", "supply vector"
  )

  invisible(TRUE)
}

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
    .assert_numeric(supply, "supply")
    .assert_class(distance_raster, "SpatRaster", "distance_raster")
    .assert_lengths_match(length(supply), nlyr(distance_raster),
                          "supply length", "number of distance layers")
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

#' Fast version of iterative computation for parameter tuning
#' @description
#' Optimized version that returns only utilization vector, designed for parameter
#' tuning scenarios where intermediate results and history aren't needed.
#'
#' @param supply Numeric vector of facility capacities
#' @param weights Multi-layer SpatRaster of spatial weights from distance decay
#' @param demand SpatRaster of demand (single layer or matching max_iter)
#' @param lambda Learning rate between 0 and 1
#' @param max_iter Maximum iterations to attempt
#' @param tolerance Convergence threshold
#' @param window_size Size of rolling window for convergence checking
#' @return Numeric vector of predicted utilization for all facilities
#' @keywords internal
compute_iterative_fast <- function(supply, weights, demand,
                                   lambda = 0.5,
                                   max_iter = 100,
                                   tolerance = 1e-6,
                                   window_size = 5) {
  # Initialize state vectors for current iteration
  n_facilities <- length(supply)
  current_attractiveness <- supply # Initial attractiveness = supply
  current_util <- numeric(n_facilities) # Initialize utilization vector

  # Initialize rolling window for convergence checking
  diff_window <- numeric(window_size)
  window_idx <- 1
  window_filled <- FALSE

  # Main iteration loop
  iter <- 0
  while (iter < max_iter && (!window_filled || mean(diff_window) > tolerance)) {
    iter <- iter + 1

    # Get current demand layer - either indexed or single layer reused
    current_demand <- if (nlyr(demand) > 1) demand[[iter]] else demand

    # Fast computation path:
    # 1. Convert attractiveness to probabilities
    huff_probs <- calc_choice(weights,
      attractiveness = current_attractiveness,
      snap = TRUE
    )

    # 2. Calculate utilization probabilities
    util_probs <- huff_probs * weights

    # 3. Calculate new utilization using current demand layer
    new_util <- gather_weighted(current_demand, util_probs, snap = TRUE, simplify = TRUE)

    # 4. Calculate new supply/demand ratio
    new_ratio <- ifelse(new_util > 0, supply / new_util, 0)

    # 5. Update attractiveness with learning rate
    new_attractiveness <- (1 - lambda) * current_attractiveness + lambda * new_ratio

    # Fast convergence check using rolling window
    if (iter > 1) {
      diff_window[window_idx] <- max(abs(new_util - current_util), na.rm = TRUE)
      window_idx <- (window_idx %% window_size) + 1
      if (iter >= window_size) window_filled <- TRUE
    }

    # Update current values for next iteration
    current_attractiveness <- new_attractiveness
    current_util <- new_util
  }

  # Warning if not converged
  if (!window_filled || mean(diff_window) > tolerance) {
    warning("Fast iteration did not converge after ", iter, " iterations")
  }

  # Return only the final utilization vector
  current_util
}

#' Full iterative computation with history tracking
#' @description
#' Implements the iterative floating catchment area method with comprehensive
#' output including convergence history and detailed state tracking. This version
#' supports both single-layer and time-series demand patterns.
#'
#' @param supply Numeric vector of facility capacities
#' @param weights Multi-layer SpatRaster of spatial weights from distance decay
#' @param demand SpatRaster of demand (single layer or matching max_iter)
#' @param lambda Learning rate between 0 and 1
#' @param max_iter Maximum iterations to attempt
#' @param tolerance Convergence threshold
#' @param window_size Size of rolling window for convergence checking
#' @param convergence_type Character; "utilization" or "ratio" for convergence metric
#' @param debug Logical; if TRUE provides detailed convergence information
#' @return List containing:
#'   \itemize{
#'     \item iterations: Number of iterations completed
#'     \item converged: Logical indicating convergence status
#'     \item state: Array of historical state values [iterations, facilities, metrics]
#'     \item util_probs: Final utilization probability surfaces
#'     \item convergence: List with convergence details
#'   }
#' @keywords internal
compute_iterative <- function(supply, weights, demand,
                              lambda = 0.5,
                              max_iter = 100,
                              tolerance = 1e-6,
                              window_size = 5,
                              convergence_type = "utilization",
                              debug = FALSE) {
  n_facilities <- length(supply)

  # State array: [iterations, facilities, metrics]
  # metrics: [1]=utilization, [2]=ratio, [3]=attractiveness
  state <- array(0, dim = c(max_iter, n_facilities, 3))
  state[1, , 3] <- supply # Initial attractiveness

  # Initialize rolling window for convergence
  diff_window <- numeric(window_size)
  window_idx <- 1
  window_filled <- FALSE

  # Main iteration loop
  iter <- 1
  while (iter < max_iter && (!window_filled || mean(diff_window) > tolerance)) {
    # Get current values
    current <- state[iter, , ]

    # Get current demand layer
    current_demand <- if (nlyr(demand) > 1) demand[[iter]] else demand

    # Regular computation path with full tracking
    huff_probs <- calc_choice(weights,
      attractiveness = current[, 3],
      snap = TRUE
    )
    util_probs <- huff_probs * weights
    new_util <- gather_weighted(current_demand, util_probs,
      snap = TRUE, simplify = TRUE
    )

    # Update state array
    iter <- iter + 1
    state[iter, , 1] <- new_util # utilization
    state[iter, , 2] <- ifelse(new_util > 0, supply / new_util, 0) # ratio
    state[iter, , 3] <- (1 - lambda) * current[, 3] +
      lambda * state[iter, , 2] # attractiveness

    # Convergence check with type support
    if (iter > 1) {
      # Select metric based on convergence type
      current_diff <- if (convergence_type == "utilization") {
        max(abs(state[iter, , 1] - state[iter - 1, , 1]), na.rm = TRUE)
      } else {
        max(abs(state[iter, , 2] - state[iter - 1, , 2]), na.rm = TRUE)
      }

      # Update rolling window
      diff_window[window_idx] <- current_diff
      window_idx <- (window_idx %% window_size) + 1
      if (iter >= window_size) window_filled <- TRUE

      # Debug output if requested
      if (debug) {
        cat(
          sprintf("Iteration: %d\n", iter),
          sprintf("- Difference: %.6f\n", current_diff),
          sprintf("- Rolling avg: %.6f\n", mean(diff_window))
        )
      }
    }
  }

  # Trim state array to actual iterations used
  state <- state[1:iter, , , drop = FALSE]

  # Return comprehensive results
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
#' that combines Huff-model based choice probabilities with distance decay effects. This
#' enhanced version supports time-series demand patterns and customizable decay functions.
#'
#' The iFCA method extends traditional FCA approaches by:
#' \itemize{
#'   \item Supporting dynamic demand patterns through multi-layer inputs
#'   \item Incorporating flexible distance decay specifications
#'   \item Iteratively balancing supply and demand
#'   \item Using a learning rate to control convergence
#' }
#'
#' @param distance_raster A multi-layer SpatRaster where each layer represents distances
#'        to one facility. All layers must share the same extent and resolution.
#' @param demand SpatRaster of demand distribution. Can be either:
#'        - Single layer for static demand
#'        - Multiple layers (matching max_iter) for dynamic demand
#' @param supply Numeric vector of facility capacities. Length must match number
#'        of layers in distance_raster.
#' @param supply_names Optional character vector of facility names. If NULL, use default names
#' @param decay_params List of parameters for decay function:
#'        \itemize{
#'          \item method: "gaussian", "exponential", "power", or custom function
#'          \item sigma: decay parameter controlling spatial interaction strength
#'          \item Additional parameters passed to custom decay functions
#'        }
#' @param lambda Learning rate between 0 and 1 controlling convergence speed.
#'        Lower values provide more stability but slower convergence.
#'        Default: 0.5
#' @param max_iter Maximum number of iterations to attempt. For multi-layer demand,
#'        this must match the number of demand layers. Default: 100
#' @param tolerance Convergence tolerance threshold. Iteration stops when the rolling
#'        average of differences falls below this value. Default: 1e-6
#' @param window_size Size of rolling window for convergence checking. Default: 5
#' @param convergence_type Character string specifying convergence metric, one of:
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
#'   A spax object containing:
#'   \describe{
#'     \item{accessibility}{SpatRaster of accessibility scores}
#'     \item{type}{Character string "iFCA"}
#'     \item{parameters}{List of model parameters including decay_params, lambda, etc.}
#'     \item{facilities}{data.frame with columns:
#'       \itemize{
#'         \item id: Facility identifiers
#'         \item utilization: Predicted facility utilization
#'         \item ratio: Supply-to-demand ratios
#'         \item attractiveness: Final facility attractiveness
#'       }
#'     }
#'     \item{iterations}{List containing:
#'       \itemize{
#'         \item history: Array of historical state values if converged
#'         \item convergence: Convergence details including iterations, status, etc.
#'       }
#'     }
#'     \item{call}{The original function call}
#'   }
#'
#' @examples
#' \dontrun{
#' # Load example data
#' library(terra)
#' library(sf)
#'
#' # Prepare inputs
#' pop <- read_spax_example("u5pd.tif")
#' distance <- read_spax_example("hos_iscr.tif")
#' hospitals <- st_drop_geometry(hc12_hos)
#'
#' # Basic usage with default parameters
#' result <- spax_ifca(
#'   distance_raster = distance,
#'   demand = pop,
#'   supply = hospitals$s_doc,
#'   decay_params = list(
#'     method = "gaussian",
#'     sigma = 30 # 30-minute characteristic distance
#'   )
#' )
#'
#' # Plot accessibility surface
#' plot(result$accessibility, main = "Doctor Accessibility (iFCA)")
#'
#' # Examine facility-level results
#' head(result$facilities)
#'
#' # Check convergence information
#' print(result$iterations$convergence)
#'
#' # Fast computation mode - returns only utilization
#' util <- spax_ifca(
#'   distance_raster = distance,
#'   demand = pop,
#'   supply = hospitals$s_doc,
#'   decay_params = list(method = "gaussian", sigma = 30),
#'   snap = TRUE
#' )
#'
#' # Compare predicted utilization with facility capacity
#' data.frame(
#'   id = hospitals$id,
#'   capacity = hospitals$s_doc,
#'   predicted = util
#' )
#'
#' # Using custom decay function
#' custom_decay <- function(distance, sigma = 30, threshold = 60) {
#'   weights <- exp(-distance^2 / (2 * sigma^2))
#'   weights[distance > threshold] <- 0
#'   return(weights)
#' }
#'
#' result_custom <- spax_ifca(
#'   distance_raster = distance,
#'   demand = pop,
#'   supply = hospitals$s_doc,
#'   decay_params = list(
#'     method = custom_decay,
#'     sigma = 30,
#'     threshold = 60
#'   )
#' )
#' }
#'
#' @seealso
#' \code{\link{calc_decay}} for available decay functions
#' @export
spax_ifca <- function(distance_raster,
                      demand,
                      supply,
                      supply_names = NULL,  # New parameter
                      decay_params = list(method = "gaussian", sigma = 30),
                      lambda = 0.5,
                      max_iter = 100,
                      tolerance = 1e-6,
                      window_size = 5,
                      convergence_type = c("utilization", "ratio"),
                      snap = FALSE,
                      debug = FALSE) {
  convergence_type <- match.arg(convergence_type)

  # Process facilities and validate inputs (unless in snap mode)
  if (!snap) {
    .chck_spax_ifca(
      distance_raster, demand, supply, decay_params,
      lambda, max_iter, tolerance, window_size
    )
  }

  # Generate/validate facility names at the start
  if (is.null(supply_names)) {
    supply_names <- paste0("facility_", seq_along(supply))
  }

  # Process facilities and remove any with zero supply
  processed <- .help_prep_facilities(
    supply = supply,
    distance_raster = distance_raster,
    names = supply_names,
    snap = snap)

  # Compute weights using specified decay function
  weights <- do.call(
    calc_decay,
    c(
      list(distance = processed$distances),
      decay_params,
      list(snap = snap)
    )
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

  # Compute final accessibility using valid weights
  raw_ratios <- final_state[, 2]
  accessibility <- spread_weighted(raw_ratios, weights, snap = snap)
  util_prob_surface <- sum(results$util_probs, na.rm = TRUE)

  # Reintegrate zeros into final values
  utilization <- .help_add_0facilities(final_state[, 1], processed$zero_map)
  ratios <- .help_add_0facilities(final_state[, 2], processed$zero_map)
  attractiveness <- .help_add_0facilities(final_state[, 3], processed$zero_map)

  # Create facilities data.frame
  facilities <- data.frame(
    id = supply_names,
    utilization = utilization,
    ratio = ratios,
    attractiveness = attractiveness,
    row.names = NULL
  )

  # Create spax object
  .create_spax(
    accessibility = results$accessibility,
    type = "iFCA",
    parameters = list(
      decay_params = decay_params,
      lambda = lambda,
      tolerance = tolerance,
      window_size = window_size,
      convergence_type = convergence_type
    ),
    facilities = facilities,
    iterations = list(
      history = if (results$converged) results$state else NULL,
      convergence = list(
        iterations = results$iterations,
        converged = results$converged,
        type = convergence_type,
        final_average = results$convergence$final_average
      )
    ),
    call = match.call(),
    snap = snap
  )
}
