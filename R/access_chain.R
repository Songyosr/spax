#' Validate inputs for accessibility chain algorithm
#' @keywords internal
validate_access_chain_inputs <- function(distance_raster, demand, supply,
                                         lambda, sigma, method) {
  # Validate raster inputs
  if (!inherits(distance_raster, "SpatRaster"))
    stop("distance_raster must be a SpatRaster")
  if (!inherits(demand, "SpatRaster"))
    stop("demand must be a SpatRaster")

  # Check raster compatibility
  if (!all(res(distance_raster) == res(demand))) {
    stop("distance_raster and demand must have the same resolution")
  }
  if (!all(ext(distance_raster) == ext(demand))) {
    stop("distance_raster and demand must have the same extent")
  }

  # Validate supply vector
  if (length(supply) != nlyr(distance_raster)) {
    stop("Number of supply values must match number of distance raster layers")
  }

  # Validate parameters
  if (lambda <= 0 || lambda > 1)
    stop("lambda must be between 0 and 1")

  # Validate method
  if (is.character(method) &&
      !method %in% c("gaussian", "exponential", "power", "binary")) {
    stop("method must be one of: gaussian, exponential, power, binary, or a function")
  }
}

#' Iterative Spatial Accessibility Chain Algorithm with Zero-Supply Handling
#'
#' @description
#' Implements an iterative spatial accessibility model that finds equilibrium between
#' supply and demand through a chain of facility choices. The model combines Huff-based
#' choice probabilities with distance decay to simulate how demand is distributed across
#' facilities, using a learning rate to dampen oscillations and achieve stable convergence.
#'
#' Key features of this implementation:
#' - Properly handles facilities with zero supply by temporarily excluding them from calculations
#'   while maintaining their positions in the final output
#' - Offers two computation modes: full analysis with detailed outputs (default) and
#'   fast mode for utilization-only calculations
#' - Implements convergence monitoring through a rolling window approach
#' - Supports multiple distance decay functions and custom decay methods
#'
#' The algorithm follows these steps:
#' 1. Preprocess and validate inputs, handling zero-supply facilities
#' 2. Compute initial choice probabilities based on facility attractiveness
#' 3. Calculate expected utilization at each facility
#' 4. Update supply-demand ratios
#' 5. Adjust facility attractiveness using learning rate
#' 6. Check for convergence using rolling window
#' 7. Reintegrate zero-supply facilities in final output
#'
#' @param distance_raster Multi-layer SpatRaster where each layer represents distances
#'        to one facility. All layers must share the same extent and resolution.
#' @param demand SpatRaster of demand distribution (e.g., population density).
#'        Must have same extent and resolution as distance_raster.
#' @param supply Numeric vector of facility capacities. Length must match number
#'        of layers in distance_raster. Zero values are allowed and handled appropriately.
#' @param sigma Distance decay parameter controlling spatial interaction strength.
#'        Interpretation depends on chosen decay function.
#' @param lambda Learning rate between 0 and 1 controlling convergence speed.
#'        Lower values provide more stability but slower convergence.
#' @param method Either a string specifying the decay function ("gaussian",
#'        "exponential", "power", "binary") or a custom decay function that takes
#'        parameters specified in decay_params.
#' @param decay_params List of additional parameters passed to custom decay functions.
#'        Ignored when using built-in decay methods.
#' @param convergence_type Character indicating convergence check method:
#'        "utilization" (default) or "ratio"
#' @param max_iter Maximum number of iterations (default 100)
#' @param tolerance Convergence tolerance (default 1e-6)
#' @param window_size Size of rolling window for convergence checking (default 5)
#' @param track_history Logical; if TRUE records iteration history (default TRUE)
#' @param snap Logical; if TRUE enables fast mode, returning only utilization vector
#' @param internal_snap Logical; if TRUE passes snap=TRUE to internal functions
#' @param debug Logical; if TRUE provides detailed convergence information
#'
#' @return
#' If snap = TRUE:
#'   Numeric vector of predicted utilization for all facilities (including zeros for
#'   zero-supply facilities)
#'
#' If snap = FALSE:
#'   A list containing:
#'   \item{utilization}{Matrix [iterations x facilities] of utilization history}
#'   \item{attractiveness}{Vector of final attractiveness values for each facility}
#'   \item{ratios}{Vector of final supply-demand ratios}
#'   \item{accessibility}{SpatRaster of final accessibility surface}
#'   \item{convergence}{List with convergence details:
#'     \itemize{
#'       \item iterations: Number of iterations run
#'       \item converged: Logical indicating if convergence was achieved
#'       \item differences: Matrix of differences across iterations
#'       \item ratios: Matrix of supply-demand ratios across iterations
#'       \item type: Convergence check method used
#'       \item rolling_average: Final rolling average of differences
#'       \item window_size: Size of rolling window used
#'     }
#'   }
#'
#' @note
#' Facilities with zero supply are handled specially:
#' 1. They are temporarily removed from calculations to improve efficiency
#' 2. Their positions are tracked throughout the process
#' 3. They are reinserted with zero values in all results
#' 4. Original facility ordering is preserved in all outputs
#'
#' The snap parameter enables a fast computation mode that:
#' 1. Skips input validation
#' 2. Omits history tracking and detailed outputs
#' 3. Returns only the final utilization vector
#' 4. Maintains correct handling of zero-supply facilities
#'
#' @examples
#' \dontrun{
#' # Basic usage including zero-supply facilities
#' result <- run_access_chain(
#'   distance_raster = dist_rast,
#'   demand = pop_rast,
#'   supply = c(100, 0, 50, 75),  # Second facility has zero supply
#'   sigma = 30,
#'   method = "gaussian"
#' )
#'
#' # Fast mode for optimization
#' util <- run_access_chain(
#'   distance_raster = dist_rast,
#'   demand = pop_rast,
#'   supply = facility_capacity,
#'   sigma = 30,
#'   method = "gaussian",
#'   snap = TRUE
#' )
#' }
#'

#' @export
run_access_chain <- function(distance_raster, demand, supply, sigma,
                             lambda = 0.5,
                             method = "gaussian",
                             decay_params = list(),
                             convergence_type = c("utilization", "ratio"),
                             max_iter = 100,
                             tolerance = 1e-6,
                             window_size = 5,
                             track_history = TRUE,
                             snap = FALSE,
                             internal_snap = TRUE,
                             debug = FALSE) {

  # Only validate if not in snap mode
  if (!snap) {
    validate_access_chain_inputs(
      distance_raster = distance_raster,
      demand = demand, supply = supply,
      lambda = lambda, sigma = sigma, method = method)
  }

  convergence_type <- match.arg(convergence_type)

  # Debug output if needed
  if (debug) {
    cat("Configuration:\n")
    cat("- Number of facilities:", length(supply), "\n")
    cat("- Learning rate (lambda):", lambda, "\n")
    cat("- Convergence type:", convergence_type, "\n")
    cat("- Distance decay parameter (sigma):", sigma, "\n")
    cat("- Method:", if(is.function(method)) "custom" else method, "\n")
    if (length(decay_params) > 0) {
      cat("- Additional decay parameters:\n")
      for (param in names(decay_params)) {
        cat("  *", param, ":", decay_params[[param]], "\n")
      }
    }
    cat("- Snap mode:", snap, "\n")
    cat("- Internal snap mode:", internal_snap, "\n")
  }

  # Store original facility indices and identify zero-supply facilities
  n_total_facilities <- length(supply)
  facility_indices <- seq_len(n_total_facilities)
  zero_supply <- supply == 0

  # Remove zero-supply facilities from analysis
  if (any(zero_supply)) {
    if (!snap) warning("Zero supply facilities detected - temporarily removing from analysis")
    valid_facilities <- !zero_supply
    supply_valid <- supply[valid_facilities]
    distance_raster_valid <- distance_raster[[valid_facilities]]
  } else {
    supply_valid <- supply
    distance_raster_valid <- distance_raster
  }

  # Get facility names
  facility_names <- names(distance_raster)
  if (is.null(facility_names) && !is.null(names(supply))) {
    facility_names <- names(supply)
  }
  if (is.null(facility_names)) {
    facility_names <- paste0("facility_", facility_indices)
  }

  # Set up computation mode
  compute_mode <- if(snap) "fast" else "full"

  # Initialize state variables
  n_valid_facilities <- length(supply_valid)
  current_attractiveness <- setNames(supply_valid, facility_names[!zero_supply])
  current_util <- setNames(numeric(n_valid_facilities), facility_names[!zero_supply])
  current_ratio <- setNames(supply_valid, facility_names[!zero_supply])

  # Compute weights only for valid facilities
  weights <- do.call(compute_weights,
                     c(list(distance = distance_raster_valid,
                            method = method,
                            sigma = sigma,
                            snap = internal_snap),
                       decay_params))

  # Initialize history matrices if tracking enabled and in full mode
  if (track_history && compute_mode == "full") {
    iter_names <- paste0("iter_", seq_len(max_iter))
    util_matrix <- matrix(NA_real_, nrow = max_iter, ncol = n_total_facilities,
                          dimnames = list(iter_names, facility_names))
    diff_matrix <- matrix(NA_real_, nrow = max_iter, ncol = n_total_facilities,
                          dimnames = list(iter_names, facility_names))
    ratio_matrix <- matrix(NA_real_, nrow = max_iter, ncol = n_total_facilities,
                           dimnames = list(iter_names, facility_names))
  } else {
    util_matrix <- diff_matrix <- ratio_matrix <- NULL
  }

  # Initialize convergence control
  iter <- 0
  diff_window <- numeric(window_size)
  window_idx <- 1
  window_filled <- FALSE

  # Main iteration loop
  while (iter < max_iter && (!window_filled || mean(diff_window) > tolerance)) {
    iter <- iter + 1
    if (debug) cat("\nIteration:", iter, "\n")

    # Compute choice probabilities
    huff_probs <- compute_choice(weights,
                                 attractiveness = current_attractiveness,
                                 snap = internal_snap)

    # Calculate utilization
    util_probs <- huff_probs * weights
    new_util <- weighted_gather(demand, util_probs,
                                snap = internal_snap)$weighted_sum

    # Compute supply-demand ratios
    new_ratio <- ifelse(new_util > 0, supply_valid / new_util, 0)

    # Update attractiveness with learning rate
    new_attractiveness <- (1 - lambda) * current_attractiveness +
      lambda * new_ratio

    # Track history if needed
    if (track_history && compute_mode == "full") {
      # Create full-size vectors including zeros for excluded facilities
      full_util <- numeric(n_total_facilities)
      full_ratio <- numeric(n_total_facilities)
      full_util[!zero_supply] <- new_util
      full_ratio[!zero_supply] <- new_ratio

      util_matrix[iter, ] <- full_util
      ratio_matrix[iter, ] <- full_ratio

      if (iter > 1) {
        diff_matrix[iter,] <- if (convergence_type == "utilization") {
          abs(full_util - util_matrix[iter-1,])
        } else {
          abs(full_ratio - ratio_matrix[iter-1,])
        }
      }
    }

    # Update convergence check
    if (iter > 1) {
      current_diff <- if (convergence_type == "utilization") {
        max(abs(new_util - current_util), na.rm = TRUE)
      } else {
        max(abs(new_ratio - current_ratio), na.rm = TRUE)
      }

      diff_window[window_idx] <- current_diff
      window_idx <- (window_idx %% window_size) + 1
      if (iter >= window_size) window_filled <- TRUE

      if (debug) {
        cat("- Current difference:", round(current_diff, 6), "\n")
        cat("- Rolling average:", round(mean(diff_window), 6), "\n")
        cat("- Mean utilization:", round(mean(new_util), 2), "\n")
        cat("- Supply/demand ratios range:",
            round(range(new_ratio[new_ratio > 0], na.rm = TRUE), 3), "\n")
      }
    }

    # Update current values
    current_attractiveness <- new_attractiveness
    current_util <- new_util
    current_ratio <- new_ratio
  }

  # If in fast mode (snap=TRUE), return only the utilization vector
  if (compute_mode == "fast") {
    # Create full-size vector including zeros for excluded facilities
    result <- numeric(n_total_facilities)
    result[!zero_supply] <- current_util
    return(result)
  }

  # For full mode, compute final accessibility and prepare complete results
  accessibility <- weighted_spread(current_ratio, weights,
                                   snap = internal_snap)

  # Create full-size vectors for final results
  final_attractiveness <- numeric(n_total_facilities)
  final_ratio <- numeric(n_total_facilities)
  final_attractiveness[!zero_supply] <- current_attractiveness
  final_ratio[!zero_supply] <- current_ratio

  # Trim matrices to actual iterations if tracking enabled
  if (track_history) {
    util_matrix <- util_matrix[1:iter,, drop = FALSE]
    diff_matrix <- diff_matrix[1:iter,, drop = FALSE]
    ratio_matrix <- ratio_matrix[1:iter,, drop = FALSE]
  }

  if (debug) {
    cat("\nAlgorithm completed:\n")
    cat("- Total iterations:", iter, "\n")
    cat("- Final rolling average:", round(mean(diff_window), 6), "\n")
    cat("- Converged:", window_filled && mean(diff_window) <= tolerance, "\n")
  }

  # Return complete results
  list(
    utilization = if(track_history) util_matrix else NULL,
    attractiveness = final_attractiveness,
    ratios = final_ratio,
    accessibility = accessibility,
    convergence = list(
      iterations = iter,
      converged = window_filled && mean(diff_window) <= tolerance,
      differences = if(track_history) diff_matrix else NULL,
      ratios = if(track_history) ratio_matrix else NULL,
      type = convergence_type,
      rolling_average = mean(diff_window),
      window_size = window_size
    )
  )
}
