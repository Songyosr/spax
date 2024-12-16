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

#' Iterative Spatial Accessibility Chain Algorithm
#'
#' @description
#' Implements an iterative spatial accessibility model that finds equilibrium between
#' supply and demand through a chain of facility choices. The model combines Huff-based
#' choice probabilities with distance decay to simulate how demand is distributed across
#' facilities, using a learning rate to dampen oscillations and achieve stable convergence.
#'
#' The algorithm follows these steps in each iteration:
#' 1. Compute choice probabilities based on facility attractiveness
#' 2. Calculate expected utilization at each facility
#' 3. Update supply-demand ratios
#' 4. Adjust facility attractiveness using learning rate
#' 5. Check for convergence
#'
#' The function implements two convergence criteria:
#' - "utilization": Checks for stability in facility utilization
#' - "ratio": Checks for stability in supply-demand ratios
#'
#' @param distance_raster Multi-layer SpatRaster where each layer represents distances
#'        to one facility. All layers must share the same extent and resolution.
#' @param demand SpatRaster of demand distribution (e.g., population density).
#'        Must have same extent and resolution as distance_raster.
#' @param supply Numeric vector of facility capacities. Length must match number
#'        of layers in distance_raster.
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
#' @param snap Logical; if TRUE skips input validation for faster execution
#' @param internal_snap Logical; if TRUE passes snap=TRUE to internal functions
#' @param debug Logical; if TRUE provides detailed convergence information
#'
#' @return A list containing:
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
#'     }
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic usage with Gaussian decay
#' result <- run_access_chain(
#'   distance_raster = dist_rast,
#'   demand = pop_rast,
#'   supply = facility_capacity,
#'   sigma = 30,
#'   method = "gaussian"
#' )
#'
#' # Using custom decay function
#' custom_decay <- function(distance, sigma, k) {
#'   1 / (1 + (distance/(sigma*k))^2)
#' }
#' result <- run_access_chain(
#'   distance_raster = dist_rast,
#'   demand = pop_rast,
#'   supply = facility_capacity,
#'   sigma = 30,
#'   method = custom_decay,
#'   decay_params = list(k = 0.5)
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

  # Only do this if not in snap mode
  if (!snap) {
    # Validate inputs
    validate_access_chain_inputs(
      distance_raster = distance_raster,
      demand = demand, supply = supply,
      lambda = lambda, sigma = sigma, method = method)
    # Validate max_iter and tolerance
    if (!(max_iter > 0 && is.numeric(max_iter) && max_iter == floor(max_iter)))
      stop("max_iter must be a positive integer")

    if (!(tolerance > 0 && is.numeric(tolerance)))
      stop("tolerance must be positive")
  }

  convergence_type <- match.arg(convergence_type)

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

  # Guard against zero supply (keep even in snap mode for safety)
  if (any(supply == 0)) {
    warning("Zero supply facilities detected - removing from analysis")
    valid_facilities <- supply > 0
    supply <- supply[valid_facilities]
    distance_raster <- distance_raster[[valid_facilities]]
  }

  # Get facility names
  facility_names <- names(distance_raster)
  if (is.null(facility_names) && !is.null(names(supply))) {
    facility_names <- names(supply)
  }
  n_facilities <- length(supply)
  if (is.null(facility_names)) {
    facility_names <- paste0("facility_", seq_len(n_facilities))
  }

  # Initial setup
  if (debug) cat("\nInitializing model components...\n")

  # Pass decay_params to compute_weights if using custom method
  weights <- do.call(compute_weights,
                     c(list(distance = distance_raster,
                            method = method,
                            sigma = sigma,
                            snap = internal_snap),
                       decay_params))


  # Initialize history matrices if tracking enabled
  if (track_history) {
    iter_names <- paste0("iter_", seq_len(max_iter))
    util_matrix <- matrix(NA_real_, nrow = max_iter, ncol = n_facilities,
                          dimnames = list(iter_names, facility_names))
    diff_matrix <- matrix(NA_real_, nrow = max_iter, ncol = n_facilities,
                          dimnames = list(iter_names, facility_names))
    ratio_matrix <- matrix(NA_real_, nrow = max_iter, ncol = n_facilities,
                           dimnames = list(iter_names, facility_names))
  } else {
    util_matrix <- diff_matrix <- ratio_matrix <- NULL
  }

  # Initialize state variables
  current_attractiveness <- setNames(supply, facility_names)
  current_util <- setNames(numeric(n_facilities), facility_names)
  current_ratio <- setNames(supply, facility_names)

  # Initialize loop control variables
  iter <- 0
  max_diff <- Inf

  # Initialize rolling window buffer
  diff_window <- numeric(window_size)
  window_idx <- 1
  window_filled <- FALSE

  if (debug) cat("Starting main iteration loop...\n")

  while (iter < max_iter && max_diff > tolerance) {
    iter <- iter + 1
    if (debug) cat("\nIteration:", iter, "\n")

    # Step 1: Compute choice probabilities
    huff_probs <- compute_choice(weights,
                                 attractiveness = current_attractiveness,
                                 snap = internal_snap)

    # Step 2: Calculate utilization
    util_probs <- huff_probs * weights
    new_util <- weighted_gather(demand, util_probs,
                              snap = internal_snap)$weighted_sum

    # Step 3: Compute supply-demand ratios
    new_ratio <- ifelse(new_util > 0, supply / new_util, 0)

    # Step 4: Update attractiveness with learning rate
    new_attractiveness <- (1 - lambda) * current_attractiveness +
      lambda * (new_ratio)

    # Update history if tracking enabled
    if (track_history) {
      util_matrix[iter, ] <- new_util
      ratio_matrix[iter, ] <- new_ratio
      if (iter > 1) {
        diff_matrix[iter,] <- if (convergence_type == "utilization") {
          abs(new_util - current_util)
        } else {
          abs(new_ratio - current_ratio)
        }
      }
    }

    # Calculate max difference for convergence
    if (iter > 1) {
      max_diff <- if (convergence_type == "utilization") {
        max(abs(new_util - current_util), na.rm = TRUE)
      } else {
        max(abs(new_ratio - current_ratio), na.rm = TRUE)
      }
      # Update rolling window
      diff_window[window_idx] <- current_diff
      window_idx <- (window_idx %% window_size) + 1
      if (iter >= window_size) window_filled <- TRUE

      if (debug) {
        cat("- Current difference:", round(current_diff, 6), "\n")
        cat("- Rolling average:", round(mean(diff_window), 6), "\n")
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
  accessibility <- weighted_spread(current_ratio, weights,
                                 snap = internal_snap)

  # Trim matrices to actual iterations if tracking enabled
  if (track_history) {
    util_matrix <- util_matrix[1:iter,, drop = FALSE]
    diff_matrix <- diff_matrix[1:iter,, drop = FALSE]
    ratio_matrix <- ratio_matrix[1:iter,, drop = FALSE]
  }

  if (debug) {
    cat("\nAlgorithm completed:\n")
    cat("- Total iterations:", iter, "\n")
    cat("- Final max difference:", round(max_diff, 6), "\n")
    cat("- Converged:", max_diff <= tolerance, "\n")
  }

  # Return results with preserved names
  list(
    utilization = if(track_history) util_matrix else NULL,
    attractiveness = current_attractiveness,
    ratios = current_ratio,
    accessibility = accessibility,
    convergence = list(
      iterations = iter,
      converged = max_diff <= tolerance,
      differences = if(track_history) diff_matrix else NULL,
      ratios = if(track_history) ratio_matrix else NULL,
      type = convergence_type,
      rolling_average = mean(diff_window),  # Add final rolling average
      window_size = window_size
    )
  )
}
