#' #' Run iterative accessibility model until convergence
#' #' @param distance_raster Multi-layer SpatRaster of distances
#' #' @param demand SpatRaster of demand locations
#' #' @param supply Vector of facility capacities
#' #' @param sigma Distance decay parameter
#' #' @param max_iter Maximum iterations (default 100)
#' #' @param tolerance Convergence tolerance (default 1e-6)
#' #' @param debug Logical; if TRUE prints debug information (default FALSE)
#' #' @return List containing final results and convergence information
#' #' @export
#' run_accessibility_model <- function(distance_raster, demand, supply, sigma,
#'                                     max_iter = 100, tolerance = 1e-6,
#'                                     debug = FALSE) {
#'
#'   if(debug) {
#'     cat("Starting validation...\n")
#'     cat("Distance raster dimensions:", dim(distance_raster), "\n")
#'     cat("Demand raster dimensions:", dim(demand), "\n")
#'     cat("Supply vector length:", length(supply), "\n")
#'     cat("Sigma:", sigma, "\n")
#'   }
#'
#'   # Guard against zero supply
#'   if (any(supply == 0)) {
#'     warning("Zero supply facilities detected - removing from analysis")
#'     valid_facilities <- supply > 0
#'     supply <- supply[valid_facilities]
#'     distance_raster <- distance_raster[[valid_facilities]]
#'   }
#'
#'   # Initial setup
#'   if(debug) cat("Computing initial weights...\n")
#'   weights <- compute_weights(distance_raster, method = "gaussian", sigma = sigma)
#'
#'   # Initialize vectors instead of lists
#'   n_facilities <- length(supply)
#'   current_ratio <- supply  # Initial ratio is just supply
#'   current_util <- numeric(n_facilities)
#'
#'   # Initialize tracking matrix for differences
#'   diff_matrix <- matrix(NA, nrow = max_iter, ncol = n_facilities)
#'
#'   # Initialize loop control variables
#'   iter <- 0
#'   max_diff <- Inf
#'
#'   if(debug) cat("Starting main loop...\n")
#'
#'   while (iter < max_iter && max_diff > tolerance) {
#'     if(debug) cat("\nIteration:", iter + 1, "\n")
#'
#'     # Compute choice probabilities based on current ratios
#'     huff_probs <- compute_choice(weights, attractiveness = current_ratio)
#'
#'     # Get utilization probabilities and expected utilization
#'     util_probs <- huff_probs * weights
#'     new_util <- gather_demand(demand, util_probs)$potential_demand
#'
#'     # Safely compute new ratios
#'     new_ratio <- ifelse(new_util > 0, supply / new_util, 0)
#'
#'     # Update iteration counter
#'     iter <- iter + 1
#'
#'     # Calculate and store differences
#'     if (iter > 1) {
#'       diff_matrix[iter,] <- abs(new_util - current_util)
#'       max_diff <- max(diff_matrix[iter,], na.rm = TRUE)
#'
#'       if(debug) {
#'         cat("Max difference:", round(max_diff, 6), "\n")
#'       }
#'     }
#'
#'     # Update current values
#'     current_ratio <- new_ratio
#'     current_util <- new_util
#'   }
#'
#'   # Only compute final accessibility
#'   accessibility <- spread_access(current_ratio, weights)
#'
#'   if(debug) {
#'     cat("\nLoop completed:\n")
#'     cat("Total iterations:", iter, "\n")
#'     cat("Final max difference:", round(max_diff, 6), "\n")
#'     cat("Converged:", max_diff <= tolerance, "\n")
#'   }
#'
#'   # Trim diff_matrix to actual iterations
#'   diff_matrix <- diff_matrix[1:iter,, drop = FALSE]
#'
#'   # Return results
#'   list(
#'     utilization = current_util,
#'     supply_ratio = current_ratio,
#'     accessibility = accessibility,
#'     iterations = iter,
#'     converged = max_diff <= tolerance,
#'     differences = diff_matrix
#'   )
#' }
#'
#'
#' #' Plot facility-wise convergence patterns
#' #'
#' #' @param diff_matrix Matrix of differences where rows are iterations and columns are facilities
#' #' @param log_scale Logical; if TRUE use log scale for y-axis (default TRUE)
#' #' @param facilities Vector of facility IDs to plot. If NULL, plots all facilities
#' #' @param title Optional custom title for the plot
#' #' @return A ggplot object showing convergence patterns
#' #' @importFrom ggplot2 ggplot aes geom_line scale_y_log10 theme_minimal labs
#' #' @export
#' plot_convergence <- function(diff_matrix, log_scale = TRUE,
#'                              facilities = NULL, title = "Convergence Pattern by Facility") {
#'
#'   # Convert matrix to long format for ggplot
#'   n_iter <- nrow(diff_matrix)
#'   n_facilities <- ncol(diff_matrix)
#'
#'   # Create facility names if not provided
#'   if (is.null(facilities)) {
#'     facilities <- paste0("Facility ", 1:n_facilities)
#'   }
#'
#'   # Create data frame for plotting
#'   plot_data <- data.frame(
#'     iteration = rep(1:n_iter, n_facilities),
#'     facility = rep(facilities, each = n_iter),
#'     difference = as.vector(diff_matrix)
#'   )
#'
#'   # Create base plot
#'   p <- ggplot(plot_data, aes(x = iteration, y = difference, color = facility)) +
#'     geom_line() +
#'     theme_minimal() +
#'     labs(
#'       title = title,
#'       x = "Iteration",
#'       y = "Absolute Difference",
#'       color = "Facility"
#'     ) +
#'     theme(
#'       legend.position = "right",
#'       plot.title = element_text(hjust = 0.5),
#'       legend.key.size = unit(0.5, "cm")
#'     )
#'
#'   # Add log scale if requested
#'   if (log_scale) {
#'     p <- p + scale_y_log10()
#'   }
#'
#'   return(p)
#' }
#'
#' #' Plot facility convergence summary statistics
#' #'
#' #' @param diff_matrix Matrix of differences where rows are iterations and columns are facilities
#' #' @param log_scale Logical; if TRUE use log scale for y-axis (default TRUE)
#' #' @param title Optional custom title for the plot
#' #' @return A ggplot object showing summary convergence statistics
#' #' @importFrom ggplot2 ggplot aes geom_line geom_ribbon scale_y_log10 theme_minimal labs
#' #' @export
#' plot_convergence_summary <- function(diff_matrix, log_scale = TRUE,
#'                                      title = "Convergence Pattern Summary") {
#'
#'   # Calculate summary statistics for each iteration
#'   summary_data <- data.frame(
#'     iteration = 1:nrow(diff_matrix),
#'     mean_diff = rowMeans(diff_matrix, na.rm = TRUE),
#'     min_diff = apply(diff_matrix, 1, min, na.rm = TRUE),
#'     max_diff = apply(diff_matrix, 1, max, na.rm = TRUE)
#'   )
#'
#'   # Create plot
#'   p <- ggplot(summary_data, aes(x = iteration)) +
#'     geom_ribbon(aes(ymin = min_diff, ymax = max_diff), alpha = 0.2) +
#'     geom_line(aes(y = mean_diff), color = "blue") +
#'     theme_minimal() +
#'     labs(
#'       title = title,
#'       x = "Iteration",
#'       y = "Difference"
#'     ) +
#'     theme(plot.title = element_text(hjust = 0.5))
#'
#'   # Add log scale if requested
#'   if (log_scale) {
#'     p <- p + scale_y_log10()
#'   }
#'
#'   return(p)
#' }
#'
#'
#' #' Analyze facility convergence patterns
#' #'
#' #' @param util_matrix Matrix of utilization values where rows are iterations and columns are facilities
#' #' @param threshold Numeric threshold for coefficient of variation to flag instability (default 0.5)
#' #' @return List containing diagnostic information about facility patterns
#' #' @export
#' analyze_convergence <- function(util_matrix, threshold = 0.5) {
#'   # Remove the first row which contains NAs
#'   util_matrix <- util_matrix[-1,, drop = FALSE]
#'
#'   n_facilities <- ncol(util_matrix)
#'   n_iter <- nrow(util_matrix)
#'
#'   # Calculate metrics for each facility
#'   diagnostics <- data.frame(
#'     facility = 1:n_facilities,
#'     # Coefficient of variation
#'     cv = sapply(1:n_facilities, function(i) sd(util_matrix[,i]) / mean(util_matrix[,i])),
#'     # Mean utilization
#'     mean_util = colMeans(util_matrix),
#'     # Check for oscillation
#'     oscillation = sapply(1:n_facilities, function(i) {
#'       diffs <- diff(util_matrix[,i])
#'       changes <- diff(sign(diffs))
#'       sum(abs(changes)) / length(diffs)
#'     }),
#'     # Add final utilization value
#'     final_util = util_matrix[n_iter,]
#'   )
#'
#'   # Flag problematic facilities
#'   diagnostics$flags <- ifelse(
#'     diagnostics$cv > threshold, "High variability",
#'     ifelse(diagnostics$oscillation > 0.4, "Oscillating",
#'            ifelse(diagnostics$cv < 0.01, "Stagnant", "OK"))
#'   )
#'
#'   # Sort by severity of issues
#'   diagnostics <- diagnostics[order(diagnostics$cv, decreasing = TRUE),]
#'
#'   # Create plot data
#'   plot_data <- data.frame(
#'     iteration = rep(1:n_iter, n_facilities),
#'     facility = factor(rep(1:n_facilities, each = n_iter)),
#'     utilization = as.vector(util_matrix),
#'     problematic = rep(diagnostics$flags != "OK", each = n_iter)
#'   )
#'
#'   # Generate plot
#'   p <- ggplot(plot_data, aes(x = iteration, y = utilization,
#'                              group = facility,
#'                              color = problematic)) +
#'     geom_line(aes(alpha = problematic)) +
#'     scale_y_log10() +
#'     scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.2)) +
#'     scale_color_manual(values = c("TRUE" = "red", "FALSE" = "grey")) +
#'     theme_minimal() +
#'     labs(
#'       title = "Facility Utilization Patterns",
#'       subtitle = paste(sum(diagnostics$flags != "OK"), "facilities flagged as problematic"),
#'       y = "Utilization (log scale)"
#'     )
#'
#'   return(list(
#'     diagnostics = diagnostics,
#'     plot = p,
#'     problematic_facilities = which(diagnostics$flags != "OK")
#'   ))
#' }
#'
#' #' Plot convergence with highlighted problematic facilities
#' #'
#' #' @param util_matrix Matrix of utilization values
#' #' @param analysis Results from analyze_convergence()
#' #' @param highlight Which type of issues to highlight: "all", "variability", "oscillation", or "convergence"
#' #' @return ggplot object
#' #' @export
#' plot_convergence_diagnosis <- function(util_matrix, analysis,
#'                                        highlight = "all") {
#'
#'   # Prepare data for plotting
#'   n_iter <- nrow(util_matrix)
#'   n_facilities <- ncol(util_matrix)
#'
#'   plot_data <- data.frame(
#'     iteration = rep(1:n_iter, n_facilities),
#'     facility = rep(1:n_facilities, each = n_iter),
#'     utilization = as.vector(util_matrix)
#'   )
#'
#'   # Add problem flags
#'   plot_data$problem <- "OK"
#'   if(highlight == "all" || highlight == "variability") {
#'     plot_data$problem[plot_data$facility %in% analysis$high_variability] <- "High variability"
#'   }
#'   if(highlight == "all" || highlight == "oscillation") {
#'     plot_data$problem[plot_data$facility %in% analysis$oscillating] <- "Oscillating"
#'   }
#'   if(highlight == "all" || highlight == "convergence") {
#'     plot_data$problem[plot_data$facility %in% analysis$not_converging] <- "Not converging"
#'   }
#'
#'   # Create plot
#'   ggplot(plot_data, aes(x = iteration, y = utilization,
#'                         group = facility, color = problem)) +
#'     geom_line(aes(alpha = problem != "OK")) +
#'     scale_y_log10() +
#'     scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.2)) +
#'     scale_color_manual(values = c(
#'       "OK" = "grey",
#'       "High variability" = "red",
#'       "Oscillating" = "orange",
#'       "Not converging" = "purple"
#'     )) +
#'     theme_minimal() +
#'     labs(
#'       title = "Facility Utilization Patterns",
#'       subtitle = "Problematic facilities highlighted",
#'       y = "Utilization (log scale)",
#'       color = "Issue Type"
#'     ) +
#'     theme(legend.position = "right")
#' }
#'
#' #' #' Perform single iteration of accessibility calculation
#' #' #'
#' #' #' @param supply Original supply capacity vector
#' #' #' @param supply_ratio Current supply-to-demand ratios
#' #' #' @param weights Multi-layer SpatRaster of spatial weights
#' #' #' @param demand SpatRaster of demand locations
#' #' #' @return List containing updated state information
#' #' #' @keywords internal
#' #' iterate_accessibility <- function(supply, supply_ratio, weights, demand) {
#' #'   # 1. Compute accessibility with current ratios
#' #'   accessibility <- spread_access(supply_ratio, weights)
#' #'
#' #'   # 2. Compute choice probabilities based on accessibility
#' #'   huff_probs <- compute_choice(weights, attractiveness = supply_ratio)
#' #'
#' #'   # 3. Get utilization probabilities and expected utilization
#' #'   util_probs <- huff_probs * weights
#' #'   new_util <- gather_demand(demand, util_probs)$potential_demand
#' #'
#' #'   # 4. Update supply-to-demand ratios using original supply
#' #'   new_ratio <- supply / new_util
#' #'
#' #'   list(
#' #'     supply_ratio = new_ratio,
#' #'     utilization = new_util,
#' #'     accessibility = accessibility
#' #'   )
#' #' }
#' #'
#' #'
#' #' #' Run iterative accessibility model until convergence
#' #' #'
#' #' #' @param distance_raster Multi-layer SpatRaster of distances
#' #' #' @param demand SpatRaster of demand locations
#' #' #' @param supply Vector of facility capacities
#' #' #' @param sigma Distance decay parameter
#' #' #' @param max_iter Maximum iterations (default 100)
#' #' #' @param tolerance Convergence tolerance (default 1e-6)
#' #' #' @param debug Logical; if TRUE prints debug information (default FALSE)
#' #' #' @return List containing final results and convergence information
#' #' #' @export
#' #' run_accessibility_model <- function(distance_raster, demand, supply, sigma,
#' #'                                     max_iter = 100, tolerance = 1e-6,
#' #'                                     debug = FALSE) {
#' #'
#' #'   # Input validation with debug info
#' #'   if(debug) {
#' #'     cat("Starting validation...\n")
#' #'     cat("Distance raster dimensions:", dim(distance_raster), "\n")
#' #'     cat("Demand raster dimensions:", dim(demand), "\n")
#' #'     cat("Supply vector length:", length(supply), "\n")
#' #'     cat("Sigma:", sigma, "\n")
#' #'   }
#' #'
#' #'   if (!inherits(distance_raster, "SpatRaster")) {
#' #'     stop("distance_raster must be a SpatRaster object")
#' #'   }
#' #'   if (!inherits(demand, "SpatRaster")) {
#' #'     stop("demand must be a SpatRaster object")
#' #'   }
#' #'   if (!is.numeric(supply) || any(supply < 0, na.rm = TRUE)) {
#' #'     stop("supply must be a non-negative numeric vector")
#' #'   }
#' #'   if (length(supply) != nlyr(distance_raster)) {
#' #'     stop("Length of supply must match number of distance layers")
#' #'   }
#' #'
#' #'   # Initial setup
#' #'   if(debug) cat("Computing initial weights...\n")
#' #'   weights <- compute_weights(distance_raster, method = "gaussian", sigma = sigma)
#' #'
#' #'   state <- list(
#' #'     supply_ratio = supply,
#' #'     utilization = NULL,
#' #'     accessibility = NULL
#' #'   )
#' #'
#' #'   # Initialize loop control variables
#' #'   iter <- 0
#' #'   max_diff <- Inf
#' #'
#' #'   if(debug) cat("Starting main loop...\n")
#' #'
#' #'   # Main loop
#' #'   while (iter < max_iter && max_diff > tolerance) {
#' #'     if(debug) {
#' #'       cat("\nIteration:", iter + 1, "\n")
#' #'       cat("Current supply ratios:", round(state$supply_ratio, 4), "\n")
#' #'     }
#' #'
#' #'     # Run single iteration
#' #'     tryCatch({
#' #'       new_state <- iterate_accessibility(supply, state$supply_ratio, weights, demand)
#' #'
#' #'       if(debug) {
#' #'         cat("New utilization:", round(new_state$utilization, 4), "\n")
#' #'       }
#' #'
#' #'     }, error = function(e) {
#' #'       cat("\nError in iterate_accessibility:\n")
#' #'       cat("Current state:\n")
#' #'       str(state)
#' #'       cat("\nError message:", conditionMessage(e), "\n")
#' #'       stop(e)
#' #'     })
#' #'
#' #'     iter <- iter + 1
#' #'
#' #'     # Update convergence metric
#' #'     max_diff <- if (!is.null(state$utilization)) {
#' #'       diff <- abs(new_state$utilization - state$utilization)
#' #'       if(debug) {
#' #'         cat("Differences:", round(diff, 6), "\n")
#' #'         cat("Max difference:", round(max(diff), 6), "\n")
#' #'       }
#' #'       max(diff)
#' #'     } else {
#' #'       if(debug) cat("First iteration - no difference calculation\n")
#' #'       Inf
#' #'     }
#' #'
#' #'     state <- new_state
#' #'   }
#' #'
#' #'   if(debug) {
#' #'     cat("\nLoop completed:\n")
#' #'     cat("Total iterations:", iter, "\n")
#' #'     cat("Final max difference:", round(max_diff, 6), "\n")
#' #'     cat("Converged:", max_diff <= tolerance, "\n")
#' #'   }
#' #'
#' #'   list(
#' #'     utilization = state$utilization,
#' #'     supply_ratio = state$supply_ratio,
#' #'     accessibility = state$accessibility,
#' #'     iterations = iter,
#' #'     converged = max_diff <= tolerance
#' #'   )
#' #' }
#' #' #' #' Run iterative accessibility chain model until convergence
#' #' #' #'
#' #' #' #' Run iterative accessibility model until convergence
#' #' #' #'
#' #' #' #' @param distance_raster Multi-layer SpatRaster of distances
#' #' #' #' @param demand SpatRaster of demand locations
#' #' #' #' @param supply Vector of facility capacities
#' #' #' #' @param sigma Distance decay parameter
#' #' #' #' @param max_iter Maximum iterations (default 100)
#' #' #' #' @param tolerance Convergence tolerance (default 1e-6)
#' #' #' #' @return List containing final results and convergence information
#' #' #' #' @export
#' #' #' run_accessibility_model <- function(distance_raster, demand, supply, sigma,
#' #' #'                                     max_iter = 100, tolerance = 1e-6) {
#' #' #'
#' #' #'   # Initial setup
#' #' #'   weights <- compute_weights(distance_raster, method = "gaussian", sigma = sigma)
#' #' #'
#' #' #'   # Generate sequence of iterations
#' #' #'   results <- lapply(seq_len(max_iter), function(i) {
#' #' #'     if (i == 1) {
#' #' #'       # First iteration uses original supply as ratio
#' #' #'       return(iterate_accessibility(supply, supply, weights, demand))
#' #' #'     } else {
#' #' #'       # Get previous iteration result
#' #' #'       prev <- results[[i - 1]]
#' #' #'
#' #' #'       # Check convergence
#' #' #'       if (!is.null(prev$utilization)) {
#' #' #'         new <- iterate_accessibility(supply, prev$supply_ratio, weights, demand)
#' #' #'         max_diff <- max(abs(new$utilization - prev$utilization))
#' #' #'
#' #' #'         # If converged, add convergence info and return
#' #' #'         if (max_diff < tolerance) {
#' #' #'           new$converged <- TRUE
#' #' #'           new$iterations <- i
#' #' #'           return(new)
#' #' #'         }
#' #' #'       }
#' #' #'
#' #' #'       # Continue iteration
#' #' #'       return(iterate_accessibility(supply, prev$supply_ratio, weights, demand))
#' #' #'     }
#' #' #'   })
#' #' #'
#' #' #'   # Find the final result (either converged or last iteration)
#' #' #'   converged_idx <- Position(function(x) !is.null(x$converged) && x$converged, results)
#' #' #'   final_result <- if (!is.null(converged_idx)) {
#' #' #'     results[[converged_idx]]
#' #' #'   } else {
#' #' #'     last <- results[[max_iter]]
#' #' #'     last$converged <- FALSE
#' #' #'     last$iterations <- max_iter
#' #' #'     last
#' #' #'   }
#' #' #'
#' #' #'   return(final_result)
#' #' #' }
