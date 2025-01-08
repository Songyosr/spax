# Validation function --------------------------------------------------------

#' Validate inputs for IFCA convergence checks
#' @param x spax object to validate
#' @param metrics character vector of requested metrics
#' @param plot logical for plot request
#' @param by_facility logical for facility-level breakdowns
#' @param relative_cutoff numeric between 0 and 1 for defining relative thresholds
#' @return Invisible TRUE if validation passes
#' @keywords internal
.chck_ifca_convergence <- function(x, metrics, plot, by_facility = FALSE,
                                   relative_cutoff = NULL) {

  # Validate ifca
  .assert_ifca(x, check_history = TRUE, name = "x")

  # Validate metrics specification
  valid_metrics <- c("speed", "stability", "efficiency")
  if (!all(metrics %in% valid_metrics)) {
    stop(sprintf(
      "Invalid metrics specified. Valid options are: %s",
      paste(valid_metrics, collapse = ", ")
    ))
  }

  # Check plot parameter
  .assert_class(plot, "logical", "plot")

  # Check by_facility parameter
  .assert_class(by_facility, "logical", "by_facility")

  # Check relative_cutoff parameter
  if (!is.null(relative_cutoff)) {
    .assert_numeric(relative_cutoff, "relative_cutoff")
    .assert_range(relative_cutoff, 0, 1, "relative_cutoff")
  }




  invisible(TRUE)
}

# Core computation functions ------------------------------------------------

#' Calculate speed-related convergence metrics
#' @param history Array of iteration history
#' @return List of speed metrics
#' @keywords internal
.calc_speed_metrics <- function(history) {
  n_iter <- dim(history)[1]
  util_means <- apply(history[,,1], 1, mean)

  list(
    iterations = n_iter,
    avg_change_per_iter = mean(diff(util_means)),
    time_to_half_conv = which.min(abs(util_means - 0.5 * max(util_means)))
  )
}

#' Calculate stability-related convergence metrics
#' @param history Array of iteration history
#' @return List of stability metrics
#' @keywords internal
.calc_stability_metrics <- function(history) {
  iter_changes <- apply(history[,,1], 2, diff)

  list(
    max_oscillation = max(abs(iter_changes)),
    avg_oscillation = mean(abs(iter_changes)),
    stability_score = 1 - sd(abs(iter_changes)) / mean(abs(iter_changes))
  )
}

#' Calculate efficiency-related convergence metrics
#' @param history Array of iteration history
#' @return List of efficiency metrics
#' @keywords internal
.calc_efficiency_metrics <- function(history) {
  n_iter <- dim(history)[1]
  final_util <- history[n_iter,,1]
  initial_util <- history[1,,1]

  list(
    convergence_ratio = sum(abs(final_util - initial_util)) / n_iter,
    facility_variation = sd(apply(history[,,1], 2, diff))
  )
}

# Plotting function -------------------------------------------------------

#' Create convergence plot with summary statistics
#' @param history Array of iteration history
#' @param type Character; "trajectory" or "difference"
#' @return ggplot object
#' @keywords internal
.plot_convergence <- function(history, type = c("trajectory", "difference")) {
  type <- match.arg(type)
  n_iter <- dim(history)[1]
  n_facilities <- dim(history)[2]

  if (type == "trajectory") {
    # Prepare trajectory data
    plot_data <- data.frame(
      iteration = rep(1:n_iter, n_facilities),
      facility = factor(rep(1:n_facilities, each = n_iter)),
      value = as.vector(history[,,1])
    )

    # Create trajectory plot
    p <- ggplot(plot_data, aes(x = iteration, y = value)) +
      geom_line(aes(group = facility), color = "grey70", alpha = 0.7) +
      geom_ribbon(stat = "summary",
                  fun.data = mean_cl_normal,
                  alpha = 0.5,
                  fill = "darkred") +
      geom_line(stat = "summary",
                fun = mean,
                color = "darkred",
                size = 1.5) +
      labs(
        title = "Facility Utilization Trajectories",
        x = "Iteration",
        y = "Utilization",
        subtitle = "Darkred line shows mean across facilities with 95% CI"
      )

  } else {  # difference plot
    # Calculate differences
    diffs <- apply(history[,,1], 2, diff)

    # Prepare difference data
    plot_data <- data.frame(
      iteration = rep(1:(n_iter-1), n_facilities),
      facility = factor(rep(1:n_facilities, each = (n_iter-1))),
      value = as.vector(diffs)
    )

    # Create difference plot
    p <- ggplot(plot_data, aes(x = iteration, y = value)) +
      geom_line(aes(group = facility), color = "grey70", alpha = 0.7) +
      geom_ribbon(stat = "summary",
                  fun.data = mean_cl_normal,
                  alpha = 0.5,
                  fill = "darkred") +
      geom_line(stat = "summary",
                fun = mean,
                color = "darkred",
                size = 1.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "darkblue", alpha = 0.5) +
      labs(
        title = "Changes Between Iterations",
        x = "Iteration",
        y = "Change in Utilization",
        subtitle = "Darkred line shows mean change with 95% CI"
      )
  }

  # Common theme elements
  p + theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 9, color = "grey40")
    )
}


# Main function ----------------------------------------------------------

#' Analyze convergence patterns in IFCA model results
#'
#' @description
#' Evaluates convergence behavior of an IFCA model by analyzing iteration history
#' and convergence patterns, with optional facility-level details.
#'
#' @param x A spax object from spax_ifca()
#' @param plot Logical; whether to return convergence plot (default = FALSE)
#' @param metrics Character vector specifying which metrics to calculate:
#'   - "speed": iterations to convergence
#'   - "stability": variance in convergence
#'   - "efficiency": resource usage metrics
#'   Default is all metrics.
#' @param by_facility Logical; whether to include facility-level breakdowns
#'        (default = FALSE)
#' @param relative_cutoff Numeric between 0 and 1; threshold for defining relative
#'        service levels when by_facility=TRUE. Default NULL uses quartile-based
#'        classification.
#' @return A list containing convergence diagnostics:
#'   \describe{
#'     \item{convergence_speed}{Metrics about convergence rate}
#'     \item{stability_metrics}{Measures of solution stability}
#'     \item{facility_metrics}{Optional facility-level metrics if by_facility=TRUE}
#'     \item{plot}{Optional ggplot object if plot=TRUE}
#'   }
#' @export
check_ifca_convergence <- function(x, plot = FALSE,
                                   metrics = c("speed", "stability", "efficiency"),
                                   by_facility = FALSE,
                                   relative_cutoff = NULL) {
  # Validation
  .chck_ifca_convergence(x, metrics, plot, by_facility, relative_cutoff)

  # Extract history data
  history <- x$iterations$history

  # Calculate requested metrics
  results <- list()

  if ("speed" %in% metrics) {
    results$convergence_speed <- .calc_speed_metrics(history)
  }

  if ("stability" %in% metrics) {
    results$stability_metrics <- .calc_stability_metrics(history)
  }

  if ("efficiency" %in% metrics) {
    results$efficiency_metrics <- .calc_efficiency_metrics(history)
  }

  # Add facility-level metrics if requested
  if (by_facility) {
    results$facility_metrics <- .calc_facility_patterns(history, relative_cutoff)
  }

  # Handle plotting
  if (plot) {
    results$plots <- list(
      trajectory = .plot_convergence(history, type = "trajectory"),
      difference = .plot_convergence(history, type = "difference")
    )

    # Add facility-specific plots if requested
    if (by_facility) {
      results$plots$facility_patterns <-
        .plot_facility_patterns(results$facility_metrics)
    }
  }

  class(results) <- c("ifca_convergence", "list")
  return(results)
}

#' Print method for IFCA convergence diagnostics
#' @param x An ifca_convergence object
#' @param ... Additional arguments passed to print
#' @export
print.ifca_convergence <- function(x, ...) {
  cat("IFCA Convergence Diagnostics\n")
  cat("-------------------------\n")

  if (!is.null(x$convergence_speed)) {
    cat("\nConvergence Speed:\n")
    cat("  Iterations to convergence:", x$convergence_speed$iterations, "\n")
    cat("  Average change per iteration:",
        round(x$convergence_speed$avg_change_per_iter, 4), "\n")
    cat("  Iterations to half convergence:",
        x$convergence_speed$time_to_half_conv, "\n")
  }

  if (!is.null(x$stability_metrics)) {
    cat("\nStability Metrics:\n")
    cat("  Maximum oscillation:", round(x$stability_metrics$max_oscillation, 4), "\n")
    cat("  Average oscillation:", round(x$stability_metrics$avg_oscillation, 4), "\n")
    cat("  Stability score:", round(x$stability_metrics$stability_score, 4), "\n")
  }

  if (!is.null(x$efficiency_metrics)) {
    cat("\nEfficiency Metrics:\n")
    cat("  Convergence ratio:", round(x$efficiency_metrics$convergence_ratio, 4), "\n")
    cat("  Facility variation:", round(x$efficiency_metrics$facility_variation, 4), "\n")
  }

  # Add facility-level summary if present
  if (!is.null(x$facility_metrics)) {
    cat("\nFacility-Level Metrics:\n")
    pattern_counts <- table(x$facility_metrics$metrics$classification)
    for (class in names(pattern_counts)) {
      cat(sprintf("  %s: %d facilities\n", class, pattern_counts[class]))
    }
    cat(sprintf("  Balance score: %.3f\n",
                x$facility_metrics$summary$balance_score))
  }

  invisible(x)
}
