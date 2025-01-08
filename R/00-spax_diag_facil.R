#' Validate inputs for IFCA facility analysis
#' @param x spax object to validate
#' @param relative_cutoff Numeric between 0 and 1 for defining relative thresholds
#' @param plot Logical for plot request
#' @return Invisible TRUE if validation passes
#' @keywords internal
.chck_ifca_facilities <- function(x, relative_cutoff = NULL, plot) {
  # Use shared IFCA validation
  .assert_ifca(x, check_history = TRUE)

  # Validate additional parameters
  if (!is.null(relative_cutoff)) {
    .assert_numeric(relative_cutoff, "relative_cutoff")
    .assert_range(relative_cutoff, 0, 1, "relative_cutoff")
  }

  .assert_class(plot, "logical", "plot")

  invisible(TRUE)
}

#' Analyze facility-level patterns in IFCA model results
#'
#' @description
#' Evaluates facility-specific behavior in an IFCA model by analyzing utilization
#' patterns, convergence characteristics, and relative performance metrics.
#'
#' @param x A spax object from spax_ifca()
#' @param relative_cutoff Numeric between 0 and 1; threshold for defining relative
#'        utilization patterns (e.g., 0.2 means ±20% from mean). Default NULL uses
#'        quartile-based classification.
#' @param plot Logical; whether to return facility analysis plots (default = FALSE)
#'
#' @return A list containing facility diagnostics:
#'   \describe{
#'     \item{patterns}{Relative utilization patterns and classifications}
#'     \item{convergence}{Facility-specific convergence characteristics}
#'     \item{plots}{Optional ggplot objects if plot=TRUE}
#'   }
#' @export
check_ifca_facilities <- function(x, relative_cutoff = NULL, plot = FALSE) {
  # Validation
  .chck_ifca_facilities(x, relative_cutoff, plot)

  # Core computations
  results <- list(
    patterns = .calc_facility_patterns(x$iterations$history, relative_cutoff),
    convergence = .calc_facility_convergence(x$iterations$history)
  )

  # Handle plotting if requested
  if (plot) {
    results$plots <- list(
      patterns = .plot_facility_patterns(results$patterns),
      convergence = .plot_facility_convergence(results$convergence)
    )
  }

  class(results) <- c("ifca_facilities", "list")
  return(results)
}

#' Calculate facility utilization patterns and classifications
#' @param history Array of iteration history
#' @param relative_cutoff Numeric threshold for pattern classification
#' @return List of pattern metrics and classifications
#' @keywords internal
.calc_facility_patterns <- function(history, relative_cutoff = NULL) {
  # Extract final state - focus on supply/demand ratios
  final_state <- history[dim(history)[1], , , drop = TRUE]
  ratios <- final_state[, 2]  # Column 2 contains supply/demand ratios

  # Calculate relative metrics using ratios
  mean_ratio <- mean(ratios)
  median_ratio <- median(ratios)
  rel_to_mean <- ratios / mean_ratio

  # Classify patterns based on cutoff or quartiles
  if (!is.null(relative_cutoff)) {
    classifications <- cut(rel_to_mean,
                           breaks = c(-Inf, 1 - relative_cutoff, 1 + relative_cutoff, Inf),
                           labels = c("Under-served", "Balanced", "Over-served")
    )
  } else {
    classifications <- cut(ratios,
                           breaks = quantile(ratios, probs = c(0, 0.25, 0.75, 1)),
                           labels = c("Under-served", "Balanced", "Over-served"),
                           include.lowest = TRUE
    )
  }

  list(
    metrics = data.frame(
      ratio = ratios,
      relative_to_mean = rel_to_mean,
      relative_to_median = ratios / median_ratio,
      classification = classifications
    ),
    summary = list(
      quartiles = quantile(ratios, probs = c(0.25, 0.5, 0.75)),
      variation = sd(ratios) / mean_ratio,
      balance_score = 1 - (max(rel_to_mean) - min(rel_to_mean)) / 2
    )
  )
}

#' Calculate additional facility-specific metrics beyond basic convergence
#' @param history Array of iteration history
#' @return Data frame of extended facility metrics
#' @keywords internal
.calc_facility_extended <- function(history) {
  # Use existing convergence metrics as base
  base_metrics <- .calc_stability_metrics(history)

  # Add facility-specific extensions if needed
  # For example: spatial competition, service area dominance, etc.

  # Return combined metrics
  base_metrics
}

#' Plot facility ratio patterns
#' @param patterns List of pattern metrics
#' @return List of ggplot objects
#' @keywords internal
.plot_facility_patterns <- function(patterns) {
  metrics_df <- patterns$metrics

  # Create supply/demand ratio distribution plot
  p <- ggplot(metrics_df, aes(x = relative_to_mean, fill = classification)) +
    geom_histogram(bins = 30, alpha = 0.7) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "darkred") +
    scale_fill_viridis_d(end = 0.8) +
    labs(
      title = "Facility Supply/Demand Distribution",
      x = "Ratio Relative to Mean",
      y = "Count",
      fill = "Service Level"
    ) +
    theme_minimal()

  list(distribution = p)
}

#' Analyze facility-level patterns in IFCA model results
#'
#' @description
#' Evaluates facility-specific behavior focusing on supply/demand ratios and extending
#' basic convergence diagnostics with facility-level detail.
#'
#' @param x A spax object from spax_ifca()
#' @param relative_cutoff Numeric between 0 and 1; threshold for defining relative
#'        service levels (e.g., 0.2 means ±20% from mean). Default NULL uses
#'        quartile-based classification.
#' @param plot Logical; whether to return facility analysis plots (default = FALSE)
#'
#' @return A list containing facility diagnostics:
#'   \describe{
#'     \item{patterns}{Supply/demand ratio patterns and classifications}
#'     \item{metrics}{Extended facility-specific metrics}
#'     \item{plots}{Optional ggplot objects if plot=TRUE}
#'   }
#' @export
check_ifca_facilities <- function(x, relative_cutoff = NULL, plot = FALSE) {
  # Validation
  .chck_ifca_facilities(x, relative_cutoff, plot)

  # Core computations - reuse existing convergence where possible
  convergence <- check_ifca_convergence(x, metrics = c("stability", "efficiency"))

  # Add facility-specific patterns and extensions
  results <- list(
    patterns = .calc_facility_patterns(x$iterations$history, relative_cutoff),
    metrics = .calc_facility_extended(x$iterations$history)
  )

  # Handle plotting if requested
  if (plot) {
    results$plots <- .plot_facility_patterns(results$patterns)
  }

  class(results) <- c("ifca_facilities", "list")
  return(results)
}

#' Calculate facility-specific convergence characteristics
#' @param history Array of iteration history
#' @return Data frame of convergence metrics by facility
#' @keywords internal
.calc_facility_convergence <- function(history) {
  n_iter <- dim(history)[1]
  n_facilities <- dim(history)[2]

  # Calculate metrics for each facility
  metrics <- t(vapply(1:n_facilities, function(i) {
    facility_hist <- history[, i, 1]  # Utilization history for facility i
    changes <- diff(facility_hist)

    # Find convergence iteration (first stable point)
    stable_point <- which(abs(changes) < 1e-6)[1] %||% n_iter

    # Calculate oscillation metrics
    pos_changes <- sum(changes > 0)
    neg_changes <- sum(changes < 0)

    c(
      converge_iter = stable_point,
      total_change = abs(facility_hist[length(facility_hist)] - facility_hist[1]),
      oscillation = sd(changes),
      direction_ratio = pos_changes / neg_changes,
      volatility = sd(abs(changes))
    )
  }, numeric(5)))

  # Convert to data frame with meaningful names
  data.frame(
    converge_iter = metrics[, 1],
    total_change = metrics[, 2],
    oscillation = metrics[, 3],
    direction_ratio = metrics[, 4],
    volatility = metrics[, 5]
  )
}

#' Plot facility utilization patterns
#' @param patterns List of pattern metrics from .calc_facility_patterns
#' @return ggplot object
#' @keywords internal
.plot_facility_patterns <- function(patterns) {
  metrics_df <- patterns$metrics

  # Create relative utilization plot
  p1 <- ggplot(metrics_df, aes(x = relative_to_mean, fill = classification)) +
    geom_histogram(bins = 30, alpha = 0.7) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "darkred") +
    scale_fill_viridis_d(end = 0.8) +
    labs(
      title = "Facility Utilization Distribution",
      x = "Utilization Relative to Mean",
      y = "Count",
      fill = "Classification"
    ) +
    theme_minimal()

  # Create utilization vs ratio scatterplot
  p2 <- ggplot(metrics_df, aes(x = utilization, y = ratio, color = classification)) +
    geom_point(alpha = 0.7) +
    scale_color_viridis_d(end = 0.8) +
    labs(
      title = "Utilization vs Supply/Demand Ratio",
      x = "Utilization",
      y = "Supply/Demand Ratio",
      color = "Classification"
    ) +
    theme_minimal()

  list(distribution = p1, relationship = p2)
}

#' Plot facility convergence characteristics
#' @param convergence Data frame of convergence metrics
#' @return ggplot object
#' @keywords internal
.plot_facility_convergence <- function(convergence) {
  # Create convergence timing plot
  p1 <- ggplot(convergence, aes(x = converge_iter, y = oscillation)) +
    geom_point(aes(size = volatility), alpha = 0.7) +
    scale_size_continuous(name = "Volatility") +
    labs(
      title = "Facility Convergence Patterns",
      x = "Iterations to Convergence",
      y = "Oscillation Magnitude"
    ) +
    theme_minimal()

  # Create stability metrics plot
  p2 <- ggplot(convergence, aes(x = direction_ratio, y = total_change)) +
    geom_point(aes(size = volatility), alpha = 0.7) +
    scale_size_continuous(name = "Volatility") +
    labs(
      title = "Facility Stability Metrics",
      x = "Direction Ratio (positive/negative changes)",
      y = "Total Change"
    ) +
    theme_minimal()

  list(convergence = p1, stability = p2)
}

#' Print method for IFCA facility diagnostics
#' @param x An ifca_facilities object
#' @param ... Additional arguments passed to print
#' @export
print.ifca_facilities <- function(x, ...) {
  cat("IFCA Facility Diagnostics\n")
  cat("------------------------\n")

  # Pattern summary
  cat("\nUtilization Patterns:\n")
  pattern_counts <- table(x$patterns$metrics$classification)
  for (class in names(pattern_counts)) {
    cat(sprintf("  %s utilization: %d facilities\n",
                class, pattern_counts[class]))
  }

  cat(sprintf("\nBalance Score: %.3f", x$patterns$summary$balance_score))
  cat(sprintf("\nVariation Coefficient: %.3f", x$patterns$summary$variation))

  # Convergence summary
  cat("\n\nConvergence Characteristics:\n")
  conv_summary <- summary(x$convergence$converge_iter, na.rm = TRUE)
  cat(sprintf("  Median iterations to convergence: %.1f\n", conv_summary["Median"]))
  cat(sprintf("  Range: %.0f - %.0f iterations\n",
              conv_summary["Min."], conv_summary["Max."]))

  invisible(x)
}
