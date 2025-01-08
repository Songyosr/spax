#' Validate iteration history array
#' @param history Array of iteration history values
#' @param metric_names Character vector of metric names (optional)
#' @keywords internal
.chck_history <- function(history, metric_names = NULL) {
  # Basic class validation
  if (!is.array(history)) {
    stop("history must be an array")
  }

  # Dimension validation
  if (length(dim(history)) != 3) {
    stop("history must be a 3D array [iterations, units, metrics]")
  }

  # Metric names validation if provided
  if (!is.null(metric_names)) {
    if (length(metric_names) != dim(history)[3]) {
      stop("Length of metric_names must match number of metrics in history")
    }
  }

  invisible(TRUE)
}

#' Calculate changes between iterations
#' @param x Numeric matrix [iterations, units] or vector
#' @param type Character; "absolute" or "relative"
#' @param na.rm Logical; whether to remove NA values
#' @return Matrix or vector of changes
#' @keywords internal
.help_calc_changes <- function(x, type = "absolute", na.rm = TRUE) {
  if (type == "absolute") {
    changes <- diff(x)
  } else if (type == "relative") {
    changes <- diff(x) / abs(x[-nrow(x)])
  } else {
    stop("type must be 'absolute' or 'relative'")
  }

  if (na.rm) {
    # Replace Inf/NaN with NA then handle NAs
    changes[!is.finite(changes)] <- NA
  }

  return(changes)
}

#' Calculate rolling statistics
#' @param x Numeric vector
#' @param window Integer window size
#' @param stats Character vector of statistics to compute
#' @return List of rolling statistics
#' @keywords internal
.help_calc_rolling_stats <- function(x, window,
                                     stats = c("mean", "max", "sd")) {
  n <- length(x)
  if (n < window) {
    stop("Window size larger than data length")
  }

  # Initialize results
  result <- list()

  # Calculate requested statistics
  for (stat in stats) {
    result[[stat]] <- sapply(
      window:n,
      function(i) {
        window_data <- x[(i - window + 1):i]
        switch(stat,
               "mean" = mean(window_data, na.rm = TRUE),
               "max" = max(abs(window_data), na.rm = TRUE),
               "sd" = sd(window_data, na.rm = TRUE))
      }
    )
  }

  return(result)
}

#' Analyze convergence patterns in iteration history
#'
#' @description
#' Analyzes convergence patterns in iteration history data, including
#' change magnitudes, convergence rates, and oscillation detection.
#'
#' @param history Array [iterations, units, metrics] of iteration history
#' @param metric_names Character vector of metric names
#' @param window_size Integer size of rolling window for statistics
#' @param type Character; "absolute" or "relative" changes
#' @return List containing convergence statistics
#' @keywords internal
analyze_convergence <- function(history, metric_names = NULL,
                                window_size = 5, type = "absolute") {
  # Validate inputs
  .chck_history(history, metric_names)

  # Dimensions
  n_iter <- dim(history)[1]
  n_units <- dim(history)[2]
  n_metrics <- dim(history)[3]

  # Use provided or default metric names
  if (is.null(metric_names)) {
    metric_names <- paste0("metric", 1:n_metrics)
  }

  # Initialize results list
  results <- list(
    iterations = n_iter,
    units = n_units,
    metrics = metric_names,
    changes = list(),
    rolling = list(),
    oscillation = list()
  )

  # Analyze each metric
  for (m in 1:n_metrics) {
    metric_data <- history[, , m, drop = FALSE]
    metric_name <- metric_names[m]

    # Calculate changes
    changes <- .help_calc_changes(metric_data, type = type)

    # Calculate rolling statistics
    rolling_stats <- .help_calc_rolling_stats(
      apply(abs(changes), 1, max),
      window = min(window_size, n_iter - 1)
    )

    # Check for oscillation using autocorrelation
    if (n_iter > 3) {
      acf_result <- acf(apply(changes, 1, mean),
                        lag.max = min(10, n_iter - 2),
                        plot = FALSE)
      oscillation <- list(
        autocorr = acf_result$acf[-1],
        has_oscillation = any(abs(acf_result$acf[-1]) > 0.5)
      )
    } else {
      oscillation <- list(
        autocorr = numeric(0),
        has_oscillation = NA
      )
    }

    # Store results for this metric
    results$changes[[metric_name]] <- changes
    results$rolling[[metric_name]] <- rolling_stats
    results$oscillation[[metric_name]] <- oscillation
  }

  # Add convergence assessment
  results$converged <- all(
    sapply(results$rolling, function(x) tail(x$max, 1) < 1e-6)
  )

  return(results)
}

#' Analyze distribution patterns
#'
#' @description
#' Analyzes distribution of values using flexible threshold determination methods.
#' Can be used for analyzing ratios, utilization patterns, or any numeric distribution.
#'
#' @param values Numeric vector of values to analyze
#' @param method List specifying threshold determination method:
#'        \itemize{
#'          \item type: "mean", "median", or "custom"
#'          \item threshold: For mean/median, number of SD/MAD units
#'                          For custom, c(lower, upper) bounds
#'        }
#' @param categories Character vector of category labels
#' @return List of distribution statistics and categorization
#' @keywords internal
analyze_distribution <- function(values,
                                 method = list(type = "median", threshold = 1.5),
                                 categories = c("Low", "Normal", "High")) {
  # Input validation
  if (!is.numeric(values)) {
    stop("values must be numeric")
  }

  if (!is.list(method) || !all(c("type", "threshold") %in% names(method))) {
    stop("method must be a list with 'type' and 'threshold' elements")
  }

  if (!method$type %in% c("mean", "median", "custom")) {
    stop("method$type must be 'mean', 'median', or 'custom'")
  }

  # Calculate basic statistics
  stats <- list(
    mean = mean(values, na.rm = TRUE),
    median = median(values, na.rm = TRUE),
    sd = sd(values, na.rm = TRUE),
    mad = mad(values, na.rm = TRUE),
    range = range(values, na.rm = TRUE)
  )

  # Determine thresholds
  if (method$type == "mean") {
    center <- stats$mean
    spread <- stats$sd
    thresholds <- center + c(-1, 1) * method$threshold * spread
  } else if (method$type == "median") {
    center <- stats$median
    spread <- stats$mad
    thresholds <- center + c(-1, 1) * method$threshold * spread
  } else { # custom
    thresholds <- method$threshold
  }

  # Categorize values
  cats <- cut(values,
              breaks = c(-Inf, thresholds, Inf),
              labels = categories,
              include.lowest = TRUE
  )

  # Return results
  list(
    statistics = stats,
    method = method,
    thresholds = thresholds,
    categories = cats,
    category_counts = table(cats)
  )
}

#' Create standard diagnostic plots
#'
#' @description
#' Creates a collection of standard diagnostic plots for iteration analysis.
#'
#' @param convergence_results Results from analyze_convergence()
#' @param distribution_results Results from analyze_distribution()
#' @return List of plot functions
#' @keywords internal
create_diagnostic_plots <- function(convergence_results = NULL,
                                    distribution_results = NULL) {
  plots <- list()

  # Convergence plots if available
  if (!is.null(convergence_results)) {
    plots$convergence <- function() {
      # Setup multi-panel plot
      n_metrics <- length(convergence_results$metrics)
      par(mfrow = c(n_metrics, 1))

      # Plot each metric
      for (metric in convergence_results$metrics) {
        rolling <- convergence_results$rolling[[metric]]

        plot(rolling$max,
             type = "l", col = "red",
             main = paste("Convergence:", metric),
             xlab = "Iteration", ylab = "Change Magnitude",
             ylim = range(c(rolling$mean, rolling$max))
        )
        lines(rolling$mean, col = "blue")
        legend("topright",
               legend = c("Maximum", "Mean"),
               col = c("red", "blue"),
               lty = 1
        )
      }
    }
  }

  # Distribution plots if available
  if (!is.null(distribution_results)) {
    plots$distribution <- function() {
      par(mfrow = c(1, 2))

      # Histogram with thresholds
      hist(distribution_results$values,
           main = "Value Distribution",
           xlab = "Value",
           breaks = 30
      )
      abline(v = distribution_results$thresholds,
             col = "red",
             lty = 2
      )

      # Category counts
      barplot(distribution_results$category_counts,
              main = "Category Distribution",
              col = c("red", "green", "blue")
      )
    }
  }

  return(plots)
}
