#' Create convergence trajectory plot
#' @keywords internal
.plot_convergence_trajectory <- function(data, facilities) {
  trajectory_df <- data.frame(
    iteration = rep(seq_len(nrow(data)), ncol(data)),
    utilization = as.vector(data),
    facility = rep(facilities, each = nrow(data))
  )

  ggplot(trajectory_df, aes(x = iteration, y = utilization, color = facility)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Utilization Convergence",
         x = "Iteration",
         y = "Utilization") +
    theme(legend.position = "right")
}
#' Create convergence rate plot
#' @keywords internal
.plot_convergence_rate <- function(changes, facilities) {
  changes_df <- data.frame(
    iteration = rep(seq_len(nrow(changes)), ncol(changes)),
    change = as.vector(log10(abs(changes))),
    facility = rep(facilities, each = nrow(changes))
  )

  ggplot(changes_df, aes(x = iteration, y = change, color = facility)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Convergence Rate",
         x = "Iteration",
         y = "Log10(|Change|)")
}

#' Create iterations to convergence plot
#' @keywords internal
.plot_convergence_speed <- function(summary) {
  ggplot(summary, aes(x = facility, y = convergence_iterations)) +
    geom_col() +
    theme_minimal() +
    labs(title = "Iterations to 90% Convergence",
         x = "Facility",
         y = "Iterations") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#' Create oscillation index plot
#' @keywords internal
.plot_oscillation_index <- function(summary) {
  ggplot(summary, aes(x = facility, y = oscillation_index)) +
    geom_col() +
    theme_minimal() +
    labs(title = "Oscillation Index",
         x = "Facility",
         y = "Index") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#' Create prediction accuracy plots
#' @keywords internal
.plot_accuracy_comparison <- function(summary) {
  ggplot(summary, aes(x = observed, y = predicted)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = 2) +
    theme_minimal() +
    labs(title = "Predicted vs Observed",
         x = "Observed Utilization",
         y = "Predicted Utilization")
}

#' Create error distribution plot
#' @keywords internal
.plot_error_dist <- function(errors) {
  error_df <- data.frame(error = errors$raw)
  ggplot(error_df, aes(x = error)) +
    geom_histogram(bins = 30) +
    theme_minimal() +
    labs(title = "Error Distribution",
         x = "Prediction Error",
         y = "Count")
}

#' Create percent error plot
#' @keywords internal
.plot_percent_error <- function(summary) {
  ggplot(summary, aes(x = facility, y = pct_error)) +
    geom_col() +
    theme_minimal() +
    labs(title = "Percent Error by Facility",
         x = "Facility",
         y = "Percent Error") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#' Create metrics summary plot
#' @keywords internal
.plot_metrics_summary <- function(metrics) {
  metrics_df <- data.frame(
    metric = c("MAE", "RMSE", "MAPE"),
    value = c(metrics$mae, metrics$rmse, metrics$mape)
  )

  ggplot(metrics_df, aes(x = metric, y = value)) +
    geom_col() +
    theme_minimal() +
    labs(title = "Accuracy Metrics",
         x = NULL,
         y = "Value") +
    geom_text(aes(label = sprintf("%.2f", value)), vjust = -0.5)
}
