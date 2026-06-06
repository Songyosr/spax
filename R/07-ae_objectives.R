# AE outer-objective helpers --------------------------------------------------

#' Weighted sum of squared errors
#'
#' Computes sum((predicted - observed)^2 / (observed + eta)).
#' @keywords internal
.weighted_sse_loss <- function(predicted, observed, eta = 1) {
  .chck_numeric_vector(predicted, "predicted")
  .chck_numeric_vector(observed, "observed")
  .chck_nonnegative_scalar(eta, "eta")
  predicted <- .coerce_numeric_vector(predicted)
  observed <- .coerce_numeric_vector(observed)
  eta <- as.numeric(eta)
  if (length(predicted) != length(observed)) {
    stop("`predicted` and `observed` must have the same length")
  }
  sum((predicted - observed)^2 / (observed + eta))
}

#' Gradient of weighted SSE with respect to parameters
#'
#' `sensitivity` is d predicted / d theta, with one row per observation and one
#' column per parameter.
#' @keywords internal
.weighted_sse_gradient <- function(predicted, observed, sensitivity, eta = 1) {
  .chck_numeric_vector(predicted, "predicted")
  .chck_numeric_vector(observed, "observed")
  .chck_numeric_matrix(sensitivity, "sensitivity")
  .chck_nonnegative_scalar(eta, "eta")
  predicted <- .coerce_numeric_vector(predicted)
  observed <- .coerce_numeric_vector(observed)
  sensitivity <- .coerce_numeric_matrix(sensitivity)
  eta <- as.numeric(eta)
  if (length(predicted) != length(observed)) {
    stop("`predicted` and `observed` must have the same length")
  }
  if (nrow(sensitivity) != length(predicted)) {
    stop("`sensitivity` must have one row per prediction")
  }
  weights <- 2 * (predicted - observed) / (observed + eta)
  as.vector(crossprod(sensitivity, weights))
}
