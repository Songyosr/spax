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

#' Poisson negative log-likelihood for count calibration targets
#'
#' Up to the data constant `sum(log(y!))`, `NLL = sum(mu - y log mu)` with `mu`
#' the predicted count. Jointly fitting `(sigma, v0)` uses both the count total
#' and its facility distribution. It does not generally force an exact total
#' match; that requires an explicit total-matching constraint or a suitable
#' independently fitted scale parameter.
#' @keywords internal
.poisson_loss <- function(predicted, observed, eps = 1e-9) {
  .chck_numeric_vector(predicted, "predicted")
  .chck_numeric_vector(observed, "observed")
  .chck_positive_scalar(eps, "eps")
  predicted <- pmax(.coerce_numeric_vector(predicted), as.numeric(eps))
  observed <- .coerce_numeric_vector(observed)
  if (length(predicted) != length(observed)) {
    stop("`predicted` and `observed` must have the same length")
  }
  sum(predicted - observed * log(predicted))
}

#' Gradient of the Poisson NLL with respect to parameters
#'
#' This is the gradient of [.poisson_loss()], floor included. A prediction
#' below `eps` adds the constant `eps - y log(eps)` to the loss, so it adds
#' nothing to the gradient, whatever its observed count. Quasi-Newton
#' optimizers check every step against the loss, so a gradient that kept the
#' slope `1 - y / eps` below the floor would point where the loss does not
#' fall and stop line searches early (DEC-029).
#' @keywords internal
#' @md
.poisson_gradient <- function(predicted, observed, sensitivity, eps = 1e-9) {
  .chck_numeric_vector(predicted, "predicted")
  .chck_numeric_vector(observed, "observed")
  .chck_numeric_matrix(sensitivity, "sensitivity")
  .chck_positive_scalar(eps, "eps")
  eps <- as.numeric(eps)
  predicted <- .coerce_numeric_vector(predicted)
  observed <- .coerce_numeric_vector(observed)
  sensitivity <- .coerce_numeric_matrix(sensitivity)
  if (length(predicted) != length(observed)) {
    stop("`predicted` and `observed` must have the same length")
  }
  if (nrow(sensitivity) != length(predicted)) {
    stop("`sensitivity` must have one row per prediction")
  }
  floored <- predicted < eps
  predicted <- pmax(predicted, eps)
  weights <- 1 - observed / predicted          # d NLL / d mu_j
  weights[floored] <- 0                        # the floored loss is flat there
  as.vector(crossprod(sensitivity, weights))
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
