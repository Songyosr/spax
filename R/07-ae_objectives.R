# AE outer-objective helpers --------------------------------------------------

# Keep the bodies of .weighted_sse_loss() and .poisson_loss() unchanged, plain
# stop() included: fits store their loss function and later steps compare it by
# body (.same_function()), so a changed body would make saved fits look as if
# they used another loss.

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
  # Nothing is copied for the floor unless a cell is floored: one logical vector, then
  # a copy of `predicted` and a zeroing of those cells only when there are any. The
  # values are those of 1 - observed / pmax(predicted, eps), zero where floored.
  floored <- predicted < eps
  any_floored <- any(floored)
  if (any_floored) predicted[floored] <- eps
  weights <- 1 - observed / predicted          # d NLL / d mu_j
  if (any_floored) weights[floored] <- 0       # the floored loss is flat there
  as.vector(crossprod(sensitivity, weights))
}

# The floor a built-in Poisson fit used: its stored eps, or 1e-9 (the default of
# .poisson_loss) for fits that stored none, which is every fit made before the
# default floor of fit_allocation() moved to 1e-250. NULL for any other loss.
.poisson_floor_eps <- function(best) {
  if (!.same_function(best$loss_fn, .poisson_loss)) return(NULL)
  if ("eps" %in% names(best$loss_args)) best$loss_args$eps else 1e-9
}

# Cells with positive observed counts that a selected Poisson fit models below its floor
# (zero included): the loss is capped there, so the fit is not the Poisson
# maximum likelihood estimate. NULL for any other loss.
.poisson_floor <- function(best) {
  eps <- .poisson_floor_eps(best)
  if (is.null(eps)) return(NULL)
  mask <- if (is.null(best$target_mask)) rep(TRUE, length(best$observed)) else best$target_mask
  observed <- as.numeric(best$observed)[mask]
  predicted <- as.numeric(best$predicted)[mask]
  c(eps = eps, positive_below = sum(observed > 0 & predicted < eps))
}

# Loss arguments as the loss sees them: a built-in Poisson fit without a stored
# eps used the default 1e-9 of .poisson_loss.
.effective_loss_args <- function(best) {
  args <- best$loss_args
  eps <- .poisson_floor_eps(best)
  if (!is.null(eps) && !"eps" %in% names(args)) args$eps <- eps
  args
}

# Whether two fits' losses use the same arguments. Built-in Poisson fits are
# compared by their floors, a missing eps meaning 1e-9; different floors still
# give the same criterion when no masked prediction of either fit lies below the
# larger floor, since the floor then changes neither loss.
.same_loss_args <- function(a, b) {
  args_a <- .effective_loss_args(a)
  args_b <- .effective_loss_args(b)
  if (identical(args_a, args_b)) return(TRUE)
  eps_a <- .poisson_floor_eps(a)
  eps_b <- .poisson_floor_eps(b)
  if (is.null(eps_a) || is.null(eps_b) ||
      !identical(args_a[names(args_a) != "eps"], args_b[names(args_b) != "eps"])) return(FALSE)
  higher <- max(eps_a, eps_b)
  below <- function(f) {
    mask <- if (is.null(f$target_mask)) rep(TRUE, length(f$predicted)) else f$target_mask
    any(as.numeric(f$predicted)[mask] < higher)
  }
  !below(a) && !below(b)
}

# Warning text for a selected Poisson fit with positive counts under its floor. For an
# outside-count output the causes include an outside count where the outside weight is 0.
.poisson_floor_message <- function(floor_info, output = NULL) {
  n <- floor_info[["positive_below"]]
  outside <- is.character(output) && length(output) == 1L && !is.na(output) &&
    output %in% names(.outside_outputs)
  sprintf(paste0("%d %s with positive observed counts %s modelled below the Poisson floor ",
                 "(eps = %g, zero included); the loss is capped there, so the fit is not the ",
                 "Poisson maximum likelihood estimate. Look for observed counts on absent edges",
                 if (outside) {
                   paste0(", at facilities with zero supply or on an outside count where ",
                          "the outside weight is 0, ")
                 } else " or at facilities with zero supply, ",
                 "or use a kernel with a heavier tail."),
          n, if (n == 1) "cell" else "cells", if (n == 1) "is" else "are", floor_info[["eps"]])
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
