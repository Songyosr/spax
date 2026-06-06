# SPAX-008: shared AE calibration helpers ------------------------------------

#' Fixed-point implicit gradient under F(x, theta) = x - T_theta(x)
#'
#' `jac_state` is dT/dx and `jac_theta` is dT/dtheta, both evaluated at the
#' selected fixed point. Returns dx*/dtheta = (I - dT/dx)^(-1) dT/dtheta.
#' @keywords internal
implicit_gradient <- function(jac_state, jac_theta) {
  jac_state <- .ae_numeric_matrix(jac_state, "jac_state")
  jac_theta <- .ae_numeric_matrix(jac_theta, "jac_theta")
  if (nrow(jac_state) != ncol(jac_state)) {
    stop("`jac_state` must be square")
  }
  if (nrow(jac_theta) != nrow(jac_state)) {
    stop("`jac_theta` must have one row per state element")
  }
  solve(diag(nrow(jac_state)) - jac_state, jac_theta)
}

#' Weighted sum of squared errors
#'
#' Computes sum((predicted - observed)^2 / (observed + eta)).
#' @keywords internal
wsse_loss <- function(predicted, observed, eta = 1) {
  predicted <- .ae_numeric_vector(predicted, "predicted")
  observed <- .ae_numeric_vector(observed, "observed")
  eta <- .ae_scalar_nonnegative(eta, "eta")
  if (length(predicted) != length(observed)) {
    stop("`predicted` and `observed` must have the same length")
  }
  sum((predicted - observed)^2 / (observed + eta))
}

#' Gradient of WSSE with respect to parameters
#'
#' `sensitivity` is d predicted / d theta, with one row per observation and one
#' column per parameter.
#' @keywords internal
wsse_grad <- function(predicted, observed, sensitivity, eta = 1) {
  predicted <- .ae_numeric_vector(predicted, "predicted")
  observed <- .ae_numeric_vector(observed, "observed")
  sensitivity <- .ae_numeric_matrix(sensitivity, "sensitivity")
  eta <- .ae_scalar_nonnegative(eta, "eta")
  if (length(predicted) != length(observed)) {
    stop("`predicted` and `observed` must have the same length")
  }
  if (nrow(sensitivity) != length(predicted)) {
    stop("`sensitivity` must have one row per prediction")
  }
  weights <- 2 * (predicted - observed) / (observed + eta)
  as.vector(crossprod(sensitivity, weights))
}

#' Derivative of log decay weight with respect to sigma
#' @keywords internal
decay_dlog_dsigma <- function(method, distance, sigma) {
  method <- match.arg(method, c("gaussian", "exponential", "power"))
  distance <- .ae_numeric_vector(distance, "distance")
  sigma <- .ae_scalar_positive(sigma, "sigma")
  if (method == "gaussian") {
    return(distance^2 / sigma^3)
  }
  if (method == "exponential") {
    return(-distance)
  }
  if (any(distance <= 0)) {
    stop("power decay derivative requires positive `distance`")
  }
  -log(distance)
}

#' Check an analytic Jacobian against finite differences
#' @keywords internal
grad_check <- function(fn, x, analytic_jac, ..., eps = 1e-6,
                       tolerance = 1e-6,
                       method = c("central", "forward")) {
  fd <- fd_jacobian(fn, x, ..., eps = eps, method = method)
  analytic <- if (is.function(analytic_jac)) {
    analytic_jac(x, ...)
  } else {
    analytic_jac
  }
  analytic <- .ae_numeric_matrix(analytic, "analytic_jac")
  if (!identical(dim(fd), dim(analytic))) {
    stop("analytic and finite-difference Jacobians must have the same dimensions")
  }
  diff <- analytic - fd
  denom <- pmax(abs(fd), abs(analytic), 1)
  max_abs <- max(abs(diff))
  max_rel <- max(abs(diff) / denom)
  list(
    passed = max_abs <= tolerance || max_rel <= tolerance,
    max_abs = max_abs,
    max_rel = max_rel,
    analytic = analytic,
    finite_difference = fd
  )
}

.ae_numeric_matrix <- function(x, name) {
  if (!is.numeric(x) && !is.matrix(x)) {
    stop("`", name, "` must be numeric")
  }
  x <- as.matrix(x)
  storage.mode(x) <- "double"
  if (!all(is.finite(x))) {
    stop("`", name, "` must contain only finite values")
  }
  x
}

.ae_scalar_nonnegative <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0) {
    stop("`", name, "` must be a nonnegative scalar")
  }
  as.numeric(x)
}
