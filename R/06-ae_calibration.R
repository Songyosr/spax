# SPAX-008: shared AE fixed-point derivative helpers --------------------------

#' Fixed-point implicit gradient under F(x, theta) = x - T_theta(x)
#'
#' `jac_state` is dT/dx and `jac_param` is dT/dtheta, both evaluated at the
#' selected fixed point. Returns dx*/dtheta = (I - dT/dx)^(-1) dT/dtheta.
#' @keywords internal
implicit_gradient <- function(jac_state, jac_param) {
  .chck_numeric_matrix(jac_state, "jac_state")
  .chck_numeric_matrix(jac_param, "jac_param")
  jac_state <- .coerce_numeric_matrix(jac_state)
  jac_param <- .coerce_numeric_matrix(jac_param)
  if (nrow(jac_state) != ncol(jac_state)) {
    stop("`jac_state` must be square")
  }
  if (nrow(jac_param) != nrow(jac_state)) {
    stop("`jac_param` must have one row per state element")
  }
  solve(diag(nrow(jac_state)) - jac_state, jac_param)
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
  .chck_numeric_matrix(analytic, "analytic_jac")
  analytic <- .coerce_numeric_matrix(analytic)
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

.coerce_numeric_matrix <- function(x) {
  x <- as.matrix(x)
  storage.mode(x) <- "double"
  x
}
