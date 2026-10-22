# SPAX-008: shared AE fixed-point derivative helpers --------------------------

#' Fixed-point implicit gradient under F(x, theta) = x - T_theta(x)
#'
#' `jac_state` is dT/dx and `jac_param` is dT/dtheta, both evaluated at the
#' selected fixed point. Returns dx*/dtheta = (I - dT/dx)^(-1) dT/dtheta.
#' @noRd
implicit_gradient <- function(jac_state, jac_param) {
  .chck_numeric_matrix(jac_state, "jac_state")
  .chck_numeric_matrix(jac_param, "jac_param")
  jac_state <- .coerce_numeric_matrix(jac_state)
  jac_param <- .coerce_numeric_matrix(jac_param)
  if (nrow(jac_state) != ncol(jac_state)) {
    .stop("`jac_state` must be square")
  }
  if (nrow(jac_param) != nrow(jac_state)) {
    .stop("`jac_param` must have one row per state element")
  }
  solve(diag(nrow(jac_state)) - jac_state, jac_param)
}

.coerce_numeric_matrix <- function(x) {
  x <- as.matrix(x)
  storage.mode(x) <- "double"
  x
}
