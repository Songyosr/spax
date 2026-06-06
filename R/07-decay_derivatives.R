# Decay-kernel derivative helpers --------------------------------------------

#' Derivative of log decay weight with respect to the decay parameter
#' @keywords internal
.decay_log_jacobian <- function(method, distance, param) {
  method <- match.arg(method, c("gaussian", "exponential", "power"))
  .chck_numeric_vector(distance, "distance")
  .chck_positive_scalar(param, "param")
  distance <- .coerce_numeric_vector(distance)
  param <- as.numeric(param)
  if (method == "gaussian") {
    return(distance^2 / param^3)
  }
  if (method == "exponential") {
    return(-distance)
  }
  if (any(distance <= 0)) {
    stop("power decay derivative requires positive `distance`")
  }
  -log(distance)
}
