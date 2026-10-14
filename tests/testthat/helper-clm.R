# Static CLM choice algebra written out from scratch, for checking package
# results: no package kernels, normalization or contraction. `distance` is an
# origin-by-facility matrix; NA or infinite travel contributes no attraction.
# `allocation = "huff_decay"` gives the legacy form that applies the kernel
# twice.
clm_oracle <- function(distance, demand, supply, sigma, v0 = 0,
                       family = "gaussian", beta = 1, kappa = 1,
                       allocation = "clm") {
  K <- switch(family,
    gaussian = exp(-distance^2 / (2 * sigma^2)),
    exponential = exp(-sigma * distance),
    power = distance^(-sigma))
  K[!is.finite(K)] <- 0
  score <- sweep(K, 2, (kappa * supply)^beta, "*")
  denom <- rowSums(score) + v0
  P <- score * ifelse(denom > 0, 1 / denom, 0)
  W <- if (allocation == "clm") P else P * K
  U <- as.vector(crossprod(W, demand))
  r <- ifelse(U > 0, supply / U, 0)
  A <- as.vector(W %*% r)
  rho <- rowSums(W)
  list(kernel = K, denom = denom, probability = P, allocation = W,
       flow = W * demand, utilization = U, ratio = r, A = A, rho = rho,
       abar = ifelse(rho > 0, A / rho, NA_real_),
       outside_share = v0 * ifelse(denom > 0, 1 / denom, 0))
}
