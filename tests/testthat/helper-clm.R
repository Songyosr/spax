# Static CLM choice algebra written out from scratch, for checking package
# results: no package kernels, normalization or contraction. `distance` is an
# origin-by-facility matrix; NA or infinite travel contributes no attraction.
# `allocation = "huff_decay"` gives the legacy form that applies the kernel
# twice. A `kernel` given as a function of (distance, sigma), written out by the
# caller, replaces the three named families.
clm_oracle <- function(distance, demand, supply, sigma, v0 = 0,
                       family = "gaussian", beta = 1, kappa = 1,
                       allocation = "clm", kernel = NULL) {
  K <- if (is.function(kernel)) kernel(distance, sigma) else switch(family,
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

# The grouped static CLM written out with clm_oracle() per group, plus the
# pooled quantities. `distance_groups` is a named list of origin-by-facility
# matrices (the same matrix repeated for shared travel), `D_groups` an
# origin-by-group demand matrix with group column names; `sigma`, `v0`, `family`
# and `kernels` (functions of distance and sigma, written out by the caller) are
# scalars or vectors/lists named by group. The pooled allocation is the demand
# weighted `P_bar = sum_g D_g P_g / D`; loads, ratios and coverage follow from
# `P_bar` and the total demand `D`.
clm_group_oracle <- function(distance_groups, D_groups, supply, sigma, v0 = 0,
                             family = "gaussian", kernels = NULL, beta = 1,
                             kappa = 1) {
  groups <- colnames(D_groups)
  by_group <- function(x) {
    if (is.function(x) || (is.atomic(x) && length(x) == 1L && is.null(names(x)))) {
      x <- rep(list(x), length(groups))
    }
    if (is.null(names(x))) names(x) <- groups
    as.list(x)[groups]
  }
  sigma <- by_group(sigma)
  v0 <- by_group(v0)
  family <- by_group(family)
  kernels <- if (is.null(kernels)) stats::setNames(vector("list", length(groups)), groups) else
    by_group(kernels)
  per <- lapply(groups, function(g) {
    clm_oracle(distance_groups[[g]], D_groups[, g], supply, sigma[[g]], v0[[g]],
               family = family[[g]], beta = beta, kappa = kappa,
               kernel = kernels[[g]])
  })
  names(per) <- groups
  D <- rowSums(D_groups)
  P <- Reduce(`+`, lapply(groups, function(g) per[[g]]$allocation * (D_groups[, g] / D)))
  outside <- Reduce(`+`, lapply(groups, function(g) per[[g]]$outside_share * (D_groups[, g] / D)))
  U <- as.vector(crossprod(P, D))
  r <- ifelse(U > 0, supply / U, 0)
  A <- as.vector(P %*% r)
  rho <- rowSums(P)
  list(groups = per, D = D, allocation = P, flow = P * D, rho = rho,
       outside_share = outside, utilization = U,
       utilization_by_group = vapply(per, function(o) o$utilization,
                                     numeric(length(supply))),
       access_by_group = vapply(per, function(o) o$rho, numeric(nrow(D_groups))),
       outside_share_by_group = vapply(per, function(o) o$outside_share,
                                       numeric(nrow(D_groups))),
       ratio = r, A = A, abar = ifelse(rho > 0, A / rho, NA_real_))
}
