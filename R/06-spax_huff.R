# SPAX-029: private static Huff / aggregate-CLM allocation map ---------------
#
# The no-state member of the AE ladder (DEC-012): attractiveness is exogenous,
# allocation is one Huff pass (optionally with an outside option), and the
# fixed-point map is constant (dT/dstate = 0), so the equilibrium is reached in
# a single pass. Default allocation is the CLM share itself. The explicit
# `huff_decay` form retains the historical Huff-then-attenuate kernel, which
# equals one HAAE allocation pass at the same attractiveness (SPAX-033).

#' Exogenous Huff attractiveness a_j = (kappa * S_j)^beta
#'
#' Classic Huff uses `beta = 1` (attractiveness proportional to supply). A
#' global scale on `a` cancels when `v0 = 0`. With a positive outside option,
#' scaling attractiveness changes the inside/outside balance unless `v0` is
#' scaled by the same factor.
#' @keywords internal
.huff_attractiveness <- function(supply, kappa = 1, beta = 1) {
  .chck_numeric_vector(supply, "supply")
  .chck_positive_scalar(kappa, "kappa")
  .chck_positive_scalar(beta, "beta")
  supply <- .coerce_numeric_vector(supply)
  a <- (as.numeric(kappa) * supply)^as.numeric(beta)
  a[!is.finite(a)] <- 0
  a
}

#' Evaluate the static Huff allocation and derived outputs at exogenous a
#'
#' `v0` is the optional outside-option mass in the choice denominator
#' (`v0 = 0` is pure Huff; `v0 > 0` is the aggregate conditional-logit form
#' with an outside option). `allocation = "clm"` returns the normalized share;
#' `"huff_decay"` multiplies that share by the decay kernel once more, retaining
#' the historical allocation. In both forms `outside_share` describes the
#' explicit choice outside option, not any additional decay attenuation.
#' A zero denominator (no opportunity and no outside option) yields zero
#' inside and outside shares. The returned `target` is the exogenous
#' attractiveness, so the map `T(x) = a` is state-independent.
#' @keywords internal
.huff_state <- function(a, plan, v0 = 0,
                        allocation = c("clm", "huff_decay")) {
  allocation <- match.arg(allocation)
  .chck_numeric_vector(a, "a")
  .chck_nonnegative_scalar(v0, "v0")
  a <- .coerce_numeric_vector(a)
  if (length(a) != ncol(plan$Kd_active)) {
    stop("`a` must have one element per facility")
  }
  v0 <- as.numeric(v0)

  opportunity <- .scale(plan$Kd_active, a, margin = "cols")
  denom <- rowSums(opportunity) + v0
  share_factor <- ifelse(denom > 0, 1 / denom, 0)
  huff_share <- .scale(opportunity, share_factor, margin = "rows")
  allocation <- if (allocation == "clm") huff_share else huff_share * plan$Kd_active
  access <- rowSums(allocation)
  utilization <- .contract(plan$D_active, allocation, over = "rows")
  outside_share <- if (v0 > 0) v0 * share_factor else rep(0, length(denom))

  list(
    target = a,
    utilization = utilization,
    attractiveness = a,
    access = access,
    allocation = allocation,
    huff_share = huff_share,
    opportunity = opportunity,
    outside_share = outside_share
  )
}

#' Evaluate the grouped static CLM at exogenous a
#'
#' Per group g the inside choice probability `P_g` has its own kernel and outside
#' weight `v0[g]`; the pooled allocation is the demand-weighted `P_bar = sum_g
#' D_g P_g / D` with `D` the total demand, so the pooled outputs are the ungrouped
#' ones on the total demand: `utilization = sum_g U_g = crossprod(P_bar, D)`,
#' `access = rowSums(P_bar)` and `outside_share` the demand-weighted outside
#' probability (`access + outside_share = 1`). The by-group outputs
#' `utilization_by_group` (facility by group), `access_by_group` and
#' `outside_share_by_group` (origin by group) are returned too; the group
#' probability matrices are not kept.
#' @param a Attractiveness, one value per facility.
#' @param kernels A list with the weight matrix (origin by facility) of every
#'   group, in the order of the columns of `D_groups`.
#' @param D_groups Demand by origin (rows) and group (columns), nonnegative with
#'   a positive total on every row.
#' @param v0 Outside-option weight of each group.
#' @keywords internal
.clm_group_state <- function(a, kernels, D_groups, v0) {
  groups <- colnames(D_groups)
  G <- ncol(D_groups)
  if (length(kernels) != G || length(v0) != G) {
    stop("`kernels` and `v0` must have one element per group")
  }
  K1 <- kernels[[1L]]
  if (length(a) != ncol(K1) || nrow(D_groups) != nrow(K1)) {
    stop("`a` and `D_groups` must match the weights by facility and origin")
  }
  v0 <- as.numeric(v0)
  if (anyNA(v0) || any(v0 < 0)) stop("`v0` must be nonnegative")
  D <- rowSums(D_groups)
  weight <- D_groups * ifelse(D > 0, 1 / D, 0)
  pooled <- matrix(0, nrow(K1), ncol(K1), dimnames = dimnames(K1))
  utilization_g <- matrix(0, ncol(K1), G, dimnames = list(colnames(K1), groups))
  access_g <- outside_g <- matrix(0, nrow(K1), G, dimnames = list(rownames(K1), groups))
  for (g in seq_len(G)) {
    opportunity <- .scale(kernels[[g]], a, margin = "cols")
    denom <- rowSums(opportunity) + v0[g]
    share_factor <- ifelse(denom > 0, 1 / denom, 0)
    P <- .scale(opportunity, share_factor, margin = "rows")
    pooled <- pooled + .scale(P, weight[, g], margin = "rows")
    utilization_g[, g] <- .contract(D_groups[, g], P, over = "rows")
    access_g[, g] <- rowSums(P)
    if (v0[g] > 0) outside_g[, g] <- v0[g] * share_factor
  }
  access <- rowSums(pooled)
  outside_share <- if (any(v0 > 0)) rowSums(outside_g * weight) else rep(0, nrow(K1))
  list(
    target = a,
    utilization = unname(rowSums(utilization_g)),
    attractiveness = a,
    access = access,
    allocation = pooled,
    huff_share = pooled,
    outside_share = outside_share,
    utilization_by_group = utilization_g,
    access_by_group = access_g,
    outside_share_by_group = outside_g
  )
}
