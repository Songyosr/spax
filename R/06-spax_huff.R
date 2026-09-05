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

#' Static Huff map T(a) = a (constant; reaches equilibrium in one pass)
#' @keywords internal
.huff_map <- function(a, plan, v0 = 0,
                      allocation = c("clm", "huff_decay")) {
  .huff_state(a, plan = plan, v0 = v0, allocation = match.arg(allocation))$target
}
