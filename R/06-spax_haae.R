# SPAX-024: private HAAE fixed-point map -------------------------------------

#' Compile inputs for the compact HAAE map
#' @keywords internal
.haae_compact_plan <- function(demand, supply, kernel, kappa = 1,
                               id_col = NULL, supply_cols = NULL) {
  .chck_positive_scalar(kappa, "kappa")
  plan <- .fca_compact_plan(
    demand = demand,
    supply = supply,
    demand_kernel = kernel,
    access_kernel = kernel,
    demand_normalize = "identity",
    id_col = id_col,
    supply_cols = supply_cols
  )
  if (!is.null(dim(plan$D_active))) {
    stop("HAAE currently supports one demand layer")
  }
  if (ncol(plan$S) != 1L) {
    stop("HAAE currently supports one supply measure")
  }
  plan$S <- as.vector(plan$S[, 1])
  plan$kappa_supply <- as.numeric(kappa) * plan$S
  class(plan) <- c("haae_compact_plan", class(plan))
  plan
}

#' Evaluate the HAAE map and derived state at one attractiveness vector
#' @keywords internal
.haae_state <- function(a, plan, eps = 1e-8, a_min = 1e-6,
                        a_max = Inf) {
  .chck_numeric_vector(a, "a")
  .chck_positive_scalar(eps, "eps")
  .chck_positive_scalar(a_min, "a_min")
  a <- pmin(pmax(.coerce_numeric_vector(a), as.numeric(a_min)), a_max)
  if (length(a) != ncol(plan$Kd_active)) {
    stop("`a` must have one element per facility")
  }

  opportunity <- .scale(plan$Kd_active, a, margin = "cols")
  huff_share <- .normalize(opportunity, over = "cols")
  allocation <- huff_share * plan$Kd_active
  utilization <- .contract(plan$D_active, allocation, over = "rows")
  ratio <- plan$kappa_supply / (utilization + as.numeric(eps))
  target <- pmin(pmax(ratio, as.numeric(a_min)), a_max)

  list(
    target = target,
    ratio = ratio,
    utilization = utilization,
    attractiveness = a,
    allocation = allocation,
    huff_share = huff_share,
    opportunity = opportunity,
    pooled = rowSums(opportunity)
  )
}

#' HAAE fixed-point map T(a)
#' @keywords internal
.haae_map <- function(a, plan, eps = 1e-8, a_min = 1e-6,
                      a_max = Inf) {
  .haae_state(
    a, plan = plan, eps = eps, a_min = a_min, a_max = a_max
  )$target
}

#' Analytic state Jacobian dT/da for the compact HAAE map
#' @keywords internal
.haae_jacobian_state <- function(a, plan, eps = 1e-8, a_min = 1e-6,
                                 a_max = Inf) {
  state <- .haae_state(
    a, plan = plan, eps = eps, a_min = a_min, a_max = a_max
  )
  a <- state$attractiveness
  K <- as.matrix(plan$Kd_active)
  D <- plan$D_active
  eps <- as.numeric(eps)

  pooled <- state$pooled
  inv_pooled <- ifelse(pooled > 0, 1 / pooled, 0)
  inv_pooled2 <- ifelse(pooled > 0, 1 / pooled^2, 0)
  K2 <- K * K

  direct <- as.numeric(crossprod(K2, D * inv_pooled))
  G <- .scale(K2, D * inv_pooled2, margin = "rows")
  jac_utilization <- diag(direct, nrow = length(a)) -
    .scale(crossprod(G, K), a, margin = "rows")

  target_is_active <- state$ratio > as.numeric(a_min) & state$ratio < a_max
  ratio_grad <- ifelse(
    target_is_active,
    -plan$kappa_supply / (state$utilization + eps)^2,
    0
  )
  jac_state <- .scale(jac_utilization, ratio_grad, margin = "rows")

  list(
    jac_state = jac_state,
    jac_utilization = jac_utilization,
    utilization = state$utilization,
    target = state$target
  )
}

