# SPAX-009: private SAE fixed-point map --------------------------------------

#' Compile inputs for the compact SAE map
#' @keywords internal
.sae_compact_plan <- function(demand, supply, kernel, kappa = 1,
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
    stop("SAE currently supports one demand layer")
  }
  if (ncol(plan$S) != 1L) {
    stop("SAE currently supports one supply measure")
  }
  plan$S <- as.vector(plan$S[, 1])
  plan$kappa_supply <- as.numeric(kappa) * plan$S
  class(plan) <- c("sae_compact_plan", class(plan))
  plan
}

#' Smooth cap at one with value zero at zero
#' @keywords internal
.sae_soft_cap <- function(x, beta = 20) {
  .chck_numeric_vector(x, "x")
  .chck_positive_scalar(beta, "beta")
  x <- .coerce_numeric_vector(x)
  beta <- as.numeric(beta)
  a <- -beta * x
  b <- rep(-beta, length(x))
  m <- pmax(a, b)
  -(m + log(exp(a - m) + exp(b - m))) / beta + log1p(exp(-beta)) / beta
}

#' Evaluate the SAE map and derived state at one serviceability vector
#' @keywords internal
.sae_state <- function(c, plan, beta = 20, eps = 1e-8) {
  .chck_numeric_vector(c, "c")
  .chck_positive_scalar(beta, "beta")
  .chck_positive_scalar(eps, "eps")
  c <- pmin(pmax(.coerce_numeric_vector(c), 0), 1)
  if (length(c) != ncol(plan$Kd_active)) {
    stop("`c` must have one element per facility")
  }

  opportunity <- .scale(plan$Kd_active, c, margin = "cols")
  pooled <- rowSums(opportunity)
  access <- .sae_soft_cap(pooled, beta = beta)
  choice <- .normalize(opportunity, over = "cols")
  allocation <- .scale(choice, access, margin = "rows")
  utilization <- .contract(plan$D_active, allocation, over = "rows")
  adequacy <- plan$kappa_supply / (utilization + as.numeric(eps))
  target <- .sae_soft_cap(adequacy, beta = beta)

  list(
    target = target,
    utilization = utilization,
    serviceability = c,
    adequacy = adequacy,
    access = access,
    allocation = allocation,
    opportunity = opportunity
  )
}

#' SAE fixed-point map T(c)
#' @keywords internal
.sae_map <- function(c, plan, beta = 20, eps = 1e-8) {
  .sae_state(c, plan = plan, beta = beta, eps = eps)$target
}

#' Solve the SAE serviceability fixed point
#' @keywords internal
.sae_equilibrium <- function(plan, x0 = NULL, beta = 20, eps = 1e-8,
                             lambda = 0.7, tol = 1e-8, max_iter = 1000,
                             norm = c("max", "l2"), keep_history = TRUE,
                             warn = TRUE, check = TRUE) {
  if (is.null(x0)) {
    x0 <- rep(1, ncol(plan$Kd_active))
  }
  fit <- solve_equilibrium(
    map = .sae_map,
    x0 = x0,
    plan = plan,
    beta = beta,
    eps = eps,
    lambda = lambda,
    tol = tol,
    max_iter = max_iter,
    norm = norm,
    keep_history = keep_history,
    warn = warn,
    check = check
  )
  state <- .sae_state(fit$x_star, plan = plan, beta = beta, eps = eps)
  fit$serviceability <- fit$x_star
  fit$utilization <- state$utilization
  fit$adequacy <- state$adequacy
  fit$access <- state$access
  fit$allocation <- state$allocation
  fit
}
