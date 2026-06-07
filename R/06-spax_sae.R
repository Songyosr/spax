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

#' Derivative of the SAE smooth cap
#' @keywords internal
.sae_soft_cap_gradient <- function(x, beta = 20) {
  .chck_numeric_vector(x, "x")
  .chck_positive_scalar(beta, "beta")
  x <- .coerce_numeric_vector(x)
  stats::plogis(as.numeric(beta) * (1 - x))
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

#' Analytic state Jacobian dT/dc for the compact SAE map
#' @keywords internal
.sae_jacobian_state <- function(c, plan, beta = 20, eps = 1e-8) {
  state <- .sae_state(c, plan = plan, beta = beta, eps = eps)
  c <- state$serviceability
  K <- as.matrix(plan$Kd_active)
  D <- plan$D_active
  beta <- as.numeric(beta)
  eps <- as.numeric(eps)

  opportunity <- state$opportunity
  pooled <- rowSums(opportunity)
  positive <- pooled > 0
  inv_pooled <- ifelse(positive, 1 / pooled, 0)
  inv_pooled2 <- ifelse(positive, 1 / pooled^2, 0)

  access_grad <- .sae_soft_cap_gradient(pooled, beta = beta)
  w <- D * state$access * inv_pooled
  direct <- as.numeric(crossprod(K, w))

  q <- D * (access_grad * inv_pooled - state$access * inv_pooled2)
  G <- .scale(opportunity, q, margin = "rows")
  jac_utilization <- diag(direct, nrow = length(c)) + crossprod(G, K)

  adequacy_grad <- .sae_soft_cap_gradient(state$adequacy, beta = beta)
  ratio_grad <- -adequacy_grad * plan$kappa_supply / (state$utilization + eps)^2
  jac_state <- .scale(jac_utilization, ratio_grad, margin = "rows")

  list(
    jac_state = jac_state,
    jac_utilization = jac_utilization,
    utilization = state$utilization,
    adequacy = state$adequacy
  )
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

#' Predict SAE utilization for a decay parameter
#' @keywords internal
.sae_predict_decay <- function(theta, family, demand, supply, distance,
                               kappa = 1, beta = 20, eps = 1e-8,
                               lambda = 0.7, tol = 1e-8, max_iter = 1000,
                               x0 = NULL, check = FALSE) {
  family <- match.arg(family, c("gaussian", "exponential", "power"))
  .chck_positive_scalar(theta, "theta")
  kernel <- calc_decay(distance, method = family, sigma = as.numeric(theta),
                       snap = TRUE)
  names(kernel) <- names(distance)
  plan <- .sae_compact_plan(demand, supply, kernel, kappa = kappa)
  fit <- .sae_equilibrium(
    plan,
    x0 = x0,
    beta = beta,
    eps = eps,
    lambda = lambda,
    tol = tol,
    max_iter = max_iter,
    check = check
  )
  list(
    theta = as.numeric(theta),
    family = family,
    kernel = kernel,
    plan = plan,
    fit = fit,
    predicted = fit$utilization,
    serviceability = fit$serviceability
  )
}

#' Fit one SAE decay parameter by weighted SSE
#' @keywords internal
.sae_fit_decay <- function(demand, supply, distance, observed,
                           family = c("gaussian", "exponential", "power"),
                           init, lower, upper, kappa = 1, beta = 20,
                           eps = 1e-8, lambda = 0.7, tol = 1e-8,
                           max_iter = 1000, eta = 1,
                           control = list(maxit = 25)) {
  family <- match.arg(family)
  .chck_numeric_vector(observed, "observed")
  .chck_positive_scalar(init, "init")
  .chck_positive_scalar(lower, "lower")
  .chck_positive_scalar(upper, "upper")
  .chck_nonnegative_scalar(eta, "eta")
  if (lower >= upper) {
    stop("`lower` must be less than `upper`")
  }
  if (init < lower || init > upper) {
    stop("`init` must be inside [`lower`, `upper`]")
  }

  observed <- .coerce_numeric_vector(observed)
  warm <- NULL
  objective <- function(log_theta) {
    theta <- exp(log_theta)
    pred <- .sae_predict_decay(
      theta = theta,
      family = family,
      demand = demand,
      supply = supply,
      distance = distance,
      kappa = kappa,
      beta = beta,
      eps = eps,
      lambda = lambda,
      tol = tol,
      max_iter = max_iter,
      x0 = warm,
      check = FALSE
    )
    if (!isTRUE(pred$fit$converged)) {
      return(1e12)
    }
    warm <<- pred$serviceability
    .weighted_sse_loss(pred$predicted, observed, eta = eta)
  }

  elapsed <- system.time({
    opt <- stats::optim(
      par = log(init),
      fn = objective,
      method = "L-BFGS-B",
      lower = log(lower),
      upper = log(upper),
      control = control
    )
  })[["elapsed"]]

  theta_hat <- exp(opt$par)
  final <- .sae_predict_decay(
    theta = theta_hat,
    family = family,
    demand = demand,
    supply = supply,
    distance = distance,
    kappa = kappa,
    beta = beta,
    eps = eps,
    lambda = lambda,
    tol = tol,
    max_iter = max_iter,
    x0 = warm,
    check = FALSE
  )
  loss <- .weighted_sse_loss(final$predicted, observed, eta = eta)
  structure(
    list(
      family = family,
      theta_hat = theta_hat,
      loss = loss,
      wsse = loss,
      convergence = opt$convergence,
      message = opt$message,
      seconds = unname(elapsed),
      predicted = final$predicted,
      serviceability = final$serviceability,
      equilibrium = final$fit,
      optim = opt
    ),
    class = "sae_decay_fit"
  )
}
