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

#' Solve the HAAE attractiveness fixed point
#' @keywords internal
.haae_equilibrium <- function(plan, x0 = NULL, eps = 1e-8,
                              a_min = 1e-6, a_max = Inf,
                              lambda = 0.7, tol = 1e-8,
                              max_iter = 1000,
                              norm = c("max", "l2"),
                              keep_history = TRUE,
                              warn = TRUE, check = TRUE) {
  if (is.null(x0)) {
    x0 <- rep(1, ncol(plan$Kd_active))
  }
  fit <- solve_equilibrium(
    map = .haae_map,
    x0 = x0,
    plan = plan,
    eps = eps,
    a_min = a_min,
    a_max = a_max,
    lambda = lambda,
    tol = tol,
    max_iter = max_iter,
    norm = norm,
    keep_history = keep_history,
    warn = warn,
    check = check
  )
  state <- .haae_state(
    fit$x_star, plan = plan, eps = eps, a_min = a_min, a_max = a_max
  )
  fit$attractiveness <- fit$x_star
  fit$utilization <- state$utilization
  fit$allocation <- state$allocation
  fit$huff_share <- state$huff_share
  fit
}

#' Predict HAAE utilization for a decay parameter
#' @keywords internal
.haae_predict_decay <- function(theta, family, demand, supply, distance,
                                kappa = 1, eps = 1e-8, a_min = 1e-6,
                                a_max = Inf, lambda = 0.7, tol = 1e-8,
                                max_iter = 1000, x0 = NULL,
                                check = FALSE) {
  family <- match.arg(family, c("gaussian", "exponential", "power"))
  .chck_positive_scalar(theta, "theta")
  kernel <- calc_decay(distance, method = family, sigma = as.numeric(theta),
                       snap = TRUE)
  names(kernel) <- names(distance)
  plan <- .haae_compact_plan(demand, supply, kernel, kappa = kappa)
  fit <- .haae_equilibrium(
    plan,
    x0 = x0,
    eps = eps,
    a_min = a_min,
    a_max = a_max,
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
    attractiveness = fit$attractiveness
  )
}

#' Fit one HAAE decay parameter by weighted SSE
#' @keywords internal
.haae_fit_decay <- function(demand, supply, distance, observed,
                            family = c("gaussian", "exponential", "power"),
                            init, lower, upper, kappa = 1, eps = 1e-8,
                            a_min = 1e-6, a_max = Inf,
                            lambda = 0.7, tol = 1e-8,
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
    pred <- .haae_predict_decay(
      theta = theta,
      family = family,
      demand = demand,
      supply = supply,
      distance = distance,
      kappa = kappa,
      eps = eps,
      a_min = a_min,
      a_max = a_max,
      lambda = lambda,
      tol = tol,
      max_iter = max_iter,
      x0 = warm,
      check = FALSE
    )
    if (!isTRUE(pred$fit$converged)) {
      return(1e12)
    }
    warm <<- pred$attractiveness
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
  final <- .haae_predict_decay(
    theta = theta_hat,
    family = family,
    demand = demand,
    supply = supply,
    distance = distance,
    kappa = kappa,
    eps = eps,
    a_min = a_min,
    a_max = a_max,
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
      attractiveness = final$attractiveness,
      equilibrium = final$fit,
      optim = opt
    ),
    class = "haae_decay_fit"
  )
}
