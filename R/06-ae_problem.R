# Private AE problem / map compiler scaffold ---------------------------------
#
# Ring-1 design object: a theta-free inner problem whose `bind` closure performs
# theta-only work once, then returns a hot-loop step function of state only.
# This is intentionally private and thin; SAE/HAAE remain the extraction oracles.

#' Compile checked raster inputs into a decay-ready compact substrate
#' @keywords internal
.decay_substrate <- function(demand, supply, distance,
                             id_col = NULL, supply_cols = NULL) {
  .chck_raster_alignment(demand, distance[[1]], "demand", "distance")

  ids <- names(distance)
  processed <- .help_process_supply(supply, id_col = id_col,
                                    supply_cols = supply_cols,
                                    weight_ids = ids)
  D <- terra::values(demand, mat = TRUE)
  if (ncol(D) != 1L) {
    stop("AE problems currently support one demand layer")
  }
  Dist <- terra::values(distance, mat = TRUE)
  keep <- which(is.finite(D[, 1]) & D[, 1] > 0)
  D0 <- D[keep, 1]
  Dist0 <- Dist[keep, , drop = FALSE]

  list(
    D_active = D0,
    distance_active = Dist0,
    S = as.matrix(processed$values),
    facility_ids = ids,
    supply_cols = processed$cols,
    demand_kept_index = keep,
    template = demand
  )
}

#' Construct an SAE model spec
#' @keywords internal
.sae_spec <- function(family = c("gaussian", "exponential", "power"),
                      kappa = 1, beta = 20, eps = 1e-8) {
  family <- match.arg(family)
  .chck_positive_scalar(kappa, "kappa")
  .chck_positive_scalar(beta, "beta")
  .chck_positive_scalar(eps, "eps")
  list(
    model = "sae",
    family = family,
    kappa = as.numeric(kappa),
    beta = as.numeric(beta),
    eps = as.numeric(eps)
  )
}

#' Construct an HAAE model spec
#' @keywords internal
.haae_spec <- function(family = c("gaussian", "exponential", "power"),
                       kappa = 1, eps = 1e-8, a_min = 1e-6,
                       a_max = Inf) {
  family <- match.arg(family)
  .chck_positive_scalar(kappa, "kappa")
  .chck_positive_scalar(eps, "eps")
  .chck_positive_scalar(a_min, "a_min")
  list(
    model = "haae",
    family = family,
    kappa = as.numeric(kappa),
    eps = as.numeric(eps),
    a_min = as.numeric(a_min),
    a_max = a_max
  )
}

#' Create an AE inner problem object
#' @keywords internal
.new_problem <- function(model, substrate, state, theta, bind, metadata = list()) {
  if (!is.function(bind)) {
    stop("`bind` must be a function")
  }
  structure(
    list(
      model = model,
      substrate = substrate,
      state = state,
      theta = theta,
      bind = bind,
      metadata = metadata
    ),
    class = c("ae_problem", paste0(model, "_problem"))
  )
}

#' Compile a model spec and substrate into problem closures
#' @keywords internal
.compile_ae_map <- function(spec, substrate) {
  if (!is.list(spec) || is.null(spec$model)) {
    stop("`spec` must be a model spec")
  }
  switch(spec$model,
    "sae" = .compile_sae_map(spec, substrate),
    "haae" = .compile_haae_map(spec, substrate),
    stop("unsupported AE model spec")
  )
}

#' Construct a private SAE problem
#' @keywords internal
.sae_problem <- function(demand, supply, distance,
                         family = c("gaussian", "exponential", "power"),
                         kappa = 1, beta = 20, eps = 1e-8,
                         id_col = NULL, supply_cols = NULL) {
  substrate <- .decay_substrate(demand, supply, distance,
                                id_col = id_col, supply_cols = supply_cols)
  if (ncol(substrate$S) != 1L) {
    stop("SAE currently supports one supply measure")
  }
  spec <- .sae_spec(family = family, kappa = kappa, beta = beta, eps = eps)
  compiled <- .compile_ae_map(spec, substrate)
  .new_problem(
    model = "sae",
    substrate = substrate,
    state = list(
      name = "c",
      axis = "J",
      init = rep(1, length(substrate$facility_ids)),
      lower = 0,
      upper = 1
    ),
    theta = list(names = "sigma", lower = 0, upper = Inf),
    bind = compiled$bind,
    metadata = list(spec = spec)
  )
}

#' Construct a private HAAE problem
#' @keywords internal
.haae_problem <- function(demand, supply, distance,
                          family = c("gaussian", "exponential", "power"),
                          kappa = 1, eps = 1e-8, a_min = 1e-6,
                          a_max = Inf, id_col = NULL,
                          supply_cols = NULL) {
  substrate <- .decay_substrate(demand, supply, distance,
                                id_col = id_col, supply_cols = supply_cols)
  if (ncol(substrate$S) != 1L) {
    stop("HAAE currently supports one supply measure")
  }
  spec <- .haae_spec(family = family, kappa = kappa, eps = eps,
                     a_min = a_min, a_max = a_max)
  compiled <- .compile_ae_map(spec, substrate)
  .new_problem(
    model = "haae",
    substrate = substrate,
    state = list(
      name = "a",
      axis = "J",
      init = rep(1, length(substrate$facility_ids)),
      lower = a_min,
      upper = a_max
    ),
    theta = list(names = "sigma", lower = 0, upper = Inf),
    bind = compiled$bind,
    metadata = list(spec = spec)
  )
}

.compile_sae_map <- function(spec, substrate) {
  S <- as.vector(substrate$S[, 1])
  bind <- function(theta) {
    theta <- .coerce_problem_theta(theta, list(names = "sigma", lower = 0, upper = Inf))
    K <- calc_decay(substrate$distance_active, method = spec$family,
                    sigma = theta[["sigma"]], snap = TRUE)
    K[!is.finite(K)] <- 0
    plan <- substrate
    plan$Kd_active <- K
    plan$S <- S
    plan$kappa_supply <- spec$kappa * S
    class(plan) <- c("sae_compact_plan", "fca_compact_plan")

    eval <- function(x, full = FALSE) {
      state <- .sae_state(x, plan = plan, beta = spec$beta, eps = spec$eps)
      if (!full) {
        return(state$target)
      }
      state
    }
    list(
      map = function(x) eval(x, full = FALSE),
      outputs = function(x) eval(x, full = TRUE),
      jac_state = function(x) {
        .sae_jacobian_state(x, plan = plan, beta = spec$beta, eps = spec$eps)
      },
      theta = theta,
      plan = plan
    )
  }
  list(bind = bind)
}

.compile_haae_map <- function(spec, substrate) {
  S <- as.vector(substrate$S[, 1])
  bind <- function(theta) {
    theta <- .coerce_problem_theta(theta, list(names = "sigma", lower = 0, upper = Inf))
    K <- calc_decay(substrate$distance_active, method = spec$family,
                    sigma = theta[["sigma"]], snap = TRUE)
    K[!is.finite(K)] <- 0
    plan <- substrate
    plan$Kd_active <- K
    plan$S <- S
    plan$kappa_supply <- spec$kappa * S
    class(plan) <- c("haae_compact_plan", "fca_compact_plan")

    eval <- function(x, full = FALSE) {
      state <- .haae_state(
        x, plan = plan, eps = spec$eps, a_min = spec$a_min,
        a_max = spec$a_max
      )
      if (!full) {
        return(state$target)
      }
      state
    }
    list(
      map = function(x) eval(x, full = FALSE),
      outputs = function(x) eval(x, full = TRUE),
      jac_state = function(x) {
        .haae_jacobian_state(
          x, plan = plan, eps = spec$eps, a_min = spec$a_min,
          a_max = spec$a_max
        )
      },
      theta = theta,
      plan = plan
    )
  }
  list(bind = bind)
}

#' Bind theta once and return the state-only inner-loop step
#' @keywords internal
.bind_theta <- function(problem, theta) {
  .chck_class(problem, "ae_problem", "problem")
  bound <- problem$bind(theta)
  if (!is.function(bound$map)) {
    stop("bound AE step must provide a `map` function")
  }
  if (!is.function(bound$outputs)) {
    stop("bound AE step must provide an `outputs` function")
  }
  structure(bound, class = c("ae_bound_step", paste0(problem$model, "_bound_step")))
}

#' Evaluate a problem map at one state and theta
#' @keywords internal
.problem_map <- function(problem, x, theta) {
  .bind_theta(problem, theta)$map(x)
}

#' Evaluate rich outputs at one state and theta
#' @keywords internal
.problem_outputs <- function(problem, x, theta) {
  .bind_theta(problem, theta)$outputs(x)
}

#' Evaluate the state Jacobian provider at one state and theta
#' @keywords internal
.problem_jac_state <- function(problem, x, theta) {
  step <- .bind_theta(problem, theta)
  if (!is.function(step$jac_state)) {
    stop("problem does not provide an analytic state Jacobian")
  }
  step$jac_state(x)
}

#' Solve an AE problem at fixed theta
#' @keywords internal
.solve_problem <- function(problem, theta, x0 = NULL, lambda = 1,
                           tol = 1e-8, max_iter = 1000,
                           norm = c("max", "l2"),
                           keep_history = TRUE, warn = TRUE,
                           check = TRUE) {
  .chck_class(problem, "ae_problem", "problem")
  step <- .bind_theta(problem, theta)
  if (is.null(x0)) {
    x0 <- problem$state$init
  }
  .validate_problem_state(problem, x0)
  fit <- solve_equilibrium(
    map = step$map,
    x0 = x0,
    lambda = lambda,
    tol = tol,
    max_iter = max_iter,
    norm = norm,
    keep_history = keep_history,
    warn = warn,
    check = check
  )
  fit$outputs <- step$outputs(fit$x_star)
  if (!is.null(fit$outputs$utilization)) {
    fit$utilization <- fit$outputs$utilization
  }
  fit
}

.validate_problem_state <- function(problem, x) {
  .chck_numeric_vector(x, "x0")
  x <- .coerce_numeric_vector(x)
  if (length(x) != length(problem$state$init)) {
    stop("`x0` must match the problem state length")
  }
  if (any(x < problem$state$lower | x > problem$state$upper)) {
    stop("`x0` is outside the problem state bounds")
  }
  invisible(TRUE)
}

.coerce_problem_theta <- function(theta, contract) {
  .chck_numeric_vector(theta, "theta")
  theta <- .coerce_numeric_vector(theta)
  expected <- contract$names
  if (is.null(names(theta)) || any(names(theta) == "")) {
    if (length(theta) != length(expected)) {
      stop("unnamed `theta` must match the theta contract length")
    }
    names(theta) <- expected
  }
  missing <- setdiff(expected, names(theta))
  if (length(missing) > 0) {
    stop("`theta` is missing required parameter(s): ",
         paste(missing, collapse = ", "))
  }
  theta <- theta[expected]
  if (any(theta < contract$lower | theta > contract$upper)) {
    stop("`theta` is outside the theta bounds")
  }
  theta
}
