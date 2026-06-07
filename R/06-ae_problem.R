# Private AE problem / map compiler scaffold ---------------------------------
#
# Ring-1 design object: a theta-free inner problem whose `bind` closure performs
# theta-only work once, then returns a hot-loop step function of state only.
# This is intentionally private and thin; SAE/HAAE remain the extraction oracles.

#' Canonical theta contract for single decay-parameter AE models
#'
#' Single source of truth for the sigma validity bounds. Both the bind closure
#' and the problem object read this, so the contract cannot drift between them.
#' @keywords internal
.decay_theta_contract <- function() {
  list(names = "sigma", lower = 0, upper = Inf)
}

#' Require an AE outputs list to carry the contracted keys
#'
#' Every bound step's `outputs(x)` must expose `target` and `utilization` so
#' runners and loss functions can bind to known quantities without defensive
#' NULL checks.
#' @keywords internal
.validate_ae_outputs <- function(outputs) {
  if (!is.list(outputs)) {
    stop("AE outputs must be a list")
  }
  missing <- setdiff(c("target", "utilization"), names(outputs))
  if (length(missing) > 0) {
    stop("AE outputs must include: ", paste(missing, collapse = ", "))
  }
  outputs
}

#' Warn when a calibrated parameter lands on its search-box bound
#'
#' A boundary optimum usually means the search box, not the model, picked the
#' answer. `par` is the optimized value on the log scale (matching the optimizer).
#' @keywords internal
.warn_search_boundary <- function(par, lower, upper, name = "parameter") {
  par <- as.numeric(par)
  lower <- as.numeric(lower)
  upper <- as.numeric(upper)
  name <- rep(name, length.out = length(par))
  for (i in seq_along(par)) {
    span <- log(upper[i]) - log(lower[i])
    tol <- 1e-6 + 1e-3 * span
    if (par[i] <= log(lower[i]) + tol) {
      warning("fitted ", name[i], " reached the lower search bound (",
              format(lower[i]), "); widen `lower` or revisit the model.",
              call. = FALSE)
    } else if (par[i] >= log(upper[i]) - tol) {
      warning("fitted ", name[i], " reached the upper search bound (",
              format(upper[i]), "); widen `upper` or revisit the model.",
              call. = FALSE)
    }
  }
  invisible(NULL)
}

#' Compile checked raster inputs into a compact interaction substrate
#' @keywords internal
.interaction_substrate <- function(demand, supply, distance,
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
    eps = as.numeric(eps),
    theta = .decay_theta_contract()
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
    a_max = a_max,
    theta = .decay_theta_contract()
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
  substrate <- .interaction_substrate(
    demand, supply, distance, id_col = id_col, supply_cols = supply_cols
  )
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
    theta = spec$theta,
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
  substrate <- .interaction_substrate(
    demand, supply, distance, id_col = id_col, supply_cols = supply_cols
  )
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
    theta = spec$theta,
    bind = compiled$bind,
    metadata = list(spec = spec)
  )
}

.compile_sae_map <- function(spec, substrate) {
  S <- as.vector(substrate$S[, 1])
  bind <- function(theta) {
    theta <- .coerce_problem_theta(theta, spec$theta)
    K <- calc_decay(substrate$distance_active, method = spec$family,
                    sigma = theta[[spec$theta$names]], snap = TRUE)
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
    theta <- .coerce_problem_theta(theta, spec$theta)
    K <- calc_decay(substrate$distance_active, method = spec$family,
                    sigma = theta[[spec$theta$names]], snap = TRUE)
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
                           check = TRUE, diagnostics = TRUE) {
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
  fit$outputs <- .validate_ae_outputs(step$outputs(fit$x_star))
  fit$utilization <- fit$outputs$utilization
  if (isTRUE(diagnostics) && is.function(step$jac_state)) {
    rho <- tryCatch({
      jac <- step$jac_state(fit$x_star)
      if (is.list(jac)) {
        jac <- jac$jac_state
      }
      spectral_radius(jac)
    }, error = function(e) NA_real_)
    fit$spectral_radius <- rho
    fit$contraction <- if (is.na(rho)) NA else rho < 1
  }
  fit
}

#' Fit a private AE problem by nested fixed point
#' @keywords internal
.fit_problem_nfxp <- function(problem, observed, init, lower, upper,
                              output = "utilization",
                              loss = .weighted_sse_loss,
                              loss_args = list(eta = 1),
                              lambda = 1, tol = 1e-8, max_iter = 1000,
                              norm = c("max", "l2"),
                              control = list(maxit = 25),
                              check = FALSE, penalty = 1e12) {
  .chck_class(problem, "ae_problem", "problem")
  .chck_numeric_vector(observed, "observed")
  if (!is.function(loss)) {
    stop("`loss` must be a function")
  }
  if (!is.list(loss_args)) {
    stop("`loss_args` must be a list")
  }
  if (!is.character(output) || length(output) != 1L || output == "") {
    stop("`output` must be a length-one character value")
  }
  .chck_positive_scalar(penalty, "penalty")
  norm <- match.arg(norm)

  expected <- problem$theta$names
  init <- .coerce_fit_theta(init, expected, "init")
  lower <- .coerce_fit_theta(lower, expected, "lower")
  upper <- .coerce_fit_theta(upper, expected, "upper")
  .coerce_problem_theta(init, problem$theta)
  .coerce_problem_theta(lower, problem$theta)
  .coerce_problem_theta(upper, problem$theta)
  if (any(init <= 0 | lower <= 0 | upper <= 0)) {
    stop("`init`, `lower`, and `upper` must be positive for log-scale optimization")
  }
  if (any(lower >= upper)) {
    stop("`lower` must be less than `upper`")
  }
  if (any(init < lower | init > upper)) {
    stop("`init` must be inside [`lower`, `upper`]")
  }

  observed <- .coerce_numeric_vector(observed)
  warm <- NULL
  objective <- function(log_theta) {
    tryCatch({
      theta <- stats::setNames(exp(log_theta), expected)
      x0 <- if (.state_within_problem_bounds(problem, warm)) warm else NULL
      fit <- .solve_problem(
        problem,
        theta = theta,
        x0 = x0,
        lambda = lambda,
        tol = tol,
        max_iter = max_iter,
        norm = norm,
        keep_history = FALSE,
        warn = FALSE,
        check = check,
        diagnostics = FALSE
      )
      if (!isTRUE(fit$converged)) {
        return(penalty)
      }
      predicted <- .problem_fit_output(fit, output, required = FALSE)
      if (is.null(predicted) || length(predicted) != length(observed)) {
        return(penalty)
      }
      value <- do.call(
        loss,
        c(list(predicted = predicted, observed = observed), loss_args)
      )
      if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
        return(penalty)
      }
      warm <<- fit$x_star
      value
    }, error = function(e) penalty)
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

  theta_hat <- stats::setNames(exp(opt$par), expected)
  .warn_search_boundary(opt$par, lower, upper, expected)
  final <- .solve_problem(
    problem,
    theta = theta_hat,
    x0 = if (.state_within_problem_bounds(problem, warm)) warm else NULL,
    lambda = lambda,
    tol = tol,
    max_iter = max_iter,
    norm = norm,
    keep_history = TRUE,
    warn = TRUE,
    check = check
  )
  predicted <- .problem_fit_output(final, output, required = TRUE)
  loss_value <- do.call(
    loss,
    c(list(predicted = predicted, observed = observed), loss_args)
  )
  if (!is.numeric(loss_value) || length(loss_value) != 1L || !is.finite(loss_value)) {
    stop("final loss must be a finite numeric scalar")
  }
  predicted <- .name_problem_vector(predicted, problem$substrate$facility_ids)
  state_name <- problem$state$name
  state <- .name_problem_vector(final$x_star, problem$substrate$facility_ids)

  structure(
    list(
      model = problem$model,
      family = problem$metadata$spec$family,
      theta_hat = theta_hat,
      theta = theta_hat,
      loss = as.numeric(loss_value),
      output = output,
      loss_args = loss_args,
      convergence = opt$convergence,
      message = opt$message,
      seconds = unname(elapsed),
      predicted = predicted,
      state = state,
      state_name = state_name,
      equilibrium = final,
      spectral_radius = final$spectral_radius,
      optim = opt
    ),
    class = c("ae_problem_nfxp_fit", paste0(problem$model, "_problem_nfxp_fit"))
  )
}

#' Fit one decay parameter for an AE problem by weighted SSE
#' @keywords internal
.fit_problem_decay <- function(problem, observed, init, lower, upper,
                               lambda = 1, tol = 1e-8, max_iter = 1000,
                               eta = 1, control = list(maxit = 25),
                               check = FALSE) {
  .chck_class(problem, "ae_problem", "problem")
  .chck_positive_scalar(init, "init")
  .chck_positive_scalar(lower, "lower")
  .chck_positive_scalar(upper, "upper")
  .chck_nonnegative_scalar(eta, "eta")
  if (length(problem$theta$names) != 1L) {
    stop(".fit_problem_decay() currently supports a single-parameter theta")
  }
  pname <- problem$theta$names
  fit <- .fit_problem_nfxp(
    problem = problem,
    observed = observed,
    init = stats::setNames(init, pname),
    lower = stats::setNames(lower, pname),
    upper = stats::setNames(upper, pname),
    output = "utilization",
    loss = .weighted_sse_loss,
    loss_args = list(eta = eta),
    lambda = lambda,
    tol = tol,
    max_iter = max_iter,
    control = control,
    check = check
  )
  fit$theta_hat <- unname(fit$theta[[pname]])
  fit$wsse <- fit$loss
  fit$eta <- eta
  class(fit) <- c(
    "ae_problem_decay_fit",
    paste0(problem$model, "_problem_decay_fit"),
    class(fit)
  )
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

.state_within_problem_bounds <- function(problem, x) {
  if (is.null(x)) {
    return(FALSE)
  }
  x <- .coerce_numeric_vector(x)
  length(x) == length(problem$state$init) &&
    all(is.finite(x)) &&
    all(x >= problem$state$lower & x <= problem$state$upper)
}

.coerce_fit_theta <- function(x, expected, name) {
  .chck_numeric_vector(x, name)
  x <- .coerce_numeric_vector(x)
  if (is.null(names(x)) || any(names(x) == "")) {
    if (length(x) != length(expected)) {
      stop("unnamed `", name, "` must match the theta contract length")
    }
    names(x) <- expected
  }
  missing <- setdiff(expected, names(x))
  if (length(missing) > 0) {
    stop("`", name, "` is missing required parameter(s): ",
         paste(missing, collapse = ", "))
  }
  x[expected]
}

.problem_fit_output <- function(fit, output, required = TRUE) {
  if (!is.list(fit$outputs) || !output %in% names(fit$outputs)) {
    if (required) {
      stop("problem outputs do not include requested output `", output, "`")
    }
    return(NULL)
  }
  value <- fit$outputs[[output]]
  ok <- tryCatch({
    .chck_numeric_vector(value, output)
    TRUE
  }, error = function(e) {
    if (required) {
      stop(e$message, call. = FALSE)
    }
    FALSE
  })
  if (!ok) {
    return(NULL)
  }
  .coerce_numeric_vector(value)
}

.name_problem_vector <- function(x, ids) {
  if (!is.null(ids) && length(x) == length(ids)) {
    names(x) <- ids
  }
  x
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
