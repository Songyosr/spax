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

#' Construct a static Huff / aggregate-CLM model spec
#'
#' The no-state member of the ladder: attractiveness is exogenous
#' (`a_j = (kappa * S_j)^beta`), allocation is one Huff pass, and `v0` is the
#' optional outside-option mass (`0` = pure Huff). Calibrated theta is the
#' decay parameter only; `kappa`, `beta`, `v0` are spec constants for the MVP.
#' @keywords internal
.huff_spec <- function(family = c("gaussian", "exponential", "power"),
                       kappa = 1, beta = 1, v0 = 0) {
  family <- match.arg(family)
  .chck_positive_scalar(kappa, "kappa")
  .chck_positive_scalar(beta, "beta")
  .chck_nonnegative_scalar(v0, "v0")
  list(
    model = "huff",
    family = family,
    kappa = as.numeric(kappa),
    beta = as.numeric(beta),
    v0 = as.numeric(v0),
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
    "huff" = .compile_huff_map(spec, substrate),
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

#' Construct a private static Huff / aggregate-CLM problem
#'
#' The `dT/dstate = 0` member of the ladder: the map is constant, the
#' equilibrium is reached in one pass, and the decay parameter enters only
#' through the allocation output (not the state target). It calibrates through
#' the shared NFXP runner unchanged.
#' @keywords internal
.huff_problem <- function(demand, supply, distance,
                          family = c("gaussian", "exponential", "power"),
                          kappa = 1, beta = 1, v0 = 0,
                          id_col = NULL, supply_cols = NULL) {
  substrate <- .interaction_substrate(
    demand, supply, distance, id_col = id_col, supply_cols = supply_cols
  )
  if (ncol(substrate$S) != 1L) {
    stop("static Huff currently supports one supply measure")
  }
  spec <- .huff_spec(family = family, kappa = kappa, beta = beta, v0 = v0)
  compiled <- .compile_ae_map(spec, substrate)
  a_init <- .huff_attractiveness(
    as.vector(substrate$S[, 1]), kappa = kappa, beta = beta
  )
  .new_problem(
    model = "huff",
    substrate = substrate,
    state = list(
      name = "a",
      axis = "J",
      init = a_init,
      lower = 0,
      upper = Inf
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

.compile_huff_map <- function(spec, substrate) {
  S <- as.vector(substrate$S[, 1])
  a_fixed <- .huff_attractiveness(S, kappa = spec$kappa, beta = spec$beta)
  n <- length(a_fixed)
  n_theta <- length(spec$theta$names)
  zero_state <- matrix(0, n, n)
  bind <- function(theta) {
    theta <- .coerce_problem_theta(theta, spec$theta)
    K <- calc_decay(substrate$distance_active, method = spec$family,
                    sigma = theta[[spec$theta$names]], snap = TRUE)
    K[!is.finite(K)] <- 0
    plan <- substrate
    plan$Kd_active <- K
    plan$S <- S
    plan$kappa_supply <- spec$kappa * S
    class(plan) <- c("huff_compact_plan", "fca_compact_plan")

    # State-independent: the full state is fixed by theta (via the kernel) and
    # the exogenous attractiveness, so compute it once at bind time.
    state <- .huff_state(a_fixed, plan = plan, v0 = spec$v0)
    list(
      map = function(x) state$target,
      outputs = function(x) state,
      jac_state = function(x) {
        list(
          jac_state = zero_state,
          jac_utilization = zero_state,
          utilization = state$utilization,
          target = state$target
        )
      },
      jac_param = function(x) matrix(0, n, n_theta),
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

#' Evaluate rich outputs for a problem at arbitrary theta and state
#' @keywords internal
.problem_outputs_at <- function(problem, theta, state) {
  .chck_class(problem, "ae_problem", "problem")
  .validate_problem_state(problem, state)
  .validate_ae_outputs(.bind_theta(problem, theta)$outputs(state))
}

#' List model outputs that can be rewrapped onto the demand-cell surface
#' @keywords internal
.problem_available_surfaces <- function(problem, outputs) {
  .chck_class(problem, "ae_problem", "problem")
  if (!is.list(outputs)) {
    stop("`outputs` must be a list")
  }
  active_n <- length(problem$substrate$demand_kept_index)
  .surface_output_names(outputs, active_n)
}

#' Rewrap one origin-side problem output onto the demand raster template
#' @keywords internal
.problem_output_surface <- function(problem, theta, state, output) {
  .chck_class(problem, "ae_problem", "problem")
  if (!is.character(output) || length(output) != 1L || output == "") {
    stop("`output` must be a length-one character value")
  }
  outputs <- .problem_outputs_at(problem, theta = theta, state = state)
  .rewrap_problem_surface(
    outputs = outputs,
    output = output,
    template = problem$substrate$template,
    kept_cell_index = problem$substrate$demand_kept_index
  )
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
                           check = TRUE, diagnostics = TRUE,
                           keep_state_history = FALSE) {
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
    check = check,
    keep_state_history = keep_state_history
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
#'
#' `observed` may be a numeric vector (e.g. facility marginals) or a numeric
#' matrix (e.g. origin-by-facility flows) matching the shape of the requested
#' `output`; the state axis stays whatever the problem declares. `NA` cells in
#' `observed` are treated as unobserved (e.g. suppressed flow cells) and are
#' excluded from the loss and its gradient.
#' @keywords internal
.fit_problem_nfxp <- function(problem, observed, init, lower, upper,
                              output = "utilization",
                              loss = .weighted_sse_loss,
                              loss_grad = .weighted_sse_gradient,
                              loss_args = list(eta = 1),
                              lambda = 1, tol = 1e-8, max_iter = 1000,
                              norm = c("max", "l2"),
                              control = list(maxit = 25),
                              check = FALSE, penalty = 1e12,
                              gradient = FALSE, fd_eps = 1e-6) {
  .chck_class(problem, "ae_problem", "problem")
  target <- .fit_target_meta(observed, "observed")
  if (!is.function(loss)) {
    stop("`loss` must be a function")
  }
  if (isTRUE(gradient) && !is.function(loss_grad)) {
    stop("`loss_grad` must be a function when `gradient = TRUE`")
  }
  if (!is.list(loss_args)) {
    stop("`loss_args` must be a list")
  }
  if (!is.character(output) || length(output) != 1L || output == "") {
    stop("`output` must be a length-one character value")
  }
  .chck_positive_scalar(penalty, "penalty")
  .chck_positive_scalar(fd_eps, "fd_eps")
  norm <- match.arg(norm)
  gradient <- isTRUE(gradient)

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

  observed_values <- target$values
  warm <- NULL
  evaluate <- function(log_theta, keep_history = FALSE, warn = FALSE,
                       diagnostics = FALSE, update_warm = TRUE) {
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
      keep_history = keep_history,
      warn = warn,
      check = check,
      diagnostics = diagnostics
    )
    if (!isTRUE(fit$converged)) {
      return(NULL)
    }
    predicted_full <- .problem_fit_output(fit, output, required = FALSE)
    if (is.null(predicted_full) || !.fit_target_shape_ok(predicted_full, target)) {
      return(NULL)
    }
    predicted <- as.numeric(predicted_full)[target$mask]
    value <- do.call(
      loss,
      c(list(predicted = predicted, observed = observed_values), loss_args)
    )
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
      return(NULL)
    }
    if (isTRUE(update_warm)) {
      warm <<- fit$x_star
    }
    list(theta = theta, fit = fit, predicted = predicted, loss = as.numeric(value))
  }

  objective <- function(log_theta) {
    tryCatch({
      evaluated <- evaluate(log_theta, keep_history = FALSE, warn = FALSE,
                            diagnostics = FALSE, update_warm = TRUE)
      if (is.null(evaluated)) {
        return(penalty)
      }
      evaluated$loss
    }, error = function(e) penalty)
  }

  objective_grad <- NULL
  if (gradient) {
    objective_grad <- function(log_theta) {
      tryCatch({
        evaluated <- evaluate(log_theta, keep_history = FALSE, warn = FALSE,
                              diagnostics = FALSE, update_warm = FALSE)
        if (is.null(evaluated)) {
          return(.nfxp_fd_log_gradient(objective, log_theta, log(lower), log(upper),
                                       fd_eps = fd_eps))
        }
        sensitivity <- .problem_output_sensitivity(
          problem = problem,
          theta = evaluated$theta,
          x = evaluated$fit$x_star,
          output = output,
          fd_eps = fd_eps
        )
        if (nrow(sensitivity) != target$length) {
          return(.nfxp_fd_log_gradient(objective, log_theta, log(lower), log(upper),
                                       fd_eps = fd_eps))
        }
        sensitivity <- sensitivity[target$mask, , drop = FALSE]
        grad <- do.call(
          loss_grad,
          c(
            list(
              predicted = evaluated$predicted,
              observed = observed_values,
              sensitivity = sensitivity
            ),
            loss_args
          )
        )
        if (!is.numeric(grad) || length(grad) != length(log_theta) ||
            any(!is.finite(grad))) {
          return(.nfxp_fd_log_gradient(objective, log_theta, log(lower), log(upper),
                                       fd_eps = fd_eps))
        }
        as.numeric(grad) * exp(log_theta)
      }, error = function(e) {
        .nfxp_fd_log_gradient(objective, log_theta, log(lower), log(upper),
                              fd_eps = fd_eps)
      })
    }
  }

  elapsed <- system.time({
    opt <- stats::optim(
      par = log(init),
      fn = objective,
      gr = objective_grad,
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
  predicted_full <- .problem_fit_output(final, output, required = TRUE)
  .check_fit_target_shape(predicted_full, target, output)
  predicted <- as.numeric(predicted_full)[target$mask]
  loss_value <- do.call(
    loss,
    c(list(predicted = predicted, observed = observed_values), loss_args)
  )
  if (!is.numeric(loss_value) || length(loss_value) != 1L || !is.finite(loss_value)) {
    stop("final loss must be a finite numeric scalar")
  }
  if (is.null(target$dim)) {
    predicted_store <- .name_problem_vector(predicted_full,
                                            problem$substrate$facility_ids)
    observed_store <- .name_problem_vector(.coerce_numeric_vector(observed),
                                           problem$substrate$facility_ids)
  } else {
    predicted_store <- predicted_full
    observed_store <- observed
  }
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
      gradient = gradient,
      convergence = opt$convergence,
      message = opt$message,
      seconds = unname(elapsed),
      observed = observed_store,
      predicted = predicted_store,
      target_dim = target$dim,
      n_observed = target$n_observed,
      target_mask = if (all(target$mask)) NULL else target$mask,
      state = state,
      state_name = state_name,
      outputs = final$outputs,
      surface_meta = .problem_surface_meta(problem),
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
    loss_grad = .weighted_sse_gradient,
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

#' Rewrap one final-fit origin-side output onto the demand raster template
#' @keywords internal
.fit_output_surface <- function(fit, output) {
  .chck_class(fit, "ae_problem_nfxp_fit", "fit")
  if (!is.character(output) || length(output) != 1L || output == "") {
    stop("`output` must be a length-one character value")
  }
  .rewrap_problem_surface(
    outputs = fit$outputs,
    output = output,
    template = fit$surface_meta$template,
    kept_cell_index = fit$surface_meta$demand_kept_index
  )
}

#' Facility-level table for an AE calibration fit
#' @keywords internal
.ae_fit_facility_table <- function(fit) {
  .chck_class(fit, "ae_problem_nfxp_fit", "fit")
  if (is.null(fit$target_dim)) {
    predicted <- fit$predicted
    observed <- fit$observed
  } else {
    # Matrix-target fits (e.g. flows): predicted/observed are cell-level, so
    # the facility-side column is the model's utilization output instead.
    predicted <- .name_problem_vector(
      .coerce_numeric_vector(fit$outputs$utilization),
      names(fit$state)
    )
    observed <- NULL
  }
  n <- length(predicted)
  facility_id <- names(predicted)
  if (is.null(facility_id)) {
    facility_id <- as.character(seq_len(n))
  }
  out <- data.frame(
    facility_id = facility_id,
    predicted = as.numeric(predicted),
    state = as.numeric(fit$state),
    stringsAsFactors = FALSE
  )
  if (!is.null(observed) && length(observed) == n) {
    out$observed <- as.numeric(observed)
    out$residual <- out$predicted - out$observed
  }
  out
}

.problem_map_jac_state <- function(step, x, fd_eps = 1e-6) {
  if (is.function(step$jac_state)) {
    return(.problem_jac_state_matrix(step$jac_state(x)))
  }
  fd_jacobian_state(step$map, x, eps = fd_eps, check = FALSE)
}

.problem_jac_state_matrix <- function(jac) {
  if (is.list(jac)) {
    jac <- jac$jac_state
  }
  .chck_numeric_matrix(jac, "jac_state")
  .coerce_numeric_matrix(jac)
}

.nfxp_fd_log_gradient <- function(objective, log_theta, lower, upper,
                                  fd_eps = 1e-6) {
  .chck_positive_scalar(fd_eps, "fd_eps")
  log_theta <- .coerce_numeric_vector(log_theta)
  lower <- .coerce_numeric_vector(lower)
  upper <- .coerce_numeric_vector(upper)
  grad <- numeric(length(log_theta))
  for (i in seq_along(log_theta)) {
    h <- as.numeric(fd_eps) * max(abs(log_theta[i]), 1)
    hi <- log_theta
    lo <- log_theta
    hi[i] <- min(log_theta[i] + h, upper[i])
    lo[i] <- max(log_theta[i] - h, lower[i])
    if (hi[i] > log_theta[i] && lo[i] < log_theta[i]) {
      grad[i] <- (objective(hi) - objective(lo)) / (hi[i] - lo[i])
    } else if (hi[i] > log_theta[i]) {
      grad[i] <- (objective(hi) - objective(log_theta)) / (hi[i] - log_theta[i])
    } else if (lo[i] < log_theta[i]) {
      grad[i] <- (objective(log_theta) - objective(lo)) / (log_theta[i] - lo[i])
    } else {
      grad[i] <- 0
    }
  }
  grad[!is.finite(grad)] <- 0
  grad
}

.problem_map_jac_param <- function(problem, theta, x, step = NULL,
                                   fd_eps = 1e-6) {
  if (is.null(step)) {
    step <- .bind_theta(problem, theta)
  }
  if (is.function(step$jac_param)) {
    jac <- step$jac_param(x)
    if (is.list(jac)) {
      jac <- jac$jac_param
    }
    .chck_numeric_matrix(jac, "jac_param")
    return(.coerce_numeric_matrix(jac))
  }
  fn <- function(theta_value) {
    .bind_theta(problem, theta_value)$map(x)
  }
  fd_jacobian(fn, theta, eps = fd_eps, check = FALSE)
}

.problem_output_jac_state <- function(step, x, output, fd_eps = 1e-6,
                                      jac_state_result = NULL) {
  if (is.function(step$jac_output_state)) {
    jac <- step$jac_output_state(x, output = output)
    .chck_numeric_matrix(jac, "jac_output_state")
    return(.coerce_numeric_matrix(jac))
  }
  if (is.null(jac_state_result) && is.function(step$jac_state)) {
    jac_state_result <- step$jac_state(x)
  }
  jac_name <- paste0("jac_", output)
  if (is.list(jac_state_result) && jac_name %in% names(jac_state_result)) {
    jac <- jac_state_result[[jac_name]]
    .chck_numeric_matrix(jac, jac_name)
    return(.coerce_numeric_matrix(jac))
  }
  fn <- function(x_value) {
    .problem_output_from_step(step, x_value, output, required = TRUE)
  }
  fd_jacobian(fn, x, eps = fd_eps, check = FALSE)
}

.problem_output_jac_param <- function(problem, theta, x, output,
                                      fd_eps = 1e-6) {
  fn <- function(theta_value) {
    step <- .bind_theta(problem, theta_value)
    .problem_output_from_step(step, x, output, required = TRUE)
  }
  fd_jacobian(fn, theta, eps = fd_eps, check = FALSE)
}

.problem_output_sensitivity <- function(problem, theta, x,
                                        output = "utilization",
                                        fd_eps = 1e-6) {
  step <- .bind_theta(problem, theta)
  jac_state_result <- if (is.function(step$jac_state)) step$jac_state(x) else NULL
  jac_state <- if (is.null(jac_state_result)) {
    fd_jacobian_state(step$map, x, eps = fd_eps, check = FALSE)
  } else {
    .problem_jac_state_matrix(jac_state_result)
  }
  jac_param <- .problem_map_jac_param(problem, theta, x, step = step,
                                      fd_eps = fd_eps)
  state_sensitivity <- implicit_gradient(jac_state, jac_param)
  output_jac_state <- .problem_output_jac_state(
    step, x, output = output, fd_eps = fd_eps,
    jac_state_result = jac_state_result
  )
  output_jac_param <- .problem_output_jac_param(
    problem, theta, x, output = output, fd_eps = fd_eps
  )
  output_jac_state %*% state_sensitivity + output_jac_param
}

.problem_output_from_step <- function(step, x, output, required = TRUE) {
  outputs <- step$outputs(x)
  if (!is.list(outputs) || !output %in% names(outputs)) {
    if (required) {
      stop("problem outputs do not include requested output `", output, "`")
    }
    return(NULL)
  }
  value <- outputs[[output]]
  ok <- tryCatch({
    .chck_target_numeric(value, output)
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
  # Flattened (column-major) so FD Jacobians stack matrix outputs as rows.
  as.numeric(value)
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

#' Validate a calibration value as a finite numeric vector or matrix
#' @keywords internal
.chck_target_numeric <- function(x, name = "Input") {
  .chck_is_numeric(x, name = name)
  if (!is.null(dim(x)) && length(dim(x)) != 2L) {
    stop(sprintf("%s must be a numeric vector or matrix", name))
  }
  if (any(!is.finite(x))) {
    stop(sprintf("%s must contain only finite values", name))
  }
  invisible(TRUE)
}

#' Describe a calibration target: shape, observation mask, observed values
#'
#' `NA` cells mark unobserved targets (e.g. HSAF flow suppression below 11
#' cases) and are excluded from the loss; predicted/observed comparison is
#' over flattened (column-major) values at the observed cells only.
#' @keywords internal
.fit_target_meta <- function(observed, name = "observed") {
  .chck_is_numeric(observed, name = name)
  if (!is.null(dim(observed)) && length(dim(observed)) != 2L) {
    stop(sprintf("%s must be a numeric vector or matrix", name))
  }
  if (length(observed) == 0L) {
    stop(sprintf("%s must not be empty", name))
  }
  flat <- as.numeric(observed)
  mask <- !is.na(flat)
  if (!any(mask)) {
    stop(sprintf("%s must contain at least one non-missing value", name))
  }
  if (any(!is.finite(flat[mask]))) {
    stop(sprintf("%s non-missing values must be finite", name))
  }
  list(
    dim = dim(observed),
    length = length(flat),
    mask = mask,
    values = flat[mask],
    n_observed = sum(mask)
  )
}

#' Does a predicted output have the same shape as the calibration target?
#' @keywords internal
.fit_target_shape_ok <- function(predicted, target) {
  if (!is.numeric(predicted)) {
    return(FALSE)
  }
  pdim <- dim(predicted)
  if (is.null(target$dim)) {
    return(is.null(pdim) && length(predicted) == target$length)
  }
  !is.null(pdim) && length(pdim) == 2L && all(pdim == target$dim)
}

#' Require shape agreement between a predicted output and the target
#' @keywords internal
.check_fit_target_shape <- function(predicted, target, output) {
  if (.fit_target_shape_ok(predicted, target)) {
    return(invisible(TRUE))
  }
  describe <- function(d, n) {
    if (is.null(d)) paste0("length ", n) else paste0("dim ", paste(d, collapse = " x "))
  }
  stop("output `", output, "` (", describe(dim(predicted), length(predicted)),
       ") does not match `observed` (", describe(target$dim, target$length), ")")
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
    .chck_target_numeric(value, output)
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
  if (is.null(dim(value))) {
    .coerce_numeric_vector(value)
  } else {
    .coerce_numeric_matrix(value)
  }
}

.name_problem_vector <- function(x, ids) {
  if (!is.null(ids) && length(x) == length(ids)) {
    names(x) <- ids
  }
  x
}

.problem_surface_meta <- function(problem) {
  list(
    template = problem$substrate$template,
    demand_kept_index = problem$substrate$demand_kept_index
  )
}

.rewrap_problem_surface <- function(outputs, output, template, kept_cell_index) {
  if (is.null(template) || is.null(kept_cell_index)) {
    stop("surface metadata is not available for this AE object")
  }
  if (!is.list(outputs) || !output %in% names(outputs)) {
    stop("problem outputs do not include requested output `", output, "`")
  }

  active_n <- length(kept_cell_index)
  value <- outputs[[output]]
  surface_names <- .surface_output_names(outputs, active_n)
  if (!is.numeric(value) || !is.null(dim(value)) || length(value) != active_n) {
    msg <- paste0("output `", output, "` is not an origin-side surface")
    if (length(surface_names) > 0) {
      msg <- paste0(msg, "; available surfaces: ",
                    paste(surface_names, collapse = ", "))
    }
    stop(msg)
  }
  .chck_numeric_vector(value, output, finite = FALSE)
  if (any(!is.finite(value) & !is.na(value))) {
    stop(output, " must contain only finite or missing values")
  }
  out <- .rewrap_cells(.coerce_numeric_vector(value), template, kept_cell_index)
  names(out) <- output
  out
}

.surface_output_names <- function(outputs, active_n) {
  names(Filter(
    function(value) {
      is.numeric(value) &&
        is.null(dim(value)) &&
        length(value) == active_n &&
        all(is.finite(value) | is.na(value))
    },
    outputs
  ))
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
