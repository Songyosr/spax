.mk_ae_problem_data <- function() {
  demand <- terra::rast(nrows = 2, ncols = 2)
  terra::values(demand) <- c(10, 20, 30, 40)

  d1 <- terra::rast(nrows = 2, ncols = 2)
  d2 <- terra::rast(nrows = 2, ncols = 2)
  terra::values(d1) <- c(1, 2, 4, 5)
  terra::values(d2) <- c(5, 4, 2, 1)
  distance <- c(d1, d2)
  names(distance) <- c("facility1", "facility2")

  supply <- c(facility1 = 30, facility2 = 45)
  list(demand = demand, distance = distance, supply = supply)
}

.mk_two_theta_problem <- function() {
  contract <- list(
    names = c("alpha", "beta"),
    lower = c(0, 0),
    upper = c(10, 10)
  )
  .new_problem(
    model = "toy",
    substrate = list(facility_ids = c("facility1", "facility2")),
    state = list(
      name = "x",
      axis = "J",
      init = c(1, 1),
      lower = c(0, 0),
      upper = c(10, 10)
    ),
    theta = contract,
    bind = function(theta) {
      theta <- .coerce_problem_theta(theta, contract)
      list(
        map = function(x) theta,
        outputs = function(x) list(target = theta, utilization = x),
        theta = theta
      )
    },
    metadata = list(spec = list(family = "toy"))
  )
}

test_that(".sae_problem binds theta and matches existing SAE map helpers", {
  td <- .mk_ae_problem_data()
  theta <- c(sigma = 2)
  p <- .sae_problem(
    td$demand, td$supply, td$distance,
    family = "gaussian", kappa = 1 / 3, beta = 20, eps = 1e-8
  )
  step <- .bind_theta(p, theta)

  kernel <- calc_decay(td$distance, method = "gaussian", sigma = theta, snap = TRUE)
  plan <- .sae_compact_plan(td$demand, td$supply, kernel, kappa = 1 / 3)
  x <- c(0.45, 0.65)

  expect_s3_class(p, "ae_problem")
  expect_s3_class(step, "ae_bound_step")
  expect_equal(names(formals(step$map)), "x")
  expect_equal(step$map(x), .sae_map(x, plan = plan, beta = 20, eps = 1e-8),
               tolerance = 1e-12)
  expect_equal(step$outputs(x)$utilization,
               .sae_state(x, plan = plan, beta = 20, eps = 1e-8)$utilization,
               tolerance = 1e-12)
  expect_equal(step$jac_state(x)$jac_state,
               .sae_jacobian_state(x, plan = plan, beta = 20, eps = 1e-8)$jac_state,
               tolerance = 1e-12)
})

test_that(".haae_problem binds theta and matches existing HAAE map helpers", {
  td <- .mk_ae_problem_data()
  theta <- c(sigma = 2)
  p <- .haae_problem(
    td$demand, td$supply, td$distance,
    family = "gaussian", kappa = 1 / 3, eps = 1e-8, a_min = 1e-6
  )
  step <- .bind_theta(p, theta)

  kernel <- calc_decay(td$distance, method = "gaussian", sigma = theta, snap = TRUE)
  plan <- .haae_compact_plan(td$demand, td$supply, kernel, kappa = 1 / 3)
  x <- c(0.7, 1.1)

  expect_equal(step$map(x), .haae_map(x, plan = plan, eps = 1e-8, a_min = 1e-6),
               tolerance = 1e-12)
  expect_equal(step$outputs(x)$utilization,
               .haae_state(x, plan = plan, eps = 1e-8, a_min = 1e-6)$utilization,
               tolerance = 1e-12)
  expect_equal(step$jac_state(x)$jac_state,
               .haae_jacobian_state(x, plan = plan, eps = 1e-8,
                                    a_min = 1e-6)$jac_state,
               tolerance = 1e-12)
})

test_that(".solve_problem reproduces existing SAE equilibrium", {
  td <- .mk_ae_problem_data()
  theta <- c(sigma = 2)
  p <- .sae_problem(
    td$demand, td$supply, td$distance,
    family = "gaussian", kappa = 1 / 3, beta = 20, eps = 1e-8
  )

  kernel <- calc_decay(td$distance, method = "gaussian", sigma = theta, snap = TRUE)
  plan <- .sae_compact_plan(td$demand, td$supply, kernel, kappa = 1 / 3)
  old <- solve_equilibrium(
    map = .sae_map, x0 = c(1, 1), plan = plan, beta = 20, eps = 1e-8,
    lambda = 0.7, tol = 1e-10, max_iter = 500, check = FALSE
  )
  old_util <- .sae_state(old$x_star, plan = plan, beta = 20, eps = 1e-8)$utilization
  new <- .solve_problem(
    p, theta = theta, lambda = 0.7, tol = 1e-10,
    max_iter = 500, check = FALSE
  )

  expect_true(new$converged)
  expect_equal(new$x_star, old$x_star, tolerance = 1e-10)
  expect_equal(new$utilization, old_util, tolerance = 1e-10)
})

test_that(".solve_problem reports spectral radius and contraction by default", {
  td <- .mk_ae_problem_data()
  p <- .sae_problem(
    td$demand, td$supply, td$distance,
    family = "gaussian", kappa = 1 / 3, beta = 20
  )
  fit <- .solve_problem(
    p, theta = c(sigma = 2), lambda = 0.7, tol = 1e-10,
    max_iter = 500, check = FALSE
  )

  expect_true(fit$converged)
  expect_true(is.numeric(fit$spectral_radius))
  expect_false(is.na(fit$spectral_radius))
  expect_lt(fit$spectral_radius, 1)
  expect_identical(fit$contraction, fit$spectral_radius < 1)

  bare <- .solve_problem(
    p, theta = c(sigma = 2), lambda = 0.7, tol = 1e-10,
    max_iter = 500, check = FALSE, diagnostics = FALSE
  )
  expect_null(bare$spectral_radius)
})

test_that(".solve_problem threads optional state history", {
  td <- .mk_ae_problem_data()
  p <- .sae_problem(
    td$demand, td$supply, td$distance,
    family = "gaussian", kappa = 1 / 3, beta = 20
  )
  fit <- .solve_problem(
    p, theta = c(sigma = 2), lambda = 0.7, tol = 1e-10,
    max_iter = 500, check = FALSE, keep_state_history = TRUE
  )

  expect_true(fit$converged)
  expect_equal(ncol(fit$state_history), length(p$state$init))
  expect_equal(fit$state_history[nrow(fit$state_history), ], fit$x_star)
})

test_that("problem outputs can be rendered as origin-side surfaces", {
  td <- .mk_ae_problem_data()
  terra::values(td$demand) <- c(10, 0, 30, 40)
  problem <- .sae_problem(
    td$demand, td$supply, td$distance,
    family = "gaussian", kappa = 1 / 3, beta = 20
  )
  solved <- .solve_problem(
    problem, theta = c(sigma = 2), lambda = 0.7, tol = 1e-10,
    max_iter = 500, check = FALSE
  )

  outputs <- .problem_outputs_at(problem, theta = c(sigma = 2),
                                 state = solved$x_star)
  expect_equal(outputs$utilization, solved$outputs$utilization)
  expect_true("access" %in% .problem_available_surfaces(problem, outputs))
  expect_false("utilization" %in% .problem_available_surfaces(problem, outputs))

  surface <- .problem_output_surface(
    problem, theta = c(sigma = 2), state = solved$x_star, output = "access"
  )
  expect_s4_class(surface, "SpatRaster")
  values <- terra::values(surface)[, 1]
  expect_true(is.na(values[2]))
  expect_equal(values[problem$substrate$demand_kept_index], outputs$access)
  expect_error(
    .problem_output_surface(
      problem, theta = c(sigma = 2), state = solved$x_star,
      output = "utilization"
    ),
    "not an origin-side surface"
  )
  expect_error(
    .problem_output_surface(
      problem, theta = c(sigma = 2), state = solved$x_star,
      output = "missing"
    ),
    "requested output"
  )
})

test_that("problem surface discovery is model generic", {
  td <- .mk_ae_problem_data()
  problem <- .haae_problem(
    td$demand, td$supply, td$distance,
    family = "gaussian", kappa = 1 / 3
  )
  solved <- .solve_problem(
    problem, theta = c(sigma = 2), lambda = 0.7, tol = 1e-10,
    max_iter = 500, check = FALSE
  )
  outputs <- .problem_outputs_at(problem, theta = c(sigma = 2),
                                 state = solved$x_star)

  expect_true("pooled" %in% .problem_available_surfaces(problem, outputs))
  expect_s4_class(
    .problem_output_surface(
      problem, theta = c(sigma = 2), state = solved$x_star, output = "pooled"
    ),
    "SpatRaster"
  )
})

test_that(".fit_problem_decay records eta and the fitted spectral radius", {
  td <- .mk_ae_problem_data()
  problem <- .sae_problem(
    td$demand, td$supply, td$distance,
    family = "gaussian", kappa = 1 / 3, beta = 20
  )
  observed <- .solve_problem(
    problem, theta = c(sigma = 2), lambda = 0.7, tol = 1e-10, max_iter = 500
  )$utilization
  fit <- .fit_problem_decay(
    problem = problem, observed = observed,
    init = 1.5, lower = 0.5, upper = 4, lambda = 0.7, tol = 1e-10,
    max_iter = 500, eta = 2, control = list(maxit = 20)
  )

  expect_equal(fit$eta, 2)
  expect_true(is.numeric(fit$spectral_radius))
  expect_lt(fit$spectral_radius, 1)
})

test_that(".fit_problem_nfxp reproduces SAE generated-data fitting", {
  td <- .mk_ae_problem_data()
  problem <- .sae_problem(
    td$demand, td$supply, td$distance,
    family = "gaussian", kappa = 1 / 3, beta = 20
  )
  observed <- .solve_problem(
    problem, theta = c(sigma = 2), lambda = 0.7, tol = 1e-10, max_iter = 500
  )$utilization

  fit <- .fit_problem_nfxp(
    problem = problem,
    observed = observed,
    init = c(sigma = 1.5),
    lower = c(sigma = 0.5),
    upper = c(sigma = 4),
    lambda = 0.7,
    tol = 1e-10,
    max_iter = 500,
    control = list(maxit = 20)
  )

  expect_s3_class(fit, "ae_problem_nfxp_fit")
  expect_equal(fit$convergence, 0)
  expect_lt(fit$loss, 1e-6)
  expect_equal(fit$theta["sigma"], c(sigma = 2), tolerance = 1e-3)
  expect_equal(unname(fit$predicted), unname(observed), tolerance = 1e-4)
})

test_that(".fit_problem_nfxp stores inspection fields and QoL helpers work", {
  td <- .mk_ae_problem_data()
  problem <- .sae_problem(
    td$demand, td$supply, td$distance,
    family = "gaussian", kappa = 1 / 3, beta = 20
  )
  observed <- .solve_problem(
    problem, theta = c(sigma = 2), lambda = 0.7, tol = 1e-10, max_iter = 500
  )$utilization

  fit <- .fit_problem_nfxp(
    problem = problem,
    observed = observed,
    init = c(sigma = 1.5),
    lower = c(sigma = 0.5),
    upper = c(sigma = 4),
    lambda = 0.7,
    tol = 1e-10,
    max_iter = 500,
    control = list(maxit = 20)
  )

  expect_equal(unname(fit$observed), unname(observed))
  expect_named(fit$observed, c("facility1", "facility2"))
  expect_true(is.list(fit$outputs))
  expect_equal(fit$surface_meta$demand_kept_index,
               problem$substrate$demand_kept_index)

  table <- .ae_fit_facility_table(fit)
  expect_named(table, c("facility_id", "predicted", "state", "observed",
                        "residual"))
  expect_equal(table$residual, table$predicted - table$observed)

  surface <- .fit_output_surface(fit, "access")
  expect_s4_class(surface, "SpatRaster")
  expect_equal(
    terra::values(surface)[problem$substrate$demand_kept_index, 1],
    fit$outputs$access
  )

  expect_output(print(fit), "ae_problem_nfxp_fit")
  expect_output(print(fit$equilibrium), "ae_equilibrium")
  expect_s3_class(summary(fit), "summary.ae_problem_nfxp_fit")
  expect_output(print(summary(fit)), "theta")

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(plot(fit, type = "fit"))
  expect_no_error(plot(fit, type = "convergence"))
  expect_no_error(plot(fit, type = "state"))
})

test_that("AE QoL methods smoke-test on HAAE fits", {
  td <- .mk_ae_problem_data()
  problem <- .haae_problem(
    td$demand, td$supply, td$distance,
    family = "gaussian", kappa = 1 / 3
  )
  observed <- .solve_problem(
    problem, theta = c(sigma = 2), lambda = 0.7, tol = 1e-10, max_iter = 500
  )$utilization

  fit <- .fit_problem_nfxp(
    problem = problem,
    observed = observed,
    init = c(sigma = 1.5),
    lower = c(sigma = 0.5),
    upper = c(sigma = 4),
    lambda = 0.7,
    tol = 1e-10,
    max_iter = 500,
    control = list(maxit = 20)
  )

  expect_output(print(fit), "haae")
  expect_s3_class(summary(fit), "summary.ae_problem_nfxp_fit")
  expect_s4_class(.fit_output_surface(fit, "pooled"), "SpatRaster")
})

test_that(".fit_problem_nfxp reproduces HAAE generated-data fitting", {
  td <- .mk_ae_problem_data()
  problem <- .haae_problem(
    td$demand, td$supply, td$distance,
    family = "gaussian", kappa = 1 / 3
  )
  observed <- .solve_problem(
    problem, theta = c(sigma = 2), lambda = 0.7, tol = 1e-10, max_iter = 500
  )$utilization

  fit <- .fit_problem_nfxp(
    problem = problem,
    observed = observed,
    init = c(sigma = 1.5),
    lower = c(sigma = 0.5),
    upper = c(sigma = 4),
    lambda = 0.7,
    tol = 1e-10,
    max_iter = 500,
    control = list(maxit = 20)
  )

  expect_s3_class(fit, "ae_problem_nfxp_fit")
  expect_equal(fit$convergence, 0)
  expect_lt(fit$loss, 1e-6)
  expect_equal(fit$theta["sigma"], c(sigma = 2), tolerance = 1e-3)
  expect_equal(unname(fit$predicted), unname(observed), tolerance = 1e-4)
})

test_that(".fit_problem_nfxp supports named multi-parameter theta", {
  problem <- .mk_two_theta_problem()
  observed <- c(2, 3)

  fit <- .fit_problem_nfxp(
    problem = problem,
    observed = observed,
    init = c(alpha = 1, beta = 1.5),
    lower = c(alpha = 0.5, beta = 0.5),
    upper = c(alpha = 4, beta = 4),
    tol = 1e-10,
    max_iter = 20,
    loss_args = list(eta = 1),
    control = list(maxit = 80)
  )

  expect_s3_class(fit, "ae_problem_nfxp_fit")
  expect_equal(fit$convergence, 0)
  expect_lt(fit$loss, 1e-6)
  expect_equal(fit$theta, c(alpha = 2, beta = 3), tolerance = 1e-3)
  expect_named(fit$predicted, c("facility1", "facility2"))
})

test_that(".problem_output_sensitivity supports finite-difference theta Jacobians", {
  problem <- .mk_two_theta_problem()
  theta <- c(alpha = 2, beta = 3)
  step <- .bind_theta(problem, theta)
  x <- step$map(problem$state$init)

  sensitivity <- .problem_output_sensitivity(
    problem = problem,
    theta = theta,
    x = x,
    output = "utilization"
  )

  expect_equal(unname(sensitivity), diag(2), tolerance = 1e-5)
})

test_that(".problem_output_jac_state uses provided output Jacobians", {
  step <- list(
    map = function(x) x,
    outputs = function(x) list(target = x, utilization = x),
    jac_state = function(x) {
      list(
        jac_state = matrix(0, 1, 1),
        jac_utilization = matrix(9, 1, 1)
      )
    }
  )

  jac <- .problem_output_jac_state(step, x = 1, output = "utilization")

  expect_equal(jac, matrix(9, 1, 1))
})

test_that(".fit_problem_nfxp supports gradient-backed multi-parameter fitting", {
  problem <- .mk_two_theta_problem()
  observed <- c(2, 3)

  fit <- .fit_problem_nfxp(
    problem = problem,
    observed = observed,
    init = c(alpha = 1, beta = 1.5),
    lower = c(alpha = 0.5, beta = 0.5),
    upper = c(alpha = 4, beta = 4),
    tol = 1e-10,
    max_iter = 20,
    loss_args = list(eta = 1),
    control = list(maxit = 80),
    gradient = TRUE
  )

  expect_s3_class(fit, "ae_problem_nfxp_fit")
  expect_true(fit$gradient)
  expect_equal(fit$convergence, 0)
  expect_lt(fit$loss, 1e-6)
  expect_equal(fit$theta, c(alpha = 2, beta = 3), tolerance = 1e-3)
})

test_that(".fit_problem_nfxp falls back when implicit gradients are unavailable", {
  contract <- list(names = "sigma", lower = 0, upper = 10)
  problem <- .new_problem(
    model = "toy",
    substrate = list(facility_ids = "facility1"),
    state = list(name = "x", axis = "J", init = 1, lower = 0, upper = 10),
    theta = contract,
    bind = function(theta) {
      theta <- .coerce_problem_theta(theta, contract)
      list(
        map = function(x) theta,
        outputs = function(x) list(target = theta, utilization = x),
        jac_state = function(x) matrix(Inf, 1, 1),
        theta = theta
      )
    },
    metadata = list(spec = list(family = "toy"))
  )

  fit <- .fit_problem_nfxp(
    problem = problem,
    observed = 2,
    init = c(sigma = 4),
    lower = c(sigma = 0.5),
    upper = c(sigma = 5),
    gradient = TRUE,
    control = list(maxit = 40)
  )

  expect_true(fit$gradient)
  expect_equal(fit$convergence, 0)
  expect_lt(fit$loss, 1e-6)
  expect_equal(fit$theta, c(sigma = 2), tolerance = 1e-3)
})

test_that(".fit_problem_decay warns when the optimum hits a search bound", {
  td <- .mk_ae_problem_data()
  problem <- .sae_problem(
    td$demand, td$supply, td$distance,
    family = "gaussian", kappa = 1 / 3, beta = 20
  )
  observed <- .solve_problem(
    problem, theta = c(sigma = 2), lambda = 0.7, tol = 1e-10, max_iter = 500
  )$utilization
  # True optimum is sigma = 2, but the box floor (3) forces a boundary solution.
  expect_warning(
    .fit_problem_decay(
      problem = problem, observed = observed,
      init = 4, lower = 3, upper = 8, lambda = 0.7, tol = 1e-10,
      max_iter = 500, control = list(maxit = 20)
    ),
    "lower search bound"
  )
})

test_that("problem runners validate theta and initial state without mutation", {
  td <- .mk_ae_problem_data()
  p <- .sae_problem(td$demand, td$supply, td$distance, family = "gaussian")

  expect_error(.bind_theta(p, c(alpha = 2)), "missing required")
  expect_error(
    .solve_problem(p, theta = 2, x0 = c(-0.1, 1), check = FALSE),
    "outside the problem state bounds"
  )
  expect_no_error(.bind_theta(p, 2))
})

test_that(".fit_problem_nfxp validates calibration contracts", {
  td <- .mk_ae_problem_data()
  problem <- .sae_problem(td$demand, td$supply, td$distance, family = "gaussian")
  observed <- .solve_problem(
    problem, theta = c(sigma = 2), lambda = 0.7, tol = 1e-10, max_iter = 500
  )$utilization

  expect_error(
    .fit_problem_nfxp(
      problem, observed, init = c(alpha = 1), lower = 0.5, upper = 4
    ),
    "missing required"
  )
  expect_error(
    .fit_problem_nfxp(problem, observed, init = 1, lower = 0, upper = 4),
    "positive"
  )
  expect_error(
    .fit_problem_nfxp(problem, observed, init = 1, lower = 2, upper = 1),
    "less than"
  )
  expect_error(
    .fit_problem_nfxp(
      problem, observed, init = 1, lower = 0.5, upper = 4,
      output = "missing", control = list(maxit = 1)
    ),
    "requested output"
  )
  expect_error(
    .fit_problem_nfxp(
      problem, observed, init = 1, lower = 0.5, upper = 4,
      loss_grad = NULL, gradient = TRUE
    ),
    "loss_grad"
  )
})

test_that(".fit_problem_decay reproduces SAE decay fitting oracle", {
  td <- .mk_ae_problem_data()
  problem <- .sae_problem(
    td$demand, td$supply, td$distance,
    family = "gaussian", kappa = 1 / 3, beta = 20
  )
  observed <- .solve_problem(
    problem, theta = c(sigma = 2), lambda = 0.7, tol = 1e-10, max_iter = 500
  )$utilization

  fit <- .fit_problem_decay(
    problem = problem,
    observed = observed,
    init = 1.5,
    lower = 0.5,
    upper = 4,
    lambda = 0.7,
    tol = 1e-10,
    max_iter = 500,
    eta = 1,
    control = list(maxit = 20)
  )

  expect_s3_class(fit, "ae_problem_decay_fit")
  expect_equal(fit$convergence, 0)
  expect_lt(fit$wsse, 1e-6)
  expect_equal(fit$theta_hat, 2, tolerance = 1e-3)
  expect_equal(unname(fit$predicted), unname(observed), tolerance = 1e-4)
})

test_that(".fit_problem_decay reproduces HAAE decay fitting oracle", {
  td <- .mk_ae_problem_data()
  problem <- .haae_problem(
    td$demand, td$supply, td$distance,
    family = "gaussian", kappa = 1 / 3
  )
  observed <- .solve_problem(
    problem, theta = c(sigma = 2), lambda = 0.7, tol = 1e-10, max_iter = 500
  )$utilization

  fit <- .fit_problem_decay(
    problem = problem,
    observed = observed,
    init = 1.5,
    lower = 0.5,
    upper = 4,
    lambda = 0.7,
    tol = 1e-10,
    max_iter = 500,
    eta = 1,
    control = list(maxit = 20)
  )

  expect_s3_class(fit, "ae_problem_decay_fit")
  expect_equal(fit$convergence, 0)
  expect_lt(fit$wsse, 1e-6)
  expect_equal(fit$theta_hat, 2, tolerance = 1e-3)
  expect_equal(unname(fit$predicted), unname(observed), tolerance = 1e-4)
})
