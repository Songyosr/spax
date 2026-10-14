# SAE provider. Keep or retire is an open maintainer decision; every SAE-
# specific check lives here so retiring SAE means deleting this file.

.mk_sae_data <- function() {
  td <- ae_toy_inputs("2x2")
  td$kernel <- calc_decay(td$distance, method = "gaussian", sigma = 2, snap = TRUE)
  td
}

.manual_sae_state <- function(c, D, K, S, kappa, beta, eps) {
  soft_cap <- function(x) .sae_soft_cap(x, beta = beta)
  opportunity <- sweep(K, 2, c, `*`)
  pooled <- rowSums(opportunity)
  access <- soft_cap(pooled)
  denom <- rowSums(opportunity)
  choice <- sweep(opportunity, 1, ifelse(denom > 0, 1 / denom, 0), `*`)
  allocation <- sweep(choice, 1, access, `*`)
  utilization <- as.vector(crossprod(allocation, D))
  adequacy <- kappa * S / (utilization + eps)
  target <- soft_cap(adequacy)
  list(
    target = target,
    utilization = utilization,
    adequacy = adequacy,
    access = access,
    allocation = allocation,
    opportunity = opportunity
  )
}

test_that(".sae_state matches explicit compact matrix algebra", {
  td <- .mk_sae_data()
  plan <- .sae_compact_plan(td$demand, td$supply, td$kernel, kappa = 1 / 3)
  c <- c(a = 0.6, b = 0.8)
  beta <- 20
  eps <- 1e-8

  got <- .sae_state(c, plan = plan, beta = beta, eps = eps)
  expected <- .manual_sae_state(
    c = unname(c),
    D = terra::values(td$demand)[, 1],
    K = terra::values(td$kernel, mat = TRUE),
    S = unname(td$supply),
    kappa = 1 / 3,
    beta = beta,
    eps = eps
  )

  expect_equal(got$target, expected$target, tolerance = 1e-12)
  expect_equal(got$utilization, expected$utilization, tolerance = 1e-12)
  expect_equal(got$adequacy, expected$adequacy, tolerance = 1e-12)
  expect_equal(got$access, expected$access, tolerance = 1e-12)
  expect_equal(got$allocation, expected$allocation, tolerance = 1e-12)
  # The smooth cap tends to 1 + log1p(exp(-beta)) / beta, not exactly 1.
  expect_gt(.sae_soft_cap(100, beta = 1), 1)
  expect_equal(.sae_soft_cap(100, beta = 1), 1 + log1p(exp(-1)))
})

test_that(".sae_jacobian_state matches finite differences at an interior state", {
  td <- .mk_sae_data()
  plan <- .sae_compact_plan(td$demand, td$supply, td$kernel, kappa = 1 / 3)
  c <- c(0.45, 0.65)

  analytic <- .sae_jacobian_state(c, plan = plan, beta = 20, eps = 1e-8)
  fd <- fd_jacobian_state(.sae_map, c, plan = plan, beta = 20, eps = 1e-8)

  expect_equal(analytic$jac_state, fd, tolerance = 1e-5, ignore_attr = TRUE)
})

test_that(".sae_jacobian_state supports damped-iteration stability diagnostics", {
  td <- .mk_sae_data()
  plan <- .sae_compact_plan(td$demand, td$supply, td$kernel, kappa = 1 / 3)
  lambda <- 0.7
  fit <- solve_equilibrium(
    map = .sae_map, x0 = c(1, 1), plan = plan, beta = 20, eps = 1e-8,
    lambda = lambda, tol = 1e-10, max_iter = 500, check = FALSE
  )
  util <- .sae_state(fit$x_star, plan = plan, beta = 20, eps = 1e-8)$utilization

  jac <- .sae_jacobian_state(fit$x_star, plan = plan, beta = 20, eps = 1e-8)
  iteration_jac <- (1 - lambda) * diag(length(fit$x_star)) +
    lambda * jac$jac_state

  expect_equal(jac$utilization, util, tolerance = 1e-10)
  expect_lt(spectral_radius(iteration_jac), 1)
})

test_that("SAE keeps the first slice to one supply measure", {
  td <- .mk_sae_data()
  supply <- cbind(doctors = td$supply, nurses = td$supply)

  expect_error(
    .sae_compact_plan(td$demand, supply, td$kernel, kappa = 1 / 3),
    "one supply measure"
  )
  expect_error(.sae_problem(td$demand, supply, td$distance), "one supply measure")
})

test_that(".sae_problem binds and solves through the compact SAE map", {
  td <- .mk_sae_data()
  theta <- c(sigma = 2)
  p <- .sae_problem(
    td$demand, td$supply, td$distance,
    family = "gaussian", kappa = 1 / 3, beta = 20, eps = 1e-8
  )
  step <- .bind_theta(p, theta)
  plan <- .sae_compact_plan(td$demand, td$supply, td$kernel, kappa = 1 / 3)
  x <- c(0.45, 0.65)

  expect_s3_class(p, "ae_problem")
  expect_s3_class(step, "ae_bound_step")
  expect_equal(step$map(x), .sae_map(x, plan = plan, beta = 20, eps = 1e-8),
               tolerance = 1e-12)
  expect_equal(step$outputs(x)$utilization,
               .sae_state(x, plan = plan, beta = 20, eps = 1e-8)$utilization,
               tolerance = 1e-12)
  expect_equal(step$jac_state(x)$jac_state,
               .sae_jacobian_state(x, plan = plan, beta = 20, eps = 1e-8)$jac_state,
               tolerance = 1e-12)

  old <- solve_equilibrium(
    map = .sae_map, x0 = c(1, 1), plan = plan, beta = 20, eps = 1e-8,
    lambda = 0.7, tol = 1e-10, max_iter = 500, check = FALSE
  )
  new <- .solve_problem(p, theta = theta, lambda = 0.7, tol = 1e-10,
                        max_iter = 500, check = FALSE)
  expect_true(new$converged)
  expect_equal(new$x_star, old$x_star, tolerance = 1e-10)
  expect_equal(
    new$utilization,
    .sae_state(old$x_star, plan = plan, beta = 20, eps = 1e-8)$utilization,
    tolerance = 1e-10
  )

  # Declared axes cover every default output; only access is a surface.
  out <- .problem_outputs_at(p, theta, p$state$init)
  expect_setequal(names(p$metadata$spec$output_axes), names(out))
  expect_identical(.problem_available_surfaces(p, out), "access")
  expect_true(all(out$access >= 0 & out$access <= 1 + 1e-9))
  expect_equal(out$access, rowSums(out$allocation), tolerance = 1e-10)
})

test_that("SAE generated data are recovered through the NFXP runner", {
  problem <- ae_toy_problem("sae", kappa = 1 / 3, beta = 20)
  fit <- ae_recovery_fit(problem)

  expect_s3_class(fit, "sae_problem_nfxp_fit")
  expect_equal(fit$convergence, 0)
  expect_lt(fit$loss, 1e-6)
  expect_equal(fit$theta, c(sigma = 2), tolerance = 1e-3)
  expect_equal(fit$predicted, fit$observed, tolerance = 1e-4)
  expect_lt(fit$spectral_radius, 1)
})
