# HAAE provider. Keep or retire is an open maintainer decision; every HAAE-
# specific check lives here so retiring HAAE means deleting this file.

.mk_haae_data <- function() {
  td <- ae_toy_inputs("2x2")
  td$kernel <- calc_decay(td$distance, method = "gaussian", sigma = 2, snap = TRUE)
  td
}

.manual_haae_state <- function(a, D, K, S, kappa, eps, a_min, a_max) {
  a <- pmin(pmax(a, a_min), a_max)
  opportunity <- sweep(K, 2, a, `*`)
  denom <- rowSums(opportunity)
  huff_share <- sweep(opportunity, 1, ifelse(denom > 0, 1 / denom, 0), `*`)
  allocation <- huff_share * K
  utilization <- as.vector(crossprod(allocation, D))
  target <- kappa * S / (utilization + eps)
  list(
    target = target,
    utilization = utilization,
    allocation = allocation,
    huff_share = huff_share,
    opportunity = opportunity
  )
}

test_that(".haae_state matches explicit compact matrix algebra", {
  td <- .mk_haae_data()
  plan <- .haae_compact_plan(td$demand, td$supply, td$kernel, kappa = 1 / 3)
  a <- c(a = 0.6, b = 1.2)
  eps <- 1e-8

  got <- .haae_state(a, plan = plan, eps = eps, a_min = 1e-6)
  expected <- .manual_haae_state(
    a = unname(a),
    D = terra::values(td$demand)[, 1],
    K = terra::values(td$kernel, mat = TRUE),
    S = unname(td$supply),
    kappa = 1 / 3,
    eps = eps,
    a_min = 1e-6,
    a_max = Inf
  )

  expect_equal(got$target, expected$target, tolerance = 1e-12)
  expect_equal(got$utilization, expected$utilization, tolerance = 1e-12)
  expect_equal(got$allocation, expected$allocation, tolerance = 1e-12)
  expect_equal(got$huff_share, expected$huff_share, tolerance = 1e-12)
})

test_that(".haae_jacobian_state matches finite differences at an interior state", {
  td <- .mk_haae_data()
  plan <- .haae_compact_plan(td$demand, td$supply, td$kernel, kappa = 1 / 3)
  a <- c(0.7, 1.1)

  analytic <- .haae_jacobian_state(a, plan = plan, eps = 1e-8, a_min = 1e-6)
  fd <- fd_jacobian_state(.haae_map, a, plan = plan, eps = 1e-8, a_min = 1e-6)

  expect_equal(analytic$jac_state, fd, tolerance = 1e-5, ignore_attr = TRUE)
})

test_that(".haae_jacobian_state supports damped-iteration stability diagnostics", {
  td <- .mk_haae_data()
  plan <- .haae_compact_plan(td$demand, td$supply, td$kernel, kappa = 1 / 3)
  lambda <- 0.7
  fit <- solve_equilibrium(
    map = .haae_map, x0 = c(1, 1), plan = plan, eps = 1e-8, a_min = 1e-6,
    lambda = lambda, tol = 1e-10, max_iter = 500, check = FALSE
  )
  util <- .haae_state(fit$x_star, plan = plan, eps = 1e-8, a_min = 1e-6)$utilization

  jac <- .haae_jacobian_state(fit$x_star, plan = plan, eps = 1e-8)
  iteration_jac <- (1 - lambda) * diag(length(fit$x_star)) +
    lambda * jac$jac_state

  expect_equal(jac$utilization, util, tolerance = 1e-10)
  expect_lt(spectral_radius(iteration_jac), 1)
})

test_that("HAAE keeps the first slice to one supply measure", {
  td <- .mk_haae_data()
  supply <- cbind(doctors = td$supply, nurses = td$supply)

  expect_error(
    .haae_compact_plan(td$demand, supply, td$kernel, kappa = 1 / 3),
    "one supply measure"
  )
  expect_error(.haae_problem(td$demand, supply, td$distance), "one supply measure")
})

test_that(".haae_problem binds theta onto the compact HAAE map", {
  td <- .mk_haae_data()
  theta <- c(sigma = 2)
  p <- .haae_problem(
    td$demand, td$supply, td$distance,
    family = "gaussian", kappa = 1 / 3, eps = 1e-8, a_min = 1e-6
  )
  step <- .bind_theta(p, theta)
  plan <- .haae_compact_plan(td$demand, td$supply, td$kernel, kappa = 1 / 3)
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

  # Declared axes cover every default output; access and pooled are surfaces.
  out <- .problem_outputs_at(p, theta, p$state$init)
  expect_setequal(names(p$metadata$spec$output_axes), names(out))
  expect_setequal(.problem_available_surfaces(p, out), c("access", "pooled"))
  expect_equal(out$access, rowSums(out$allocation), tolerance = 1e-10)
  expect_true(all(out$access >= 0 & out$access <= 1 + 1e-9))
  # HAAE inherits kernel weights above one for power distances below one unit.
  terra::values(td$distance) <- 0.5
  h <- .haae_problem(td$demand, td$supply, td$distance, family = "power")
  ho <- .problem_outputs_at(h, c(sigma = 1), h$state$init)
  expect_true(all(ho$access > 1))
  expect_true(all(ho$pooled > ho$access))
})

test_that("HAAE generated data are recovered through the NFXP runner", {
  problem <- ae_toy_problem("haae", kappa = 1 / 3)
  fit <- ae_recovery_fit(problem)

  expect_s3_class(fit, "haae_problem_nfxp_fit")
  expect_equal(fit$convergence, 0)
  expect_lt(fit$loss, 1e-6)
  expect_equal(fit$theta, c(sigma = 2), tolerance = 1e-3)
  expect_equal(fit$predicted, fit$observed, tolerance = 1e-4)
  expect_s3_class(summary(fit), "summary.ae_problem_nfxp_fit")
  expect_s4_class(.fit_output_surface(fit, "pooled"), "SpatRaster")

  # One suppressed allocation cell; sigma stays identified.
  flows <- ae_recovery_fit(problem, output = "allocation", missing = 3)
  expect_equal(flows$model, "haae")
  expect_equal(unname(flows$theta_hat["sigma"]), 2, tolerance = 1e-3)
  expect_lt(flows$loss, 1e-8)
  expect_equal(flows$n_observed, 7L)
})
