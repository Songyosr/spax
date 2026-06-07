.mk_haae_data <- function() {
  demand <- terra::rast(nrows = 2, ncols = 2)
  terra::values(demand) <- c(10, 20, 30, 40)

  k1 <- terra::rast(nrows = 2, ncols = 2)
  k2 <- terra::rast(nrows = 2, ncols = 2)
  terra::values(k1) <- c(1, 2, 4, 5)
  terra::values(k2) <- c(5, 4, 2, 1)
  distance <- c(k1, k2)
  names(distance) <- c("facility1", "facility2")
  kernel <- calc_decay(distance, method = "gaussian", sigma = 2, snap = TRUE)

  supply <- c(facility1 = 30, facility2 = 45)
  list(demand = demand, distance = distance, kernel = kernel, supply = supply)
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
  a <- c(facility1 = 0.6, facility2 = 1.2)
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

test_that(".haae_equilibrium solves the attractiveness fixed point", {
  td <- .mk_haae_data()
  plan <- .haae_compact_plan(td$demand, td$supply, td$kernel, kappa = 1 / 3)

  fit <- .haae_equilibrium(
    plan,
    x0 = c(1, 1),
    eps = 1e-8,
    a_min = 1e-6,
    lambda = 0.7,
    tol = 1e-10,
    max_iter = 500,
    check = FALSE
  )
  residual <- equilibrium_residual(
    .haae_map,
    fit$attractiveness,
    plan = plan,
    eps = 1e-8,
    a_min = 1e-6,
    check = TRUE
  )

  expect_s3_class(fit, "ae_equilibrium")
  expect_true(fit$converged)
  expect_lte(max(abs(residual)), 1e-8)
  expect_true(all(fit$attractiveness > 0))
  expect_equal(
    fit$utilization,
    .haae_state(fit$attractiveness, plan)$utilization,
    tolerance = 1e-12
  )
})

test_that(".haae_jacobian_state matches finite differences at an interior state", {
  td <- .mk_haae_data()
  plan <- .haae_compact_plan(td$demand, td$supply, td$kernel, kappa = 1 / 3)
  a <- c(0.7, 1.1)

  analytic <- .haae_jacobian_state(a, plan = plan, eps = 1e-8, a_min = 1e-6)
  check <- grad_check(
    .haae_map,
    x = a,
    analytic_jac = analytic$jac_state,
    plan = plan,
    eps = 1e-8,
    a_min = 1e-6,
    tolerance = 1e-5
  )

  expect_true(check$passed)
  expect_lte(check$max_abs, 1e-5)
})

test_that(".haae_jacobian_state supports damped-iteration stability diagnostics", {
  td <- .mk_haae_data()
  plan <- .haae_compact_plan(td$demand, td$supply, td$kernel, kappa = 1 / 3)
  lambda <- 0.7
  fit <- .haae_equilibrium(
    plan, x0 = c(1, 1), eps = 1e-8, a_min = 1e-6,
    lambda = lambda, tol = 1e-10, max_iter = 500, check = FALSE
  )

  jac <- .haae_jacobian_state(fit$attractiveness, plan = plan, eps = 1e-8)
  iteration_jac <- (1 - lambda) * diag(length(fit$attractiveness)) +
    lambda * jac$jac_state

  expect_equal(jac$utilization, fit$utilization, tolerance = 1e-10)
  expect_lt(spectral_radius(iteration_jac), 1)
})

test_that(".haae_compact_plan keeps the first slice scalar", {
  td <- .mk_haae_data()
  supply <- cbind(doctors = td$supply, nurses = td$supply)

  expect_error(
    .haae_compact_plan(td$demand, supply, td$kernel, kappa = 1 / 3),
    "one supply measure"
  )
})

test_that(".haae_fit_decay fits a generated weighted-SSE target", {
  td <- .mk_haae_data()
  truth <- .haae_predict_decay(
    theta = 2,
    family = "gaussian",
    demand = td$demand,
    supply = td$supply,
    distance = td$distance,
    kappa = 1 / 3,
    lambda = 0.7,
    tol = 1e-10,
    max_iter = 500
  )

  fit <- .haae_fit_decay(
    demand = td$demand,
    supply = td$supply,
    distance = td$distance,
    observed = truth$predicted,
    family = "gaussian",
    init = 1.5,
    lower = 0.5,
    upper = 4,
    kappa = 1 / 3,
    lambda = 0.7,
    tol = 1e-10,
    max_iter = 500,
    eta = 1,
    control = list(maxit = 20)
  )

  expect_s3_class(fit, "haae_decay_fit")
  expect_equal(fit$convergence, 0)
  expect_lt(fit$wsse, 1e-6)
  expect_equal(fit$theta_hat, 2, tolerance = 1e-3)
})
