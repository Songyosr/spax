.mk_sae_data <- function() {
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
  c <- c(facility1 = 0.6, facility2 = 0.8)
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
})

test_that(".sae_jacobian_state matches finite differences at an interior state", {
  td <- .mk_sae_data()
  plan <- .sae_compact_plan(td$demand, td$supply, td$kernel, kappa = 1 / 3)
  c <- c(0.45, 0.65)

  analytic <- .sae_jacobian_state(c, plan = plan, beta = 20, eps = 1e-8)
  check <- grad_check(
    .sae_map,
    x = c,
    analytic_jac = analytic$jac_state,
    plan = plan,
    beta = 20,
    eps = 1e-8,
    tolerance = 1e-5
  )

  expect_true(check$passed)
  expect_lte(check$max_abs, 1e-5)
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

test_that(".sae_compact_plan keeps the first slice scalar", {
  td <- .mk_sae_data()
  supply <- cbind(doctors = td$supply, nurses = td$supply)

  expect_error(
    .sae_compact_plan(td$demand, supply, td$kernel, kappa = 1 / 3),
    "one supply measure"
  )
})
