.mk_sae_data <- function() {
  demand <- terra::rast(nrows = 2, ncols = 2)
  terra::values(demand) <- c(10, 20, 30, 40)

  k1 <- terra::rast(nrows = 2, ncols = 2)
  k2 <- terra::rast(nrows = 2, ncols = 2)
  terra::values(k1) <- c(0.9, 0.4, 0.2, 0.1)
  terra::values(k2) <- c(0.1, 0.3, 0.7, 0.8)
  kernel <- c(k1, k2)
  names(kernel) <- c("facility1", "facility2")

  supply <- c(facility1 = 30, facility2 = 45)
  list(demand = demand, kernel = kernel, supply = supply)
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

test_that(".sae_equilibrium solves the serviceability fixed point", {
  td <- .mk_sae_data()
  plan <- .sae_compact_plan(td$demand, td$supply, td$kernel, kappa = 1 / 3)

  fit <- .sae_equilibrium(
    plan,
    x0 = c(1, 1),
    beta = 20,
    eps = 1e-8,
    lambda = 0.7,
    tol = 1e-10,
    max_iter = 500,
    check = FALSE
  )
  residual <- equilibrium_residual(
    .sae_map,
    fit$serviceability,
    plan = plan,
    beta = 20,
    eps = 1e-8,
    check = TRUE
  )

  expect_s3_class(fit, "ae_equilibrium")
  expect_true(fit$converged)
  expect_lte(max(abs(residual)), 1e-8)
  expect_true(all(fit$serviceability >= 0 & fit$serviceability <= 1))
  expect_equal(fit$utilization,
               .sae_state(fit$serviceability, plan)$utilization,
               tolerance = 1e-12)
})

test_that(".sae_compact_plan keeps the first slice scalar", {
  td <- .mk_sae_data()
  supply <- cbind(doctors = td$supply, nurses = td$supply)

  expect_error(
    .sae_compact_plan(td$demand, supply, td$kernel, kappa = 1 / 3),
    "one supply measure"
  )
})
