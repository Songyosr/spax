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
  old <- .sae_equilibrium(
    plan, x0 = c(1, 1), beta = 20, eps = 1e-8,
    lambda = 0.7, tol = 1e-10, max_iter = 500, check = FALSE
  )
  new <- .solve_problem(
    p, theta = theta, lambda = 0.7, tol = 1e-10,
    max_iter = 500, check = FALSE
  )

  expect_true(new$converged)
  expect_equal(new$x_star, old$serviceability, tolerance = 1e-10)
  expect_equal(new$utilization, old$utilization, tolerance = 1e-10)
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
