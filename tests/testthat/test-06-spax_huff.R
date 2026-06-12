# SPAX-029: static Huff / aggregate-CLM problem spec -------------------------

.mk_huff_data <- function() {
  demand <- terra::rast(nrows = 4, ncols = 4)
  terra::values(demand) <- c(
    10, 20, 30, 40,
    15, 25, 35, 45,
    12, 22, 32, 42,
    18, 28, 38, 48
  )

  d1 <- terra::rast(nrows = 4, ncols = 4)
  d2 <- terra::rast(nrows = 4, ncols = 4)
  d3 <- terra::rast(nrows = 4, ncols = 4)
  terra::values(d1) <- c(1, 2, 3, 4, 2, 3, 4, 5, 3, 4, 5, 6, 4, 5, 6, 7)
  terra::values(d2) <- c(7, 6, 5, 4, 6, 5, 4, 3, 5, 4, 3, 2, 4, 3, 2, 1)
  terra::values(d3) <- c(4, 3, 2, 3, 3, 2, 3, 4, 2, 3, 4, 5, 3, 4, 5, 6)
  distance <- c(d1, d2, d3)
  names(distance) <- c("facility1", "facility2", "facility3")

  supply <- c(facility1 = 30, facility2 = 45, facility3 = 20)
  list(demand = demand, distance = distance, supply = supply)
}

# Build the comparison plan the same way the HAAE oracle does, so we can check
# the static problem against a direct one-pass Huff evaluation.
.mk_huff_plan <- function(td, theta, family = "gaussian", kappa = 1) {
  kernel <- calc_decay(td$distance, method = family, sigma = theta, snap = TRUE)
  .haae_compact_plan(td$demand, td$supply, kernel, kappa = kappa)
}

test_that(".huff_problem solves in one pass with a zero state Jacobian", {
  td <- .mk_huff_data()
  theta <- c(sigma = 2)
  p <- .huff_problem(td$demand, td$supply, td$distance,
                     family = "gaussian", kappa = 1 / 3)

  expect_s3_class(p, "ae_problem")
  expect_s3_class(p, "huff_problem")

  fit <- .solve_problem(p, theta = theta, lambda = 1)
  expect_true(fit$converged)
  expect_identical(fit$iters, 1L)
  # state fixed point is the exogenous attractiveness
  expect_equal(unname(fit$x_star), p$state$init, tolerance = 1e-12)
  # dT/dstate is exactly zero => contractive, spectral radius 0
  expect_equal(fit$spectral_radius, 0)
  expect_true(fit$contraction)
})

test_that(".huff_problem map is state-independent (constant T(x) = a)", {
  td <- .mk_huff_data()
  p <- .huff_problem(td$demand, td$supply, td$distance,
                     family = "gaussian", kappa = 1 / 3)
  step <- .bind_theta(p, c(sigma = 2))

  a <- p$state$init
  x1 <- a * 0.3
  x2 <- a * 5
  expect_equal(step$map(x1), a, tolerance = 1e-12)
  expect_equal(step$map(x2), a, tolerance = 1e-12)
  # analytic Jacobians are zero, matching the constant map under FD
  expect_equal(step$jac_state(x1)$jac_state,
               matrix(0, length(a), length(a)))
  expect_equal(step$jac_state(x1)$jac_utilization,
               matrix(0, length(a), length(a)))
})

test_that("static Huff utilization is the evaluate-once case of HAAE", {
  td <- .mk_huff_data()
  theta <- c(sigma = 2)
  # classic Huff (beta = 1): attractiveness proportional to supply
  p <- .huff_problem(td$demand, td$supply, td$distance,
                     family = "gaussian", kappa = 1, beta = 1)
  step <- .bind_theta(p, theta)

  plan <- .mk_huff_plan(td, theta, family = "gaussian", kappa = 1)
  a <- as.vector(plan$S)  # classic Huff a_j = supply (kappa scale cancels)

  # the static problem output equals one HAAE-kernel pass at the same a
  expect_equal(step$outputs(c(1, 1, 1))$utilization,
               .haae_state(a, plan = plan)$utilization,
               tolerance = 1e-10)
  expect_equal(step$outputs(c(1, 1, 1))$access,
               .haae_state(a, plan = plan)$access,
               tolerance = 1e-10)

  # kappa is a global scale on attractiveness => cancels in the choice share
  p2 <- .huff_problem(td$demand, td$supply, td$distance,
                      family = "gaussian", kappa = 1 / 3, beta = 1)
  expect_equal(.bind_theta(p2, theta)$outputs(0)$utilization,
               step$outputs(0)$utilization, tolerance = 1e-10)
})

test_that("static Huff attenuates total demand and access stays in [0, 1]", {
  td <- .mk_huff_data()
  p <- .huff_problem(td$demand, td$supply, td$distance,
                     family = "gaussian", kappa = 1 / 3)
  out <- .bind_theta(p, c(sigma = 2))$outputs(0)

  total_demand <- sum(p$substrate$D_active)
  # Huff-then-attenuate (f^2 a / sum f a) loses mass to distance attenuation
  expect_lt(sum(out$utilization), total_demand)
  expect_true(all(out$access >= 0 & out$access <= 1 + 1e-9))
})

test_that("an outside option reduces utilization below the pure-Huff case", {
  td <- .mk_huff_data()
  theta <- c(sigma = 2)
  pure <- .huff_problem(td$demand, td$supply, td$distance,
                        family = "gaussian", kappa = 1 / 3, v0 = 0)
  outside <- .huff_problem(td$demand, td$supply, td$distance,
                           family = "gaussian", kappa = 1 / 3, v0 = 0.5)

  out_pure <- .bind_theta(pure, theta)$outputs(0)
  out_outside <- .bind_theta(outside, theta)$outputs(0)
  expect_lt(sum(out_outside$utilization), sum(out_pure$utilization))
  # outside_share is origin-side (one per active demand cell)
  n_active <- length(pure$substrate$D_active)
  expect_length(out_outside$outside_share, n_active)
  expect_true(all(out_outside$outside_share > 0))
  expect_equal(out_pure$outside_share, rep(0, n_active))
})

test_that(".fit_problem_nfxp recovers the decay parameter for static Huff", {
  td <- .mk_huff_data()
  sigma0 <- 2.5
  p <- .huff_problem(td$demand, td$supply, td$distance,
                     family = "gaussian", kappa = 1 / 3)
  observed <- .solve_problem(p, theta = c(sigma = sigma0))$outputs$utilization

  fit <- .fit_problem_nfxp(
    p, observed = observed,
    init = c(sigma = 6), lower = c(sigma = 0.2), upper = c(sigma = 30),
    output = "utilization", loss_args = list(eta = 1)
  )
  expect_s3_class(fit, "ae_problem_nfxp_fit")
  expect_equal(fit$model, "huff")
  expect_equal(unname(fit$theta_hat["sigma"]), sigma0, tolerance = 1e-2)
  expect_lt(fit$loss, 1e-6)
})

test_that("gradient-mode static Huff fit agrees with the black-box fit", {
  td <- .mk_huff_data()
  sigma0 <- 2.5
  p <- .huff_problem(td$demand, td$supply, td$distance,
                     family = "gaussian", kappa = 1 / 3)
  observed <- .solve_problem(p, theta = c(sigma = sigma0))$outputs$utilization

  args <- list(
    p, observed = observed,
    init = c(sigma = 6), lower = c(sigma = 0.2), upper = c(sigma = 30),
    output = "utilization", loss_args = list(eta = 1)
  )
  bb <- do.call(.fit_problem_nfxp, args)
  gd <- do.call(.fit_problem_nfxp, c(args, list(gradient = TRUE)))

  expect_true(gd$gradient)
  expect_equal(unname(gd$theta_hat["sigma"]), unname(bb$theta_hat["sigma"]),
               tolerance = 1e-2)
})
