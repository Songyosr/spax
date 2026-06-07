test_that("solve_equilibrium converges a contraction map", {
  map <- function(x) 0.5 * x + 1
  fit <- solve_equilibrium(map, x0 = 0, tol = 1e-10, max_iter = 200)

  expect_s3_class(fit, "ae_equilibrium")
  expect_true(fit$converged)
  expect_equal(fit$x_star, 2, tolerance = 1e-8)
  expect_lte(fit$residual_norm, 1e-8)
  expect_true(nrow(fit$history) >= 1)
  expect_equal(names(fit$history), c("iter", "error"))
  expect_null(fit$state_history)
})

test_that("solve_equilibrium applies damping and reports non-convergence", {
  map <- function(x) 1
  fit <- solve_equilibrium(map, x0 = 0, lambda = 0.5, max_iter = 2,
                           tol = 1e-12, warn = FALSE)
  expect_false(fit$converged)
  expect_equal(fit$x_star, 0.75)
  expect_equal(fit$iters, 2L)
  expect_equal(fit$message, "maximum iterations reached")
  expect_true(is.na(fit$residual_norm))
})

test_that("solve_equilibrium can skip repeated map-output checks", {
  map <- function(x) 0.5 * x + 1
  fit <- solve_equilibrium(map, x0 = 0, check = FALSE,
                           tol = 1e-10, max_iter = 200)

  expect_true(fit$converged)
  expect_equal(fit$x_star, 2, tolerance = 1e-8)
})

test_that("solve_equilibrium stores state history only when requested", {
  map <- function(x) c(a = 0.5 * x[["a"]] + 1, b = 0.25 * x[["b"]] + 2)
  fit <- solve_equilibrium(
    map,
    x0 = c(a = 0, b = 0),
    max_iter = 3,
    tol = 1e-300,
    warn = FALSE,
    keep_history = FALSE,
    keep_state_history = TRUE
  )

  expect_null(fit$history)
  expect_equal(dim(fit$state_history), c(3L, 2L))
  expect_equal(colnames(fit$state_history), c("a", "b"))
  expect_equal(fit$state_history[1, ], c(a = 1, b = 2))
  expect_equal(fit$state_history[3, ], fit$x_star)
})

test_that("solve_equilibrium validates map output", {
  expect_error(
    solve_equilibrium(function(x) c(x, x), x0 = 1),
    "same length"
  )
  expect_error(
    solve_equilibrium(function(x) NA_real_, x0 = 1),
    "finite"
  )
})

test_that("equilibrium_residual uses F = x - T(x)", {
  map <- function(x) c(x[1] + 1, 2 * x[2])
  expect_equal(equilibrium_residual(map, c(3, 4)), c(-1, -4))
})

test_that("spectral_radius reports maximum eigenvalue modulus", {
  J <- matrix(c(0.2, 0, 0, -0.5), nrow = 2)
  expect_equal(spectral_radius(J), 0.5)
  expect_error(spectral_radius(matrix(1, nrow = 2, ncol = 3)), "square")
})

test_that("finite-difference state Jacobian matches analytic Jacobian", {
  fn <- function(x) c(x[1]^2 + x[2], sin(x[2]))
  x <- c(1.5, 0.25)
  analytic <- matrix(
    c(2 * x[1], 0, 1, cos(x[2])),
    nrow = 2,
    byrow = FALSE
  )

  expect_equal(fd_jacobian_state(fn, x), analytic, tolerance = 1e-5)
})

test_that("finite-difference parameter Jacobian holds state fixed", {
  map <- function(x, theta) {
    c(theta[1] * x[1] + theta[2], theta[1] * x[2]^2)
  }
  x <- c(2, 3)
  theta <- c(4, 5)
  analytic <- matrix(c(x[1], x[2]^2, 1, 0), nrow = 2)

  expect_equal(fd_jacobian_param(map, theta, x), analytic, tolerance = 1e-5)
})
