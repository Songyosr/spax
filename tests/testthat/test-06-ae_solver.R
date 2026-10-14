# Fixed-point solver -----------------------------------------------------------

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
  # Skipping the per-iteration map-output checks leaves the iterates unchanged.
  unchecked <- solve_equilibrium(map, x0 = 0, check = FALSE, tol = 1e-10,
                                 max_iter = 200)
  expect_identical(unchecked$x_star, fit$x_star)
})

test_that("solve_equilibrium measures steps in the max or l2 norm", {
  map <- function(x) c(0.5 * x[1] + 1, 0.25 * x[2] + 3)
  max_fit <- solve_equilibrium(map, x0 = c(0, 0), tol = 1e-10, max_iter = 200)
  l2_fit <- solve_equilibrium(map, x0 = c(0, 0), norm = "l2", tol = 1e-10,
                              max_iter = 200)

  # The first undamped step is (1, 3).
  expect_equal(max_fit$history$error[1], 3)
  expect_equal(l2_fit$history$error[1], sqrt(10))
  expect_equal(l2_fit$x_star, c(2, 4), tolerance = 1e-8)
  expect_lte(l2_fit$residual_norm, 1e-8)
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
  expect_warning(
    solve_equilibrium(map, x0 = 0, lambda = 0.5, max_iter = 2, tol = 1e-12),
    "did not converge"
  )
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

test_that("solve_equilibrium validates its inputs and map output", {
  expect_error(solve_equilibrium(1, x0 = 1), "function")
  expect_error(solve_equilibrium(function(x) x, x0 = 1, lambda = 1.5), "<= 1")
  expect_error(
    solve_equilibrium(function(x) c(x, x), x0 = 1),
    "same length"
  )
  expect_error(
    solve_equilibrium(function(x) NA_real_, x0 = 1),
    "finite"
  )
})

test_that("spectral_radius reports maximum eigenvalue modulus", {
  J <- matrix(c(0.2, 0, 0, -0.5), nrow = 2)
  expect_equal(spectral_radius(J), 0.5)
  expect_error(spectral_radius(matrix(1, nrow = 2, ncol = 3)), "square")
})

# Derivative tools -------------------------------------------------------------

test_that("finite-difference state Jacobian matches analytic Jacobian", {
  fn <- function(x) c(x[1]^2 + x[2], sin(x[2]))
  x <- c(1.5, 0.25)
  analytic <- matrix(
    c(2 * x[1], 0, 1, cos(x[2])),
    nrow = 2,
    byrow = FALSE
  )

  expect_equal(fd_jacobian_state(fn, x), analytic, tolerance = 1e-5)
  expect_equal(fd_jacobian(fn, x, method = "forward"), analytic,
               tolerance = 1e-5)
  expect_error(fd_jacobian(1, x), "function")
})

test_that("implicit_gradient solves the fixed-point sensitivity equation", {
  A <- matrix(c(0.2, 0.1, 0, 0.3), nrow = 2)
  B <- matrix(c(1, 0, 0.5, 2), nrow = 2)
  theta <- c(3, 4)
  x_star <- as.vector(solve(diag(2) - A, B %*% theta))

  sens <- implicit_gradient(A, B)
  fd <- fd_jacobian(function(th) {
    as.vector(solve(diag(2) - A, B %*% th))
  }, theta)

  expect_equal(as.vector((diag(2) - A) %*% x_star), as.vector(B %*% theta))
  expect_equal(sens, fd, tolerance = 1e-6)
  expect_error(implicit_gradient(matrix(1, 2, 3), matrix(1, 2, 1)), "square")
  expect_error(
    implicit_gradient(diag(2), matrix(1, 3, 1)),
    "one row per state"
  )
})

# Test-only helpers in R/ (equilibrium_residual, fd_jacobian_param,
# grad_check) have no package caller. Kept pending a maintainer decision
# (test audit 2026-10-14); retiring them means deleting this block.

test_that("test-only residual and Jacobian checkers agree with closed forms", {
  map <- function(x) c(x[1] + 1, 2 * x[2])
  expect_equal(equilibrium_residual(map, c(3, 4)), c(-1, -4))

  param_map <- function(x, theta) {
    c(theta[1] * x[1] + theta[2], theta[1] * x[2]^2)
  }
  x <- c(2, 3)
  analytic <- matrix(c(x[1], x[2]^2, 1, 0), nrow = 2)
  expect_equal(fd_jacobian_param(param_map, c(4, 5), x), analytic,
               tolerance = 1e-5)

  fn <- function(x) c(exp(x[1]), x[1] * x[2])
  x <- c(0.5, 2)
  analytic <- matrix(c(exp(x[1]), x[2], 0, x[1]), nrow = 2)
  check <- grad_check(fn, x, analytic, tolerance = 1e-5)
  expect_true(check$passed)
  expect_lte(check$max_abs, 1e-5)
  expect_equal(check$analytic, analytic)
  expect_equal(dim(check$finite_difference), c(2L, 2L))
})
