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

# Finite differences inside bounds ---------------------------------------------

# A function that records the points at which it is evaluated. Its Jacobian,
# [exp(x1), 2 x2; x2 cos(x1 x2), x1 cos(x1 x2)], is written out below.
recording <- function() {
  seen <- list()
  list(
    fn = function(x) {
      seen[[length(seen) + 1L]] <<- x
      c(exp(x[1]) + x[2]^2, sin(x[1] * x[2]))
    },
    points = function() do.call(rbind, seen)
  )
}
plain_fn <- function(x) c(exp(x[1]) + x[2]^2, sin(x[1] * x[2]))
exact_jacobian <- function(x) {
  matrix(c(exp(x[1]), x[2] * cos(x[1] * x[2]), 2 * x[2], x[1] * cos(x[1] * x[2])), 2)
}
inside <- function(points, lower, upper) {
  all(sweep(points, 2, lower, ">=") & sweep(points, 2, upper, "<="))
}

test_that("fd_jacobian takes the central steps it always took when the bounds are not reached", {
  x <- c(a = 0.4, b = 0.7)
  recorder <- recording()
  bounded <- fd_jacobian(recorder$fn, x, lower = c(0, 0), upper = c(1, 1))
  expect_identical(bounded, fd_jacobian(plain_fn, x))
  expect_identical(fd_jacobian(plain_fn, x, lower = 0, upper = 1), fd_jacobian(plain_fn, x))
  expect_identical(fd_jacobian(plain_fn, x, lower = -Inf, upper = Inf), fd_jacobian(plain_fn, x))
  expect_identical(colnames(bounded), c("a", "b"))
  expect_equal(unname(bounded), exact_jacobian(x), tolerance = 1e-6)
  # The point itself and one step each side of every coordinate, nothing else.
  h <- 1e-6
  points <- unname(recorder$points())
  expect_equal(points, rbind(x, c(x[1] + h, x[2]), c(x[1] - h, x[2]),
                             c(x[1], x[2] + h), c(x[1], x[2] - h)), ignore_attr = TRUE)
  # The forward method is unchanged too.
  expect_identical(fd_jacobian(plain_fn, x, method = "forward", lower = 0, upper = 1),
                   fd_jacobian(plain_fn, x, method = "forward"))
})

test_that("fd_jacobian uses a one-sided step at each bound and stays inside", {
  h <- 1e-6
  lower <- c(0, 0)
  upper <- c(1, 1)
  # Lower bound of the first coordinate: forward, the second stays central.
  x <- c(0, 0.7)
  recorder <- recording()
  J <- fd_jacobian(recorder$fn, x, lower = lower, upper = upper)
  expect_true(inside(recorder$points(), lower, upper))
  expect_identical(J[, 1], (plain_fn(c(h, 0.7)) - plain_fn(x)) / h)
  expect_identical(J[, 2], (plain_fn(c(0, 0.7 + h)) - plain_fn(c(0, 0.7 - h))) / (2 * h))
  expect_equal(J, exact_jacobian(x), tolerance = 1e-5)
  # Upper bound of the second coordinate: backward.
  x <- c(0.4, 1)
  recorder <- recording()
  J <- fd_jacobian(recorder$fn, x, lower = lower, upper = upper)
  expect_true(inside(recorder$points(), lower, upper))
  expect_identical(J[, 2], (plain_fn(x) - plain_fn(c(0.4, 1 - h))) / h)
  expect_identical(J[, 1], (plain_fn(c(0.4 + h, 1)) - plain_fn(c(0.4 - h, 1))) / (2 * h))
  expect_equal(J, exact_jacobian(x), tolerance = 1e-5)
  # Both bounds at once, each in its own coordinate.
  x <- c(1, 0)
  recorder <- recording()
  J <- fd_jacobian(recorder$fn, x, lower = lower, upper = upper)
  expect_true(inside(recorder$points(), lower, upper))
  expect_equal(J, exact_jacobian(x), tolerance = 1e-5)
  # Within one step of a bound (the step is 1e-6, the room 5e-7): forward.
  x <- c(5e-7, 0.7)
  recorder <- recording()
  J <- fd_jacobian(recorder$fn, x, lower = lower, upper = upper)
  expect_true(inside(recorder$points(), lower, upper))
  expect_identical(J[, 1], (plain_fn(c(5e-7 + h, 0.7)) - plain_fn(x)) / h)
  expect_equal(J, exact_jacobian(x), tolerance = 1e-5)
  # Without the bounds the central step would have left them.
  unbounded <- recording()
  fd_jacobian(unbounded$fn, x)
  expect_false(inside(unbounded$points(), lower, upper))
  # Bounds elsewhere than at zero, and a single bound.
  x <- c(2, 3)
  recorder <- recording()
  J <- fd_jacobian(recorder$fn, x, lower = c(2, -Inf), upper = c(Inf, 3))
  expect_true(inside(recorder$points(), c(2, -Inf), c(Inf, 3)))
  expect_equal(J, exact_jacobian(x), tolerance = 1e-5)
})

test_that("fd_jacobian uses the larger room when neither side of the step fits", {
  # Bounds closer together than one step (1e-6): the step is the larger room, to
  # the farther bound (forward when the rooms tie), every evaluation stays inside
  # the bounds, and the derivative of exp(x) is still 1.
  cases <- list(list(x = 3e-7, moves_to = 1e-6),  # rooms 7e-7 above, 3e-7 below
                list(x = 5e-7, moves_to = 1e-6),  # equal rooms: forward
                list(x = 8e-7, moves_to = 0))     # rooms 2e-7 above, 8e-7 below
  for (case in cases) {
    seen <- numeric()
    f <- function(x) {
      seen <<- c(seen, x)
      exp(x)
    }
    J <- fd_jacobian(f, case$x, lower = 0, upper = 1e-6)
    expect_true(all(seen >= 0 & seen <= 1e-6), info = case$x)
    expect_setequal(seen, c(case$x, case$moves_to))
    expect_equal(drop(J), 1, tolerance = 1e-5, info = case$x)
  }
  # No room at all: a zero column, and the function is not evaluated elsewhere.
  recorder <- recording()
  J <- fd_jacobian(recorder$fn, c(0.4, 0.7), lower = c(0.4, 0), upper = c(0.4, 1))
  expect_identical(J[, 1], c(0, 0))
  expect_equal(J[, 2], exact_jacobian(c(0.4, 0.7))[, 2], tolerance = 1e-6)
  expect_true(all(recorder$points()[, 1] == 0.4))
})

test_that("fd_jacobian forward differences step up where they fit and down otherwise", {
  h <- 1e-6
  recorder <- recording()
  J <- fd_jacobian(recorder$fn, c(0.4, 1), method = "forward", lower = c(0, 0), upper = c(1, 1))
  expect_true(inside(recorder$points(), c(0, 0), c(1, 1)))
  expect_identical(J[, 1], (plain_fn(c(0.4 + h, 1)) - plain_fn(c(0.4, 1))) / h)
  expect_identical(J[, 2], (plain_fn(c(0.4, 1)) - plain_fn(c(0.4, 1 - h))) / h)
  # At the lower bound the forward step is the usual one.
  J <- fd_jacobian(plain_fn, c(0, 0.7), method = "forward", lower = 0, upper = 1)
  expect_identical(J, fd_jacobian(plain_fn, c(0, 0.7), method = "forward"))
  # Neither side fits: the larger room.
  seen <- numeric()
  f <- function(x) { seen <<- c(seen, x); exp(x) }
  J <- fd_jacobian(f, 8e-7, method = "forward", lower = 0, upper = 1e-6)
  expect_true(all(seen >= 0 & seen <= 1e-6))
  expect_equal(drop(J), 1, tolerance = 1e-5)
})

test_that("fd_jacobian validates its bounds", {
  x <- c(0.4, 0.7)
  expect_error(fd_jacobian(plain_fn, x, lower = c(0, NA)), "lower")
  expect_error(fd_jacobian(plain_fn, x, upper = NA_real_), "upper")
  expect_error(fd_jacobian(plain_fn, x, lower = "a"), "lower")
  expect_error(fd_jacobian(plain_fn, x, lower = c(0, 0, 0)), "length one or the length of `x`")
  expect_error(fd_jacobian(plain_fn, x, upper = numeric(0)), "upper")
  expect_error(fd_jacobian(plain_fn, x, lower = c(0, 2), upper = c(1, 1)), "must not exceed")
  expect_error(fd_jacobian(plain_fn, x, lower = 1, upper = 0), "must not exceed")
})

test_that("the bounds of a theta contract are aligned with the names of theta", {
  problem <- list(theta = list(names = c("sigma", "v0", "beta"), lower = c(10, 0, 1),
                               upper = c(40, Inf, 5)))
  expect_identical(.problem_theta_bounds(problem, c(beta = 2, sigma = 12, v0 = 3)),
                   list(lower = c(1, 10, 0), upper = c(5, 40, Inf)))
  expect_identical(.problem_theta_bounds(problem, c(sigma = 12, v0 = 3, beta = 2)),
                   list(lower = c(10, 0, 1), upper = c(40, Inf, 5)))
  # positional without names, and for a subset of the names
  expect_identical(.problem_theta_bounds(problem, c(12, 3, 2)),
                   list(lower = c(10, 0, 1), upper = c(40, Inf, 5)))
  expect_identical(.problem_theta_bounds(problem, c(beta = 2, sigma = 12)),
                   list(lower = c(1, 10), upper = c(5, 40)))
  # no bounds in the contract, or bounds that cannot be aligned: unbounded
  free <- list(theta = list(names = c("a", "b")))
  expect_identical(.problem_theta_bounds(free, c(a = 1, b = 2)),
                   list(lower = c(-Inf, -Inf), upper = c(Inf, Inf)))
  expect_identical(.problem_theta_bounds(problem, c(1, 2, 3, 4)),
                   list(lower = -Inf, upper = Inf))
  expect_identical(.problem_theta_bounds(problem, numeric(0)), list(lower = -Inf, upper = Inf))
  # a scalar bound applies to every parameter
  scalar <- list(theta = list(names = c("a", "b"), lower = 0.5, upper = Inf))
  expect_identical(.problem_theta_bounds(scalar, c(b = 1, a = 2)),
                   list(lower = c(0.5, 0.5), upper = c(Inf, Inf)))
})

test_that("a model with a finite theta bound is differentiated inside it", {
  # The ratio-response model bounds its feedback scale below by 1. At that bound
  # its derivative of an output without a provided Jacobian is one-sided, and
  # agrees with a forward difference of the solved output.
  problem <- ae_ratio_problem()
  expect_identical(problem$theta$lower, c(0, 0, 1))
  theta <- ae_ratio_truth()
  theta[["feedback_scale"]] <- 1
  solve_at <- function(th) {
    .solve_problem(problem, th, lambda = 0.7, tol = 1e-12, max_iter = 500)
  }
  x <- solve_at(theta)$x_star
  sensitivity <- .problem_output_sensitivity(problem, theta, x, "access")
  expect_true(all(is.finite(sensitivity)))
  expect_identical(colnames(sensitivity), names(theta))
  h <- 1e-6
  forward <- (solve_at(replace(theta, "feedback_scale", 1 + h))$outputs$access -
                solve_at(theta)$outputs$access) / h
  expect_equal(unname(sensitivity[, "feedback_scale"]), unname(forward), tolerance = 1e-3)
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
