test_that("implicit_gradient matches fixed-point sensitivity", {
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
})

test_that("wsse_loss and wsse_grad match finite differences", {
  C <- matrix(c(1, 2, 0, 3, 1, -1), nrow = 3)
  observed <- c(10, 20, 5)
  theta <- c(2, 3)
  pred <- as.vector(C %*% theta)

  loss_fn <- function(th) {
    wsse_loss(as.vector(C %*% th), observed, eta = 1)
  }
  fd <- fd_jacobian(function(th) loss_fn(th), theta)
  analytic <- wsse_grad(pred, observed, C, eta = 1)

  expect_equal(as.numeric(loss_fn(theta)),
               sum((pred - observed)^2 / (observed + 1)))
  expect_equal(analytic, as.vector(fd), tolerance = 1e-5)
})

test_that("decay_dlog_dsigma matches finite differences of log decay", {
  d <- c(1.2, 3.4, 5.6)
  sigma <- 2.5
  h <- 1e-6

  fd_gaussian <- (
    log(calc_decay(d, method = "gaussian", sigma = sigma + h)) -
      log(calc_decay(d, method = "gaussian", sigma = sigma - h))
  ) / (2 * h)
  fd_exponential <- (
    log(calc_decay(d, method = "exponential", sigma = sigma + h)) -
      log(calc_decay(d, method = "exponential", sigma = sigma - h))
  ) / (2 * h)
  fd_power <- (
    log(calc_decay(d, method = "power", sigma = sigma + h)) -
      log(calc_decay(d, method = "power", sigma = sigma - h))
  ) / (2 * h)

  expect_equal(decay_dlog_dsigma("gaussian", d, sigma),
               as.numeric(fd_gaussian), tolerance = 1e-7)
  expect_equal(decay_dlog_dsigma("exponential", d, sigma),
               as.numeric(fd_exponential), tolerance = 1e-7)
  expect_equal(decay_dlog_dsigma("power", d, sigma),
               as.numeric(fd_power), tolerance = 1e-7)
  expect_error(decay_dlog_dsigma("power", c(0, 1), sigma), "positive")
})

test_that("grad_check compares analytic and finite-difference Jacobians", {
  fn <- function(x) c(exp(x[1]), x[1] * x[2])
  x <- c(0.5, 2)
  analytic <- matrix(c(exp(x[1]), x[2], 0, x[1]), nrow = 2)

  check <- grad_check(fn, x, analytic, tolerance = 1e-5)

  expect_true(check$passed)
  expect_lte(check$max_abs, 1e-5)
  expect_equal(check$analytic, analytic)
  expect_equal(dim(check$finite_difference), c(2L, 2L))
})

test_that("implicit_gradient validates dimensions", {
  expect_error(
    implicit_gradient(matrix(1, 2, 3), matrix(1, 2, 1)),
    "square"
  )
  expect_error(
    implicit_gradient(diag(2), matrix(1, 3, 1)),
    "one row per state"
  )
})
