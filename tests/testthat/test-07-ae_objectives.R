# Outer calibration objectives (R/07-ae_objectives.R). The sensitivity C is
# d predicted / d theta for the linear model predicted = C %*% theta.

test_that("weighted SSE loss and gradient match closed forms and finite differences", {
  C <- matrix(c(1, 2, 0, 3, 1, -1), nrow = 3)
  observed <- c(10, 20, 5)
  theta <- c(2, 3)
  pred <- as.vector(C %*% theta)
  loss_fn <- function(th) {
    .weighted_sse_loss(as.vector(C %*% th), observed, eta = 1)
  }

  expect_equal(loss_fn(theta), sum((pred - observed)^2 / (observed + 1)))
  expect_equal(.weighted_sse_loss(pred, observed, eta = 0),
               sum((pred - observed)^2 / observed))
  expect_equal(.weighted_sse_gradient(pred, observed, C, eta = 1),
               as.vector(fd_jacobian(loss_fn, theta)), tolerance = 1e-5)
})

test_that("Poisson loss and gradient match closed forms and finite differences", {
  C <- matrix(c(1, 2, 0.5, 3, 1, 1), nrow = 3)
  observed <- c(10, 6, 5)
  theta <- c(2, 3)
  pred <- as.vector(C %*% theta)
  loss_fn <- function(th) .poisson_loss(as.vector(C %*% th), observed)

  expect_equal(loss_fn(theta), sum(pred - observed * log(pred)))
  expect_equal(.poisson_gradient(pred, observed, C),
               as.vector(fd_jacobian(loss_fn, theta)), tolerance = 1e-5)
})

test_that("Poisson objective floors predictions at eps", {
  expect_equal(.poisson_loss(c(0, 2), c(1, 2), eps = 1e-3),
               1e-3 - log(1e-3) + 2 - 2 * log(2))
  # Below eps the floored loss is flat, but the gradient keeps the slope at
  # eps rather than zero.
  # Current behaviour; maintainer decision pending (test audit 2026-10-14).
  expect_equal(.poisson_gradient(c(0, 2), c(1, 2), diag(2), eps = 1e-3),
               c(1 - 1 / 1e-3, 0))
})

test_that("objectives reject mismatched lengths and sensitivity rows", {
  expect_error(.weighted_sse_loss(c(1, 2), c(1, 2, 3)), "same length")
  expect_error(.weighted_sse_gradient(c(1, 2), c(1, 2, 3), diag(2)), "same length")
  expect_error(.weighted_sse_gradient(c(1, 2), c(1, 2), diag(3)), "one row per prediction")
  expect_error(.poisson_loss(c(1, 2), c(1, 2, 3)), "same length")
  expect_error(.poisson_gradient(c(1, 2), c(1, 2, 3), diag(2)), "same length")
  expect_error(.poisson_gradient(c(1, 2), c(1, 2), diag(3)), "one row per prediction")
})
