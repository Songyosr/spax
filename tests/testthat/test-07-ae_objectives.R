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
  # Below eps the floored loss is flat, so its gradient is zero there,
  # whatever the observed count (DEC-029).
  expect_equal(.poisson_gradient(c(0, 2), c(1, 2), diag(2), eps = 1e-3),
               c(0, 0))
  expect_equal(.poisson_gradient(c(5e-4, 0, 1e-3), c(0, 3, 4), diag(3), eps = 1e-3),
               c(0, 0, 1 - 4 / 1e-3))
})

test_that("Poisson gradient matches finite differences with predictions below eps", {
  # Predictions exp(-d * theta) decay like a travel kernel; with eps = 1e-6
  # several cells with positive counts sit below the floor.
  eps <- 1e-6
  d <- c(0.5, 1, 2, 4, 8, 12)
  observed <- c(40, 12, 3, 1, 2, 1)
  pred_at <- function(th) 50 * exp(-d * th)
  sens_at <- function(th) matrix(-d * pred_at(th), ncol = 1)
  loss_fn <- function(th) .poisson_loss(pred_at(th), observed, eps = eps)
  for (theta in c(1.5, 2.5, 3.5)) {
    pred <- pred_at(theta)
    expect_true(any(pred < eps & observed > 0))
    expect_true(any(pred > eps & observed > 0))
    expect_equal(.poisson_gradient(pred, observed, sens_at(theta), eps = eps),
                 as.vector(fd_jacobian(loss_fn, theta)), tolerance = 1e-6)
  }
})

test_that("objectives reject mismatched lengths and sensitivity rows", {
  expect_error(.weighted_sse_loss(c(1, 2), c(1, 2, 3)), "same length")
  expect_error(.weighted_sse_gradient(c(1, 2), c(1, 2, 3), diag(2)), "same length")
  expect_error(.weighted_sse_gradient(c(1, 2), c(1, 2), diag(3)), "one row per prediction")
  expect_error(.poisson_loss(c(1, 2), c(1, 2, 3)), "same length")
  expect_error(.poisson_gradient(c(1, 2), c(1, 2, 3), diag(2)), "same length")
  expect_error(.poisson_gradient(c(1, 2), c(1, 2), diag(3)), "one row per prediction")
})

test_that("the Poisson gradient equals its pmax/logical form exactly (SPAX-061 T4)", {
  reference <- function(predicted, observed, sensitivity, eps = 1e-9) {
    floored <- predicted < eps
    predicted <- pmax(predicted, eps)
    weights <- 1 - observed / predicted
    weights[floored] <- 0
    as.vector(crossprod(sensitivity, weights))
  }
  local_test_seed(61)
  n <- 40
  sens <- matrix(rnorm(2 * n), n, 2, dimnames = list(NULL, c("a", "b")))
  observed <- rpois(n, 3) + 0
  for (eps in c(1e-9, 1e-250, 0.5)) {
    predicted <- c(runif(n - 4, 0.1, 6), 0, 1e-300, 1e-12, 0.4)
    expect_identical(.poisson_gradient(predicted, observed, sens, eps = eps),
                     reference(predicted, observed, sens, eps = eps))
    named <- stats::setNames(predicted, paste0("c", seq_len(n)))
    expect_identical(.poisson_gradient(named, observed, sens, eps = eps),
                     reference(named, observed, sens, eps = eps))
    p0 <- predicted + 1
    expect_identical(.poisson_gradient(p0, observed, sens, eps = eps),
                     reference(p0, observed, sens, eps = eps))
  }
  predicted <- c(0.2, 1, 5)
  copy <- predicted
  .poisson_gradient(predicted, c(1, 1, 1), matrix(1, 3, 1), eps = 0.5)
  expect_identical(predicted, copy)
  expect_identical(.poisson_gradient(1:3 + 0L, c(1L, 2L, 3L), matrix(1, 3, 1)),
                   reference(1:3 + 0, c(1, 2, 3), matrix(1, 3, 1)))
  expect_error(.poisson_gradient(c(1, NA), c(1, 1), diag(2)), "finite")
  expect_error(.poisson_gradient(c(1, Inf), c(1, 1), diag(2)), "finite")
  expect_error(.poisson_gradient(c(1, 2), c(1, 1), matrix(c(1, NaN, 1, 1), 2)), "finite")
})

test_that(".any_nonfinite matches any(!is.finite()) on every input type (SPAX-061 T4)", {
  cases <- list(
    num = c(1, 2), nan = c(1, NaN), na = c(1, NA), inf = c(1, Inf), ninf = c(-Inf, 1),
    both = c(-Inf, Inf), empty = numeric(), one = 1, one_inf = Inf, int = 1:3,
    int_na = c(1L, NA), int_empty = integer(), lgl = c(TRUE, NA), lgl_ok = c(TRUE, FALSE),
    mat = matrix(c(1, NA, 3, 4), 2), mat_ok = matrix(1:4 + 0, 2),
    named = c(a = 1, b = Inf), big = c(1e308, -1e308), neg = c(-1, -2)
  )
  for (name in names(cases)) {
    expect_identical(.any_nonfinite(cases[[name]]), any(!is.finite(cases[[name]])),
                     label = name)
  }
})

test_that("finiteness checks keep their messages and order (SPAX-061 T4)", {
  for (bad in list(c(1, NA), c(1, NaN), c(1, Inf), c(-Inf, 1))) {
    expect_error(.chck_numeric_vector(bad, "x"), "x must contain only finite values",
                 fixed = TRUE)
    expect_error(.chck_target_numeric(bad, "y"), "y must contain only finite values",
                 fixed = TRUE)
    expect_error(.chck_numeric_matrix(matrix(bad, 1), "m"),
                 "m must contain only finite values", fixed = TRUE)
    expect_invisible(.chck_numeric_vector(bad, "x", finite = FALSE))
  }
  expect_error(.chck_numeric_vector(matrix(c(1, NA), 1), "x"), "must be a numeric vector")
  expect_error(.chck_numeric_matrix(c(1, NA), "x"), "must be a numeric matrix")
  expect_error(.chck_numeric_vector("a", "x"), "must be numeric")
  for (ok in list(numeric(), 1:3, c(a = 1, b = 2), 1e308)) {
    expect_true(.chck_numeric_vector(ok, "x"))
    expect_true(.chck_target_numeric(ok, "x"))
  }
  expect_true(.chck_numeric_matrix(matrix(1:4, 2), "m"))
})
