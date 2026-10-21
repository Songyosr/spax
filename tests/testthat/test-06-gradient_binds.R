# SPAX-061 T2: a gradient at an accepted point makes no redundant bind.

# Capture the optimizer's objective and gradient closures of a one-start fit;
# optim is replaced, everything else (real solves, losses) runs.
.capture_closures <- function(model, observed, f, output, reuse = TRUE) {
  cap <- new.env()
  local_mocked_bindings(
    optim = function(par, fn, gr, ...) {
      cap$fn <- fn; cap$gr <- gr; cap$par <- par
      list(par = par, value = fn(par), counts = c(1L, 1L), convergence = 0L,
           message = NULL)
    }, .package = "stats")
  fit_allocation(model, observed, f$starts[1, , drop = FALSE], f$lower, f$upper,
                 output = output, loss = "poisson", gradient = TRUE,
                 reuse = reuse)
  cap
}

# .bind_theta calls of one gradient request that follows an objective request
# at the same point, or of a request at a fresh point (`shift`).
.gradient_binds <- function(cap, shift = 0) {
  counter <- 0L
  original <- .bind_theta
  local_mocked_bindings(.bind_theta = function(problem, theta) {
    counter <<- counter + 1L
    original(problem, theta)
  })
  x <- cap$par + shift
  cap$fn(x)
  counter <- 0L
  g <- cap$gr(x)
  list(binds = counter, gradient = g)
}

test_that("a deterministic gradient makes two binds per parameter", {
  f <- alloc_small_case()
  flows <- f$flow
  for (case in list(list("utilization", f$observed), list("flow", flows))) {
    cap <- .capture_closures(f$model, case[[2]], f, case[[1]])
    r <- .gradient_binds(cap)
    expect_identical(r$binds, 4L)              # 2 parameters x central difference
    expect_true(all(is.finite(r$gradient)))
  }
})

test_that("without reuse the gradient keeps today's binds", {
  f <- alloc_small_case()
  cap <- .capture_closures(f$model, f$observed, f, "utilization", reuse = FALSE)
  expect_gt(.gradient_binds(cap)$binds, 4L)
})

test_that("a custom provider without a declared determinism keeps its binds", {
  p <- ae_toy_problem(shape = "1x4")
  p$metadata$spec$deterministic <- NULL
  cap <- new.env()
  local_mocked_bindings(
    optim = function(par, fn, gr, ...) {
      cap$fn <- fn; cap$gr <- gr; cap$par <- par
      list(par = par, convergence = 0L, message = NULL)
    }, .package = "stats")
  fit <- .fit_problem_nfxp(p, c(8, 12), init = c(sigma = 2), lower = c(sigma = .5),
    upper = c(sigma = 80), output = "utilization", gradient = TRUE, reuse = TRUE)
  expect_false(fit$evaluation_reuse$enabled)
  expect_null(.solve_problem(p, c(sigma = 2), keep_step = FALSE)$step)
  r <- .gradient_binds(cap)
  expect_gt(r$binds, 2L)
})

test_that("a fit result never stores the kept step", {
  p <- ae_toy_problem(shape = "1x4")
  fit <- suppressWarnings(.fit_problem_nfxp(p, c(8, 12), init = c(sigma = 2),
    lower = c(sigma = .5), upper = c(sigma = 80), output = "utilization",
    gradient = TRUE, reuse = TRUE))  # sigma is not identified by this toy target
  expect_true(fit$evaluation_reuse$enabled)
  has_step <- function(x) is.list(x) && !is.function(x) &&
    ("step" %in% names(x) || any(vapply(x, has_step, NA)))
  expect_false(has_step(unclass(fit)))
})

test_that(".solve_problem returns the bound step only on request", {
  p <- ae_toy_problem(shape = "1x4")
  theta <- c(sigma = 2)
  expect_null(.solve_problem(p, theta, check = FALSE)$step)
  kept <- .solve_problem(p, theta, check = FALSE, keep_step = TRUE)
  expect_identical(kept$step$theta, theta)
  expect_true(is.function(kept$step$map))
})

test_that("lazy f0 gives identical finite-difference Jacobians", {
  calls <- 0L
  fn <- function(x) { calls <<- calls + 1L; c(sum(x^2), prod(x), exp(x[1]) * x[2]) }
  x <- c(a = 1.5, b = 2.5, c = 0.7)
  cases <- list(
    list(lower = -Inf, upper = Inf),
    list(lower = c(0, 0, 0.7), upper = c(5, 5, 5)),        # c on its lower bound
    list(lower = c(0, 0, 0), upper = c(1.5, 5, 0.7)),       # a, c on upper bounds
    list(lower = c(1.5, 2.5, 0.7), upper = c(1.5, 2.5, 0.7))  # no room at all
  )
  for (b in cases) {
    for (method in c("central", "forward")) {
      eager <- fd_jacobian(fn, x, lower = b$lower, upper = b$upper, method = method)
      lazy <- fd_jacobian(fn, x, lower = b$lower, upper = b$upper, method = method,
                          lazy_f0 = TRUE)
      expect_identical(lazy, eager)
    }
  }
  # f0 is evaluated only when needed
  calls <- 0L
  fd_jacobian(fn, x, lazy_f0 = TRUE)
  expect_identical(calls, 6L)
  calls <- 0L
  fd_jacobian(fn, x)
  expect_identical(calls, 7L)
  # and validated when it is used
  bad <- function(x) if (all(x == c(1, 2))) "bad" else c(1, 2)
  expect_error(fd_jacobian(bad, c(1, 2), lower = c(1, 2), upper = c(3, 4),
                           lazy_f0 = TRUE))
  expect_identical(
    fd_jacobian(function(x) x[1], c(1, 2), lazy_f0 = TRUE, check = TRUE),
    fd_jacobian(function(x) x[1], c(1, 2), check = TRUE))
})
