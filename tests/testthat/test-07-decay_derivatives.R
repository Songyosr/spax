# `.decay_log_jacobian()` (R/07-decay_derivatives.R) has no package caller.
# Kept pending a maintainer decision (test audit 2026-10-14); retiring it
# means deleting this file.

test_that("decay log Jacobian matches finite differences of log decay", {
  d <- c(1.2, 3.4, 5.6)
  sigma <- 2.5
  h <- 1e-6

  for (method in c("gaussian", "exponential", "power")) {
    log_decay <- function(s) log(calc_decay(d, method = method, sigma = s))
    fd <- (log_decay(sigma + h) - log_decay(sigma - h)) / (2 * h)
    expect_equal(.decay_log_jacobian(method, d, sigma), as.numeric(fd),
                 tolerance = 1e-7)
  }
  expect_error(.decay_log_jacobian("power", c(0, 1), sigma), "positive")
})
