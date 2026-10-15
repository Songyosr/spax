# Corrected static CLM provider (SPAX-029/032/033): the no-state member of the
# AE ladder, declared through clm_allocation() over checked numeric inputs.
# The legacy huff_decay form is tested in test-06-legacy_providers.R.

test_that("static CLM reaches its constant-map equilibrium in one pass", {
  f <- alloc_huff_case()
  p <- clm_allocation(f$inputs, kappa = 1 / 3)
  expect_s3_class(p, "huff_problem")
  fit <- .solve_problem(p, c(sigma = 2), lambda = 1)
  expect_true(fit$converged)
  expect_identical(fit$iters, 1L)
  # The fixed point is the exogenous attractiveness and dT/dstate is zero.
  expect_equal(unname(fit$x_star), p$state$init, tolerance = 1e-12)
  expect_equal(fit$spectral_radius, 0)
  expect_true(fit$contraction)
  step <- .bind_theta(p, c(sigma = 2))
  a <- p$state$init
  for (x in list(a * 0.3, a * 5)) expect_equal(step$map(x), a, tolerance = 1e-12)
  expect_equal(step$jac_state(a * 0.3)$jac_state, matrix(0, 3, 3))
  expect_equal(step$jac_state(a * 0.3)$jac_utilization, matrix(0, 3, 3))
  # Fitted v0 and beta keep one pass when beta equals the declared constant,
  # so the initial state already is the bound attractiveness.
  q <- clm_allocation(f$inputs, kappa = 1 / 3, fit_v0 = TRUE, fit_beta = TRUE)
  fit <- .solve_problem(q, c(sigma = 2, v0 = 1, beta = 1))
  expect_identical(fit$iters, 1L)
  expect_equal(fit$spectral_radius, 0)
})

test_that("fitted v0 and beta follow sigma in order and are read from theta", {
  f <- alloc_huff_case()
  parameters <- function(...) clm_allocation(f$inputs, ...)$theta$names
  expect_identical(parameters(), "sigma")
  expect_identical(parameters(fit_v0 = TRUE), c("sigma", "v0"))
  expect_identical(parameters(fit_beta = TRUE), c("sigma", "beta"))
  p <- clm_allocation(f$inputs, kappa = 1 / 3, fit_v0 = TRUE, fit_beta = TRUE)
  expect_identical(p$theta$names, c("sigma", "v0", "beta"))
  expect_equal(p$theta$lower, c(0, 0, 0))
  expect_identical(.huff_spec()$allocation, "clm")
  expect_error(.huff_spec(allocation = "unknown"), "arg")
  load <- function(theta) .bind_theta(p, theta)$outputs(0)$utilization
  # More outside weight leaves less inside load.
  expect_lt(sum(load(c(sigma = 2, v0 = 5, beta = 1))),
            sum(load(c(sigma = 2, v0 = 0.01, beta = 1))))
  # A larger supply elasticity concentrates load on the largest facility.
  low <- load(c(sigma = 2, v0 = 1, beta = 0.5))
  high <- load(c(sigma = 2, v0 = 1, beta = 2))
  big <- which.max(f$supply)
  expect_gt(high[big] / sum(high), low[big] / sum(low))
})

test_that("static CLM matches choice algebra and accounts for outside mass", {
  f <- alloc_huff_case()
  theta <- c(sigma = 2.5, v0 = 4, beta = 1.3)
  p <- clm_allocation(f$inputs, fit_v0 = TRUE, fit_beta = TRUE)
  out <- .solve_problem(p, theta)$outputs
  oracle <- clm_oracle(f$distance, f$demand, f$supply, theta[["sigma"]],
                       theta[["v0"]], beta = theta[["beta"]])
  expect_equal(unname(out$allocation), unname(oracle$allocation), tolerance = 1e-12)
  expect_equal(out$huff_share, out$allocation, tolerance = 1e-12)
  expect_equal(out$utilization, oracle$utilization, tolerance = 1e-12)
  expect_equal(unname(out$access), unname(oracle$rho), tolerance = 1e-12)
  expect_equal(unname(out$outside_share), unname(oracle$outside_share), tolerance = 1e-12)
  expect_equal(unname(out$access + out$outside_share), rep(1, 16), tolerance = 1e-12)
  expect_lt(sum(out$utilization), sum(f$demand))
  # Without an outside option all demand is allocated inside.
  inside <- .bind_theta(p, replace(theta, "v0", 0))$outputs(0)
  expect_equal(sum(inside$utilization), sum(f$demand), tolerance = 1e-12)
  expect_equal(inside$outside_share, rep(0, 16))
})

test_that("static CLM conserves supported supply and preserves absent support", {
  f <- alloc_huff_case()
  f$distance[1, ] <- Inf # positive demand with no reachable facility
  f$distance[, 3] <- Inf # supplied facility with no contact
  inputs <- prepare_allocation(f$demand, f$supply, f$distance)
  out <- .solve_problem(clm_allocation(inputs, v0 = 4), c(sigma = 2.5))$outputs
  expect_length(out$access, 16)
  expect_equal(unname(out$access[1]), 0)
  expect_equal(unname(out$outside_share[1]), 1)
  expect_equal(out$utilization[3], 0)
  ratio <- ifelse(out$utilization > 0, f$supply / out$utilization, 0)
  expect_equal(sum(f$demand * as.vector(out$allocation %*% ratio)),
               sum(f$supply[out$utilization > 0]), tolerance = 1e-12)
  oracle <- clm_oracle(f$distance, f$demand, f$supply, 2.5, v0 = 4)
  expect_equal(unname(out$allocation), unname(oracle$allocation), tolerance = 1e-12)
  # No opportunity and no outside option keep the documented zero guard.
  out <- .solve_problem(clm_allocation(inputs), c(sigma = 2.5))$outputs
  expect_equal(unname(out$allocation[1, ]), c(0, 0, 0))
  expect_equal(unname(out$outside_share[1]), 0)
})

test_that("static CLM sensitivities match analytic choice algebra and finite differences", {
  f <- alloc_huff_case()
  for (family in c("gaussian", "exponential", "power")) {
    theta <- c(sigma = switch(family, gaussian = 2.5, exponential = 0.3, power = 1.2),
               v0 = 4, beta = 1.3)
    p <- clm_allocation(f$inputs, family = family, kappa = 1 / 3,
                        fit_v0 = TRUE, fit_beta = TRUE)
    choice <- function(th) clm_oracle(f$distance, f$demand, f$supply, th[["sigma"]],
      th[["v0"]], family, beta = th[["beta"]], kappa = 1 / 3)
    oracle <- choice(theta)
    P <- oracle$probability
    dlogK <- switch(family,
      gaussian = f$distance^2 / theta[["sigma"]]^3,
      exponential = -f$distance,
      power = -log(f$distance))
    logS <- matrix(log(f$supply / 3), nrow(P), ncol(P), byrow = TRUE)
    deriv <- list(P * (dlogK - rowSums(P * dlogK)), -P / oracle$denom,
                  P * (logS - rowSums(P * logS)))
    expected <- vapply(deriv, function(x) as.vector(crossprod(x, f$demand)), numeric(3))
    got <- .problem_output_sensitivity(p, theta, p$state$init)
    expect_equal(unname(got), unname(expected), tolerance = 1e-5, info = family)
    allocation <- .problem_output_sensitivity(p, theta, p$state$init, output = "allocation")
    expect_equal(unname(allocation), unname(vapply(deriv, as.vector, numeric(length(P)))),
                 tolerance = 1e-5, info = family)
    observed <- oracle$utilization * c(0.8, 1.1, 1.2)
    loss <- function(th) {
      U <- choice(th)$utilization
      sum(U - observed * log(U))
    }
    fd <- vapply(seq_along(theta), function(k) {
      h <- replace(numeric(3), k, 1e-5)
      (loss(theta + h) - loss(theta - h)) / 2e-5
    }, numeric(1))
    expect_equal(.poisson_gradient(oracle$utilization, observed, got), fd,
                 tolerance = 1e-5, info = family)
  }
})

test_that("Poisson loss and its gradient match their formula and finite differences", {
  pr <- c(2, 5, 3); ob <- c(2, 4, 3)
  expect_equal(.poisson_loss(pr, ob), sum(pr - ob * log(pr)), tolerance = 1e-12)
  sens <- matrix(c(1, 0.5, -0.2, 0.3, 1, 0.7), nrow = 3)
  fd <- vapply(1:2, function(k) {
    h <- 1e-6
    (.poisson_loss(pr + h * sens[, k], ob) - .poisson_loss(pr - h * sens[, k], ob)) / (2 * h)
  }, numeric(1))
  expect_equal(.poisson_gradient(pr, ob, sens), fd, tolerance = 1e-5)
})

test_that("black-box and gradient fits recover the decay parameter", {
  f <- alloc_huff_case()
  p <- clm_allocation(f$inputs, kappa = 1 / 3)
  observed <- .solve_problem(p, c(sigma = 2.5))$outputs$utilization
  fit <- function(gradient) fit_allocation(p, observed, rbind(start = c(sigma = 6)),
    c(sigma = 0.2), c(sigma = 30), loss_args = list(eta = 1), gradient = gradient)
  bb <- fit(FALSE)
  gd <- fit(TRUE)
  expect_s3_class(bb$best, "ae_problem_nfxp_fit")
  expect_identical(bb$best$model, "huff")
  expect_identical(summary(bb$best)$allocation_form, "clm")
  expect_equal(unname(coef(bb)), 2.5, tolerance = 1e-2)
  expect_lt(bb$best$loss, 1e-6)
  expect_true(gd$best$gradient)
  expect_equal(unname(coef(gd)), unname(coef(bb)), tolerance = 1e-2)
})

test_that("a joint Poisson fit recovers sigma, v0 and a non-unit beta", {
  f <- alloc_huff_case()
  p <- clm_allocation(f$inputs, kappa = 1 / 3, fit_v0 = TRUE, fit_beta = TRUE)
  truth <- c(sigma = 2.5, v0 = 1.0, beta = 1.4)
  observed <- .solve_problem(p, truth)$outputs$utilization
  fit <- fit_allocation(p, observed, rbind(start = c(sigma = 5, v0 = 0.5, beta = 0.8)),
    lower = c(sigma = 0.2, v0 = 1e-3, beta = 0.2), upper = c(sigma = 30, v0 = 50, beta = 4),
    loss = "poisson", loss_args = list(eps = 1e-9), gradient = TRUE,
    control = list(maxit = 400))
  expect_equal(coef(fit)[["beta"]], truth[["beta"]], tolerance = 2e-2)
  expect_equal(coef(fit)[["sigma"]], truth[["sigma"]], tolerance = 1e-2)
  expect_equal(coef(fit)[["v0"]], truth[["v0"]], tolerance = 1e-2)
  # Exact recovery of the generated mean also recovers its total.
  expect_equal(sum(fitted(fit)), sum(observed), tolerance = 1e-4)
})
