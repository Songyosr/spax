# Legacy providers and input paths awaiting maintainer decisions (test audit
# 2026-10-14). Each section can be deleted together with the code it tests:
# - "huff_decay": the explicit legacy allocation = "huff_decay" form of the
#   static Huff provider. Its first block also uses HAAE.
# The corrected static CLM over checked numeric inputs is tested in
# test-06-allocation_clm.R.

# huff_decay -------------------------------------------------------------------

test_that("explicit huff_decay is the evaluate-once case of HAAE", {
  f <- alloc_huff_case()
  theta <- c(sigma = 2)
  step <- .bind_theta(.huff_problem(f$inputs, allocation = "huff_decay"), theta)
  plan <- .bind_theta(.haae_problem(f$inputs), theta)$plan
  haae <- .haae_state(plan$S, plan = plan)
  expect_equal(step$outputs(c(1, 1, 1))$utilization, haae$utilization, tolerance = 1e-10)
  expect_equal(step$outputs(c(1, 1, 1))$access, haae$access, tolerance = 1e-10)
  # Without an outside option a global attractiveness scale cancels.
  scaled <- .huff_problem(f$inputs, kappa = 1 / 3, allocation = "huff_decay")
  expect_equal(.bind_theta(scaled, theta)$outputs(0)$utilization,
               step$outputs(0)$utilization, tolerance = 1e-10)
})

test_that("huff_decay attenuates demand, conserves supported supply and keeps support", {
  f <- alloc_huff_case()
  out <- .bind_theta(.huff_problem(f$inputs, kappa = 1 / 3, allocation = "huff_decay"),
                     c(sigma = 2))$outputs(0)
  # Huff-then-attenuate loses mass to the second decay factor.
  expect_lt(sum(out$utilization), sum(f$demand))
  expect_true(all(out$access >= 0 & out$access <= 1 + 1e-9))
  f$distance[1, ] <- Inf
  f$distance[, 3] <- Inf
  inputs <- prepare_allocation(f$demand, f$supply, f$distance)
  out <- .solve_problem(.huff_problem(inputs, v0 = 4, allocation = "huff_decay"),
                        c(sigma = 2.5))$outputs
  expect_equal(unname(out$access[1]), 0)
  expect_equal(unname(out$outside_share[1]), 1)
  expect_equal(out$utilization[3], 0)
  ratio <- ifelse(out$utilization > 0, f$supply / out$utilization, 0)
  expect_equal(sum(f$demand * as.vector(out$allocation %*% ratio)),
               sum(f$supply[out$utilization > 0]), tolerance = 1e-12)
  oracle <- clm_oracle(f$distance, f$demand, f$supply, 2.5, v0 = 4, allocation = "huff_decay")
  expect_equal(unname(out$allocation), unname(oracle$allocation), tolerance = 1e-12)
})

test_that("huff_decay sensitivities match analytic algebra and finite differences", {
  f <- alloc_huff_case()
  for (family in c("gaussian", "exponential", "power")) {
    theta <- c(sigma = switch(family, gaussian = 2.5, exponential = 0.3, power = 1.2),
               v0 = 4, beta = 1.3)
    p <- .huff_problem(f$inputs, family = family, kappa = 1 / 3, fit_v0 = TRUE,
                       fit_beta = TRUE, allocation = "huff_decay")
    choice <- function(th) clm_oracle(f$distance, f$demand, f$supply, th[["sigma"]],
      th[["v0"]], family, beta = th[["beta"]], kappa = 1 / 3, allocation = "huff_decay")
    oracle <- choice(theta)
    P <- oracle$probability
    K <- oracle$kernel
    dlogK <- switch(family,
      gaussian = f$distance^2 / theta[["sigma"]]^3,
      exponential = -f$distance,
      power = -log(f$distance))
    logS <- matrix(log(f$supply / 3), nrow(P), ncol(P), byrow = TRUE)
    # d(P * K) = K * dP, plus P * dK for sigma.
    deriv <- list(P * (dlogK - rowSums(P * dlogK)) * K + P * K * dlogK,
                  -P / oracle$denom * K, P * (logS - rowSums(P * logS)) * K)
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

test_that("allocation form survives fitting, inspection and comparison", {
  f <- alloc_huff_case()
  observed <- .solve_problem(.huff_problem(f$inputs, v0 = 4), c(sigma = 2.5))$outputs$utilization
  # One observed target is necessary for a legitimate model comparison.
  fits <- lapply(c(clm = "clm", huff_decay = "huff_decay"), function(form) {
    p <- .huff_problem(f$inputs, v0 = 4, allocation = form)
    fit <- .fit_problem_nfxp(p, observed = observed, init = c(sigma = 2),
                             lower = c(sigma = 0.5), upper = c(sigma = 30))
    expect_identical(fit$allocation_form, form)
    expect_identical(summary(fit)$allocation_form, form)
    expect_output(print(fit), form)
    expect_output(print(summary(fit)), form)
    fit
  })
  expect_identical(.compare_problem_fits(fits)$allocation_form, c("clm", "huff_decay"))
  # Prediction snapshots exist only for the corrected CLM.
  legacy <- .solve_problem(.huff_problem(f$inputs, allocation = "huff_decay"), c(sigma = 2))
  expect_error(predict_allocation(legacy, f$distance), "regenerate")
})
