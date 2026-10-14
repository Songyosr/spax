# Legacy providers and input paths kept internal (DEC-027). Each section can
# be deleted together with the code it tests:
# - "huff_decay": the explicit legacy allocation = "huff_decay" form of the
#   static Huff provider. Its first block also uses HAAE.
# - "SAE and HAAE": the SAE/HAAE fixed-point providers; the raster-path blocks
#   below also loop over them and lose those entries.
# - "Raster input path": raster demand/travel inputs to the AE providers
#   (.interaction_substrate) and the legacy prediction sources they create.
# The corrected static CLM over checked numeric inputs is tested in
# test-06-allocation_clm.R and test-06-ae_prediction.R.

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

# SAE and HAAE -----------------------------------------------------------------

test_that("SAE and HAAE reject invalid kernels, scaled supply and raw prepared arguments", {
  p <- prepare_allocation(c(o1 = 1), c(f1 = 2), matrix(0, 1, 1, dimnames = list("o1", "f1")))
  big <- prepare_allocation(c(o1 = 1), c(f1 = 1e200), matrix(1, 1, 1, dimnames = list("o1", "f1")))
  for (constructor in list(.sae_problem, .haae_problem)) {
    expect_error(.bind_theta(constructor(p, family = "power"), c(sigma = 2)), "strictly positive")
    expect_error(.bind_theta(constructor(p), c(sigma = 0)), "positive sigma")
    expect_error(.bind_theta(constructor(big, kappa = 1e200), c(sigma = 2)),
                 "scaled supply is nonfinite")
    for (raw in list(list(supply = c(f1 = 2)), list(distance = matrix(1)),
                     list(supply_cols = "x"))) {
      expect_error(do.call(constructor, c(list(p), raw)), "cannot be combined")
    }
  }
})

# Raster input path --------------------------------------------------------------

test_that("raster and prepared inputs give identical provider numerics", {
  d <- terra::rast(nrows = 1, ncols = 4)
  terra::values(d) <- c(10, 0, NA, 20)
  dist <- c(d, d); names(dist) <- c("b", "a")
  terra::values(dist) <- cbind(c(0, -1, NA, 3), c(2, NA, -1, NA))
  S <- c(a = 15, b = 30)
  D <- stats::setNames(terra::values(d)[, 1], as.character(1:4))
  M <- terra::values(dist); rownames(M) <- names(D)
  prepared <- prepare_allocation(D, S, M, missing_demand = "exclude",
                                 missing_travel = "legacy_zero")
  theta <- c(sigma = 2)
  # Drop the SAE/HAAE entries if those providers are retired.
  for (constructor in list(.huff_problem, .sae_problem, .haae_problem)) {
    a <- constructor(d, S, dist)
    b <- constructor(prepared)
    expect_equal(unname(.bind_theta(a, theta)$plan$Kd_active),
                 unname(.bind_theta(b, theta)$plan$Kd_active), tolerance = 1e-14)
    expect_equal(lapply(.problem_outputs_at(a, theta, a$state$init), unname),
                 lapply(.problem_outputs_at(b, theta, b$state$init), unname), tolerance = 1e-12)
  }
  legacy <- .interaction_substrate(d, S, dist)
  expect_identical(legacy$input_policy$missing_demand, "legacy_positive_fit")
  expect_identical(legacy$input_policy$missing_travel, "legacy_zero")
  terra::values(d) <- c(-1, Inf, NA, 20)
  expect_identical(.interaction_substrate(d, S, dist)$origin_status,
                   c("invalid_omitted", "invalid_omitted", "unknown", "active"))
  expect_error(.interaction_substrate(d, S, terra::shift(dist, dx = 90)), "align|extent")
})

test_that("a prepared raster substrate is reused without further extraction", {
  d <- terra::rast(nrows = 1, ncols = 4); terra::values(d) <- c(5, 0, NA, 7)
  distance <- c(d, d); terra::values(distance) <- cbind(1:4, 4:1)
  names(distance) <- c("a", "b")
  original <- terra::extract; calls <- 0L
  testthat::local_mocked_bindings(extract = function(...) {
    calls <<- calls + 1L; original(...)
  }, .package = "terra")
  p <- .interaction_substrate(d, c(a = 10, b = 20), distance)
  expect_equal(calls, 1L)
  # Drop the SAE/HAAE entries if those providers are retired.
  for (constructor in list(.huff_problem, .sae_problem, .haae_problem)) {
    problem <- constructor(p)
    .problem_outputs_at(problem, c(sigma = 2), problem$state$init)
  }
  expect_equal(calls, 1L)
})

test_that("legacy raster sources impute unknown travel and invalid kernels only on request", {
  f <- alloc_prediction_case()
  M <- f$distance; M[1, 2] <- NA_real_
  d <- terra::rast(nrows = 1, ncols = 3); terra::values(d) <- f$demand
  travel <- c(d, d, d); terra::values(travel) <- M; names(travel) <- names(f$supply)
  source <- .solve_problem(.huff_problem(d, f$supply, travel, kappa = .65, beta = 1.3, v0 = 4),
                           f$theta)
  pred <- predict_allocation(source, M, outputs = "allocation")
  expect_true(all(is.na(pred$outputs$allocation[1, ])))
  legacy <- predict_allocation(source, M, outputs = "allocation", missing_travel = "legacy_zero")
  expect_equal(unname(legacy$outputs$allocation), unname(source$outputs$allocation))
  expect_true(legacy$validity$unknown_travel[1])
  expect_true(legacy$validity$legacy_imputed[1])
  zeros <- f$distance; zeros[1, 1] <- 0
  power <- .solve_problem(.huff_problem(d, f$supply, travel, family = "power"), c(sigma = 2))
  expect_error(predict_allocation(power, zeros), "strictly positive")
  guarded <- predict_allocation(power, zeros, missing_travel = "legacy_zero")
  # Power decay is Inf at distance 0 (documented in ?calc_decay).
  # The guard applies because calc_decay(0, "power") returns Inf.
  expect_true(guarded$validity$legacy_kernel_imputed[1])
  # A legacy source zeroes nonfinite attractiveness; prediction imputes it only on request.
  overflow <- .solve_problem(.huff_problem(d, c(b = 1e200, a = 4, c = 2), travel, beta = 2,
                                           v0 = 4), c(sigma = 2))
  expect_error(predict_allocation(overflow, f$distance), "attractiveness is nonfinite")
  imputed <- predict_allocation(overflow, f$distance, missing_travel = "legacy_zero")
  expect_true(imputed$policy$legacy_attractiveness_imputed)
  expect_true(all(imputed$validity$legacy_imputed))
  # Gaussian sigma zero is only defined through the legacy finite-kernel guard.
  d <- terra::rast(nrows = 1, ncols = 1); terra::values(d) <- 1
  travel <- d; names(travel) <- "f"
  source <- .solve_problem(.huff_problem(d, c(f = 1), travel, v0 = 1), c(sigma = 0))
  request <- matrix(c(0, NA_real_, Inf, 1), ncol = 1,
                    dimnames = list(c("finite_zero", "unknown", "absent", "known"), "f"))
  expect_error(predict_allocation(source, request), "positive sigma")
  x <- predict_allocation(source, request, demand = c(1, 0, NA, 2),
    outputs = c("rho", "outside_share", "flow"), missing_travel = "legacy_zero")
  expect_identical(x$validity$legacy_kernel_imputed, c(TRUE, FALSE, FALSE, FALSE))
  expect_identical(x$validity$unknown_travel, c(FALSE, TRUE, FALSE, FALSE))
  expect_equal(unname(x$outputs$rho), rep(0, 4))
  expect_equal(unname(x$outputs$outside_share), rep(1, 4))
  expect_equal(as.numeric(x$outputs$flow), c(0, 0, NA, 0))
})

test_that("legacy raster power at sigma zero cannot change known absent-edge semantics", {
  d <- terra::rast(nrows = 1, ncols = 1); terra::values(d) <- 1
  travel <- d; terra::values(travel) <- Inf; names(travel) <- "f"
  source <- .solve_problem(.huff_problem(d, c(f = 1), travel, family = "power"), c(sigma = 0))
  M <- matrix(Inf, 1, 1, dimnames = list("new", "f"))
  expect_error(predict_allocation(source, M), "checked numeric preparation or positive sigma")
  expect_error(predict_allocation(source, M, missing_travel = "legacy_zero"), "checked numeric")
})
