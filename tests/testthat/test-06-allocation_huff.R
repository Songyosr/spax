# huff_allocation(): the Huff model declared as clm_allocation() without an
# outside option, with power decay and a fitted attractiveness exponent by
# default. Expected values come from the formula written out in base R.

# The Huff shares written out directly: P_ij = a_j d_ij^-sigma / sum_k a_k d_ik^-sigma
# with a_j = (kappa * S_j)^beta.
huff_direct <- function(distance, demand, supply, sigma, beta = 1, kappa = 1) {
  a <- (kappa * supply)^beta
  score <- sweep(distance^(-sigma), 2, a, "*")
  P <- score / rowSums(score)
  list(P = P, flow = P * demand, utilization = colSums(P * demand))
}

test_that("huff_allocation() declares the equivalent clm_allocation() model", {
  f <- alloc_huff_case()
  huff <- huff_allocation(f$inputs)
  clm <- clm_allocation(f$inputs, family = "power", kappa = 1, beta = 1, v0 = 0,
                        fit_v0 = FALSE, fit_beta = TRUE)
  expect_identical(huff$metadata$spec, clm$metadata$spec)
  expect_identical(huff$theta, clm$theta)
  expect_identical(huff$theta$names, c("sigma", "beta"))
  expect_identical(huff$metadata$spec$family, "power")
  expect_identical(huff$metadata$spec$v0, 0)
  expect_false(huff$metadata$spec$fit_v0)
  expect_true(huff$metadata$spec$fit_beta)
  expect_identical(huff$metadata$spec$allocation, "clm")

  # other settings pass through
  huff2 <- huff_allocation(f$inputs, family = "gaussian", kappa = 2, beta = 1.4,
                           fit_beta = FALSE)
  clm2 <- clm_allocation(f$inputs, family = "gaussian", kappa = 2, beta = 1.4,
                         v0 = 0, fit_v0 = FALSE, fit_beta = FALSE)
  expect_identical(huff2$metadata$spec, clm2$metadata$spec)
  expect_identical(huff2$theta, clm2$theta)
  expect_identical(huff2$theta$names, "sigma")
  decl <- decay_kernel("exponential")
  expect_identical(huff_allocation(f$inputs, family = decl)$metadata$spec,
                   clm_allocation(f$inputs, family = decl, v0 = 0, fit_beta = TRUE)$metadata$spec)
})

test_that("a Huff model evaluates to the direct Huff formula", {
  f <- alloc_huff_case()
  D <- f$distance
  d <- as.numeric(f$demand)
  for (case in list(list(sigma = 1.7, beta = 1, kappa = 1),
                    list(sigma = 2.4, beta = 0.6, kappa = 1),
                    list(sigma = 0.8, beta = 1.9, kappa = 0.25))) {
    model <- huff_allocation(f$inputs, kappa = case$kappa)
    out <- evaluate_allocation(model, c(sigma = case$sigma, beta = case$beta),
                               requested_outputs = "flow")$outputs
    direct <- huff_direct(D, d, f$supply, case$sigma, case$beta, case$kappa)
    expect_equal(unname(out$allocation), unname(direct$P), tolerance = 1e-12)
    expect_equal(unname(out$flow), unname(direct$flow), tolerance = 1e-12)
    expect_equal(unname(out$utilization), unname(direct$utilization), tolerance = 1e-12)
    # no outside option: shares add up to one and the loads add up to demand
    expect_equal(unname(rowSums(out$allocation)), rep(1, nrow(D)), tolerance = 1e-12)
    expect_equal(sum(out$utilization), sum(d), tolerance = 1e-9)
    expect_equal(unname(out$outside_share), rep(0, nrow(D)))
  }
  # beta fixed: only sigma is a parameter
  fixed <- huff_allocation(f$inputs, beta = 1.3, fit_beta = FALSE)
  out <- evaluate_allocation(fixed, c(sigma = 2))$outputs
  expect_equal(unname(out$utilization),
               unname(huff_direct(D, d, f$supply, 2, 1.3)$utilization), tolerance = 1e-12)
})

test_that("a Huff fit to simulated facility totals recovers sigma and beta", {
  f <- alloc_huff_case()
  model <- huff_allocation(f$inputs)
  truth <- c(sigma = 1.6, beta = 1.4)
  observed <- huff_direct(f$distance, as.numeric(f$demand), f$supply,
                          truth[["sigma"]], truth[["beta"]])$utilization
  fit <- fit_allocation(model, observed,
    starts = rbind(a = c(sigma = 1, beta = 0.8), b = c(sigma = 2.5, beta = 1.8)),
    lower = c(sigma = 0.1, beta = 0.1), upper = c(sigma = 6, beta = 4),
    loss = "poisson", gradient = TRUE)
  expect_s3_class(fit, "ae_multistart_fit")
  expect_identical(names(fit$best$theta), c("sigma", "beta"))
  # three facilities carry only a rough identification of two exponents; the
  # fitted loads reproduce the observed totals closely
  fitted_loads <- evaluate_allocation(model, fit$best$theta)$outputs$utilization
  expect_equal(unname(fitted_loads), unname(observed), tolerance = 1e-3)
  expect_equal(unname(fit$best$theta[["sigma"]]), truth[["sigma"]], tolerance = 0.1)
  expect_equal(unname(fit$best$theta[["beta"]]), truth[["beta"]], tolerance = 0.1)
  # the other workflow steps accept the fit unchanged
  # (the no-decay and capacity-only comparators search over beta without an
  # outside weight; a step beyond finite attractiveness must not stop the check)
  expect_no_error(chk <- check_allocation(model, fit))
  expect_s3_class(chk, "ae_allocation_check")
  expect_identical(chk$comparators$comparator, c("fitted", "no_decay", "capacity_only"))
  # prediction at requested locations follows the same Huff formula
  request <- f$distance[1:3, , drop = FALSE]
  pr <- predict_allocation(fit, request, demand = c(5, 6, 7), outputs = "allocation")
  direct <- huff_direct(request, c(5, 6, 7), f$supply, fit$best$theta[["sigma"]],
                        fit$best$theta[["beta"]])
  expect_equal(unname(pr$outputs$allocation), unname(direct$P), tolerance = 1e-8)
  # and a fit of a second kernel compares with it
  gauss <- huff_allocation(f$inputs, family = "gaussian")
  fit_g <- fit_allocation(gauss, observed,
    starts = rbind(a = c(sigma = 3, beta = 1.2)),
    lower = c(sigma = 0.1, beta = 0.1), upper = c(sigma = 20, beta = 4),
    loss = "poisson", gradient = TRUE)
  expect_no_error(compare_allocations(list(power = fit, gaussian = fit_g),
                                      models = list(power = model, gaussian = gauss)))
})

test_that("a Huff model refuses an outside option", {
  f <- alloc_huff_case()
  msg <- paste0("the Huff model has no outside option; use spec_clm() or ",
                "clm_allocation(fit_v0 = TRUE) for one")
  expect_error(huff_allocation(f$inputs, by_group = "v0"), msg, fixed = TRUE)
  expect_error(huff_allocation(f$inputs, by_group = c("sigma", "v0")), msg, fixed = TRUE)
  expect_error(huff_allocation(f$inputs, fixed = c(v0 = 1)), msg, fixed = TRUE)
  expect_error(huff_allocation(f$inputs, fixed = c(sigma = 2, v0.a = 0)), msg, fixed = TRUE)
  # ungrouped inputs still refuse by_group and fixed for their own reason
  expect_error(huff_allocation(f$inputs, by_group = "sigma"), "demand groups")
  expect_error(huff_allocation(f$inputs, fixed = c(sigma = 2)), "demand groups")
  # a Huff model has no fitted outside weight to bootstrap
  model <- huff_allocation(f$inputs)
  observed <- huff_direct(f$distance, as.numeric(f$demand), f$supply, 1.6, 1.4)$utilization
  fit <- fit_allocation(model, observed,
    starts = rbind(a = c(sigma = 1, beta = 0.8)),
    lower = c(sigma = 0.1, beta = 0.1), upper = c(sigma = 6, beta = 4), loss = "poisson")
  expect_error(bootstrap_allocation(model, fit, alloc_bootstrap_observation(), seed = 1),
               "fit_v0")
})

test_that("power decay refuses a zero travel time", {
  f <- alloc_huff_case()
  distance <- f$distance
  distance[1, 1] <- 0
  inputs <- prepare_allocation(f$demand, f$supply, distance)
  model <- huff_allocation(inputs)
  expect_error(evaluate_allocation(model, c(sigma = 2, beta = 1)))
})

test_that("a Huff model passes demand groups through", {
  ids <- paste0("o", 1:6)
  demand <- list(a = stats::setNames(c(10, 0, 0, 7, 4, 6), ids),
                 b = stats::setNames(c(5, 0, 8, 0, NA, 9), ids))
  M <- matrix(c(1, 2, 3, 4, 5, 6,  2, 3, 4, 5, 6, 7,  3, 4, 5, 6, 7, 8), 6, 3,
              dimnames = list(ids, c("f2", "f1", "f3")))
  p <- prepare_allocation(demand, c(f1 = 20, f2 = 15, f3 = 5), M,
                          missing_demand = "exclude")
  m <- huff_allocation(p, by_group = "sigma")
  clm <- clm_allocation(p, family = "power", v0 = 0, fit_v0 = FALSE, fit_beta = TRUE,
                        by_group = "sigma")
  expect_identical(m$metadata$spec, clm$metadata$spec)
  expect_identical(m$theta, clm$theta)
  expect_identical(m$theta$names, c("sigma.a", "sigma.b", "beta"))
  theta <- c(sigma.a = 1.5, sigma.b = 2.5, beta = 0.9)
  oracle <- clm_group_oracle(p$distance_groups, p$D_groups, as.vector(p$S),
                             sigma = c(a = 1.5, b = 2.5), v0 = 0, family = "power",
                             beta = 0.9)
  e <- evaluate_allocation(m, theta)
  expect_equal(unname(e$outputs$allocation), unname(oracle$allocation), tolerance = 1e-12)
  expect_equal(e$outputs$utilization, oracle$utilization, tolerance = 1e-12)
  # held fixed values and a family per group pass through
  m2 <- huff_allocation(p, by_group = "sigma", fixed = c(sigma.a = 1.5), fit_beta = FALSE)
  expect_identical(m2$theta$names, "sigma.b")
  m3 <- huff_allocation(p, family = list(a = "power", b = "gaussian"))
  expect_identical(m3$theta$names, c("sigma.a", "sigma.b", "beta"))
})
