# check_allocation(): comparators and implied trips. Expected values come from
# clm_oracle() (helper-clm.R) and hand-computed Poisson losses, not internals.

poisson_nll <- function(U, Y, eps) sum(pmax(U, eps) - Y * log(pmax(U, eps)))

test_that("a distance-generated fit beats both comparators", {
  f <- alloc_small_case()
  fit <- alloc_small_fit()
  out <- check_allocation(f$model, fit)
  expect_s3_class(out, "ae_allocation_check")
  tab <- as.data.frame(out)
  expect_identical(tab$comparator, c("fitted", "no_decay", "capacity_only"))
  expect_equal(tab$loss[1], fit$best$loss, tolerance = 1e-8)
  expect_true(all(tab$loss_difference[2:3] > 0))
  expect_false(any(out$flags))
  expect_equal(tab$loss_difference[1], 0)
  expect_output(print(out), "better than both comparators")
  # The floor report: the default floor binds nowhere, and an object saved before
  # the flag existed still prints.
  expect_identical(tail(names(out$flags), 1L), "positive_counts_below_poisson_floor")
  expect_identical(out$poisson_floor, c(eps = 1e-250, positive_below = 0))
  old <- out
  old$flags <- old$flags[-length(old$flags)]
  old$poisson_floor <- NULL
  expect_output(print(old), "better than both comparators")
})

test_that("comparator losses and trips match independent calculations", {
  f <- alloc_small_case()
  fit <- alloc_small_fit()
  out <- check_allocation(f$model, fit, trip_breaks = c(2, 4))
  tab <- as.data.frame(out)
  Y <- f$observed
  # Capacity only: the Poisson optimum matches the observed total.
  U_cap <- f$supply * sum(Y) / sum(f$supply)
  expect_equal(tab$loss[3], poisson_nll(U_cap, Y, eps = out$poisson_floor[["eps"]]), tolerance = 1e-6)
  # No decay: a flat kernel with the reported outside mass.
  v0 <- out$parameters$no_decay[["v0"]]
  travel <- f$travel[f$active, , drop = FALSE]
  flat <- clm_oracle(travel, f$demand[f$active], f$supply, sigma = 1e8, v0 = v0)
  expect_equal(tab$loss[2], poisson_nll(flat$utilization, Y, eps = out$poisson_floor[["eps"]]), tolerance = 1e-6)
  expect_equal(tab$contact[2], sum(f$demand[f$active] * flat$rho) / sum(f$demand[f$active]),
               tolerance = 1e-6)
  # Fitted trips: flow-weighted travel-time quantiles and exceedance shares.
  truth <- clm_oracle(travel, f$demand[f$active], f$supply, coef(fit)[["sigma"]],
                      coef(fit)[["v0"]])
  w <- as.vector(truth$flow); t <- as.vector(travel); o <- order(t)
  cw <- cumsum(w[o]) / sum(w)
  expect_equal(tab$trip_median[1], t[o][which(cw >= 0.5)[1]])
  expect_equal(tab$trips_over_2[1], sum(w[t > 2]) / sum(w), tolerance = 1e-8)
  expect_equal(tab$trips_over_4[1], sum(w[t > 4]) / sum(w), tolerance = 1e-8)
})

test_that("counts that track capacity are flagged as not identifying decay", {
  f <- alloc_small_case()
  fit <- fit_allocation(f$model, f$supply * 3, f$starts, f$lower, f$upper,
                        loss = "poisson", control = list(maxit = 100))
  out <- check_allocation(f$model, fit)
  expect_true(out$flags[["capacity_only_fits_at_least_as_well"]])
  expect_output(print(out), "without distance decay")
})

test_that("absent edges are excluded from trips and reported", {
  demand <- c(a = 50, b = 40, c = 30)
  supply <- c(x = 20, y = 30)
  travel <- matrix(c(1, 3, Inf, 4, 2, 1), 3, 2, dimnames = list(names(demand), names(supply)))
  model <- clm_allocation(prepare_allocation(demand, supply, travel), fit_v0 = TRUE)
  Y <- c(x = 25, y = 45)
  fit <- fit_allocation(model, Y, rbind(s1 = c(sigma = 2, v0 = 5)),
                        c(sigma = 0.5, v0 = 0.1), c(sigma = 20, v0 = 200),
                        loss = "poisson", control = list(maxit = 100))
  tab <- as.data.frame(check_allocation(model, fit))
  # Only the capacity-only allocation sends flow along the absent edge (c -> x).
  expect_equal(tab$trips_absent_edge[1:2], c(0, 0))
  expect_equal(tab$trips_absent_edge[3], (30 * 20 / 50) / sum(demand), tolerance = 1e-8)
})

test_that("flow targets have no capacity-only comparator", {
  f <- alloc_small_case()
  fit <- fit_allocation(f$model, f$flow[, names(f$supply)], f$starts, f$lower, f$upper,
                        output = "flow", loss = "poisson", control = list(maxit = 100))
  out <- check_allocation(f$model, fit)
  tab <- as.data.frame(out)
  expect_true(all(is.na(tab[tab$comparator == "capacity_only", c("loss", "trip_median")])))
  expect_false(out$flags[["capacity_only_fits_at_least_as_well"]])
  expect_output(print(out), "facility totals only")
})

test_that("a model without a fitted outside mass can be checked", {
  f <- alloc_small_case()
  model <- clm_allocation(f$inputs)
  fit <- fit_allocation(model, f$observed, rbind(a = c(sigma = 3)), c(sigma = 0.5), c(sigma = 20),
                        loss = "poisson", control = list(maxit = 100))
  out <- check_allocation(model, fit)
  expect_identical(names(out$boundary), "sigma")
  expect_false(out$flags[["decay_parameter_at_bound"]])
  expect_false(out$flags[["comparator_search_at_edge"]])
  # With no outside mass to fit, no decay has no parameters to report.
  tab <- as.data.frame(out)
  expect_identical(tab$parameters[2], "no decay")
  expect_length(out$parameters$no_decay, 0)
  expect_type(out$parameters$no_decay, "double")
  expect_identical(names(out$parameters$fitted), "sigma")
  # Supply shares spread the retained demand: loads are supply * D / sum(supply).
  loads <- f$supply * sum(f$demand[f$active]) / sum(f$supply)
  expect_equal(tab$loss[2], poisson_nll(loads, f$observed, eps = out$poisson_floor[["eps"]]), tolerance = 1e-10)
})

test_that("comparator searches follow the scale of the outside mass", {
  f <- alloc_small_case()
  demand <- f$demand * 1e5
  supply <- f$supply * 1e3
  inputs <- prepare_allocation(demand, supply, f$travel, missing_demand = "exclude")
  model <- clm_allocation(inputs, fit_v0 = TRUE)
  Y <- f$observed * 1e5
  fit <- fit_allocation(model, Y, rbind(a = c(sigma = 3, v0 = 1e10)),
                        c(sigma = 0.5, v0 = 1e6), c(sigma = 20, v0 = 1e14),
                        loss = "poisson", control = list(maxit = 200))
  out <- check_allocation(model, fit)
  # Brute-force optimum of the flat allocation over a wide outside-mass grid.
  travel <- f$travel[f$active, , drop = FALSE]
  v0s <- exp(seq(log(1e4), log(1e16), length.out = 400))
  brute <- min(vapply(v0s, function(v) {
    o <- clm_oracle(travel, demand[f$active], supply, sigma = 1e8, v0 = v)
    poisson_nll(o$utilization, Y, eps = out$poisson_floor[["eps"]])
  }, numeric(1)))
  expect_lte(as.data.frame(out)$loss[2], brute + 1e-6 * abs(brute))
  expect_false(out$flags[["comparator_search_at_edge"]])
})

test_that("a fitted attractiveness exponent is re-optimized in both comparators", {
  f <- alloc_small_case()
  model <- clm_allocation(f$inputs, fit_v0 = TRUE, fit_beta = TRUE)
  fit <- fit_allocation(model, f$supply^1.5 * 0.05, rbind(a = c(sigma = 3, v0 = 10, beta = 1)),
                        c(sigma = 0.5, v0 = 0.1, beta = 0.2), c(sigma = 20, v0 = 400, beta = 4),
                        loss = "poisson", control = list(maxit = 200))
  out <- check_allocation(model, fit)
  tab <- as.data.frame(out)
  expect_match(tab$parameters[3], "beta=")
  # No decay reports the re-optimized outside mass and exponent, without sigma.
  expect_identical(names(out$parameters$fitted), c("sigma", "v0", "beta"))
  expect_identical(names(out$parameters$no_decay), c("v0", "beta"))
  expect_identical(tab$parameters[2],
    paste0("no decay; ", paste(sprintf("%s=%.4g", names(out$parameters$no_decay),
                                      out$parameters$no_decay), collapse = ", ")))
  expect_match(tab$parameters[2], "^no decay; v0=[^,]+, beta=[^,]+$")
  # Capacity only with a free exponent reproduces counts proportional to supply^1.5.
  expect_equal(out$parameters$capacity_only[["beta"]], 1.5, tolerance = 1e-3)
  fixed <- f$supply^coef(fit)[["beta"]]
  fixed_loss <- poisson_nll(fixed * sum(f$supply^1.5 * 0.05) / sum(fixed), f$supply^1.5 * 0.05, eps = out$poisson_floor[["eps"]])
  expect_lte(tab$loss[3], fixed_loss + 1e-9)
})

test_that("invalid calls fail clearly", {
  f <- alloc_small_case()
  fit <- alloc_small_fit()
  expect_error(check_allocation(f$model, alloc_failed_search(f)), "no eligible allocation fit")
  expect_error(check_allocation(f$model, fit, trip_breaks = c(0, 5)), "trip_breaks")
  other <- clm_allocation(f$inputs, family = "exponential", fit_v0 = TRUE)
  expect_error(check_allocation(other, fit), "does not belong")
  expect_error(check_allocation(f$model, fit$best), "ae_multistart_fit")
})

test_that("no decay weights every finite travel time equally and does not report sigma", {
  f <- alloc_small_case()
  fit <- alloc_small_fit()
  out <- check_allocation(f$model, fit)
  tab <- as.data.frame(out)
  D <- sum(f$demand[f$active])
  Y <- f$observed
  # With equal weights every origin spreads over the facilities in proportion to
  # supply, so the loads are supply * D / (sum(supply) + v0). The Poisson-best
  # outside mass makes their total equal the observed total.
  v0 <- out$parameters$no_decay[["v0"]]
  expect_equal(v0, sum(f$supply) * (D / sum(Y) - 1), tolerance = 1e-6)
  loads <- f$supply * D / (sum(f$supply) + v0)
  expect_equal(tab$loss[2], poisson_nll(loads, Y, eps = out$poisson_floor[["eps"]]), tolerance = 1e-10)
  expect_equal(tab$contact[2], sum(f$supply) / (sum(f$supply) + v0), tolerance = 1e-10)
  # The decay parameter is not part of this allocation.
  expect_identical(names(out$parameters$fitted), c("sigma", "v0"))
  expect_identical(names(out$parameters$no_decay), "v0")
  expect_identical(tab$parameters[1],
                   sprintf("sigma=%.4g, v0=%.4g", coef(fit)[["sigma"]], coef(fit)[["v0"]]))
  expect_identical(tab$parameters[2], sprintf("no decay; v0=%.4g", v0))
})

test_that("no decay keeps absent edges absent", {
  demand <- c(a = 50, b = 40, c = 30)
  supply <- c(x = 20, y = 30)
  travel <- matrix(c(1, 3, Inf, 4, 2, 1), 3, 2, dimnames = list(names(demand), names(supply)))
  model <- clm_allocation(prepare_allocation(demand, supply, travel), fit_v0 = TRUE)
  Y <- c(x = 25, y = 45)
  fit <- fit_allocation(model, Y, rbind(s1 = c(sigma = 2, v0 = 5)),
                        c(sigma = 0.5, v0 = 0.1), c(sigma = 20, v0 = 200),
                        loss = "poisson", control = list(maxit = 100))
  out <- check_allocation(model, fit)
  v0 <- out$parameters$no_decay[["v0"]]
  # Origin c cannot reach x: with equal weights it chooses y or the outside option.
  loads <- c(x = (50 + 40) * 20 / (50 + v0), y = (50 + 40) * 30 / (50 + v0) + 30 * 30 / (30 + v0))
  expect_equal(as.data.frame(out)$loss[2], poisson_nll(loads, Y, eps = out$poisson_floor[["eps"]]), tolerance = 1e-9)
})

test_that("the no-decay comparator is the same whichever built-in kernel was fitted", {
  f <- alloc_huff_case()
  Y <- colSums(clm_oracle(f$distance, f$demand, f$supply, 2.5, v0 = 30)$flow)
  search <- list(gaussian = c(start = 3, lower = 0.2, upper = 20),
                 exponential = c(start = 0.4, lower = 0.01, upper = 5),
                 power = c(start = 1.5, lower = 0.1, upper = 8))
  v0_best <- sum(f$supply) * (sum(f$demand) / sum(Y) - 1)
  loads <- f$supply * sum(f$demand) / (sum(f$supply) + v0_best)
  for (family in names(search)) {
    s <- search[[family]]
    model <- clm_allocation(f$inputs, family = family, fit_v0 = TRUE)
    fit <- fit_allocation(model, Y, rbind(a = c(sigma = s[["start"]], v0 = 30)),
                          c(sigma = s[["lower"]], v0 = 1), c(sigma = s[["upper"]], v0 = 500),
                          loss = "poisson", control = list(maxit = 200))
    expect_identical(fit$status, "selected", info = family)
    out <- check_allocation(model, fit)
    expect_equal(out$parameters$no_decay[["v0"]], v0_best, tolerance = 1e-6, info = family)
    expect_equal(as.data.frame(out)$loss[2], poisson_nll(loads, Y, eps = out$poisson_floor[["eps"]]), tolerance = 1e-8, info = family)
    expect_identical(names(out$parameters$no_decay), "v0", info = family)
  }
})

# A custom copy of the Gaussian kernel, declared with braces and a comment, and
# its fit of the small case.
copy_kernel <- function() {
  decay_kernel(
    function(distance, sigma) {
      # The Gaussian kernel, written out.
      exp(-(distance^2) / (2 * sigma^2))
    },
    name = "gaussian-copy", parameter = "travel scale"
  )
}

copy_kernel_fit <- function() {
  f <- alloc_small_case()
  model <- clm_allocation(f$inputs, family = copy_kernel(), fit_v0 = TRUE)
  fit <- fit_allocation(model, rev(f$observed), f$starts, f$lower, f$upper,
                        loss = "poisson", gradient = TRUE, control = list(maxit = 100))
  list(f = f, model = model, fit = fit)
}

test_that("a model with a declared kernel can be checked and compared", {
  k <- alloc_memo("copy_kernel_fit", copy_kernel_fit())
  f <- k$f
  reference <- alloc_small_fit()
  expect_identical(k$fit$status, "selected")
  out <- check_allocation(k$model, k$fit)
  ref <- check_allocation(f$model, reference)
  expect_s3_class(out, "ae_allocation_check")
  expect_identical(out$family, "gaussian-copy")
  # No decay ignores the kernel. The custom copy evaluates the same arithmetic as
  # the built-in Gaussian, so the fits and the comparators agree, and the
  # no-decay allocation is the equal-weight algebra.
  tab <- as.data.frame(out)
  ref_tab <- as.data.frame(ref)
  expect_identical(tab$comparator, c("fitted", "no_decay", "capacity_only"))
  expect_equal(tab[, setdiff(names(tab), "parameters")],
               ref_tab[, setdiff(names(ref_tab), "parameters")], tolerance = 1e-6)
  expect_identical(tab[2, ], ref_tab[2, ])
  expect_identical(tab$parameters[2:3], ref_tab$parameters[2:3])
  expect_identical(out$parameters$no_decay, ref$parameters$no_decay)
  expect_identical(names(out$parameters$no_decay), "v0")
  expect_false(any(out$flags))
  expect_output(print(out), "better than both comparators")
  loads <- f$supply * sum(f$demand[f$active]) /
    (sum(f$supply) + out$parameters$no_decay[["v0"]])
  expect_equal(tab$loss[2], poisson_nll(loads, f$observed, eps = out$poisson_floor[["eps"]]), tolerance = 1e-10)
  # Comparing the custom-kernel fit with the built-in Gaussian fit is like for like.
  cmp <- compare_allocations(list(custom = k$fit, builtin = reference),
                             models = list(custom = k$model, builtin = f$model))
  table <- as.data.frame(cmp)
  expect_identical(table$family, c("gaussian-copy", "gaussian"))
  expect_true(all(table$comparable))
  expect_equal(table$loss_difference, c(0, 0), tolerance = 1e-6)
  expect_equal(table$contact[1], table$contact[2], tolerance = 1e-6)
  expect_output(print(cmp), "reference: custom")
  expect_no_error(compare_allocations(list(custom = k$fit, builtin = reference)))
})

test_that("a fit is checked only against the model that produced it", {
  k <- alloc_memo("copy_kernel_fit", copy_kernel_fit())
  f <- k$f
  reference <- alloc_small_fit()
  declared <- function(kernel) clm_allocation(f$inputs, family = kernel, fit_v0 = TRUE)
  # The same declaration made again is the same model.
  same <- declared(copy_kernel())
  expect_no_error(check_allocation(same, k$fit))
  expect_no_error(compare_allocations(list(a = k$fit, b = reference),
                                      models = list(a = same, b = f$model)))
  # A kernel with the same name is another model, whether it has other values or
  # only other text: the unbraced form below has the values of the declared
  # kernel, as does the one without parentheses, so only the stored declaration
  # tells these models apart from the one that was fitted.
  gaussian_copy <- function(fun, parameter = "travel scale") {
    declared(decay_kernel(fun, name = "gaussian-copy", parameter = parameter))
  }
  others <- list(
    unbraced = gaussian_copy(function(distance, sigma) exp(-(distance^2) / (2 * sigma^2))),
    unparenthesized = gaussian_copy(function(distance, sigma) {
      # The same values, without parentheses around the square.
      exp(-distance^2 / (2 * sigma^2))
    }),
    other_meaning = gaussian_copy(function(distance, sigma) {
      # The Gaussian kernel, written out.
      exp(-(distance^2) / (2 * sigma^2))
    }, parameter = "scale"),
    other_values = gaussian_copy(function(distance, sigma) exp(-distance / sigma)))
  for (label in names(others)) {
    expect_error(check_allocation(others[[label]], k$fit), "does not belong", info = label)
    expect_error(compare_allocations(list(a = k$fit, b = reference),
                                     models = list(a = others[[label]], b = f$model)),
                 "does not belong", info = label)
  }
  # Another kernel name, or a built-in model against a declared kernel, is refused
  # as well.
  expect_error(check_allocation(f$model, k$fit), "does not belong")
  expect_error(check_allocation(k$model, reference), "does not belong")
  # A fit without a prediction snapshot (an older saved fit) is matched on its
  # other fields, as before.
  bare <- reference
  bare$best$prediction_snapshot <- NULL
  expect_no_error(check_allocation(f$model, bare))
  bare <- k$fit
  bare$best$prediction_snapshot <- NULL
  expect_no_error(check_allocation(k$model, bare))
  expect_error(check_allocation(others$other_values, bare), "does not belong")
})

test_that("a built-in fit is matched on its kernel only, as before declared kernels existed", {
  f <- alloc_small_case()
  reference <- alloc_small_fit()
  k <- alloc_memo("copy_kernel_fit", copy_kernel_fit())
  # v0 is fitted, so the constant v0 = 3 of this declaration is never used: the fit
  # is reproduced exactly and checked, and gives the same report.
  unused <- clm_allocation(f$inputs, family = "gaussian", v0 = 3, fit_v0 = TRUE)
  expect_false(identical(unused$metadata$spec, f$model$metadata$spec))
  out <- check_allocation(unused, reference)
  expect_identical(as.data.frame(out), as.data.frame(check_allocation(f$model, reference)))
  expect_no_error(compare_allocations(list(a = reference, b = k$fit),
                                      models = list(a = unused, b = k$model)))
  # A built-in declaration is the same model as its string.
  expect_no_error(check_allocation(
    clm_allocation(f$inputs, family = decay_kernel("gaussian"), fit_v0 = TRUE), reference))
  # Constants that do change the fit are caught by reproducing its loss instead.
  for (changed in list(clm_allocation(f$inputs, kappa = 2, fit_v0 = TRUE),
                       clm_allocation(f$inputs, beta = 2, fit_v0 = TRUE))) {
    expect_error(check_allocation(changed, reference), "does not belong to this model")
    expect_error(check_allocation(changed, reference), "loss cannot be reproduced")
  }
  # Another built-in kernel is still another model.
  expect_error(check_allocation(clm_allocation(f$inputs, family = "exponential", fit_v0 = TRUE),
                                reference), "does not belong")
})

test_that("the internal no-decay model is a copy that drops the kernel and ignores sigma", {
  f <- alloc_small_case()
  scaled <- decay_kernel(function(distance, sigma) exp(-distance / sigma), name = "scaled-exp",
                         parameter = "scale", lower = 0.5, upper = 20)
  for (family in list("gaussian", scaled)) {
    model <- clm_allocation(f$inputs, family = family, fit_v0 = TRUE)
    before <- model
    flat <- .no_decay_model(model)
    expect_identical(model, before)
    # The copy has the no-decay family and no declaration, so no prediction snapshot,
    # but keeps the parameter contract, including the declared bounds of sigma.
    expect_identical(flat$metadata$spec$family, "none")
    expect_false("kernel" %in% names(flat$metadata$spec))
    expect_identical(flat$theta, model$theta)
    expect_identical(flat$metadata$spec$theta, model$metadata$spec$theta)
    expect_null(.bind_theta(flat, c(sigma = 3, v0 = 10))$clm_prediction)
    # Every finite travel time has weight one whatever sigma is: supply shares of
    # the retained demand, with the outside mass.
    expected <- matrix(f$supply / (sum(f$supply) + 10), 3, 3, byrow = TRUE)
    for (sigma in c(1, 3, 15)) {
      out <- .solve_problem(flat, c(sigma = sigma, v0 = 10))$outputs
      expect_equal(unname(out$allocation), expected, tolerance = 1e-12)
    }
    # The declared bounds still apply to the sigma it is solved at.
    if (!is.null(model$metadata$spec[["kernel", exact = TRUE]])) {
      expect_error(.solve_problem(flat, c(sigma = 25, v0 = 10)), "outside the theta bounds")
    }
  }
})

test_that("a declared-kernel model with a fitted exponent can be checked", {
  f <- alloc_small_case()
  kernel <- decay_kernel(function(distance, sigma) exp(-(distance^2) / (2 * sigma^2)),
                         name = "gaussian-copy", parameter = "travel scale")
  model <- clm_allocation(f$inputs, family = kernel, fit_v0 = TRUE, fit_beta = TRUE)
  fit <- fit_allocation(model, f$supply^1.5 * 0.05, rbind(a = c(sigma = 3, v0 = 10, beta = 1)),
                        c(sigma = 0.5, v0 = 0.1, beta = 0.2), c(sigma = 20, v0 = 400, beta = 4),
                        loss = "poisson", control = list(maxit = 200))
  out <- check_allocation(model, fit)
  expect_identical(names(out$parameters$no_decay), c("v0", "beta"))
  expect_match(as.data.frame(out)$parameters[2], "^no decay; v0=[^,]+, beta=[^,]+$")
  expect_equal(out$parameters$capacity_only[["beta"]], 1.5, tolerance = 1e-3)
})
