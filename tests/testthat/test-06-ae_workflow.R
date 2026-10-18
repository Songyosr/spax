# Public contract of the experimental allocation workflow: clm_allocation(),
# fit_allocation(), evaluate_allocation(), predict_allocation() and the saved
# fit accessors. Preparation, prediction algebra and the bootstrap have their
# own files.

test_that("workflow model declaration retains only the corrected prepared CLM", {
  f <- alloc_small_case()
  expect_identical(f$model$substrate, f$inputs)
  expect_identical(f$model$metadata$spec$allocation, "clm")
  expect_identical(f$model$theta$names, c("sigma", "v0"))
  expect_error(clm_allocation(f$demand), "ae_prepared_inputs")
  # The legacy huff_decay form serves as the non-CLM model.
  wrong <- .huff_problem(f$inputs, allocation = "huff_decay")
  expect_error(evaluate_allocation(wrong, c(sigma = 3)), "corrected static CLM")
  expect_error(fit_allocation(wrong, f$observed, f$starts[, "sigma", drop = FALSE],
    f$lower["sigma"], f$upper["sigma"]), "corrected static CLM")
  expect_error(clm_allocation(f$inputs, family = "cauchy"), "arg")
  for (bad in list(0, "1")) expect_error(clm_allocation(f$inputs, kappa = bad), "kappa")
  for (bad in list(0, c(1, 2))) expect_error(clm_allocation(f$inputs, beta = bad), "beta")
  for (bad in list(-1, NA_real_)) expect_error(clm_allocation(f$inputs, v0 = bad), "v0")
  for (bad in list("yes", NA, c(TRUE, FALSE), 1)) {
    expect_error(clm_allocation(f$inputs, fit_v0 = bad), "must be TRUE or FALSE")
    expect_error(clm_allocation(f$inputs, fit_beta = bad), "must be TRUE or FALSE")
  }
})

test_that("a selected fit recovers the truth and its saved methods read without recomputation", {
  f <- alloc_small_case()
  fit <- alloc_small_fit()
  expect_s3_class(fit, "ae_multistart_fit")
  expect_identical(fit$status, "selected")
  expect_equal(stats::coef(fit), f$truth, tolerance = 1e-3)
  # The target was supplied in reverse order; fitted values are canonical.
  expect_equal(stats::fitted(fit), f$observed, tolerance = 1e-3)
  testthat::local_mocked_bindings(
    .solve_problem = function(...) stop("accessor attempted a solve"),
    .fit_problem_nfxp = function(...) stop("accessor attempted a fit"))
  expect_identical(stats::coef(fit), fit$best$theta)
  expect_identical(stats::fitted(fit), fit$best$predicted)
  report <- summary(fit)
  expect_s3_class(report, "summary.ae_multistart_fit")
  expect_identical(report$status, fit$status)
  expect_identical(report$selected_start, fit$best_start)
  expect_identical(report$starts, fit$table)
  expect_identical(report$boundary, fit$boundary)
  expect_identical(report$warnings, fit$warnings)
  expect_identical(report$errors, fit$errors)
  expect_identical(report$disagreement$loss, fit$loss_disagreement)
  expect_s3_class(report$selected, "summary.ae_problem_nfxp_fit")
  expect_output(print(report), "Selected fit")
  expect_identical(as.data.frame(fit), data.frame(parameter = c("sigma", "v0"),
    estimate = unname(stats::coef(fit))))
})

test_that("a search without an eligible start stays inspectable but has no estimates", {
  f <- alloc_small_case()
  failed <- alloc_failed_search(f)
  expect_identical(failed$status, "no_valid_fit")
  report <- summary(failed)
  expect_null(report$selected)
  expect_true(all(grepl("injected loss failure", unlist(report$errors))))
  expect_output(print(report), "none")
  expect_output(print(report), "\\$errors")
  for (method in list(stats::coef, stats::fitted, as.data.frame, plot)) {
    expect_error(method(failed), "no eligible")
  }
  expect_error(predict_allocation(failed, f$travel), "no eligible")
})

test_that("loss choices pass through and count targets reject negatives", {
  f <- alloc_small_case()
  calls <- 0L; received <- NULL
  testthat::local_mocked_bindings(.fit_problem_multistart = function(problem, observed, ...) {
    calls <<- calls + 1L
    received <<- c(list(problem = problem, observed = observed), list(...))
    "passed"
  })
  utilization <- f$observed; utilization[2] <- -0.5
  expect_error(fit_allocation(f$model, utilization, f$starts, f$lower, f$upper),
               "expected counts must be nonnegative.*utilization")
  flow <- f$flow; flow[1, 1] <- NA_real_; flow[2, 3] <- -0.5
  expect_error(fit_allocation(f$model, flow, f$starts, f$lower, f$upper, output = "flow"),
               "expected counts must be nonnegative.*flow")
  expect_error(fit_allocation(f$model, f$observed, f$starts, f$lower, f$upper, loss = 1),
               "loss must be")
  expect_equal(calls, 0L)
  # Weighted SSE (eta = 1) and no gradient are the defaults.
  expect_identical(fit_allocation(f$model, f$observed, f$starts, f$lower, f$upper), "passed")
  expect_identical(received$loss, .weighted_sse_loss)
  expect_identical(received$loss_grad, .weighted_sse_gradient)
  expect_identical(received$loss_args, list())
  expect_false(received$gradient)
  predicted <- f$observed * 1.1
  expect_equal(received$loss(predicted, f$observed),
               sum((predicted - f$observed)^2 / (f$observed + 1)), tolerance = 1e-12)
  fit_allocation(f$model, f$observed, f$starts, f$lower, f$upper, loss = "poisson")
  expect_identical(received$loss, .poisson_loss)
  expect_identical(received$loss_grad, .poisson_gradient)
  # The default Poisson floor is stored explicitly; a given eps is kept.
  expect_identical(received$loss_args, list(eps = 1e-250))
  fit_allocation(f$model, f$observed, f$starts, f$lower, f$upper, loss = "poisson",
                 loss_args = list(eps = 1e-6))
  expect_identical(received$loss_args, list(eps = 1e-6))
  flow[2, 3] <- 0.5
  expect_identical(fit_allocation(f$model, flow, f$starts, f$lower, f$upper,
                                  output = "flow"), "passed")
  expect_identical(received$observed, flow)
  # Other output domains and custom losses pass through unchanged.
  probability <- f$probability; probability[1, 1] <- -0.5
  custom <- function(predicted, observed) sum((predicted - observed)^2)
  expect_identical(fit_allocation(f$model, probability, f$starts, f$lower, f$upper,
    output = "allocation", loss = custom), "passed")
  expect_identical(received$observed, probability)
  expect_identical(received$loss, custom)
  expect_null(received$loss_grad)
  expect_equal(calls, 5L)
})

test_that("custom workflow callbacks retain canonical masked order and auxiliary arguments", {
  f <- alloc_small_case()
  y <- f$flow; y[1, 2] <- NA_real_
  mask <- !is.na(as.numeric(y)); expected <- as.numeric(y)[mask]
  weights <- seq_along(expected); seen <- list(); gradients <- 0L; optimised <- 0L
  loss <- function(predicted, observed, weights, offset) {
    seen[[length(seen) + 1L]] <<- list(observed = observed, weights = weights, offset = offset)
    sum(weights * (predicted - observed)^2) + offset
  }
  grad <- function(predicted, observed, sensitivity, weights, offset) {
    gradients <<- gradients + 1L
    as.numeric(crossprod(sensitivity, 2 * weights * (predicted - observed)))
  }
  # Evaluate the objective and gradient at each start instead of optimising.
  testthat::local_mocked_bindings(optim = function(par, fn, gr, ...) {
    optimised <<- optimised + 1L
    list(par = par, value = fn(par), gradient = gr(par), convergence = 0L)
  }, .package = "stats")
  fit <- fit_allocation(f$model, y[c(3, 1, 2), c(2, 3, 1), drop = FALSE],
    f$starts, f$lower, f$upper, output = "flow", loss = loss,
    loss_grad = grad, loss_args = list(weights = weights, offset = 7), gradient = TRUE)
  expect_identical(optimised, nrow(f$starts))
  expect_identical(fit$status, "selected")
  expect_true(length(seen) > 0)
  expect_true(gradients > 0)
  expect_true(all(vapply(seen, function(x) identical(x$observed, expected) &&
    identical(x$weights, weights) && identical(x$offset, 7), logical(1))))
  expect_equal(fit$best$loss,
    sum(weights * (as.numeric(fit$best$predicted)[mask] - expected)^2) + 7,
    tolerance = 1e-10)
})

test_that("fixed evaluation solves once and coverage reads the evaluated allocation", {
  f <- alloc_small_case()
  solve <- .solve_problem; solves <- 0L
  testthat::local_mocked_bindings(
    .solve_problem = function(...) { solves <<- solves + 1L; solve(...) },
    .problem_outputs_at = function(...) stop("unexpected second output evaluation"))
  result <- evaluate_allocation(f$model, f$truth, requested_outputs = "flow",
    coverage_args = list(norm = 1, units = "slots/request"))
  expect_equal(solves, 1L)
  expect_s3_class(result, "ae_equilibrium")
  expect_s3_class(result$coverage, "ae_coverage")
  expect_true(result$coverage$conservation$passed)
  expect_equal(result$outputs$allocation, f$probability, tolerance = 1e-12)
  expect_equal(result$outputs$flow, f$flow, tolerance = 1e-12)
  expect_equal(result$coverage$cells$rho, unname(rowSums(f$probability)), tolerance = 1e-12)
  expect_identical(result$prediction_snapshot$theta, f$truth)
  cells <- as.data.frame(result$coverage, type = "cells")
  expect_identical(cells$origin_id, c("o1", "o3", "o5"))
  expect_equal(cells$rho, result$coverage$cells$rho)
  expect_identical(attr(cells, "input_metadata"), f$inputs$input_metadata)
  expect_equal(as.data.frame(result$coverage)$rho, result$coverage$regional$rho)
  # Unnamed parameters follow the declared order.
  expect_identical(evaluate_allocation(f$model, unname(f$truth))$prediction_snapshot$theta, f$truth)
  expect_error(evaluate_allocation(f$model, f$truth, coverage_args = list(unknown = 1)),
               "coverage controls")
  testthat::local_mocked_bindings(.solve_problem = function(...) list(converged = FALSE))
  expect_error(evaluate_allocation(f$model, f$truth), "did not converge")
})

test_that("workflow predictions delegate once and read fitted and evaluated sources", {
  f <- alloc_small_case()
  fit <- alloc_small_fit()
  baseline <- evaluate_allocation(f$model, f$truth)
  source_copy <- baseline
  original_predict <- .predict_clm; calls <- 0L
  testthat::local_mocked_bindings(.predict_clm = function(...) {
    calls <<- calls + 1L; original_predict(...)
  })
  p <- predict_allocation(baseline, f$travel[, c(3, 1, 2)], demand = f$demand,
                          outputs = c("rho", "A", "abar", "flow"))
  expect_equal(calls, 1L)
  expect_s3_class(p, "ae_clm_prediction")
  expect_identical(baseline, source_copy)
  # A multistart fit predicts from its selected fit at the fitted parameters.
  q <- predict_allocation(fit, f$travel, demand = f$demand, outputs = c("rho", "A", "flow"))
  theta <- stats::coef(fit)
  expected <- alloc_request_oracle(f$travel, f$travel[f$active, ], f$demand[f$active],
    f$supply, theta[["sigma"]], v0 = theta[["v0"]], demand = f$demand)
  expect_equal(lapply(q$outputs, unname), lapply(expected[c("rho", "A", "flow")], unname),
               tolerance = 1e-10)
  # Raster conversion is explicit and permits one vector output.
  expect_error(predict_allocation(baseline, f$travel, outputs = "flow", type = "raster"),
               "exactly one requested vector")
  expect_error(predict_allocation(baseline, f$travel, type = "raster"),
               "exactly one requested vector")
  template <- terra::rast(nrows = 2, ncols = 4)
  cells <- stats::setNames(c(7, 2, 8, 1, 5), names(f$demand))
  spatial <- list(template = template, cell_index = cells)
  p <- predict_allocation(baseline, f$travel, outputs = "rho", spatial = spatial)
  map <- predict_allocation(baseline, f$travel, outputs = "rho", spatial = spatial, type = "raster")
  expect_equal(terra::values(map)[cells, 1], unname(p$outputs$rho), ignore_attr = TRUE)
  expect_true(all(is.na(terra::values(map)[-cells, 1])))
})
