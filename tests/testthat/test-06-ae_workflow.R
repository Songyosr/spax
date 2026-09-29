.workflow_fixture <- function() {
  demand <- c(o1 = 40, o2 = 0, o3 = 55, o4 = NA, o5 = 35)
  supply <- c(f2 = 18, f1 = 25, f3 = 15)
  travel <- matrix(c(1, 5, 3, 4, 6, 6, 2, 2, 3, 4, 3, 4, 6, 1, 2), 5, 3,
                    dimnames = list(names(demand), names(supply)))
  inputs <- prepare_allocation(demand, supply, travel, missing_demand = "exclude",
    metadata = list(demand_units = "requests", demand_period = "period1",
      supply_units = "slots", supply_period = "period1", travel_units = "minutes"))
  model <- clm_allocation(inputs, fit_v0 = TRUE)
  truth <- c(sigma = 3, v0 = 10)
  active <- which(is.finite(demand) & demand > 0)
  w <- sweep(exp(-travel[active, , drop = FALSE]^2 / (2 * truth[["sigma"]]^2)),
             2, supply, "*")
  probability <- w / (rowSums(w) + truth[["v0"]])
  flow <- probability * demand[active]
  list(inputs = inputs, model = model, demand = demand, supply = supply, travel = travel,
       truth = truth, active = active, probability = probability, flow = flow,
       observed = colSums(flow),
       starts = rbind(near = c(sigma = 3.2, v0 = 9), far = c(sigma = 5, v0 = 20)),
       lower = c(sigma = 1, v0 = 1), upper = c(sigma = 8, v0 = 40))
}

test_that("workflow model declaration retains only the corrected prepared CLM", {
  f <- .workflow_fixture()
  expect_identical(f$model$substrate, f$inputs)
  expect_identical(f$model$metadata$spec$allocation, "clm")
  expect_identical(f$model$theta$names, c("sigma", "v0"))
  expect_error(clm_allocation(f$demand), "ae_prepared_inputs")
  expect_error(evaluate_allocation(.sae_problem(f$inputs), c(sigma = 3)), "corrected static CLM")
  wrong <- .huff_problem(f$inputs, allocation = "huff_decay")
  expect_error(fit_allocation(wrong, f$observed, f$starts[, "sigma", drop = FALSE],
    f$lower["sigma"], f$upper["sigma"]), "corrected static CLM")
})

test_that("workflow preserves explicit loss choice, selection diagnostics and canonical targets", {
  f <- .workflow_fixture()
  fit <- fit_allocation(f$model, rev(f$observed), f$starts, f$lower, f$upper,
    loss = "poisson", gradient = TRUE, control = list(maxit = 100))
  expect_s3_class(fit, "ae_multistart_fit")
  expect_identical(fit$status, "selected")
  expect_equal(stats::coef(fit), f$truth, tolerance = 1e-3)
  expect_equal(stats::fitted(fit), f$observed, tolerance = 1e-3)
  report <- summary(fit)
  expect_identical(report$starts, fit$table)
  expect_identical(report$boundary, fit$boundary)
  expect_identical(report$warnings, fit$warnings)
  expect_identical(report$errors, fit$errors)
  expect_identical(report$disagreement$loss, fit$loss_disagreement)
  sse <- fit_allocation(f$model, f$observed, f$starts, f$lower, f$upper,
                         control = list(maxit = 100))
  predicted <- stats::fitted(sse)
  expect_equal(sse$best$loss, sum((predicted - f$observed)^2 / (f$observed + 1)),
               tolerance = 1e-10)
  expect_false(sse$best$gradient)
  testthat::local_mocked_bindings(
    .solve_problem = function(...) stop("accessor attempted a solve"),
    .fit_problem_nfxp = function(...) stop("accessor attempted a fit"))
  expect_identical(stats::coef(fit), fit$best$theta)
  expect_identical(stats::fitted(fit), fit$best$predicted)
  expect_identical(summary(fit)$selected_start, fit$best_start)
})

test_that("custom workflow callbacks retain canonical masked order and auxiliary arguments", {
  f <- .workflow_fixture()
  y <- f$flow; y[1, 2] <- NA_real_
  mask <- !is.na(as.numeric(y)); expected <- as.numeric(y)[mask]
  weights <- seq_along(expected); seen <- list(); gradients <- 0L
  loss <- function(predicted, observed, weights, offset) {
    seen[[length(seen) + 1L]] <<- list(observed = observed, weights = weights, offset = offset)
    sum(weights * (predicted - observed)^2) + offset
  }
  grad <- function(predicted, observed, sensitivity, weights, offset) {
    gradients <<- gradients + 1L
    as.numeric(crossprod(sensitivity, 2 * weights * (predicted - observed)))
  }
  fit <- fit_allocation(f$model, y[c(3, 1, 2), c(2, 3, 1), drop = FALSE],
    f$starts, f$lower, f$upper, output = "flow", loss = loss,
    loss_grad = grad, loss_args = list(weights = weights, offset = 7),
    gradient = TRUE, control = list(maxit = 100))
  expect_identical(fit$status, "selected")
  expect_true(length(seen) > 0)
  expect_true(gradients > 0)
  expect_true(all(vapply(seen, function(x) identical(x$observed, expected) &&
    identical(x$weights, weights) && identical(x$offset, 7), logical(1))))
  expect_equal(fit$best$loss,
    sum(weights * (as.numeric(fit$best$predicted)[mask] - expected)^2) + 7,
    tolerance = 1e-10)
})

test_that("public count targets reject negatives while other callback domains pass through", {
  f <- .workflow_fixture()
  calls <- 0L; received <- NULL
  testthat::local_mocked_bindings(.fit_problem_multistart = function(...) {
    calls <<- calls + 1L; received <<- list(...); "passed"
  })
  utilization <- f$observed; utilization[2] <- -0.5
  expect_error(fit_allocation(f$model, utilization, f$starts, f$lower, f$upper),
                "expected counts must be nonnegative.*utilization")
  flow <- f$flow; flow[1, 1] <- NA_real_; flow[2, 3] <- -0.5
  expect_error(fit_allocation(f$model, flow, f$starts, f$lower, f$upper, output = "flow"),
                "expected counts must be nonnegative.*flow")
  expect_equal(calls, 0L)
  flow[2, 3] <- 0.5
  expect_identical(fit_allocation(f$model, flow, f$starts, f$lower, f$upper,
                                 output = "flow"), "passed")
  expect_identical(received[[2]], flow)
  probability <- f$probability; probability[1, 1] <- -0.5
  custom <- function(predicted, observed) sum((predicted - observed)^2)
  expect_identical(fit_allocation(f$model, probability, f$starts, f$lower, f$upper,
    output = "allocation", loss = custom), "passed")
  expect_identical(received[[2]], probability)
  expect_identical(received$loss, custom)
  expect_equal(calls, 2L)
})

test_that("fixed evaluation solves once and coverage reads the evaluated allocation", {
  f <- .workflow_fixture()
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
  expect_error(evaluate_allocation(f$model, f$truth, coverage_args = list(unknown = 1)),
                "coverage controls")
})

test_that("workflow predictions retain validity and fixed-system scenario identity", {
  f <- .workflow_fixture()
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
  expect_identical(p$validity$demand_status, c("positive", "zero", "positive", "unknown", "positive"))
  expect_true(all(p$validity$probability_valid))
  expect_equal(unname(p$outputs$flow[2, ]), rep(0, 3))
  expect_true(all(is.na(p$outputs$flow[4, ])))
  expect_identical(baseline, source_copy)
  larger <- f$demand * 1e6
  predict_allocation(baseline, f$travel, demand = larger, outputs = "flow")
  expect_identical(baseline$outputs$utilization, source_copy$outputs$utilization)
  supply <- f$supply; supply["f1"] <- supply["f1"] * 1.2
  inputs <- prepare_allocation(f$demand, supply, f$travel, missing_demand = "exclude")
  scenario <- evaluate_allocation(clm_allocation(inputs, fit_v0 = TRUE), f$truth)
  expect_identical(scenario$prediction_snapshot$theta, baseline$prediction_snapshot$theta)
  expect_false(isTRUE(all.equal(scenario$outputs$utilization, baseline$outputs$utilization)))
  expect_identical(baseline, source_copy)
})

test_that("failed searches remain inspectable and raster conversion is explicit", {
  empty <- structure(list(status = "no_valid_fit", best = NULL, best_start = NULL,
    table = data.frame(start = "failed", eligible = FALSE), boundary = matrix(FALSE, 1, 1),
    errors = list(failed = "test failure"), warnings = list(failed = character()),
    loss_disagreement = NA, parameter_disagreement = NA, tolerances = list()),
    class = "ae_multistart_fit")
  expect_identical(summary(empty)$errors$failed, "test failure")
  expect_error(stats::coef(empty), "no eligible")
  expect_error(stats::fitted(empty), "no eligible")
  expect_error(predict_allocation(empty, matrix(1, 1, 1)), "no eligible")
  f <- .workflow_fixture()
  evaluated <- evaluate_allocation(f$model, f$truth)
  expect_error(predict_allocation(evaluated, f$travel, outputs = "flow", type = "raster"),
                "exactly one requested vector")
  expect_error(predict_allocation(evaluated, f$travel, type = "raster"),
                "exactly one requested vector")
  template <- terra::rast(nrows = 2, ncols = 4)
  cells <- stats::setNames(c(7, 2, 8, 1, 5), names(f$demand))
  spatial <- list(template = template, cell_index = cells)
  p <- predict_allocation(evaluated, f$travel, outputs = "rho", spatial = spatial)
  map <- predict_allocation(evaluated, f$travel, outputs = "rho", spatial = spatial, type = "raster")
  expect_equal(terra::values(map)[cells, 1], unname(p$outputs$rho), ignore_attr = TRUE)
  expect_true(all(is.na(terra::values(map)[-cells, 1])))
})
