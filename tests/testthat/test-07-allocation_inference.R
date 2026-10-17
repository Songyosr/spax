# Private SPAX-047 observation and first-order uncertainty diagnostic
# (R/07-allocation_inference.R), kept internal (DEC-027). It goes with that
# file if it is ever retired.

.observation <- function(f, y, output = "utilization", sampling = "independent_poisson",
                         groups = NULL, model = f$model) {
  .allocation_observation(model, y, output, event = "events", exposure_units = "opportunities",
    period = "year", exposure = "poisson_mean", sampling = sampling, groups = groups)
}

# Whole-count expectations of a modified generated system at the truth.
.rounded_counts <- function(f, distance = f$distance, supply = f$supply, output = "utilization") {
  oracle <- clm_oracle(distance, f$demand, supply, f$truth[["sigma"]], f$truth[["v0"]])
  if (output == "flow") return(round(oracle$flow))
  stats::setNames(round(oracle$utilization), names(supply))
}

test_that("inference aligns masked counts and dependence declarations by IDs", {
  f <- alloc_generated_case()
  y <- f$flow; y[2, 3] <- NA; y[, 24] <- NA; y[1, 1] <- 0
  fit <- alloc_count_fit(f, y, "flow")
  groups <- stats::setNames(colnames(y), colnames(y))
  a <- .observation(f, y, "flow", "independent_groups", groups)
  b <- .observation(f, y[nrow(y):1, ncol(y):1], "flow", "independent_groups", rev(groups))
  ia <- .allocation_inference(f$model, fit, a, group_adjustment = "none")
  ib <- .allocation_inference(f$model, fit, b, group_adjustment = "none")
  expect_identical(ia$covariance_log, ib$covariance_log)
  expect_identical(ia$fitted_mean, ib$fitted_mean)
  tampered <- a; tampered$target$values <- rev(tampered$target$values)
  expect_error(.allocation_inference(f$model, fit, tampered), "contract conflicts")
  tampered <- a; tampered$target$mask <- rev(tampered$target$mask)
  expect_error(.allocation_inference(f$model, fit, tampered), "contract conflicts")
  # Uncorrected cluster-robust covariance from the independent score algebra.
  expect_equal(ia$diagnostics$independent_groups, 23L)
  expect_equal(sum(ia$diagnostics$group_leverage), ia$diagnostics$rank, tolerance = 1e-8)
  J <- ia$jacobian_log; mu <- ia$fitted_mean; obs <- a$target$values
  labels <- a$labels[a$target$mask]
  score <- J * ((obs - mu) / mu)
  meat <- crossprod(rowsum(score, labels, reorder = FALSE))
  B <- crossprod(J / sqrt(mu))
  V <- solve(B) %*% meat %*% solve(B) * 23 / 22
  expect_equal(unname(ia$covariance_log), unname(V), tolerance = 1e-7)
  # Block HC3 from leave-group leverage, written out directly.
  hc3 <- .allocation_inference(f$model, fit, a)
  W <- J / sqrt(mu); residual <- (obs - mu) / sqrt(mu)
  adjusted <- t(vapply(unique(labels), function(label) {
    sel <- labels == label; block <- W[sel, , drop = FALSE]
    as.numeric(crossprod(block, solve(diag(sum(sel)) - block %*% solve(B) %*% t(block),
                                      residual[sel])))
  }, numeric(ncol(W))))
  independent <- solve(B) %*% crossprod(adjusted) %*% solve(B)
  expect_equal(unname(hc3$covariance_log), unname(independent), tolerance = 1e-7)
  expect_identical(hc3$diagnostics$group_adjustment, "HC3")
  expect_true(hc3$valid_regular_interval)
  expect_lt(max(hc3$diagnostics$group_max_leverage), 1)
  expect_false(ia$valid_regular_interval)
  expect_true("uncorrected_group_comparator" %in% ia$reasons)
  # Perturbation solutions keep only portable values, which still predict.
  expect_null(ia$base$outputs$allocation)
  expect_null(ia$perturbations[[1]]$plus$outputs$flow)
  expect_equal(predict_allocation(ia$base, f$distance, outputs = "rho")$outputs$rho,
               predict_allocation(fit, f$distance, outputs = "rho")$outputs$rho)
  expect_error(.observation(f, f$y + .2), "whole counts")
  expect_error(.observation(f, f$y, sampling = "independent_groups", groups = unname(groups)),
               "facility ID")
  changed <- f$model; changed$substrate$D_active[1] <- changed$substrate$D_active[1] + 1
  expect_error(.allocation_inference(changed, fit, a), "contract conflicts")
  changed <- f$model
  changed$substrate$distance_active[1, 1] <- changed$substrate$distance_active[1, 1] + 1
  expect_error(.allocation_inference(changed, fit, a), "conflicts")
  modified <- f$model; modified$substrate$input_metadata$travel_units <- "seconds"
  declared <- .observation(f, y, "flow", "independent_groups", groups, model = modified)
  expect_error(.allocation_inference(modified, fit, declared), "source conflicts")
})

test_that("inference accepts a fit read back from disk as it accepts the original", {
  f <- alloc_generated_case()
  fit <- alloc_generated_fit()
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(fit, path)
  reloaded <- readRDS(path)
  observation <- .observation(f, f$y)
  expect_identical(.allocation_inference(f$model, reloaded, observation),
                   .allocation_inference(f$model, fit, observation))
})

test_that("source agreement includes supply invisible to calibration support", {
  f <- alloc_generated_case(); d <- f$distance; d[, 24] <- Inf
  model <- clm_allocation(prepare_allocation(f$demand, f$supply, d), fit_v0 = TRUE)
  y <- .rounded_counts(f, distance = d)
  fit <- alloc_count_fit(f, y, model = model)
  S1 <- f$supply; S1[24] <- S1[24] * 2
  changed <- clm_allocation(prepare_allocation(f$demand, S1, d), fit_v0 = TRUE)
  expect_error(.allocation_inference(changed, fit, .observation(f, y, model = changed)),
               "source conflicts")
})

test_that("underflow is not structural absence and a live floor suppresses intervals", {
  f <- alloc_generated_case(); d <- f$distance; d[1, ] <- 1e6
  model <- clm_allocation(prepare_allocation(f$demand, f$supply, d), fit_v0 = TRUE)
  y <- .rounded_counts(f, distance = d, output = "flow")
  fit <- alloc_count_fit(f, y, "flow", model = model)
  info <- .allocation_inference(model, fit, .observation(f, y, "flow", model = model))
  expect_true("nonregular_mean_or_loss_floor" %in% info$reasons)
  expect_false(info$valid_regular_interval)
  fit <- alloc_generated_fit()
  fit$best$loss_args <- list(eps = 1e9)
  fit$best$loss <- .poisson_loss(fit$best$predicted, f$y, eps = 1e9)
  info <- .allocation_inference(f$model, fit, .observation(f, f$y))
  expect_true("nonregular_mean_or_loss_floor" %in% info$reasons)
})

test_that("co-location preserves aggregate support while distinguishing totals and flows", {
  f <- alloc_generated_case(colocated = TRUE)
  info <- .allocation_inference(f$model, alloc_generated_fit(colocated = TRUE),
                                .observation(f, f$y))
  callback <- function(theta, z) {
    c(sigma = unname(theta[1]), v0 = unname(theta[2]),
      rho = sum(z$utilization) / sum(f$demand), A = sum(f$supply) / sum(f$demand))
  }
  transforms <- c("log", "log", "logit", "identity")
  fixed <- c(FALSE, FALSE, FALSE, TRUE)
  q <- .allocation_estimands(info, callback, transforms, fixed)
  expect_equal(info$diagnostics$rank, 1)
  expect_identical(q$table$status[1:2], rep("unsupported_local_direction", 2))
  expect_lt(q$table$null_fraction[3], 1e-4)
  expect_identical(q$table$status[4], "fixed_input_identity")
  groups <- stats::setNames(names(f$supply), names(f$supply))
  grouped <- .allocation_inference(f$model, alloc_generated_fit(colocated = TRUE),
    .observation(f, f$y, sampling = "independent_groups", groups = groups))
  expect_equal(grouped$diagnostics$rank, 1)
  expect_true(all(is.finite(grouped$covariance_log)))
  expect_false(grouped$valid_regular_interval)
  expect_true("grouped_totals_not_validated" %in% grouped$reasons)
  expect_identical(.allocation_estimands(grouped, callback, transforms, fixed)$table$status[1:2],
                   rep("unsupported_local_direction", 2))
  # Origin-facility flows identify both directions.
  flow_fit <- alloc_generated_fit(colocated = TRUE, output = "flow")
  flow_info <- .allocation_inference(f$model, flow_fit, .observation(f, f$flow, "flow"))
  expect_equal(flow_info$diagnostics$rank, 2)
  expect_error(.allocation_estimands(info, function(theta, z) c(a = 1, a = 2)), "unique named")
  wrong <- q$gradient_log[4:1, , drop = FALSE]
  expect_error(.allocation_delta(info, stats::setNames(q$table$estimate, q$table$quantity), wrong),
               "quantity and parameter order")
})

test_that("a dominant group cannot produce regular HC3 intervals", {
  f <- alloc_generated_case(); S <- f$supply; S[6:24] <- 1e-9
  model <- clm_allocation(prepare_allocation(f$demand, S, f$distance), fit_v0 = TRUE)
  y <- .rounded_counts(f, supply = S)
  fit <- alloc_count_fit(f, y, model = model)
  groups <- stats::setNames(c(rep("dominant", 5), paste0("g", 6:24)), names(S))
  info <- .allocation_inference(model, fit,
    .observation(f, y, sampling = "independent_groups", groups = groups, model = model))
  expect_equal(info$diagnostics$independent_groups, 20)
  expect_true("dominant_group_leverage" %in% info$reasons)
  expect_false(info$valid_regular_interval)
  expect_true(any(info$diagnostics$group_max_leverage >= 1 - 1e-8))
})

test_that("paired scenario inference uses the difference gradient and one covariance", {
  f <- alloc_generated_case(); fit <- alloc_generated_fit()
  info <- .allocation_inference(f$model, fit, .observation(f, f$y))
  S1 <- f$supply; S1[2] <- S1[2] * 1.2
  changed <- clm_allocation(prepare_allocation(f$demand, S1, f$distance), fit_v0 = TRUE)
  callback <- function(theta, z) {
    baseline <- sum(z$utilization)
    scenario <- sum(evaluate_allocation(changed, theta)$utilization)
    c(baseline = baseline, scenario = scenario, difference = scenario - baseline)
  }
  q <- .allocation_estimands(info, callback)
  expect_equal(q$gradient_log[3, ], q$gradient_log[2, ] - q$gradient_log[1, ], tolerance = 1e-8)
  g <- q$gradient_log[3, ]
  expect_equal(q$table$standard_error[3]^2, as.numeric(crossprod(g, info$covariance_log %*% g)),
               tolerance = 1e-8)
  groups <- stats::setNames(rep(paste0("g", 1:8), each = 3), names(f$supply))
  few <- .allocation_inference(f$model, fit,
    .observation(f, f$y, sampling = "independent_groups", groups = groups))
  expect_false(few$valid_regular_interval)
  expect_equal(few$diagnostics$independent_groups, 8)
  few_q <- .allocation_estimands(few, callback)
  expect_true(all(grepl("few_independent_groups", few_q$table$status)))
  one <- stats::setNames(rep("one", length(f$supply)), names(f$supply))
  one_info <- .allocation_inference(f$model, fit,
    .observation(f, f$y, sampling = "independent_groups", groups = one))
  expect_no_warning(one_q <- .allocation_estimands(one_info, callback))
  expect_true(all(grepl("few_independent_groups", one_q$table$status)))
  bounded <- fit; bounded$boundary[, ] <- TRUE
  expect_true("parameter_boundary" %in%
    .allocation_inference(f$model, bounded, .observation(f, f$y))$reasons)
  floored <- fit; floored$best$loss_args <- list(eps = 1e9)
  expect_error(.allocation_inference(f$model, floored, .observation(f, f$y)), "loss conflict")
  unnamed <- fit; unnamed$best$loss_args <- list(1e9)
  expect_error(.allocation_inference(f$model, unnamed, .observation(f, f$y)),
               "unweighted Poisson fit")
})

test_that("grouped total covariances remain diagnostics without regular intervals", {
  f <- alloc_generated_case(); fit <- alloc_generated_fit()
  callback <- function(theta, z) {
    c(sigma = unname(theta[1]), rho = sum(z$utilization) / sum(f$demand))
  }
  groups <- stats::setNames(names(f$supply), names(f$supply))
  observation <- .observation(f, f$y, sampling = "independent_groups", groups = groups)
  for (adjustment in c("HC3", "none")) {
    info <- .allocation_inference(f$model, fit, observation, group_adjustment = adjustment)
    expect_equal(info$diagnostics$rank, 2)
    expect_true(all(is.finite(info$covariance_log)))
    expect_false(info$valid_regular_interval)
    # Saved objects with stale validity flags are rechecked against the scope.
    stale <- info; stale$valid_regular_interval <- TRUE; stale$reasons <- character()
    for (x in list(info, unserialize(serialize(stale, NULL)))) {
      q <- .allocation_estimands(x, callback, c("log", "logit"))$table
      expect_true(all(grepl("grouped_totals_not_validated", q$status)))
      if (adjustment == "none") expect_true(all(grepl("uncorrected_group_comparator", q$status)))
      expect_true(all(is.na(q$standard_error) & is.na(q$lower) & is.na(q$upper)))
    }
  }
})
