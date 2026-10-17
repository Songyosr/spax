# Saved allocation objects from an earlier version of the package: results saved
# with saveRDS() before declared decay kernels, the general bootstrap and the
# no-decay comparator existed must still read, print and predict, and match the
# models of the current code.
#
# The fixture tests/testthat/fixtures/spax_f819102_allocation.rds was written by
# the package at commit f819102 (version 0.2.0.9000, SPAX-051 merged; the base of
# the decay-kernel branch) with R 4.5.2, loaded from source with
# pkgload::load_all() in a separate R process. It is one list:
#   fit        the Poisson multistart fit of the small allocation case,
#              fit_allocation(model, rev(observed), starts, lower, upper,
#                loss = "poisson", gradient = TRUE, control = list(maxit = 100)),
#   evaluation the fixed evaluation at the generating parameters,
#              evaluate_allocation(model, truth, coverage_args = list(norm = 3),
#                requested_outputs = "flow"),
#   provenance the commit, package version, R version and the calls above.
# The case is alloc_small_case() of helper-allocation.R (helper-clm.R builds its
# counts), whose definition was the same at that commit. Source references were
# removed from the saved functions, as an installed package has none. Do not
# regenerate the fixture with the current code: its purpose is to be old.

saved_objects <- function() {
  readRDS(testthat::test_path("fixtures", "spax_f819102_allocation.rds"))
}

# The current code's fit of the same inputs, with the settings that made the
# fixture (alloc_small_fit() is the call recorded in the fixture's provenance).
fresh_fit <- function() alloc_small_fit()

test_that("the fixture of the earlier version reads and has its documented content", {
  x <- saved_objects()
  expect_identical(names(x), c("fit", "evaluation", "provenance"))
  expect_s3_class(x$fit, "ae_multistart_fit")
  expect_s3_class(x$fit$best, "ae_problem_nfxp_fit")
  expect_s3_class(x$evaluation, "ae_equilibrium")
  expect_s3_class(x$evaluation$coverage, "ae_coverage")
  expect_identical(x$provenance$commit, "f819102")
  expect_identical(x$provenance$package_version, "0.2.0.9000")
  expect_match(x$provenance$r_version, "^R version 4\\.")
  # It is a built-in Gaussian model, saved with the version 1 prediction snapshot.
  expect_identical(x$fit$best$family, "gaussian")
  expect_identical(x$fit$best$prediction_snapshot$schema, "spax_clm_prediction_v1")
  expect_identical(x$fit$best$prediction_snapshot$kind, "builtin_static_clm")
  expect_false("kernel" %in% names(x$fit$best$prediction_snapshot$spec))
  expect_identical(x$evaluation$prediction_snapshot$schema, "spax_clm_prediction_v1")
  expect_identical(x$fit$status, "selected")
  # It is small.
  expect_lt(file.size(testthat::test_path("fixtures", "spax_f819102_allocation.rds")), 20000)
})

test_that("saved fits and evaluations of the earlier version print and summarize", {
  x <- saved_objects()
  shown <- function(object) utils::capture.output(print(object))
  expect_true("family: gaussian (sigma = travel scale)" %in% shown(x$fit$best))
  expect_true("model: clm" %in% shown(x$fit$best))
  expect_true(any(grepl("^<summary.ae_multistart_fit>", shown(summary(x$fit)))))
  expect_true("family: gaussian (sigma = travel scale)" %in% shown(summary(x$fit$best)))
  expect_true(all(c("<ae_multistart_fit>", "selected start: far") %in% shown(x$fit)))
  expect_output(print(x$evaluation), "<ae_equilibrium>")
  expect_output(print(x$evaluation$coverage), "<ae_coverage> huff/gaussian (sigma=3, v0=10)",
                fixed = TRUE)
  expect_s3_class(summary(x$evaluation$coverage), "summary.ae_coverage")
  expect_true(any(grepl("^Facility ratio", shown(summary(x$evaluation$coverage)))))
  # The accessors read the saved values.
  expect_equal(coef(x$fit), c(sigma = 3, v0 = 10), tolerance = 1e-3)
  expect_identical(names(fitted(x$fit)), names(x$fit$best$observed))
  expect_identical(as.data.frame(x$fit)$parameter, c("sigma", "v0"))
  expect_identical(as.data.frame(x$evaluation$coverage)$A, x$evaluation$coverage$regional$A)
})

test_that("a saved fit of the earlier version predicts like a fresh fit of the current code", {
  x <- saved_objects()
  f <- alloc_small_case()
  fresh <- fresh_fit()
  # The saved and the fresh fit are the same estimate of the same inputs.
  expect_equal(coef(x$fit), coef(fresh), tolerance = 1e-10)
  request <- f$travel[c("o3", "o1", "o5"), , drop = FALSE]
  rownames(request) <- c("n1", "n2", "n3")
  demand <- c(n1 = 2, n2 = 0, n3 = 3)
  outputs <- c("rho", "outside_share", "A", "abar", "A_supported", "abar_supported",
               "unsupported_contact", "allocation", "flow")
  saved <- predict_allocation(x$fit, request, demand = demand, outputs = outputs)
  current <- predict_allocation(fresh, request, demand = demand, outputs = outputs)
  for (name in outputs) {
    expect_equal(saved$outputs[[name]], current$outputs[[name]], tolerance = 1e-10, info = name)
  }
  expect_equal(saved$facilities, current$facilities, tolerance = 1e-10)
  expect_identical(saved$validity, current$validity)
  expect_identical(saved$units, current$units)
  expect_identical(saved$policy, current$policy)
  # Predictions are the choice algebra at the saved parameters, not only equal to
  # the current code's.
  theta <- coef(x$fit)
  loads <- clm_oracle(f$travel[f$active, , drop = FALSE], f$demand[f$active], f$supply,
                      theta[["sigma"]], theta[["v0"]])$utilization
  expected <- clm_oracle(request, demand, f$supply, theta[["sigma"]], theta[["v0"]])
  expect_equal(unname(saved$outputs$allocation), unname(expected$allocation), tolerance = 1e-10)
  expect_equal(unname(saved$outputs$flow), unname(expected$flow), tolerance = 1e-10)
  expect_equal(saved$facilities$utilization, loads, tolerance = 1e-10)
  # The saved evaluation predicts the same way, and so does the selected fit alone.
  from_evaluation <- predict_allocation(x$evaluation, request, outputs = c("rho", "A", "abar"))
  fresh_evaluation <- predict_allocation(
    evaluate_allocation(f$model, f$truth, requested_outputs = "flow"), request,
    outputs = c("rho", "A", "abar"))
  expect_equal(from_evaluation$outputs, fresh_evaluation$outputs, tolerance = 1e-10)
  expect_identical(predict_allocation(x$fit$best, request, outputs = "rho")$outputs,
                   predict_allocation(x$fit, request, outputs = "rho")$outputs)
})

test_that("a saved evaluation of the earlier version equals a fresh evaluation", {
  x <- saved_objects()
  f <- alloc_small_case()
  fresh <- evaluate_allocation(f$model, f$truth, coverage_args = list(norm = 3),
                               requested_outputs = "flow")
  expect_equal(x$evaluation$outputs, fresh$outputs, tolerance = 1e-12)
  expect_equal(x$evaluation$prediction_snapshot, fresh$prediction_snapshot, tolerance = 1e-12)
  expect_equal(x$evaluation$coverage$regional, fresh$coverage$regional, tolerance = 1e-12)
  expect_equal(x$evaluation$coverage$cells, fresh$coverage$cells, tolerance = 1e-12)
})

test_that("a saved fit of the earlier version can be checked against the current model", {
  x <- saved_objects()
  f <- alloc_small_case()
  # The model is declared again by the current code; it matches the saved fit.
  out <- check_allocation(f$model, x$fit)
  expect_s3_class(out, "ae_allocation_check")
  expect_identical(out$family, "gaussian")
  expect_identical(names(out$parameters$fitted), c("sigma", "v0"))
  expect_identical(names(out$parameters$no_decay), "v0")
  expect_output(print(out), "ae_allocation_check")
  # The fixture's counts were built with exp() on the platform that wrote it, and
  # exp() can differ in the last bit between platforms, so the fresh fit uses the
  # saved fit's own counts.
  refit <- fit_allocation(f$model, x$fit$best$observed, f$starts, f$lower, f$upper,
                          loss = "poisson", gradient = TRUE, control = list(maxit = 100))
  # It gives the report of the fresh fit. The no-decay comparator re-optimizes v0
  # on a flat loss, whose optimum is set only to about 1e-8 relative, and floating
  # point differs between platforms: hence the tolerance.
  fresh <- check_allocation(f$model, refit)
  expect_equal(as.data.frame(out)[, setdiff(names(as.data.frame(out)), "parameters")],
               as.data.frame(fresh)[, setdiff(names(as.data.frame(fresh)), "parameters")],
               tolerance = 1e-6)
  # Like-for-like needs identical observed counts, which the refit has.
  compared <- compare_allocations(list(saved = x$fit, fresh = refit),
                                  models = list(saved = f$model, fresh = f$model))
  table <- as.data.frame(compared)
  expect_identical(table$family, c("gaussian", "gaussian"))
  expect_true(all(table$comparable))
  expect_equal(table$loss_difference, c(0, 0), tolerance = 1e-8)
})
