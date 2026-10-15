# SPAX-030: nested static-vs-feedback comparison. The ladder pairs the
# ratio-feedback model with its static null (feedback_scale fixed at 1).
# The legs have different theta contracts, so starts and lower bounds are
# per-problem lists; the shared upper bound names every parameter and each
# leg reads only its own.

.compare_ladder <- function() {
  ratio <- ae_ratio_problem()
  list(ratio = ratio, static = .fix_problem_parameter(ratio, "feedback_scale", 1))
}

# Distinct per-leg values, so a leg reading another leg's entry is visible.
.compare_starts <- function() {
  list(
    init = list(ratio = c(sigma = 1.5, v0 = 20, feedback_scale = 1.2),
                static = c(sigma = 1.8, v0 = 25)),
    lower = list(ratio = c(sigma = 0.5, v0 = 1, feedback_scale = 1),
                 static = c(sigma = 0.4, v0 = 2)),
    upper = c(sigma = 4, v0 = 100, feedback_scale = 3)
  )
}

.fit_compare_ladder <- function(problems, observed) {
  starts <- .compare_starts()
  .fit_problem_ladder(
    problems,
    observed = observed,
    init = starts$init,
    lower = starts$lower,
    upper = starts$upper,
    output = "flow",
    lambda = 0.7,
    tol = 1e-10,
    gradient = TRUE,
    control = list(maxit = 100)
  )
}

test_that("the ladder compares both legs on one masked target and detects feedback", {
  problems <- .compare_ladder()
  observed <- .solve_problem(problems$ratio, ae_ratio_truth(), lambda = 0.7,
                             tol = 1e-10)$outputs$flow
  observed[2, 1] <- NA  # one suppressed flow cell
  original <- .fit_problem_nfxp
  seen <- list()
  testthat::local_mocked_bindings(
    .fit_problem_nfxp = function(problem, observed, init, lower, upper, ...) {
      seen[[length(seen) + 1L]] <<- list(init = init, lower = lower, upper = upper)
      original(problem, observed, init, lower, upper, ...)
    })

  out <- .fit_compare_ladder(problems, observed)
  tab <- out$table
  # Each leg receives its own start and lower bound and the shared upper bound.
  starts <- .compare_starts()
  for (i in 1:2) {
    expect_identical(seen[[i]]$init, starts$init[[i]])
    expect_identical(seen[[i]]$lower, starts$lower[[i]])
    expect_identical(seen[[i]]$upper, starts$upper)
  }
  expect_named(out, c("fits", "table"))
  expect_named(out$fits, c("ratio", "static"))
  expect_s3_class(out$fits$ratio, "ratio_problem_nfxp_fit")
  expect_equal(tab$label, c("ratio", "static"))
  expect_equal(tab$model, c("ratio", "ratio"))
  expect_equal(tab$family, c("gaussian", "gaussian"))
  expect_equal(tab$allocation_form, c("clm", "clm"))
  expect_equal(tab$n_params, c(3L, 2L))
  expect_match(tab$theta, "^sigma=")
  expect_equal(tab$n_observed, rep(length(observed) - 1L, 2L))
  expect_true(all(tab$solver_converged))
  expect_equal(tab$optim_convergence, c(0L, 0L))
  keep <- !is.na(observed)
  static_predicted <- out$fits$static$predicted[keep]
  expect_equal(tab$rmse[2], sqrt(mean((static_predicted - observed[keep])^2)))
  expect_equal(tab$correlation[2], stats::cor(static_predicted, observed[keep]))

  # Feedback-generated data: the feedback leg recovers the truth and the
  # static null cannot match it.
  expect_lt(tab$loss[1], 1e-8)
  expect_gt(tab$loss[2], 0.1)
  expect_equal(out$fits$ratio$theta, ae_ratio_truth(), tolerance = 1e-3)
  # Only the feedback leg depends on the state (dT/dU = 0 under the null).
  expect_gt(tab$spectral_radius[1], 0)
  expect_equal(tab$spectral_radius[2], 0)
  # The table is a function of the stored fits alone.
  expect_equal(.compare_problem_fits(out$fits), tab)
})

test_that("static data give the feedback leg no gain over its null", {
  problems <- .compare_ladder()
  observed <- .solve_problem(problems$static, ae_ratio_truth()[1:2], lambda = 0.7,
                             tol = 1e-10)$outputs$flow

  # Nested legs cannot show the null strictly winning: the feedback estimate
  # is pushed onto its null bound and both legs fit equally well.
  expect_warning(out <- .fit_compare_ladder(problems, observed),
                 "feedback_scale reached the lower search bound")
  tab <- out$table
  expect_lt(tab$loss[2], 1e-8)
  expect_lt(tab$loss[1], 1e-8)
  expect_equal(out$fits$ratio$theta[["feedback_scale"]], 1, tolerance = 1e-3)
  expect_equal(out$fits$static$theta, ae_ratio_truth()[1:2], tolerance = 1e-3)
})

test_that("comparability guards reject fits that differ in what they target", {
  base <- ae_recovery_fit(ae_toy_problem())
  compare <- function(other) .compare_problem_fits(list(a = base, b = other))
  edited <- function(field, value) {
    other <- base
    other[[field]] <- value
    other
  }
  masked <- base$observed
  masked[1] <- NA

  expect_no_error(compare(base))
  expect_error(compare(edited("observed", base$observed * 2)),
               "different observed values")
  expect_error(compare(edited("observed", masked)), "different observation masks")
  expect_error(compare(edited("observed", matrix(base$observed, 1))),
               "different target shapes")
  expect_error(compare(edited("loss_args", list(eta = 10))), "different `loss_args`")
  expect_error(compare(edited("output", "allocation")), "different outputs")
  expect_error(compare(edited("loss_fn", function(predicted, observed) 0)),
               "different loss functions")

  # Canonical observation identities must agree, not just the values.
  other <- base
  other$target_alignment$ids$facility <- c("other1", "other2")
  expect_error(compare(other), "observation identities")
  other$target_alignment <- NULL
  expect_error(compare(other), "observation identities")
  legacy <- edited("target_alignment", NULL)
  expect_no_error(.compare_problem_fits(list(a = legacy, b = other)))
  # Providers without an allocation form report NA.
  formless <- .compare_problem_fits(list(a = base, b = edited("allocation_form", NULL)))
  expect_identical(formless$allocation_form, c("clm", NA_character_))

  expect_error(.compare_problem_fits(list(base)), "at least two")
  expect_error(.compare_problem_fits(list(a = base, a = base)), "unique labels")
  # Unnamed fits are labelled by model, which must then be unique.
  expect_error(.compare_problem_fits(list(base, base)), "unique labels")
})

test_that("ladders validate problems and per-problem start lists", {
  problems <- .compare_ladder()
  y <- c(1, 2)
  expect_error(.fit_problem_ladder(problems[1], y, 1, 0.5, 4), "at least two")
  # Unnamed problems are labelled by model; both legs are "ratio".
  expect_error(.fit_problem_ladder(unname(problems), y, 1, 0.5, 4), "unique labels")
  # The first leg's start is resolved before anything is fitted.
  expect_error(
    .fit_problem_ladder(problems, y, init = list(static = c(sigma = 1, v0 = 1)),
                        lower = 0.5, upper = 4),
    "covering problem `ratio`"
  )
})
