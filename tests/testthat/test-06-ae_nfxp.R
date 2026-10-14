# Nested fixed-point (NFXP) calibration through .fit_problem_nfxp(). Model
# recovery checks for SAE/HAAE live in their own files.

test_that("named multi-parameter theta is recovered with and without gradients", {
  problem <- ae_identity_problem(
    spec = list(output_axes = c(target = "facility", utilization = "facility"))
  )
  args <- list(
    problem = problem, observed = c(2, 3),
    init = c(alpha = 1, beta = 1.5),
    lower = c(alpha = 0.5, beta = 0.5), upper = c(alpha = 4, beta = 4),
    tol = 1e-10, max_iter = 20, loss_args = list(eta = 1),
    control = list(maxit = 80)
  )

  for (gradient in c(FALSE, TRUE)) {
    fit <- do.call(.fit_problem_nfxp, c(args, list(gradient = gradient)))
    expect_s3_class(fit, "ae_problem_nfxp_fit")
    expect_identical(fit$gradient, gradient)
    expect_equal(fit$convergence, 0)
    expect_lt(fit$loss, 1e-6)
    expect_equal(fit$theta, c(alpha = 2, beta = 3), tolerance = 1e-3)
  }
  expect_named(fit$predicted, c("facility1", "facility2"))
})

test_that("gradient fits fall back to finite differences when Jacobians fail", {
  problem <- ae_identity_problem(
    "sigma", step = list(jac_state = function(x) matrix(Inf, 1, 1))
  )

  fit <- .fit_problem_nfxp(
    problem = problem,
    observed = 2,
    init = c(sigma = 4),
    lower = c(sigma = 0.5),
    upper = c(sigma = 5),
    gradient = TRUE,
    control = list(maxit = 40)
  )

  expect_true(fit$gradient)
  expect_equal(fit$convergence, 0)
  expect_lt(fit$loss, 1e-6)
  expect_equal(fit$theta, c(sigma = 2), tolerance = 1e-3)
})

test_that("calibration arguments are validated before fitting", {
  problem <- ae_toy_problem()
  observed <- c(5, 10)
  fit <- function(...) .fit_problem_nfxp(problem, observed, ...)

  expect_error(fit(init = c(alpha = 1), lower = 0.5, upper = 4), "missing required")
  expect_error(fit(init = c(1, 2), lower = 0.5, upper = 4), "contract length")
  expect_error(fit(init = 1, lower = 0, upper = 4), "positive")
  expect_error(fit(init = 1, lower = 2, upper = 1), "less than")
  expect_error(fit(init = 5, lower = 0.5, upper = 4), "inside")
  expect_error(fit(init = 1, lower = 0.5, upper = 4, loss = NULL), "`loss`")
  expect_error(fit(init = 1, lower = 0.5, upper = 4, loss_args = 1), "loss_args")
  expect_error(
    fit(init = 1, lower = 0.5, upper = 4, loss_grad = NULL, gradient = TRUE),
    "loss_grad"
  )
  expect_error(
    fit(init = 1, lower = 0.5, upper = 4, output = "missing",
        control = list(maxit = 1)),
    "requested output"
  )
})

test_that("fits store inspection fields and support the QoL methods", {
  problem <- ae_toy_problem()
  fit <- ae_recovery_fit(problem)

  expect_named(fit$observed, c("a", "b"))
  expect_true(is.list(fit$outputs))
  expect_equal(fit$surface_meta$demand_kept_index,
               problem$substrate$demand_kept_index)

  table <- .ae_fit_facility_table(fit)
  expect_equal(table$facility_id, c("a", "b"))
  expect_equal(table$observed, unname(fit$observed))
  expect_equal(table$residual, table$predicted - table$observed)

  surface <- .fit_output_surface(fit, "access")
  expect_s4_class(surface, "SpatRaster")
  expect_equal(
    terra::values(surface)[problem$substrate$demand_kept_index, 1],
    fit$outputs$access
  )

  expect_output(print(fit), "sigma")
  expect_output(print(fit$equilibrium))
  expect_s3_class(summary(fit), "summary.ae_problem_nfxp_fit")
  expect_output(print(summary(fit)), "sigma")

  withr::local_pdf(NULL)
  expect_no_error(plot(fit, type = "fit"))
  expect_no_error(plot(fit, type = "convergence"))
  expect_no_error(plot(fit, type = "state"))
  stripped <- fit
  stripped$observed <- NULL
  stripped$equilibrium$history <- NULL
  expect_error(plot(stripped, type = "fit"), "observed")
  expect_error(plot(stripped, type = "convergence"), "history")
})

test_that("fits warn when a parameter ends on either search bound", {
  # The generating sigma = 2 lies below the first box and above the second.
  problem <- ae_ratio_problem(fix = c("v0", "feedback_scale"))
  expect_warning(
    ae_recovery_fit(problem, init = c(sigma = 4), lower = c(sigma = 3),
                    upper = c(sigma = 8)),
    "lower search bound"
  )
  expect_warning(
    ae_recovery_fit(problem, init = c(sigma = 1), lower = c(sigma = 0.5),
                    upper = c(sigma = 1.5)),
    "upper search bound"
  )
})

# Vector and matrix targets ---------------------------------------------------

test_that("J-state problems calibrate against masked I x J matrix targets", {
  problem <- ae_identity_problem(
    outputs = function(theta) list(allocation = outer(c(1, 2, 3), as.numeric(theta)))
  )
  theta0 <- c(alpha = 2, beta = 3)
  observed <- outer(c(1, 2, 3), as.numeric(theta0))
  # suppress two cells; both columns stay identified
  observed[2, 1] <- NA
  observed[1, 2] <- NA
  fit <- function(observed, output = "allocation", maxit = 50) {
    .fit_problem_nfxp(
      problem = problem, observed = observed,
      init = c(alpha = 1, beta = 1),
      lower = c(alpha = 0.1, beta = 0.1), upper = c(alpha = 8, beta = 8),
      output = output, control = list(maxit = maxit)
    )
  }
  matrix_fit <- fit(observed)

  # state stayed on J while the target was I x J
  expect_length(matrix_fit$state, 2L)
  expect_equal(dim(matrix_fit$predicted), c(3L, 2L))
  expect_equal(matrix_fit$target_dim, c(3L, 2L))
  expect_equal(matrix_fit$n_observed, 4L)
  # column-major flatten: cells (2,1) and (1,2) are flat positions 2 and 4
  expect_equal(matrix_fit$target_mask, c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE))
  expect_equal(unname(matrix_fit$theta_hat), unname(theta0), tolerance = 1e-3)
  expect_lt(matrix_fit$loss, 1e-8)
  # observed kept its matrix shape (with the NA cells)
  expect_equal(dim(matrix_fit$observed), c(3L, 2L))
  expect_true(is.na(matrix_fit$observed[2, 1]))

  expect_error(fit(t(observed), maxit = 2), "does not match `observed`")
  expect_error(fit(observed, output = "utilization", maxit = 2),
               "does not match `observed`")
})

test_that("Huff vector and matrix targets fit with masks and gradients", {
  problem <- ae_toy_problem(kappa = 1 / 3)
  solved <- .solve_problem(problem, theta = c(sigma = 2))$outputs
  fit <- function(observed, output, ...) {
    .fit_problem_nfxp(problem, observed, init = c(sigma = 1.2),
                      lower = c(sigma = 0.5), upper = c(sigma = 4),
                      output = output, ...)
  }

  full <- fit(solved$utilization, "utilization")
  expect_equal(full$n_observed, 2L)
  expect_named(full$predicted, c("a", "b"))
  expect_named(full$observed, c("a", "b"))

  masked <- solved$utilization
  masked[1] <- NA
  one <- fit(masked, "utilization")
  # A single observed CLM marginal need not identify sigma uniquely.
  # This checks the masked observation fit and stored target semantics.
  expect_lt(one$loss, 1e-8)
  expect_equal(unname(one$predicted[2]), solved$utilization[2], tolerance = 1e-5)
  expect_equal(one$n_observed, 1L)
  expect_equal(one$target_mask, c(FALSE, TRUE))
  expect_true(is.na(one$observed[["a"]]))

  flows <- solved$allocation
  flows[2, 1] <- NA
  bb <- fit(flows, "allocation", control = list(maxit = 25))
  gd <- fit(flows, "allocation", control = list(maxit = 25), gradient = TRUE)
  expect_equal(unname(bb$theta_hat["sigma"]), 2, tolerance = 1e-3)
  expect_equal(unname(gd$theta_hat["sigma"]), unname(bb$theta_hat["sigma"]),
               tolerance = 1e-3)
})

# .fit_problem_decay() is a single-parameter weighted-SSE wrapper kept
# pending a maintainer decision (test audit 2026-10-14); retiring it means
# deleting this block.

test_that(".fit_problem_decay equals the weighted-SSE NFXP fit", {
  problem <- ae_ratio_problem(fix = c("v0", "feedback_scale"))
  observed <- .solve_problem(problem, c(sigma = 2), lambda = 0.7,
                             tol = 1e-10)$utilization
  controls <- list(lambda = 0.7, tol = 1e-10, max_iter = 500,
                   control = list(maxit = 20))
  decay <- do.call(.fit_problem_decay, c(list(
    problem = problem, observed = observed, init = 1.5, lower = 0.5,
    upper = 4, eta = 2), controls))
  nfxp <- do.call(.fit_problem_nfxp, c(list(
    problem = problem, observed = observed, init = c(sigma = 1.5),
    lower = c(sigma = 0.5), upper = c(sigma = 4),
    loss_args = list(eta = 2)), controls))

  expect_s3_class(decay, "ae_problem_decay_fit")
  expect_s3_class(decay, "ae_problem_nfxp_fit")
  expect_equal(decay$theta_hat, unname(nfxp$theta[["sigma"]]))
  expect_equal(decay$theta_hat, 2, tolerance = 1e-3)
  expect_equal(decay$wsse, nfxp$loss)
  expect_lt(decay$wsse, 1e-6)
  expect_equal(decay$eta, 2)
  expect_equal(unname(decay$predicted), unname(observed), tolerance = 1e-4)
  expect_gt(decay$spectral_radius, 0)
  expect_lt(decay$spectral_radius, 1)
  expect_error(.fit_problem_decay(ae_ratio_problem(), observed, 1.5, 0.5, 4),
               "single-parameter")
})
