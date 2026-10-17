# Huff with a fitted outside option, observed at sigma = 2, v0 = 5.
.profile_fixture <- function() {
  p <- ae_toy_problem(shape = "1x4", fit_v0 = TRUE)
  y <- .solve_problem(p, c(sigma = 2, v0 = 5))$utilization
  list(problem = p, observed = y)
}

test_that("profile fits nuisance parameters on one prepared problem", {
  f <- .profile_fixture()
  values <- c(low = 1.5, truth = 2, high = 2.5)
  oracle <- vapply(values, function(sigma) {
    optimize(function(log_v0) {
      predicted <- .solve_problem(f$problem, c(sigma = sigma, v0 = exp(log_v0)),
                                  diagnostics = FALSE)$utilization
      .poisson_loss(predicted, f$observed)
    }, log(c(.1, 20)), tol = 1e-10)$objective
  }, numeric(1))
  testthat::local_mocked_bindings(.interaction_substrate = function(...) stop("reprepared"))
  profile <- .profile_problem(f$problem, f$observed, "sigma", values,
    init = c(sigma = 2, v0 = 4), lower = c(sigma = .5, v0 = .1),
    upper = c(sigma = 4, v0 = 20), loss = .poisson_loss,
    loss_grad = .poisson_gradient, loss_args = list(), gradient = TRUE,
    control = list(maxit = 100), keep_fits = TRUE)
  expect_true(all(profile$table$eligible))
  expect_equal(profile$table$loss, unname(oracle), tolerance = 1e-7)
  expect_identical(profile$best_point, "truth")
  expect_equal(profile$best$theta, c(sigma = 2, v0 = 5), tolerance = 1e-4)
  expect_equal(profile$best$predicted, f$observed, ignore_attr = TRUE,
               tolerance = 1e-5)
  expect_identical(names(profile$best$theta), f$problem$theta$names)
  expect_identical(profile$fits$truth$profile$parameter, "sigma")
})

test_that("profile reconstructs named theta in contract order", {
  f <- .profile_fixture()
  profile <- .profile_problem(f$problem, f$observed, "sigma", c(a = 1.8, b = 2),
    init = c(v0 = 4, sigma = 2), lower = c(v0 = .1, sigma = .5),
    upper = c(v0 = 20, sigma = 4), gradient = TRUE, control = list(maxit = 100))
  expect_identical(profile$nuisance, "v0")
  expect_identical(colnames(profile$theta), c("sigma", "v0"))
  expect_equal(unname(profile$theta[, "sigma"]), c(1.8, 2))
  expect_identical(names(profile$best$theta), c("sigma", "v0"))
  expect_null(profile$fits)
})

test_that("profile diagnostics retain failures and boundaries", {
  f <- .profile_fixture()
  template <- .fit_problem_nfxp(f$problem, f$observed, c(sigma = 2, v0 = 5),
                                c(sigma = .5, v0 = .1), c(sigma = 4, v0 = 20))
  template$convergence <- 0L
  template$equilibrium$converged <- TRUE
  testthat::local_mocked_bindings(.fit_problem_nfxp = function(problem, observed,
                                                            init, lower, upper, ...) {
    fixed <- problem$metadata$profile$value
    if (fixed == 1) stop("failed point")
    out <- template
    out$theta <- out$theta_hat <- c(v0 = if (fixed == 3) upper[["v0"]] else 5)
    out$loss <- if (fixed == 2) 1 else 2
    if (fixed == 3) warning("boundary point")
    out
  })
  profile <- .profile_problem(f$problem, f$observed, "sigma", c(1, 2, 3),
    init = c(sigma = 2, v0 = 5), lower = c(sigma = .5, v0 = .1),
    upper = c(sigma = 4, v0 = 20), keep_fits = TRUE)
  expect_identical(profile$table$status, c("error", "eligible", "eligible"))
  expect_identical(profile$errors[[1]], "failed point")
  expect_identical(profile$warnings[[3]], "boundary point")
  expect_true(profile$table$boundary[3])
  expect_true(profile$boundary[3, "v0"])
  expect_identical(profile$best_point, "point2")
  expect_null(profile$fits[[1]])
})

test_that("profile validates its complete theta and point contract", {
  f <- .profile_fixture()
  args <- list(problem = f$problem, observed = f$observed, parameter = "sigma",
               values = c(1, 2), init = c(sigma = 2, v0 = 5),
               lower = c(sigma = .5, v0 = .1), upper = c(sigma = 4, v0 = 20))
  bad <- function(...) do.call(.profile_problem, modifyList(args, list(...)))
  expect_error(bad(parameter = "bad"), "parameter")
  expect_error(bad(values = c(1, 1)), "distinct")
  expect_error(bad(values = 5), "inside")
  expect_error(bad(values = c(a = 1, a = 2)), "names")
  expect_error(bad(init = c(sigma = 2)), "missing")
  expect_error(bad(init = c(sigma = 5, v0 = 5)), "inside")
  expect_error(bad(lower = c(sigma = 0, v0 = .1)), "positive")
  expect_error(bad(warm_start = NA), "TRUE or FALSE")
  expect_error(bad(bad_control = 1), "controls")
  expect_error(bad(problem = ae_toy_problem(), init = 2, lower = .5, upper = 4),
               "nuisance")
})

test_that("fixed parameters keep the prepared problem and reduce Jacobian columns", {
  ratio <- ae_ratio_problem()
  expect_error(.fix_problem_parameter(ratio, "bad", 1), "parameter")
  expect_error(.fix_problem_parameter(ratio, "feedback_scale", 0.5), "bounds")
  expect_error(.fix_problem_parameter(ae_toy_problem(), "sigma", 2), "nuisance")

  fixed <- .fix_problem_parameter(ratio, "feedback_scale", 1.5)
  expect_identical(fixed$substrate, ratio$substrate)
  expect_identical(fixed$theta$names, c("sigma", "v0"))
  expect_identical(fixed$metadata$profile$value, 1.5)

  # A provider may return its parameter Jacobian inside a list.
  jac <- matrix(1:4, 2, 2)
  listed <- ae_identity_problem(step = list(jac_param = function(x) list(jac_param = jac)))
  step <- .bind_theta(.fix_problem_parameter(listed, "alpha", 2), c(beta = 3))
  expect_identical(step$jac_param(c(2, 3))$jac_param, jac[, 2, drop = FALSE])
  incomplete <- ae_identity_problem(step = list(jac_param = function(x) list()))
  step <- .bind_theta(.fix_problem_parameter(incomplete, "alpha", 2), c(beta = 3))
  expect_error(step$jac_param(c(2, 3)), "jac_param")
})
