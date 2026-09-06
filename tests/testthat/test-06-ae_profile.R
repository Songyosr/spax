.profile_fixture <- function() {
  d <- terra::rast(nrows = 1, ncols = 4)
  terra::values(d) <- c(10, 20, 30, 40)
  distance <- c(d, d)
  names(distance) <- c("a", "b")
  terra::values(distance) <- cbind(c(1, 2, 4, 5), c(5, 4, 2, 1))
  p <- .huff_problem(d, c(a = 30, b = 45), distance, fit_v0 = TRUE)
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

test_that("profile preserves matrix masks and reconstructs named theta", {
  f <- .profile_fixture()
  observed <- .solve_problem(f$problem, c(sigma = 2, v0 = 5))$outputs$allocation
  observed[2, 1] <- NA
  profile <- .profile_problem(f$problem, observed, "sigma", c(a = 1.8, b = 2),
    init = c(v0 = 4, sigma = 2), lower = c(v0 = .1, sigma = .5),
    upper = c(v0 = 20, sigma = 4), output = "allocation",
    gradient = TRUE, control = list(maxit = 100))
  expect_identical(profile$nuisance, "v0")
  expect_identical(colnames(profile$theta), c("sigma", "v0"))
  expect_null(profile$fits)
  expect_identical(profile$best$target_mask, as.numeric(!is.na(observed)) == 1)
  expect_equal(profile$best$predicted[!is.na(observed)], observed[!is.na(observed)],
               tolerance = 1e-4)
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
  expect_error(do.call(.profile_problem, modifyList(args, list(parameter = "bad"))), "parameter")
  expect_error(do.call(.profile_problem, modifyList(args, list(values = c(1, 1)))), "distinct")
  expect_error(do.call(.profile_problem, modifyList(args, list(values = 5))), "inside")
  expect_error(do.call(.profile_problem, modifyList(args, list(init = c(sigma = 2)))), "missing")
  expect_error(do.call(.profile_problem, c(args, list(warm_start = NA))), "TRUE or FALSE")
  expect_error(do.call(.profile_problem, c(args, list(bad_control = 1))), "controls")
})
