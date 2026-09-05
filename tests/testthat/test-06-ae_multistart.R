.multistart_fixture <- function(model = "huff", flows = FALSE) {
  d <- terra::rast(nrows = 1, ncols = 4)
  terra::values(d) <- c(10, 0, 30, 40)
  distance <- c(d, d)
  names(distance) <- c("a", "b")
  terra::values(distance) <- cbind(c(1, 2, 4, 5), c(5, 4, 2, 1))
  ctor <- get(paste0(".", model, "_problem"))
  p <- ctor(d, c(a = 30, b = 45), distance)
  solved <- .solve_problem(p, c(sigma = 2), lambda = .7)
  y <- if (flows) solved$outputs$allocation else solved$utilization
  if (flows) y[2, 1] <- NA
  list(p = p, y = y, starts = matrix(c(1.5, 2.5), ncol = 1,
    dimnames = list(c("near", "far"), "sigma")))
}

test_that("multistart matches separate CLM and fixed-point fits with shared preparation", {
  fixtures <- lapply(c("huff", "haae"), .multistart_fixture)
  for (f in fixtures) {
    ref <- lapply(seq_len(2), function(i) .fit_problem_nfxp(f$p, f$y,
      f$starts[i, ], c(sigma = .5), c(sigma = 4), lambda = .7,
      gradient = TRUE, control = list(maxit = 100)))
    # A prepared problem must suffice throughout the entire search.
    testthat::local_mocked_bindings(.interaction_substrate = function(...) stop("reprepared"))
    a <- .fit_problem_multistart(f$p, f$y, f$starts, c(sigma = .5), c(sigma = 4),
      lambda = .7, gradient = TRUE, control = list(maxit = 100), keep_fits = TRUE)
    winner <- which.min(vapply(ref, `[[`, numeric(1), "loss"))
    expect_identical(a$best_start, rownames(f$starts)[winner])
    expect_equal(a$table$loss, vapply(ref, `[[`, numeric(1), "loss"))
    for (i in 1:2) expect_equal(a$fits[[i]]$outputs, ref[[i]]$outputs)
    expect_identical(a$best$outputs, ref[[winner]]$outputs)
    expect_true(all(a$table$eligible))
  }
})

test_that("matrix masks and private reporting survive selection", {
  f <- .multistart_fixture(flows = TRUE)
  a <- .fit_problem_multistart(f$p, f$y, f$starts, .5, 4,
    output = "allocation", gradient = TRUE, control = list(maxit = 100))
  expect_null(a$fits)
  expect_identical(a$best$target_mask, as.numeric(!is.na(f$y)) == 1)
  expect_equal(a$best$predicted[!is.na(f$y)], f$y[!is.na(f$y)], tolerance = 1e-4)
  expect_output(print(a), "eligible starts: 2 / 2")
  expect_identical(rownames(a$theta), rownames(f$starts))
})

test_that("failures and nonconvergence cannot win and boundary fits remain visible", {
  f <- .multistart_fixture()
  template <- .fit_problem_nfxp(f$p, f$y, c(sigma = 2), .5, 4)
  template$convergence <- 0L
  testthat::local_mocked_bindings(.fit_problem_nfxp = function(problem, observed, init,
                                                            lower, upper, ...) {
    if (init[1] == 1) stop("failed start")
    out <- template
    out$loss <- switch(as.character(init[1]), `2` = -20, `3` = -30, `4` = 1, 1)
    if (init[1] == 2) out$convergence <- 1L
    if (init[1] == 3) out$equilibrium$converged <- FALSE
    if (init[1] == 4) {
      out$theta <- c(sigma = exp(log(5)))
      warning("boundary diagnostic")
    }
    out
  })
  starts <- matrix(1:5, ncol = 1, dimnames = list(NULL, "sigma"))
  a <- .fit_problem_multistart(f$p, f$y, starts, .5, 5, keep_fits = TRUE)
  expect_identical(a$best_start, "start4") # exact loss tie chooses first
  expect_identical(a$table$eligible, c(FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_identical(a$table$status[1:3], c("error", "not_converged", "not_converged"))
  expect_identical(a$errors[[1]], "failed start")
  expect_identical(a$warnings[[4]], "boundary diagnostic")
  expect_true(a$table$boundary[4])
  expect_true(a$boundary[4, "sigma"])
  expect_false(a$loss_disagreement)
  expect_true(a$parameter_disagreement)
  expect_null(a$fits[[1]])
  expect_length(a$fits, 5)
  b <- .fit_problem_multistart(f$p, f$y, starts[1:3, , drop = FALSE], .5, 5)
  expect_identical(b$status, "no_valid_fit")
  expect_null(b$best)
  expect_null(b$best_start)
  expect_true(is.na(b$loss_disagreement))
  expect_output(print(b), "selected start: none")
})

test_that("disagreement thresholds are explicit and a single start is unassessed", {
  f <- .multistart_fixture()
  template <- .fit_problem_nfxp(f$p, f$y, c(sigma = 2), .5, 4)
  template$convergence <- 0L
  testthat::local_mocked_bindings(.fit_problem_nfxp = function(problem, observed, init,
                                                            lower, upper, ...) {
    out <- template
    out$loss <- init[1]
    out$theta <- init
    out
  })
  a <- .fit_problem_multistart(f$p, f$y, f$starts, .5, 4)
  expect_true(a$loss_disagreement)
  expect_true(a$parameter_disagreement)
  expect_equal(a$table$loss_gap, c(0, 1))
  b <- .fit_problem_multistart(f$p, f$y, f$starts, .5, 4, loss_tol = 2,
                              parameter_tol = 1)
  expect_false(b$loss_disagreement)
  expect_false(b$parameter_disagreement)
  a <- .fit_problem_multistart(f$p, f$y, f$starts[1, , drop = FALSE], .5, 4)
  expect_true(is.na(a$loss_disagreement))
  expect_true(is.na(a$parameter_disagreement))
})

test_that("invalid input is rejected before any fit", {
  f <- .multistart_fixture()
  testthat::local_mocked_bindings(.fit_problem_nfxp = function(...) stop("unexpected fit"))
  for (s in list(c(1, 2), matrix(c(1, NA), ncol = 1),
                 matrix(c(1, 1), ncol = 1, dimnames = list(NULL, "sigma")))) {
    expect_error(.fit_problem_multistart(f$p, f$y, s, .5, 4), "starts")
  }
  expect_error(.fit_problem_multistart(f$p, f$y, f$starts, 2, 4), "inside")
  expect_error(.fit_problem_multistart(f$p, f$y, f$starts, 4, .5), "lower")
  expect_error(.fit_problem_multistart(f$p, f$y, f$starts, .5, 4, loss_tol = NA), "tolerances")
  expect_error(.fit_problem_multistart(f$p, f$y, f$starts, .5, 4, init = 2), "controls")
  expect_error(.fit_problem_multistart(f$p, f$y, f$starts, .5, 4, keep_fits = NA), "keep_fits")
})

test_that("named parameter columns are aligned and starts share the same problem", {
  # Add v0 to the static provider by rebuilding once outside the search.
  d <- terra::rast(nrows = 1, ncols = 3)
  terra::values(d) <- c(10, 20, 30)
  dist <- c(d, d)
  terra::values(dist) <- cbind(c(1, 2, 3), c(3, 2, 1))
  names(dist) <- c("a", "b")
  p <- .huff_problem(d, c(a = 30, b = 20), dist, fit_v0 = TRUE)
  y <- .solve_problem(p, c(sigma = 2, v0 = 5))$utilization
  starts <- rbind(a = c(v0 = 4, sigma = 1.5), b = c(v0 = 6, sigma = 2.5))
  original <- .fit_problem_nfxp
  seen <- list()
  spy <- function(problem, observed, init, lower, upper,
                  gradient = FALSE, control = list(maxit = 25)) {
    expect_identical(problem, p)
    seen[[length(seen) + 1L]] <<- init
    original(problem, observed, init, lower, upper, gradient = gradient, control = control)
  }
  testthat::local_mocked_bindings(.fit_problem_nfxp = spy)
  a <- .fit_problem_multistart(p, y, starts, c(v0 = .1, sigma = .5),
    c(v0 = 20, sigma = 4), gradient = TRUE, control = list(maxit = 100))
  expect_identical(colnames(a$starts), c("sigma", "v0"))
  expect_identical(names(seen[[1]]), c("sigma", "v0"))
  expect_equal(unname(seen[[1]]), c(1.5, 4))
  expect_equal(a$best$predicted, y, ignore_attr = TRUE, tolerance = 1e-3)
})
