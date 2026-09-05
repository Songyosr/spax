.reuse_problem <- function(model = "huff", changed = FALSE) {
  d <- terra::rast(nrows = 1, ncols = 4)
  terra::values(d) <- if (changed) c(0, 25, NA, 15) else c(10, 20, 30, 40)
  dist <- c(d, d)
  terra::values(dist) <- cbind(c(1, 2, 4, 5), c(5, 4, 2, 1))
  names(dist) <- if (changed) c("c", "d") else c("a", "b")
  supply <- stats::setNames(if (changed) c(25, 35) else c(30, 45), names(dist))
  ctor <- get(paste0(".", model, "_problem"))
  ctor(d, supply, dist)
}

# Drive real solves through explicit optimizer request sequences. Only optim is
# replaced; numerical providers, callbacks and final reporting remain exercised.
.run_reuse_requests <- function(p = .reuse_problem(), requests = c("f0", "g0"),
                                reuse = TRUE, gradient = TRUE, fail_at = integer(),
                                observed = c(8, 12), output = "utilization",
                                tol = 1e-8, eta = 1, bad_gradient = FALSE,
                                bad_loss_at = integer()) {
  solves <- list()
  sensitivity_states <- list()
  losses <- list()
  gradients <- list()
  answers <- list()
  original_solve <- .solve_problem
  original_sensitivity <- .problem_output_sensitivity
  testthat::local_mocked_bindings(
    .solve_problem = function(problem, theta, ...) {
      args <- list(...)
      fit <- original_solve(problem, theta, ...)
      index <- length(solves) + 1L
      if (index %in% fail_at) fit$converged <- FALSE
      solves[[index]] <<- list(theta = theta, args = args, fit = fit,
                               support = problem$substrate$demand_kept_index,
                               ids = problem$substrate$facility_ids)
      fit
    },
    .problem_output_sensitivity = function(problem, theta, x, ...) {
      sensitivity_states[[length(sensitivity_states) + 1L]] <<- x
      original_sensitivity(problem, theta, x, ...)
    })
  testthat::local_mocked_bindings(
    optim = function(par, fn, gr, ...) {
      for (request in requests) {
        point <- par + as.numeric(substring(request, 2))
        answers[[length(answers) + 1L]] <<-
          if (startsWith(request, "f")) fn(point) else gr(point)
      }
      list(par = par, convergence = 0L, message = NULL)
    }, .package = "stats")
  fit <- .fit_problem_nfxp(p, observed, init = c(sigma = 2),
    lower = c(sigma = .5), upper = c(sigma = 8), output = output,
    gradient = gradient, reuse = reuse, lambda = .7, tol = tol,
    loss_args = list(eta = eta),
    loss = function(predicted, observed, eta) {
      losses[[length(losses) + 1L]] <<- list(predicted, observed, eta)
      if (length(losses) %in% bad_loss_at) return(Inf)
      .weighted_sse_loss(predicted, observed, eta)
    },
    loss_grad = function(predicted, observed, sensitivity, eta) {
      gradients[[length(gradients) + 1L]] <<- list(predicted, observed, eta)
      if (bad_gradient) return(NA_real_)
      .weighted_sse_gradient(predicted, observed, sensitivity, eta)
    })
  list(fit = fit, solves = solves, losses = losses, gradients = gradients,
       answers = answers, sensitivity_states = sensitivity_states)
}

test_that("objective-gradient reuse is single use and preserves callbacks", {
  a <- .run_reuse_requests(requests = c("f0", "g0", "g0"))
  b <- .run_reuse_requests(requests = c("f0", "g0", "g0"), reuse = FALSE)
  expect_length(a$solves, 3)
  expect_length(b$solves, 4)
  expect_identical(a$fit$evaluation_reuse, list(enabled = TRUE, reused_solves = 1L))
  expect_equal(a$answers, b$answers, tolerance = 1e-10)
  expect_equal(a$fit$outputs, b$fit$outputs, tolerance = 1e-10)
  expect_equal(a$losses, b$losses, tolerance = 1e-10)
  expect_length(a$losses, 4)
  expect_length(a$gradients, 2)
  expect_false(a$solves[[1]]$args$keep_history)
  expect_false(a$solves[[1]]$args$diagnostics)
  expect_true(a$solves[[3]]$args$keep_history)
  expect_true(is.finite(a$fit$spectral_radius))
  expect_false(is.null(a$fit$equilibrium$history))
})

test_that("changed points, gradient-first requests and new objectives solve afresh", {
  for (requests in list(c("f0", "g0.1"), c("g0", "g0"),
                        c("f0", "f0.1", "g0"))) {
    a <- .run_reuse_requests(requests = requests)
    expect_length(a$solves, length(requests) + 1L)
    expect_identical(a$fit$evaluation_reuse$reused_solves, 0L)
  }
  a <- .run_reuse_requests(requests = c("f0", "f0", "g0"))
  expect_length(a$solves, 3)
  expect_identical(a$fit$evaluation_reuse$reused_solves, 1L)
})

test_that("failed solves invalidate an earlier accepted evaluation", {
  a <- .run_reuse_requests(requests = c("f0", "f0", "g0"), fail_at = 2L)
  expect_equal(a$answers[[2]], 1e12)
  expect_length(a$solves, 4)
  expect_identical(a$fit$evaluation_reuse$reused_solves, 0L)
  expect_true(a$fit$equilibrium$converged)
})

test_that("invalid losses are not retained and gradient loss callbacks still run", {
  a <- .run_reuse_requests(requests = c("f0", "f0", "g0"), bad_loss_at = 2L)
  expect_equal(a$answers[[2]], 1e12)
  expect_length(a$solves, 4)
  expect_identical(a$fit$evaluation_reuse$reused_solves, 0L)
  # The callback rejects the paired gradient evaluation. It must run again,
  # triggering finite differences rather than using the prior finite loss.
  a <- .run_reuse_requests(bad_loss_at = 2L)
  b <- .run_reuse_requests(bad_loss_at = 2L, reuse = FALSE)
  expect_equal(a$answers, b$answers, tolerance = 1e-10)
  expect_equal(a$losses, b$losses, tolerance = 1e-10)
  expect_length(a$solves, length(b$solves) - 1L)
  expect_length(a$gradients, 0)
})

test_that("finite-difference fallback still evaluates its own parameter points", {
  a <- .run_reuse_requests(bad_gradient = TRUE)
  b <- .run_reuse_requests(bad_gradient = TRUE, reuse = FALSE)
  expect_equal(a$answers, b$answers, tolerance = 1e-10)
  expect_length(a$solves, length(b$solves) - 1L)
  expect_equal(a$losses, b$losses, tolerance = 1e-10)
  expect_true(all(is.finite(a$answers[[2]])))
})

test_that("undeclared providers and finite-difference fits remain uncached", {
  p <- .reuse_problem()
  p$metadata$spec$deterministic <- NULL
  a <- .run_reuse_requests(p)
  expect_length(a$solves, 3)
  expect_false(a$fit$evaluation_reuse$enabled)
  a <- .run_reuse_requests(requests = c("f0", "f0"), gradient = FALSE)
  expect_length(a$solves, 3)
  expect_false(a$fit$evaluation_reuse$enabled)
  expect_error(.run_reuse_requests(reuse = NA), "`reuse`")
  expect_error(.run_reuse_requests(reuse = 1), "`reuse`")
})

test_that("separate fits isolate scenarios, support, IDs, masks and controls", {
  p <- .reuse_problem()
  q <- .reuse_problem(changed = TRUE)
  a <- .run_reuse_requests(p)
  observed <- matrix(c(NA, 3, 5, 7), 2, 2)
  b <- .run_reuse_requests(q, observed = observed, output = "allocation",
                            tol = 1e-10, eta = 4)
  ref <- .run_reuse_requests(q, observed = observed, output = "allocation",
                              tol = 1e-10, eta = 4, reuse = FALSE)
  expect_length(a$solves, 2)
  expect_length(b$solves, 2)
  expect_null(b$solves[[1]]$args$x0)
  expect_identical(b$solves[[1]]$args$tol, 1e-10)
  expect_equal(b$solves[[1]]$support, c(2, 4))
  expect_identical(b$solves[[1]]$ids, c("c", "d"))
  expect_equal(b$answers, ref$answers, tolerance = 1e-10)
  expect_equal(b$fit$outputs, ref$fit$outputs, tolerance = 1e-10)
  expect_identical(b$fit$target_mask, c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(b$losses[[1]][[2]], c(3, 5, 7))
  expect_equal(b$losses[[1]][[3]], 4)
  expect_false(identical(a$fit$outputs$access, b$fit$outputs$access))
})

test_that("fixed-point gradients use the accepted objective state", {
  for (model in c("sae", "haae")) {
    p <- .reuse_problem(model)
    a <- .run_reuse_requests(p, tol = 1e-10)
    b <- .run_reuse_requests(p, tol = 1e-10, reuse = FALSE)
    expect_length(a$solves, 2)
    expect_length(b$solves, 3)
    expect_identical(a$sensitivity_states[[1]], a$solves[[1]]$fit$x_star)
    expect_identical(a$solves[[2]]$args$x0, a$solves[[1]]$fit$x_star)
    expect_equal(a$answers, b$answers, tolerance = 1e-7)
    expect_equal(a$fit$outputs, b$fit$outputs, tolerance = 1e-7)
    expect_true(a$fit$equilibrium$converged)
  }
})
