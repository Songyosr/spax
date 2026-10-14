ratio_identification_fixture <- function(gamma_truth = 0) {
  i <- seq_len(24)
  j <- seq_len(6)
  travel <- sqrt(outer(seq(1, 40, length.out = 24), seq(4, 36, length.out = 6), "-")^2 +
    outer(5 * sin(i * .9), 4 * cos(j * 1.7), "-")^2) + 1
  dimnames(travel) <- list(paste0("o", i), paste0("f", j))
  demand <- setNames(400 + 150 * (1 + sin(i * .4)), rownames(travel))
  supply <- setNames(c(4, 9, 3, 7, 5, 6), colnames(travel))
  inputs <- prepare_allocation(demand, supply, travel)
  truth <- c(sigma = 9, v0 = 0.5, feedback_scale = 1 + gamma_truth)
  problem <- .ratio_problem(inputs, ratio_ref = sum(supply) / sum(demand))
  mu <- .solve_problem(problem, truth, lambda = 0.4, tol = 1e-12, max_iter = 5000)$outputs$flow
  observed <- withr::with_seed(42, matrix(stats::rpois(length(mu), mu), nrow(mu),
                                           dimnames = dimnames(travel)))
  list(inputs = inputs, observed = observed, travel = travel, demand = demand)
}

# gamma_upper = 2 keeps the feedback solves cheap. A bounded fit can only lose
# likelihood, so its gain is a lower bound on the unbounded gain.
ratio_identification_run <- function(f) {
  .ratio_identification(f$inputs, f$observed,
    starts = rbind(a = c(sigma = 6, v0 = 1), b = c(sigma = 14, v0 = 0.2)),
    lower = c(sigma = 0.5, v0 = 1e-4), upper = c(sigma = 60, v0 = 50),
    gamma_upper = 2)
}

test_that("ratio identification reports nested fits and the free-model bound", {
  res <- ratio_identification_run(ratio_identification_fixture())

  expect_identical(res$model, c("static", "static_b", "ratio", "ratio_b", "free"))
  expect_identical(res$k, c(2L, 3L, 3L, 4L, 7L))
  loss <- setNames(res$loss, res$model)
  tol <- 1e-3 # absolute, in log-likelihood units
  # Each richer model nests the simpler ones.
  expect_true(loss[["static_b"]] <= loss[["static"]] + tol)
  expect_true(loss[["ratio"]] <= loss[["static"]] + tol)
  expect_true(loss[["ratio_b"]] <= min(loss[["static_b"]], loss[["ratio"]]) + tol)
  # Free attractiveness can reproduce any feedback equilibrium.
  expect_true(loss[["free"]] <= loss[["ratio_b"]] + tol)
  expect_equal(res$gain[res$model == "static"], 0)
  expect_equal(res$share_of_free_gain[res$model == "free"], 1)
  expect_true(abs(res$sigma[res$model == "static"] - 9) < 1.5)
  expect_true(all(res$converged))
  # Without feedback the ratio gain stays below a likelihood-ratio statistic
  # of 6 (2 x gain), which the feedback test below must exceed.
  expect_lt(res$gain[res$model == "ratio"], 3)
  expect_match(attr(res, "note"), "cannot be separated")
})

test_that("ratio identification finds a feedback gain when feedback is present", {
  res <- ratio_identification_run(ratio_identification_fixture(gamma_truth = 0.8))
  expect_gt(res$gain[res$model == "ratio"], 3)
  expect_gt(res$gamma[res$model == "ratio"], 0)
  expect_true(all(res$converged))
})

test_that("the free-attractiveness fit reaches a stationary point of the CLM likelihood", {
  f <- ratio_identification_fixture(gamma_truth = 0.8)
  free <- .clm_free_attractiveness_fit(f$inputs, f$observed, sigma = 8, v0 = 0.6)
  J <- ncol(f$observed)
  # Independent Poisson loss; the first attractiveness stays fixed.
  loss <- function(p) {
    a <- c(free$attractiveness[1], exp(p[seq_len(J - 1)]))
    w <- sweep(exp(-f$travel^2 / (2 * exp(p[J])^2)), 2, a, "*")
    mu <- f$demand * w / (rowSums(w) + exp(p[J + 1]))
    sum(mu - f$observed * log(mu))
  }
  p <- c(log(free$attractiveness[-1]), log(free$sigma), log(free$v0))
  expect_equal(loss(p), free$loss, tolerance = 1e-10)
  h <- 1e-5
  grad <- vapply(seq_along(p), function(k) {
    e <- replace(numeric(length(p)), k, h)
    (loss(p + e) - loss(p - e)) / (2 * h)
  }, numeric(1))
  # Gradients away from the optimum are of order 10-1000 here.
  expect_lt(max(abs(grad)), 1e-3)
  expect_true(free$converged)
})

test_that("ratio identification validates its inputs", {
  f <- ratio_identification_fixture()
  bad <- f$observed[, rev(colnames(f$observed))]
  expect_error(.ratio_identification(f$inputs, bad,
    starts = rbind(a = c(sigma = 6, v0 = 1)),
    lower = c(sigma = 0.5, v0 = 1e-4), upper = c(sigma = 60, v0 = 50)),
    "prepared IDs")
  expect_error(.ratio_identification(f$inputs, f$observed,
    starts = rbind(a = c(s = 6, v = 1)),
    lower = c(sigma = 0.5, v0 = 1e-4), upper = c(sigma = 60, v0 = 50)),
    "columns sigma and v0")
  expect_error(.ratio_identification(f$inputs, f$observed,
    starts = rbind(a = c(sigma = 6, v0 = 1)),
    lower = c(sigma = 0.5, v = 1e-4), upper = c(sigma = 60, v0 = 50)),
    "named sigma and v0")
})
