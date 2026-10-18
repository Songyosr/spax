ratio_identification_fixture <- function(gamma_truth = 0) {
  i <- seq_len(24)
  j <- seq_len(6)
  travel <- sqrt(outer(seq(1, 40, length.out = 24), seq(4, 36, length.out = 6), "-")^2 +
    outer(5 * sin(i * .9), 4 * cos(j * 1.7), "-")^2) + 1
  dimnames(travel) <- list(paste0("o", i), paste0("f", j))
  # Large enough counts that a feedback gain of 0.8 is detectable: across 16
  # seeds the ratio gain exceeded 3 in every feedback draw (min 4.1) and in
  # no null draw (max 2.9).
  demand <- setNames(1600 + 600 * (1 + sin(i * .4)), rownames(travel))
  supply <- setNames(c(4, 9, 3, 7, 5, 6), colnames(travel))
  inputs <- prepare_allocation(demand, supply, travel)
  truth <- c(sigma = 9, v0 = 0.5, feedback_scale = 1 + gamma_truth)
  problem <- .ratio_problem(inputs, ratio_ref = sum(supply) / sum(demand))
  mu <- .solve_problem(problem, truth, lambda = 0.4, tol = 1e-12, max_iter = 5000)$outputs$flow
  observed <- withr::with_seed(42, matrix(stats::rpois(length(mu), mu), nrow(mu),
                                           dimnames = dimnames(travel)))
  list(inputs = inputs, observed = observed, travel = travel, demand = demand,
       supply = supply)
}

# gamma_upper = 2 keeps the feedback solves cheap. A bounded fit can only lose
# likelihood, so its gain is a lower bound on the unbounded gain; the null test
# checks that the bound is inactive.
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
  expect_false(res$boundary[res$model == "ratio"])
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
  fd_grad <- function(p, h = 1e-5) vapply(seq_along(p), function(k) {
    e <- replace(numeric(length(p)), k, h)
    (loss(p + e) - loss(p - e)) / (2 * h)
  }, numeric(1))
  p <- c(log(free$attractiveness[-1]), log(free$sigma), log(free$v0))
  start <- c(log(f$supply[-1]), log(8), log(0.6))
  expect_equal(loss(p), free$loss, tolerance = 1e-10)
  # At the optimum the gradient is under a millionth of its size at the start.
  expect_lt(max(abs(fd_grad(p))), 1e-5 * max(abs(fd_grad(start))))
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

test_that("free-attractiveness gradient matches finite differences with flows below the floor", {
  # Four facilities spread along 60 minutes: at short travel scales many
  # origin-facility flows with positive counts fall below the 1e-9 floor.
  x <- seq(0, 60, length.out = 12)
  site <- c(f1 = 2, f2 = 20, f3 = 40, f4 = 58)
  travel <- abs(outer(x, site, "-")) + 1
  dimnames(travel) <- list(paste0("o", seq_along(x)), names(site))
  demand <- setNames(rep(500, length(x)), rownames(travel))
  supply <- c(f1 = 3, f2 = 5, f3 = 4, f4 = 6)
  inputs <- prepare_allocation(demand, supply, travel)
  observed <- matrix(1 + (seq_len(length(travel)) %% 7), nrow(travel),
                     dimnames = dimnames(travel))
  value <- .clm_free_attractiveness_objective(inputs, observed, eps = 1e-9)
  for (sigma in c(3, 4, 6)) {
    p <- c(log(supply[-1]) + c(0.2, -0.1, 0.3), log(sigma), log(0.5))
    attraction <- exp(c(log(supply[[1]]), p[1:3]))
    score <- sweep(exp(-travel^2 / (2 * sigma^2)), 2, attraction, `*`)
    flow <- demand * score / (rowSums(score) + 0.5)
    expect_true(any(flow < 1e-9))
    expect_true(all(abs(flow / 1e-9 - 1) > 1e-3))  # no step crosses the floor
    fd <- vapply(seq_along(p), function(k) {
      h <- replace(numeric(length(p)), k, 1e-6)
      (value(p + h)$loss - value(p - h)$loss) / 2e-6
    }, numeric(1))
    expect_equal(unname(value(p)$grad), fd, tolerance = 1e-6)
  }
})

test_that("the private ratio fits use the floor of fit_allocation()", {
  expect_identical(formals(.clm_free_attractiveness_objective)$eps, 1e-250)
})
