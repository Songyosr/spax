# Extracted from test-06-ae_ratio_identification.R:81

# prequel ----------------------------------------------------------------------
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
  list(inputs = inputs, observed = observed, travel = travel, demand = demand)
}
ratio_identification_run <- function(f) {
  .ratio_identification(f$inputs, f$observed,
    starts = rbind(a = c(sigma = 6, v0 = 1), b = c(sigma = 14, v0 = 0.2)),
    lower = c(sigma = 0.5, v0 = 1e-4), upper = c(sigma = 60, v0 = 50),
    gamma_upper = 2)
}

# test -------------------------------------------------------------------------
f <- ratio_identification_fixture(gamma_truth = 0.8)
free <- .clm_free_attractiveness_fit(f$inputs, f$observed, sigma = 8, v0 = 0.6)
J <- ncol(f$observed)
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
expect_lt(max(abs(grad)), 1e-3)
