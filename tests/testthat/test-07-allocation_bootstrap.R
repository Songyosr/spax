.bootstrap_test_fixture <- function(colocated = FALSE) {
  n <- 18L; m <- 24L
  distance <- sqrt(outer(seq(2, 38, length.out = n),
    seq(3, 37, length.out = m), "-")^2 +
    outer(5 * sin(seq_len(n)), 4 * cos(seq_len(m)), "-")^2) + 1
  if (colocated) distance <- matrix(rep(seq(2, 38, length.out = n), m), n, m)
  dimnames(distance) <- list(paste0("origin", seq_len(n)), paste0("facility", seq_len(m)))
  demand <- setNames(150 + 20 * sin(seq_len(n)), rownames(distance))
  supply <- setNames(rep(c(5, 8, 6), length.out = m), colnames(distance))
  inputs <- prepare_allocation(demand, supply, distance,
    metadata = list(demand_units = "registered people", demand_period = "reference date",
      supply_units = "sessions", supply_period = "week", travel_units = "minutes"))
  model <- clm_allocation(inputs, fit_v0 = TRUE)
  truth <- c(sigma = 8, v0 = 25)
  y <- setNames(round(evaluate_allocation(model, truth)$utilization), names(supply))
  list(model = model, inputs = inputs, D = demand, S = supply, distance = distance,
    truth = truth, y = y, starts = rbind(short = c(sigma = 7, v0 = 20),
      wide = c(sigma = 12, v0 = 50)), lower = c(sigma = 2, v0 = 1),
    upper = c(sigma = 40, v0 = 200))
}

.bootstrap_test_fit <- function(f) fit_allocation(f$model, f$y, f$starts,
  f$lower, f$upper, output = "utilization", loss = "poisson", gradient = TRUE,
  control = list(maxit = 300, factr = 10, pgtol = 1e-8))

.bootstrap_test_observation <- function(f) list(event = "registered people",
  period = "reference date unresolved", sampling = "independent_facilities")

.bootstrap_test_mean <- function(f, theta, supply = f$S) {
  weights <- exp(-f$distance^2 / (2 * theta[["sigma"]]^2)) *
    rep(supply, each = length(f$D))
  colSums(weights / (rowSums(weights) + theta[["v0"]]) * f$D)
}

.bootstrap_test_mock_fit <- function(f, theta = f$truth, success = TRUE, boundary = FALSE) {
  parameters <- if (success) theta else setNames(rep(NA_real_, 2), names(theta))
  table <- data.frame(start = rownames(f$starts), sigma = parameters[[1]],
    v0 = parameters[[2]], loss = if (success) c(1, 2) else NA_real_,
    success = success, eligible = success, boundary = boundary,
    convergence = if (success) 0L else NA_integer_,
    optim_convergence = if (success) 0L else NA_integer_,
    solver_converged = success, residual = if (success) 0 else NA_real_,
    score_norm = if (success) 0 else NA_real_, kkt_score = if (success) 0 else NA_real_,
    rank = if (success) 2L else NA_integer_,
    status = if (success) "eligible" else "fit_failed", warnings = "", errors = "")
  list(success = success, status = if (success) "eligible" else "fit_failed",
    theta = parameters, loss = if (success) 1 else NA_real_,
    best_start = if (success) rownames(f$starts)[1] else NA_character_,
    boundary = boundary, score_norm = if (success) 0 else NA_real_,
    rank = if (success) 2L else NA_integer_, table = table)
}

.bootstrap_test_preserve_rng <- function(code) {
  kind <- RNGkind()
  present <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  state <- if (present) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    do.call(RNGkind, as.list(kind))
    if (present) assign(".Random.seed", state, envir = .GlobalEnv) else
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
  }, add = TRUE)
  force(code)
}

test_that("case weights multiply observation contributions and analytic gradients", {
  mu <- c(2, 5, 7, 11); y <- c(0, 4, 6, 9); w <- c(0, 2, 1, 1)
  J <- cbind(c(.3, -.2, .8, 1.1), c(-.4, .6, .2, -.1))
  duplicated <- rep(seq_along(w), w)
  expected_loss <- sum(mu[duplicated] - y[duplicated] * log(mu[duplicated]))
  expected_gradient <- as.numeric(crossprod(J[duplicated, , drop = FALSE],
    1 - y[duplicated] / mu[duplicated]))
  expect_equal(.allocation_case_loss(mu, y, w), expected_loss, tolerance = 1e-12)
  expect_equal(.allocation_case_gradient(mu, y, J, w), expected_gradient, tolerance = 1e-12)
  h <- 1e-5
  fd <- vapply(seq_len(ncol(J)), function(k) {
    (.allocation_case_loss(mu + h * J[, k], y, w) -
      .allocation_case_loss(mu - h * J[, k], y, w)) / (2 * h)
  }, numeric(1))
  expect_equal(.allocation_case_gradient(mu, y, J, w), fd, tolerance = 1e-8)
  expect_equal(.allocation_case_loss(mu, y, rep(1, 4)), .poisson_loss(mu, y))
  expect_error(.allocation_case_loss(mu, y, c(1, -1, 1, 3)))
  expect_error(.allocation_case_loss(mu, y, c(1, NA, 1, 2)))
  expect_error(.allocation_case_loss(mu, y, c(1, 1)))
})

test_that("case losses distinguish absent counts from incompatible and floored means", {
  mu <- c(0, 2, 5); y <- c(0, 1, 4); w <- c(1, 1, 2)
  J <- rbind(c(0, 0), c(.2, -.1), c(.5, .3))
  expect_equal(.allocation_case_loss(mu, y, w),
    .allocation_case_loss(mu[-1], y[-1], w[-1]))
  expect_equal(.allocation_case_gradient(mu, y, J, w),
    .allocation_case_gradient(mu[-1], y[-1], J[-1, , drop = FALSE], w[-1]))
  y[1] <- 3
  expect_error(.allocation_case_loss(mu, y, w), "zero")
  expect_error(.allocation_case_gradient(mu, y, J, w), "zero")
  # Omitting a likelihood contribution never removes a physical alternative.
  w[1] <- 0
  expect_equal(.allocation_case_loss(mu, y, w),
    .allocation_case_loss(mu[-1], y[-1], w[-1]))
  expect_equal(.allocation_case_gradient(mu, y, J, w),
    .allocation_case_gradient(mu[-1], y[-1], J[-1, , drop = FALSE], w[-1]))
  expect_error(.allocation_case_loss(1e-12, 0, 1), "floor|eps")
  expect_error(.allocation_case_gradient(1e-12, 0, matrix(0, 1, 2), 1), "floor|eps")
  expect_equal(.allocation_case_loss(c(1e-12, 2), c(0, 1), c(0, 1)), 2 - log(2))
})

test_that("interval eligibility counts finite successes separately for each quantity", {
  n <- 399L
  point <- c(sigma = 8, v0 = 25, regional_rho = .6, delta_contacts = 10, regional_A = 2)
  draws <- data.frame(attempt = seq_len(n), success = rep(TRUE, n),
    sigma = seq(6, 10, length.out = n), v0 = seq(20, 30, length.out = n),
    regional_rho = seq(.5, .7, length.out = n),
    delta_contacts = seq(8, 12, length.out = n), regional_A = rep(2, n))
  original <- list(status = "eligible", success = TRUE)
  draws$success[381:399] <- FALSE
  draws$v0[380] <- NA_real_
  tab <- .allocation_bootstrap_intervals(original, point, draws)
  sigma <- tab[tab$quantity == "sigma", ]
  v0 <- tab[tab$quantity == "v0", ]
  expect_equal(sigma$successful, 380)
  expect_equal(c(sigma$lower, sigma$upper),
    quantile(draws$sigma[1:380], c(.025, .975), type = 7, names = FALSE))
  expect_equal(v0$successful, 379)
  expect_identical(v0$status, "insufficient_bootstrap_success")
  expect_true(is.na(v0$lower) && is.na(v0$upper))
  expect_true(all(tab$attempted == 399))
  fixed <- tab[tab$quantity == "regional_A", ]
  expect_identical(fixed$status, "fixed_input_identity")
  expect_true(is.na(fixed$lower) && is.na(fixed$upper))
  # A failed draw with a finite/extreme value must never enter a percentile.
  draws$sigma[381:399] <- 1e10
  again <- .allocation_bootstrap_intervals(original, point, draws)
  expect_identical(again$lower, tab$lower)
  expect_identical(again$upper, tab$upper)
  for (status in c("original_fit_failed", "original_mean_boundary", "original_unidentified")) {
    refused <- original; refused$status <- status
    z <- .allocation_bootstrap_intervals(refused, point, draws)
    regular <- z$quantity != "regional_A"
    expect_true(all(z$status[regular] == status))
    expect_true(all(is.na(z$lower[regular]) & is.na(z$upper[regular])))
  }
})

test_that("380 or 398 completed successes cannot stand in for 399 attempted fits", {
  point <- c(sigma = 8, v0 = 25, regional_rho = .6, regional_A = 2)
  original <- list(status = "eligible")
  for (n in c(380L, 398L)) {
    draws <- data.frame(success = rep(TRUE, n), sigma = seq(6, 10, length.out = n),
      v0 = seq(20, 30, length.out = n), regional_rho = seq(.5, .7, length.out = n),
      regional_A = rep(2, n))
    tab <- .allocation_bootstrap_intervals(original, point, draws)
    regular <- tab$quantity != "regional_A"
    expect_true(all(tab$status[regular] == "incomplete_bootstrap"))
    expect_true(all(is.na(tab$lower[regular]) & is.na(tab$upper[regular])))
    expect_true(all(tab$successful == n & tab$attempted == n & tab$planned == 399L))
    expect_identical(tab$status[!regular], "fixed_input_identity")
    expect_equal(tab$estimate[!regular], 2)
    expect_true(is.na(tab$lower[!regular]) && is.na(tab$upper[!regular]))
  }
})

test_that("a final checkpoint failure suppresses intervals even after attempt 399", {
  f <- .bootstrap_test_fixture()
  point <- c(sigma = 8, v0 = 25, regional_rho = .6, regional_A = sum(f$S) / sum(f$D))
  context <- list(model = f$model, original = list(status = "eligible"), point = point,
    settings = .allocation_bootstrap_settings(), observation = .bootstrap_test_observation(f))
  records <- lapply(seq_len(399L), function(k) {
    list(draw = data.frame(attempt = k, success = TRUE, sigma = 7 + k / 399,
      v0 = 24 + k / 399, regional_rho = .5 + k / 3990, regional_A = unname(point["regional_A"])),
      starts = data.frame(attempt = k, start = "short"),
      multiplicities = setNames(rep(1L, length(f$S)), names(f$S)))
  })
  complete <- .allocation_bootstrap_result(context, records, seed = 44L, attempts = 399L)
  expect_identical(complete$status, "complete")
  expect_true(all(is.finite(complete$intervals$lower[complete$intervals$quantity != "regional_A"])))
  failed <- .allocation_bootstrap_result(context, records, seed = 44L, attempts = 399L,
    reason = "checkpoint failed: final write")
  expect_identical(failed$status, "incomplete")
  expect_equal(failed$attempted, 399)
  expect_equal(failed$successful, 399)
  regular <- failed$intervals$quantity != "regional_A"
  expect_true(all(failed$intervals$status[regular] == "incomplete_bootstrap"))
  expect_true(all(is.na(failed$intervals$lower[regular]) & is.na(failed$intervals$upper[regular])))
  expect_identical(failed$intervals$status[!regular], "fixed_input_identity")
  expect_equal(failed$intervals$estimate[!regular], unname(point["regional_A"]))
})

test_that("case diagnostics retain the complete choice network and log derivatives", {
  f <- .bootstrap_test_fixture(); w <- rep(c(0, 2, 1), length.out = length(f$S))
  names(w) <- names(f$S)
  before <- serialize(list(f$inputs, f$y, f$starts, f$lower, f$upper), NULL)
  a <- .allocation_bootstrap_diagnostic(f$model, f$truth, f$y, w, f$lower, f$upper)
  b <- .allocation_bootstrap_diagnostic(f$model, f$truth, f$y,
    setNames(rep(1, length(w)), names(w)), f$lower, f$upper)
  expect_equal(unname(a$mean), unname(.bootstrap_test_mean(f, f$truth)), tolerance = 1e-10)
  expect_identical(a$mean, b$mean)
  expect_identical(a$jacobian_log, b$jacobian_log)
  h <- 1e-5
  fd <- vapply(seq_along(f$truth), function(k) {
    plus <- minus <- f$truth
    plus[k] <- plus[k] * exp(h); minus[k] <- minus[k] * exp(-h)
    (.bootstrap_test_mean(f, plus) - .bootstrap_test_mean(f, minus)) / (2 * h)
  }, numeric(length(f$S)))
  expect_equal(unname(a$jacobian_log), unname(fd), tolerance = 1e-7)
  removed <- f$S; removed[w == 0] <- 0
  expect_gt(max(abs(.bootstrap_test_mean(f, f$truth, removed) - a$mean)), 1)
  expect_identical(serialize(list(f$inputs, f$y, f$starts, f$lower, f$upper), NULL), before)
})

test_that("diagnostic identities align independently and numerical absence is not structural", {
  f <- .bootstrap_test_fixture()
  w <- setNames(rep(c(0, 2, 1), length.out = length(f$S)), names(f$S))
  forward <- .allocation_bootstrap_diagnostic(f$model, f$truth, f$y, w, f$lower, f$upper)
  permuted <- .allocation_bootstrap_diagnostic(f$model, f$truth, rev(f$y),
    w[c(2:length(w), 1)], f$lower, f$upper)
  expect_equal(permuted, forward)
  independent <- .allocation_bootstrap_diagnostic(f$model, f$truth, unname(f$y), rev(w),
    f$lower, f$upper)
  expect_equal(independent, forward)
  bad <- w; names(bad)[1] <- names(bad)[2]
  expect_error(.allocation_bootstrap_diagnostic(f$model, f$truth, f$y, bad, f$lower, f$upper))
  bad <- f$y; names(bad)[1] <- "wrong facility"
  expect_error(.allocation_bootstrap_diagnostic(f$model, f$truth, bad, w, f$lower, f$upper))

  distance <- f$distance; distance[, 1] <- Inf
  absent <- clm_allocation(prepare_allocation(f$D, f$S, distance), fit_v0 = TRUE)
  y <- f$y; y[1] <- 0
  unit_weights <- setNames(rep(1, length(y)), names(y))
  z <- .allocation_bootstrap_diagnostic(absent, f$truth, y, unit_weights, f$lower, f$upper)
  expect_equal(unname(z$mean[1]), 0)
  expect_true(all(z$jacobian_log[1, ] == 0))
  expect_false(z$status %in% c("incompatible_zero_mean", "nonregular_mean_or_loss_floor"))
  y[1] <- 1
  incompatible <- .allocation_bootstrap_diagnostic(absent, f$truth, y, unit_weights, f$lower, f$upper)
  expect_identical(incompatible$status, "incompatible_zero_mean")
  distance[, 1] <- 1e6
  underflow <- clm_allocation(prepare_allocation(f$D, f$S, distance), fit_v0 = TRUE)
  y[1] <- 0
  under <- .allocation_bootstrap_diagnostic(underflow, f$truth, y, unit_weights, f$lower, f$upper)
  expect_identical(under$status, "nonregular_mean_or_loss_floor")
})

test_that("weighted refits preserve starts and independently aligned observations", {
  f <- .bootstrap_test_fixture()
  w <- setNames(rep(c(0, 2, 1), length.out = length(f$S)), names(f$S))
  before <- serialize(list(f$inputs, f$y, w, f$starts, f$lower, f$upper), NULL)
  fit <- .allocation_case_fit(f$model, f$y, f$starts, f$lower, f$upper, w)
  aligned <- .allocation_case_fit(f$model, rev(f$y), f$starts, f$lower, f$upper,
    w[c(2:length(w), 1)])
  expect_true(fit$success)
  expect_equal(fit$theta, aligned$theta, tolerance = 1e-10)
  expect_equal(fit$loss, aligned$loss, tolerance = 1e-10)
  expect_identical(fit$best_start, aligned$best_start)
  expect_equal(nrow(fit$table), nrow(f$starts))
  expect_true(all(c("sigma", "v0", "status") %in% names(fit$table)))
  expect_true(is.finite(fit$score_norm) && fit$score_norm <= 1e-3)
  expected <- .bootstrap_test_mean(f, fit$theta)
  expect_equal(fit$loss, sum(w * (expected - f$y * log(expected))), tolerance = 1e-7)
  expect_identical(serialize(list(f$inputs, f$y, w, f$starts, f$lower, f$upper), NULL), before)
})

test_that("co-location is a local rank refusal even when the optimizer can fit totals", {
  f <- .bootstrap_test_fixture(colocated = TRUE)
  fit <- .allocation_case_fit(f$model, f$y, f$starts, f$lower, f$upper,
    setNames(rep(1, length(f$S)), names(f$S)))
  expect_true(fit$success)
  expect_equal(fit$rank, 1)
  point <- c(sigma = unname(fit$theta[1]), v0 = unname(fit$theta[2]),
    regional_rho = sum(.bootstrap_test_mean(f, fit$theta)) / sum(f$D),
    regional_A = sum(f$S) / sum(f$D))
  original <- fit; original$status <- "original_unidentified"
  draws <- as.data.frame(as.list(point))[rep(1, 399), , drop = FALSE]
  draws$success <- TRUE
  tab <- .allocation_bootstrap_intervals(original, point, draws)
  expect_true(all(tab$status[tab$quantity != "regional_A"] == "original_unidentified"))
  expect_true(all(is.na(tab$lower) & is.na(tab$upper)))
  original_fit <- .bootstrap_test_fit(f)
  testthat::local_mocked_bindings(.allocation_case_fit = function(...) stop("unexpected bootstrap refit"))
  refused <- .run_allocation_bootstrap(f$model, original_fit, .bootstrap_test_observation(f),
    seed = 44L, attempts = 2L)
  expect_identical(refused$status, "refused")
  expect_identical(refused$original$status, "original_unidentified")
  expect_equal(refused$attempted, 0)
})

test_that("original boundary estimates refuse intervals before bootstrap attempts", {
  f <- .bootstrap_test_fixture()
  f$lower["sigma"] <- 10
  f$starts[, "sigma"] <- c(12, 18)
  fit <- .bootstrap_test_fit(f)
  testthat::local_mocked_bindings(.allocation_case_fit = function(...) stop("unexpected bootstrap refit"))
  result <- .run_allocation_bootstrap(f$model, fit, .bootstrap_test_observation(f),
    seed = 116L, attempts = 2L)
  expect_identical(result$status, "refused")
  expect_identical(result$original$status, "original_mean_boundary")
  expect_equal(result$attempted, 0)
  expect_true(all(is.na(result$intervals$lower) & is.na(result$intervals$upper)))
})

test_that("unsupported observations and altered fitted systems cannot enter bootstrap", {
  f <- .bootstrap_test_fixture(); fit <- .bootstrap_test_fit(f)
  declaration <- .bootstrap_test_observation(f)
  testthat::local_mocked_bindings(.allocation_case_fit = function(...) stop("unexpected bootstrap refit"))
  expect_error(.run_allocation_bootstrap(f$model, fit, list(), seed = 1, attempts = 1), "observation")
  wrong <- declaration; wrong$sampling <- "fixed_total_multinomial"
  expect_error(.run_allocation_bootstrap(f$model, fit, wrong, seed = 1, attempts = 1), "observation")
  expect_error(.run_allocation_bootstrap(f$model, fit, declaration, seed = 1.5, attempts = 1), "seed")
  missing <- fit; missing$best$observed[1] <- NA_real_
  expect_error(.run_allocation_bootstrap(f$model, missing, declaration, seed = 1, attempts = 1), "complete")
  fractional <- fit; fractional$best$observed[1] <- fractional$best$observed[1] + .25
  expect_error(.run_allocation_bootstrap(f$model, fractional, declaration, seed = 1, attempts = 1), "whole")
  flow <- fit; flow$best$output <- "flow"
  expect_error(.run_allocation_bootstrap(f$model, flow, declaration, seed = 1, attempts = 1), "utilization")
  custom <- fit; custom$best$loss_fn <- .weighted_sse_loss
  expect_error(.run_allocation_bootstrap(f$model, custom, declaration, seed = 1, attempts = 1), "Poisson")
  modified <- f$S; modified[2] <- modified[2] * 2
  changed <- clm_allocation(prepare_allocation(f$D, modified, f$distance), fit_v0 = TRUE)
  expect_error(.run_allocation_bootstrap(changed, fit, declaration, seed = 1, attempts = 1), "conflict")
  demand <- f$D; demand[1] <- demand[1] + 1
  invalid_scenario <- clm_allocation(prepare_allocation(demand, f$S, f$distance), fit_v0 = TRUE)
  expect_error(.run_allocation_bootstrap(f$model, fit, declaration, invalid_scenario,
    seed = 1, attempts = 1), "supply only")
  exp_model <- clm_allocation(f$inputs, family = "exponential", fit_v0 = TRUE)
  expect_error(.run_allocation_bootstrap(exp_model, fit, declaration, seed = 1, attempts = 1), "Gaussian")
})

test_that("the public operation always delegates exactly 399 attempted fits", {
  observed <- NULL
  testthat::local_mocked_bindings(.run_allocation_bootstrap = function(model, fit, observation,
      scenario = NULL, seed, checkpoint = NULL, attempts = 399L) {
    observed <<- attempts
    "delegated"
  })
  expect_identical(bootstrap_allocation(NULL, NULL, NULL, seed = 5L), "delegated")
  expect_identical(observed, 399L)
  expect_false("attempts" %in% names(formals(bootstrap_allocation)))
  expect_error(bootstrap_allocation(NULL, NULL, NULL, seed = 5L, attempts = 1L), "unused")
})

test_that("bootstrap replay preserves RNG, physical inputs and paired scenario parameters", {
  f <- .bootstrap_test_fixture(); fit <- .bootstrap_test_fit(f)
  observation <- .bootstrap_test_observation(f)
  supply <- f$S; supply[2] <- supply[2] * 1.2
  scenario <- clm_allocation(prepare_allocation(f$D, supply, f$distance,
    metadata = f$inputs$input_metadata), fit_v0 = TRUE)
  calls <- list()
  testthat::local_mocked_bindings(.allocation_case_fit = function(model, observed, starts,
      lower, upper, weights, eps = 1e-9) {
    calls[[length(calls) + 1L]] <<- weights
    # A valid constrained inner fit remains in the bootstrap distribution.
    .bootstrap_test_mock_fit(f, theta = f$lower, boundary = TRUE)
  })
  before <- serialize(list(f$inputs, f$y, fit, observation, f$starts, supply), NULL)
  set.seed(921)
  rng <- .Random.seed; kind <- RNGkind()
  run <- .run_allocation_bootstrap(f$model, fit, observation, scenario,
    seed = 4421L, attempts = 4L)
  expect_identical(.Random.seed, rng)
  expect_identical(RNGkind(), kind)
  replay <- .run_allocation_bootstrap(f$model, fit, observation, scenario,
    seed = 4421L, attempts = 4L)
  expect_identical(run$multiplicities, replay$multiplicities)
  expect_identical(run$draws, replay$draws)
  expect_s3_class(run, "ae_allocation_bootstrap")
  expect_identical(run$planned, 399L)
  expect_equal(run$draws$seed, 4421L + seq_len(4L))
  expect_equal(run$attempted, 4)
  expect_equal(run$successful, 4)
  expect_true(all(run$draws$success))
  expect_equal(length(calls), 8)
  expect_true(all(vapply(calls, function(w) sum(w) == length(f$S) &&
    all(w >= 0 & w == floor(w)), logical(1))))
  base <- sum(.bootstrap_test_mean(f, f$lower))
  changed <- sum(.bootstrap_test_mean(f, f$lower, supply))
  expect_equal(run$draws$regional_rho, rep(base / sum(f$D), 4), tolerance = 1e-10)
  expect_equal(run$draws$delta_contacts, rep(changed - base, 4), tolerance = 1e-10)
  expect_equal(run$draws$regional_A, rep(sum(f$S) / sum(f$D), 4), tolerance = 1e-10)
  expect_true(all(run$intervals$status[run$intervals$quantity != "regional_A"] ==
    "incomplete_bootstrap"))
  expected <- .bootstrap_test_preserve_rng({
    RNGkind("Mersenne-Twister", "Inversion", "Rejection")
    do.call(rbind, lapply(4421L + seq_len(4L), function(seed) {
      set.seed(seed)
      tabulate(sample.int(length(f$S), length(f$S), replace = TRUE), nbins = length(f$S))
    }))
  })
  expect_equal(unname(run$multiplicities), expected)
  expect_identical(serialize(list(f$inputs, f$y, fit, observation, f$starts, supply), NULL), before)
})

test_that("absent facilities stay in resampling while unsupported capacity is disclosed", {
  f <- .bootstrap_test_fixture()
  f$distance[, 1:2] <- Inf
  f$S[2] <- 0
  f$inputs <- prepare_allocation(f$D, f$S, f$distance)
  f$model <- clm_allocation(f$inputs, fit_v0 = TRUE)
  f$y <- setNames(round(evaluate_allocation(f$model, f$truth)$utilization), names(f$S))
  fit <- .bootstrap_test_fit(f)
  changed_supply <- f$S; changed_supply[1] <- changed_supply[1] * 2
  scenario <- clm_allocation(prepare_allocation(f$D, changed_supply, f$distance), fit_v0 = TRUE)
  calls <- list()
  testthat::local_mocked_bindings(.allocation_case_fit = function(model, observed, starts,
      lower, upper, weights, eps = 1e-9) {
    calls[[length(calls) + 1L]] <<- list(ids = model$substrate$facility_ids,
      supply = as.numeric(model$substrate$S), weights = weights)
    .bootstrap_test_mock_fit(f)
  })
  result <- .run_allocation_bootstrap(f$model, fit, .bootstrap_test_observation(f), scenario,
    seed = 513L, attempts = 4L)
  expect_equal(result$attempted, 4)
  expect_identical(result$facility_ids, names(f$S))
  expect_identical(colnames(result$multiplicities), names(f$S))
  expect_true(all(rowSums(result$multiplicities) == length(f$S)))
  expect_true(any(result$multiplicities[, 1] > 0))
  expect_true(all(vapply(calls, function(z) identical(z$ids, names(f$S)) &&
    identical(z$supply, as.numeric(f$S)) && identical(names(z$weights), names(f$S)), logical(1))))
  expected_A <- sum(f$S[-c(1, 2)]) / sum(f$D)
  expect_equal(result$draws$regional_A, rep(expected_A, 4), tolerance = 1e-12)
  expect_equal(result$draws$delta_contacts, rep(0, 4), tolerance = 1e-12)
  expect_identical(result$capacity_support$supported_facility_ids, names(f$S)[-c(1, 2)])
  expect_identical(result$capacity_support$unsupported_facility_ids, names(f$S)[1])
  expect_equal(result$capacity_support$unsupported_supply, f$S[1])
  expect_equal(result$capacity_support$total_supply, sum(f$S))
  expect_equal(result$capacity_support$attributed_supply, sum(f$S[-c(1, 2)]))
  identity <- result$intervals[result$intervals$quantity == "regional_A", ]
  expect_identical(identity$status, "fixed_input_identity")
  expect_equal(identity$estimate, expected_A, tolerance = 1e-12)
  expect_true(is.na(identity$lower) && is.na(identity$upper))
})

test_that("bootstrap restores the absence of a caller RNG state", {
  f <- .bootstrap_test_fixture(); fit <- .bootstrap_test_fit(f)
  testthat::local_mocked_bindings(.allocation_case_fit = function(...) .bootstrap_test_mock_fit(f))
  .bootstrap_test_preserve_rng({
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
    result <- .run_allocation_bootstrap(f$model, fit, .bootstrap_test_observation(f),
      seed = 71L, attempts = 1L)
    expect_equal(result$attempted, 1)
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  })
})

test_that("checkpoint resume retains completed failures and refuses changed contracts", {
  f <- .bootstrap_test_fixture(); fit <- .bootstrap_test_fit(f)
  observation <- .bootstrap_test_observation(f)
  path <- tempfile(fileext = ".rds")
  calls <- 0L; interrupt <- TRUE
  testthat::local_mocked_bindings(.allocation_case_fit = function(model, observed, starts,
      lower, upper, weights, eps = 1e-9) {
    calls <<- calls + 1L
    if (interrupt && calls == 3L) stop(structure(
      list(message = "test interruption", call = NULL), class = c("interrupt", "condition")))
    .bootstrap_test_mock_fit(f, success = calls != 2L)
  })
  set.seed(952); rng <- .Random.seed
  partial <- .run_allocation_bootstrap(f$model, fit, observation, seed = 735L,
    checkpoint = path, attempts = 4L)
  expect_identical(.Random.seed, rng)
  expect_identical(partial$status, "incomplete")
  expect_equal(partial$attempted, 2)
  expect_equal(length(readRDS(path)$records), 2)
  expect_false(partial$draws$success[2])
  expect_error(.run_allocation_bootstrap(f$model, fit, observation, seed = 736L,
    checkpoint = path, attempts = 4L), "contract|checkpoint")
  interrupt <- FALSE; calls <- 2L
  complete <- .run_allocation_bootstrap(f$model, fit, observation, seed = 735L,
    checkpoint = path, attempts = 4L)
  expect_equal(calls, 4)
  expect_equal(complete$attempted, 4)
  expect_equal(complete$successful, 3)
  expect_false(complete$draws$success[2])
  expect_identical(complete$draws[1:2, ], partial$draws)
  expect_equal(length(readRDS(path)$records), 4)
  # A completed checkpoint resumes without repeating any fitting attempt.
  testthat::local_mocked_bindings(.allocation_case_fit = function(...) stop("unexpected refit"))
  reused <- .run_allocation_bootstrap(f$model, fit, observation, seed = 735L,
    checkpoint = path, attempts = 4L)
  expect_identical(reused$draws, complete$draws)
  expect_identical(reused$multiplicities, complete$multiplicities)
  intact <- readRDS(path)
  corrupt <- list(
    function(x) { x$records[[1]]$draw$attempt <- 2L; x },
    function(x) { x$records[[1]]$draw$sigma <- 123; x },
    function(x) { x$records[[1]]$draw$success <- FALSE; x },
    function(x) { x$records[[1]]$starts <- x$records[[1]]$starts[FALSE, ]; x }
  )
  for (change in corrupt) {
    saveRDS(change(intact), path)
    expect_error(.run_allocation_bootstrap(f$model, fit, observation, seed = 735L,
      checkpoint = path, attempts = 4L), "checkpoint|integrity")
  }
  # Structural validation remains independent of the serialized checksum.
  tampered <- corrupt[[4]](intact)
  tampered$integrity <- .allocation_bootstrap_integrity(tampered[c("contract", "records")])
  saveRDS(tampered, path)
  expect_error(.run_allocation_bootstrap(f$model, fit, observation, seed = 735L,
    checkpoint = path, attempts = 4L), "record|checkpoint")
})

test_that("ordinary fit exceptions remain completed failures alongside real start tables", {
  f <- .bootstrap_test_fixture(); fit <- .bootstrap_test_fit(f)
  prototype <- .allocation_case_fit(f$model, f$y, f$starts, f$lower, f$upper,
    setNames(rep(1, length(f$S)), names(f$S)))
  calls <- 0L
  testthat::local_mocked_bindings(.allocation_case_fit = function(...) {
    calls <<- calls + 1L
    if (calls == 1L) stop("injected ordinary fit exception")
    prototype
  })
  result <- .run_allocation_bootstrap(f$model, fit, .bootstrap_test_observation(f),
    seed = 413L, attempts = 3L)
  expect_equal(calls, 3)
  expect_equal(result$attempted, 3)
  expect_equal(result$successful, 2)
  expect_identical(result$draws$success, c(FALSE, TRUE, TRUE))
  expect_match(result$draws$status[1], "injected ordinary fit exception")
  expect_equal(nrow(result$starts), 3 * nrow(f$starts))
  expect_true(all(result$starts$errors[result$starts$attempt == 1] == "injected ordinary fit exception"))
})

test_that("checkpoint write failures stop instead of silently extending the run", {
  f <- .bootstrap_test_fixture(); fit <- .bootstrap_test_fit(f)
  observation <- .bootstrap_test_observation(f)
  calls <- 0L
  testthat::local_mocked_bindings(.allocation_case_fit = function(...) {
    calls <<- calls + 1L
    .bootstrap_test_mock_fit(f)
  })
  testthat::local_mocked_bindings(.allocation_bootstrap_save = function(...) {
    stop("injected checkpoint write failure")
  })
  result <- .run_allocation_bootstrap(f$model, fit, observation, seed = 212L,
    checkpoint = tempfile(fileext = ".rds"), attempts = 4L)
  expect_identical(result$status, "incomplete")
  expect_true(calls <= 1L)
  expect_true(result$attempted <= 1L)
  expect_true(any(grepl("checkpoint", result$reasons)))
})
