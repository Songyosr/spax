# Conditional facility-case bootstrap (bootstrap_allocation()): the case-
# weighted Poisson objective, the 399-attempt / 380-success interval rule,
# refusals, RNG preservation, checkpoints and a short run of the real engine.
# Mocked refits stand in for optimisation where the property under test is the
# engine's bookkeeping rather than the fit.

# Expected facility means of the generated design, from the CLM oracle.
.case_mean <- function(f, theta, supply = f$supply) {
  clm_oracle(f$distance, f$demand, supply, theta[["sigma"]], theta[["v0"]])$utilization
}

.case_weights <- function(f) {
  stats::setNames(rep(c(0, 2, 1), length.out = length(f$supply)), names(f$supply))
}

# One real weighted refit, shared by the blocks that need its start table.
.case_fit <- function() {
  alloc_memo("bootstrap_case_fit", {
    f <- alloc_generated_case()
    .allocation_case_fit(f$model, f$y, f$starts, f$lower, f$upper, .case_weights(f))
  })
}

# Restore the RNG kind and the presence or absence of .Random.seed.
.preserve_caller_rng <- function(code) {
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
  expect_error(.allocation_case_gradient(mu, y, J[-1, ], w), "sensitivity")
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
  expect_identical(sigma$status, "conditional_pointwise_bootstrap")
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
  f <- alloc_generated_case()
  point <- c(sigma = 8, v0 = 25, regional_rho = .6,
             regional_A = sum(f$supply) / sum(f$demand))
  context <- list(model = f$model, original = list(status = "eligible"), point = point,
    settings = .allocation_bootstrap_settings(), observation = alloc_bootstrap_observation())
  records <- lapply(seq_len(399L), function(k) {
    list(draw = data.frame(attempt = k, success = TRUE, sigma = 7 + k / 399,
      v0 = 24 + k / 399, regional_rho = .5 + k / 3990, regional_A = unname(point["regional_A"])),
      starts = data.frame(attempt = k, start = "short"),
      multiplicities = stats::setNames(rep(1L, length(f$supply)), names(f$supply)))
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
  f <- alloc_generated_case(); w <- .case_weights(f)
  before <- serialize(list(f$inputs, f$y, f$starts, f$lower, f$upper), NULL)
  a <- .allocation_bootstrap_diagnostic(f$model, f$truth, f$y, w, f$lower, f$upper)
  b <- .allocation_bootstrap_diagnostic(f$model, f$truth, f$y,
    stats::setNames(rep(1, length(w)), names(w)), f$lower, f$upper)
  expect_equal(unname(a$mean), .case_mean(f, f$truth), tolerance = 1e-10)
  expect_identical(a$mean, b$mean)
  expect_identical(a$jacobian_log, b$jacobian_log)
  h <- 1e-5
  fd <- vapply(seq_along(f$truth), function(k) {
    plus <- minus <- f$truth
    plus[k] <- plus[k] * exp(h); minus[k] <- minus[k] * exp(-h)
    (.case_mean(f, plus) - .case_mean(f, minus)) / (2 * h)
  }, numeric(length(f$supply)))
  expect_equal(unname(a$jacobian_log), fd, tolerance = 1e-7)
  removed <- f$supply; removed[w == 0] <- 0
  expect_gt(max(abs(.case_mean(f, f$truth, removed) - a$mean)), 1)
  expect_identical(serialize(list(f$inputs, f$y, f$starts, f$lower, f$upper), NULL), before)
})

test_that("diagnostic identities align independently and numerical absence is not structural", {
  f <- alloc_generated_case(); w <- .case_weights(f)
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
  expect_error(.allocation_bootstrap_diagnostic(f$model, f$truth, f$y[-1], w, f$lower, f$upper),
               "one value per facility")
  expect_error(.allocation_bootstrap_diagnostic(f$model, f$truth, f$y, w * 0, f$lower, f$upper),
               "multiplicities")

  distance <- f$distance; distance[, 1] <- Inf
  absent <- clm_allocation(prepare_allocation(f$demand, f$supply, distance), fit_v0 = TRUE)
  y <- f$y; y[1] <- 0
  unit_weights <- stats::setNames(rep(1, length(y)), names(y))
  z <- .allocation_bootstrap_diagnostic(absent, f$truth, y, unit_weights, f$lower, f$upper)
  expect_equal(unname(z$mean[1]), 0)
  expect_true(all(z$jacobian_log[1, ] == 0))
  expect_false(z$status %in% c("incompatible_zero_mean", "nonregular_mean_or_loss_floor"))
  y[1] <- 1
  incompatible <- .allocation_bootstrap_diagnostic(absent, f$truth, y, unit_weights,
                                                   f$lower, f$upper)
  expect_identical(incompatible$status, "incompatible_zero_mean")
  distance[, 1] <- 1e6
  underflow <- clm_allocation(prepare_allocation(f$demand, f$supply, distance), fit_v0 = TRUE)
  y[1] <- 0
  under <- .allocation_bootstrap_diagnostic(underflow, f$truth, y, unit_weights, f$lower, f$upper)
  expect_identical(under$status, "nonregular_mean_or_loss_floor")
})

test_that("weighted refits select an eligible start and align observations by facility ID", {
  f <- alloc_generated_case(); w <- .case_weights(f)
  before <- serialize(list(f$inputs, f$y, w, f$starts, f$lower, f$upper), NULL)
  fit <- .case_fit()
  expect_true(fit$success)
  expect_identical(fit$table$start, rownames(f$starts))
  expect_true(fit$table$eligible[match(fit$best_start, fit$table$start)])
  expect_equal(fit$loss, min(fit$table$loss[fit$table$eligible]))
  expect_true(all(c("sigma", "v0", "status") %in% names(fit$table)))
  expect_true(is.finite(fit$score_norm) && fit$score_norm <= 1e-3)
  expected <- .case_mean(f, fit$theta)
  expect_equal(fit$loss, sum(w * (expected - f$y * log(expected))), tolerance = 1e-7)
  expect_identical(serialize(list(f$inputs, f$y, w, f$starts, f$lower, f$upper), NULL), before)
  # Observations and multiplicities reach the fitter in canonical facility order.
  seen <- list()
  testthat::local_mocked_bindings(.fit_problem_multistart = function(problem, observed, ...,
                                                                     loss_args) {
    seen[[length(seen) + 1L]] <<- list(observed = observed, weights = loss_args$weights)
    stop("captured inputs")
  })
  inputs <- list(list(observed = f$y, weights = w),
                 list(observed = rev(f$y), weights = w[c(2:length(w), 1)]),
                 list(observed = unname(f$y), weights = rev(w)))
  for (input in inputs) {
    expect_error(.allocation_case_fit(f$model, input$observed, f$starts, f$lower, f$upper,
                                      input$weights), "captured inputs")
  }
  expect_identical(seen[[1]], list(observed = f$y, weights = unname(w)))
  expect_identical(seen[[2]], seen[[1]])
  expect_identical(seen[[3]], seen[[1]])
})

test_that("failed, co-located and boundary original fits refuse intervals before attempts", {
  f <- alloc_generated_case()
  colocated <- alloc_generated_case(colocated = TRUE)
  starts <- f$starts; starts[, "sigma"] <- c(12, 18)
  fits <- list(original_fit_failed = alloc_failed_search(f, f$y),
    # Co-located facilities fit their totals, but only one local direction.
    original_unidentified = alloc_generated_fit(colocated = TRUE),
    original_mean_boundary = alloc_count_fit(f, starts = starts,
                                             lower = replace(f$lower, "sigma", 10)))
  models <- list(f$model, colocated$model, f$model)
  # Rank refusal applies to the original fit only: a weighted refit of the
  # co-located design still succeeds with its single local direction.
  unit <- stats::setNames(rep(1, length(colocated$supply)), names(colocated$supply))
  refit <- .allocation_case_fit(colocated$model, colocated$y, colocated$starts,
                                colocated$lower, colocated$upper, unit)
  expect_true(refit$success)
  expect_equal(refit$rank, 1)
  testthat::local_mocked_bindings(.allocation_case_fit = function(...) {
    stop("unexpected bootstrap refit")
  })
  for (k in seq_along(fits)) {
    status <- names(fits)[k]
    result <- bootstrap_allocation(models[[k]], fits[[k]], alloc_bootstrap_observation(),
                                   seed = 44L)
    expect_identical(result$status, "refused", info = status)
    expect_identical(result$original$status, status)
    expect_equal(result$attempted, 0, info = status)
    regular <- result$intervals$quantity != "regional_A"
    expect_true(all(result$intervals$status[regular] == status), info = status)
    expect_true(all(is.na(result$intervals$lower) & is.na(result$intervals$upper)), info = status)
    if (status == "original_unidentified") expect_equal(result$original$rank, 1)
  }
})

test_that("unsupported observations, altered systems and invalid fits cannot enter bootstrap", {
  f <- alloc_generated_case(); fit <- alloc_generated_fit()
  declaration <- alloc_bootstrap_observation()
  run <- function(fit, model = f$model, observation = declaration, scenario = NULL,
                  seed = 1, attempts = 1) {
    .run_allocation_bootstrap(model, fit, observation, scenario, seed = seed, attempts = attempts)
  }
  testthat::local_mocked_bindings(.allocation_case_fit = function(...) {
    stop("unexpected bootstrap refit")
  })
  expect_error(run(fit, observation = list()), "observation")
  wrong <- declaration; wrong$sampling <- "fixed_total_multinomial"
  expect_error(run(fit, observation = wrong), "observation")
  expect_error(run(fit, seed = 1.5), "seed")
  expect_error(run(fit, attempts = 0), "attempt limit")
  expect_error(bootstrap_allocation(f$model, fit, declaration, seed = 1,
    checkpoint = file.path(withr::local_tempdir(), "missing", "run.rds")), "checkpoint must name")
  missing <- fit; missing$best$observed[1] <- NA_real_
  expect_error(run(missing), "complete")
  fractional <- fit; fractional$best$observed[1] <- fractional$best$observed[1] + .25
  expect_error(run(fractional), "whole")
  flow <- fit; flow$best$output <- "flow"
  expect_error(run(flow), "utilization")
  custom <- fit; custom$best$loss_fn <- .weighted_sse_loss
  expect_error(run(custom), "Poisson")
  ids <- fit; ids$best$target_alignment$ids[[1]] <- rev(ids$best$target_alignment$ids[[1]])
  expect_error(run(ids), "observation IDs conflict")
  bounds <- fit; bounds$bounds$lower[["sigma"]] <- 10
  expect_error(run(bounds), "saved fit bounds")
  starts <- fit; starts$starts[1, "sigma"] <- 1
  expect_error(run(starts), "saved explicit starts")
  modified <- f$supply; modified[2] <- modified[2] * 2
  changed <- clm_allocation(prepare_allocation(f$demand, modified, f$distance), fit_v0 = TRUE)
  expect_error(run(fit, model = changed), "conflict")
  demand <- f$demand; demand[1] <- demand[1] + 1
  invalid_scenario <- clm_allocation(prepare_allocation(demand, f$supply, f$distance),
                                     fit_v0 = TRUE)
  expect_error(run(fit, scenario = invalid_scenario), "supply only")
  exp_model <- clm_allocation(f$inputs, family = "exponential", fit_v0 = TRUE)
  expect_error(run(fit, model = exp_model), "Gaussian")
})

test_that("the public operation always delegates exactly 399 attempted fits", {
  observed <- NULL
  testthat::local_mocked_bindings(.run_allocation_bootstrap = function(...) {
    observed <<- list(...)$attempts
    "delegated"
  })
  expect_identical(bootstrap_allocation(NULL, NULL, NULL, seed = 5L), "delegated")
  expect_identical(observed, 399L)
  expect_false("attempts" %in% names(formals(bootstrap_allocation)))
  expect_error(bootstrap_allocation(NULL, NULL, NULL, seed = 5L, attempts = 1L), "unused")
})

test_that("bootstrap replay preserves the caller's RNG, inputs and paired scenario parameters", {
  f <- alloc_generated_case(); fit <- alloc_generated_fit()
  observation <- alloc_bootstrap_observation()
  supply <- f$supply; supply[2] <- supply[2] * 1.2
  scenario <- clm_allocation(prepare_allocation(f$demand, supply, f$distance,
    metadata = f$inputs$input_metadata), fit_v0 = TRUE)
  calls <- list()
  testthat::local_mocked_bindings(.allocation_case_fit = function(model, observed, starts,
                                                                  lower, upper, weights, ...) {
    calls[[length(calls) + 1L]] <<- weights
    # A valid constrained inner fit remains in the bootstrap distribution.
    alloc_mock_case_fit(f, theta = f$lower, boundary = TRUE)
  })
  before <- serialize(list(f$inputs, f$y, fit, observation, f$starts, supply), NULL)
  local_test_seed(921)
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
  expect_equal(length(calls), 8)
  expect_true(all(vapply(calls, function(w) sum(w) == length(f$supply) &&
    all(w >= 0 & w == floor(w)), logical(1))))
  base <- sum(.case_mean(f, f$lower))
  changed <- sum(.case_mean(f, f$lower, supply))
  expect_equal(run$draws$regional_rho, rep(base / sum(f$demand), 4), tolerance = 1e-10)
  expect_equal(run$draws$delta_contacts, rep(changed - base, 4), tolerance = 1e-10)
  expect_equal(run$draws$regional_A, rep(sum(f$supply) / sum(f$demand), 4), tolerance = 1e-10)
  expected <- .preserve_caller_rng({
    RNGkind("Mersenne-Twister", "Inversion", "Rejection")
    do.call(rbind, lapply(4421L + seq_len(4L), function(seed) {
      set.seed(seed)
      tabulate(sample.int(length(f$supply), length(f$supply), replace = TRUE),
               nbins = length(f$supply))
    }))
  })
  expect_equal(unname(run$multiplicities), expected)
  expect_identical(serialize(list(f$inputs, f$y, fit, observation, f$starts, supply), NULL), before)
  # A caller without any RNG state still has none afterwards.
  .preserve_caller_rng({
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
    .run_allocation_bootstrap(f$model, fit, observation, seed = 71L, attempts = 1L)
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  })
  # The saved-result methods read the run without changing it.
  expect_identical(as.data.frame(run), run$intervals)
  expect_identical(as.data.frame(run, type = "draws"), run$draws)
  expect_identical(as.data.frame(run, type = "starts"), run$starts)
  report <- summary(run)
  expect_equal(report$counts, data.frame(planned = 399L, attempted = 4L, successful = 4L))
  expect_equal(report$diagnostics$inner_boundary_attempts, 4)
  expect_identical(report$capacity_support, run$capacity_support)
  expect_output(print(run), "independent_facilities")
})

test_that("absent facilities stay in resampling while unsupported capacity is disclosed", {
  f <- alloc_generated_case()
  f$distance[, 1:2] <- Inf
  f$supply[2] <- 0
  f$inputs <- prepare_allocation(f$demand, f$supply, f$distance)
  f$model <- clm_allocation(f$inputs, fit_v0 = TRUE)
  f$y <- stats::setNames(round(.case_mean(f, f$truth)), names(f$supply))
  fit <- alloc_count_fit(f)
  changed_supply <- f$supply; changed_supply[1] <- changed_supply[1] * 2
  scenario <- clm_allocation(prepare_allocation(f$demand, changed_supply, f$distance),
                             fit_v0 = TRUE)
  calls <- list()
  testthat::local_mocked_bindings(.allocation_case_fit = function(model, observed, starts,
                                                                  lower, upper, weights, ...) {
    calls[[length(calls) + 1L]] <<- list(ids = model$substrate$facility_ids,
      supply = as.numeric(model$substrate$S), weights = weights)
    alloc_mock_case_fit(f)
  })
  result <- .run_allocation_bootstrap(f$model, fit, alloc_bootstrap_observation(), scenario,
    seed = 513L, attempts = 4L)
  expect_equal(result$attempted, 4)
  expect_identical(result$facility_ids, names(f$supply))
  expect_identical(colnames(result$multiplicities), names(f$supply))
  expect_true(all(rowSums(result$multiplicities) == length(f$supply)))
  expect_true(any(result$multiplicities[, 1] > 0))
  expect_true(all(vapply(calls, function(z) identical(z$ids, names(f$supply)) &&
    identical(z$supply, as.numeric(f$supply)) && identical(names(z$weights), names(f$supply)),
    logical(1))))
  expected_A <- sum(f$supply[-c(1, 2)]) / sum(f$demand)
  expect_equal(result$draws$regional_A, rep(expected_A, 4), tolerance = 1e-12)
  expect_equal(result$draws$delta_contacts, rep(0, 4), tolerance = 1e-12)
  expect_identical(result$capacity_support$supported_facility_ids, names(f$supply)[-c(1, 2)])
  expect_identical(result$capacity_support$unsupported_facility_ids, names(f$supply)[1])
  expect_equal(result$capacity_support$unsupported_supply, f$supply[1])
  expect_equal(result$capacity_support$total_supply, sum(f$supply))
  expect_equal(result$capacity_support$attributed_supply, sum(f$supply[-c(1, 2)]))
  identity <- result$intervals[result$intervals$quantity == "regional_A", ]
  expect_identical(identity$status, "fixed_input_identity")
  expect_equal(identity$estimate, expected_A, tolerance = 1e-12)
  expect_true(is.na(identity$lower) && is.na(identity$upper))
})

test_that("checkpoint resume retains completed failures and refuses changed contracts", {
  f <- alloc_generated_case(); fit <- alloc_generated_fit()
  observation <- alloc_bootstrap_observation()
  path <- withr::local_tempfile(fileext = ".rds")
  calls <- 0L; interrupt <- TRUE
  testthat::local_mocked_bindings(.allocation_case_fit = function(...) {
    calls <<- calls + 1L
    if (interrupt && calls == 3L) stop(structure(
      list(message = "test interruption", call = NULL), class = c("interrupt", "condition")))
    alloc_mock_case_fit(f, success = calls != 2L)
  })
  local_test_seed(952); rng <- .Random.seed
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
  f <- alloc_generated_case(); fit <- alloc_generated_fit()
  prototype <- .case_fit()
  calls <- 0L
  testthat::local_mocked_bindings(.allocation_case_fit = function(...) {
    calls <<- calls + 1L
    if (calls == 1L) stop("injected ordinary fit exception")
    prototype
  })
  result <- .run_allocation_bootstrap(f$model, fit, alloc_bootstrap_observation(),
    seed = 413L, attempts = 3L)
  expect_equal(calls, 3)
  expect_equal(result$attempted, 3)
  expect_equal(result$successful, 2)
  expect_identical(result$draws$success, c(FALSE, TRUE, TRUE))
  expect_match(result$draws$status[1], "injected ordinary fit exception")
  expect_equal(nrow(result$starts), 3 * nrow(f$starts))
  expect_true(all(result$starts$errors[result$starts$attempt == 1] ==
    "injected ordinary fit exception"))
})

test_that("checkpoint write failures stop instead of silently extending the run", {
  f <- alloc_generated_case(); fit <- alloc_generated_fit()
  calls <- 0L
  testthat::local_mocked_bindings(.allocation_case_fit = function(...) {
    calls <<- calls + 1L
    alloc_mock_case_fit(f)
  })
  testthat::local_mocked_bindings(.allocation_bootstrap_save = function(...) {
    stop("injected checkpoint write failure")
  })
  result <- .run_allocation_bootstrap(f$model, fit, alloc_bootstrap_observation(), seed = 212L,
    checkpoint = withr::local_tempfile(fileext = ".rds"), attempts = 4L)
  expect_identical(result$status, "incomplete")
  expect_true(calls <= 1L)
  expect_true(result$attempted <= 1L)
  expect_true(any(grepl("checkpoint", result$reasons)))
})

test_that("the bootstrap engine runs end to end and withholds intervals when incomplete", {
  # The allocation-workflow vignette loads a stored 399-attempt result, so this
  # test keeps the real engine (fits, multistart, diagnostics) executed in checks.
  f <- alloc_generated_case(); fit <- alloc_generated_fit()
  boot <- .run_allocation_bootstrap(f$model, fit, alloc_bootstrap_observation(),
    seed = 11L, attempts = 2L)
  expect_s3_class(boot, "ae_allocation_bootstrap")
  expect_identical(boot$status, "incomplete")
  expect_equal(boot$attempted, 2L)
  expect_gt(boot$successful, 0L)
  expect_equal(nrow(boot$starts), 2L * nrow(f$starts))
  expect_true(all(boot$draws$score_norm[boot$draws$success] <= 1e-3))
  expect_true(all(is.na(boot$intervals$lower) & is.na(boot$intervals$upper)))
})
