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
  # Every built-in kernel passes the model gate; the saved Gaussian fit simply
  # does not belong to an exponential model.
  exp_model <- clm_allocation(f$inputs, family = "exponential", fit_v0 = TRUE)
  expect_error(run(fit, model = exp_model), "conflict")
})

test_that("the public operation delegates 399 attempted fits by default", {
  observed <- NULL
  testthat::local_mocked_bindings(.run_allocation_bootstrap = function(...) {
    observed <<- list(...)[c("attempts", "planned")]
    "delegated"
  })
  expect_identical(bootstrap_allocation(NULL, NULL, NULL, seed = 5L), "delegated")
  expect_identical(observed, list(attempts = 399L, planned = 399L))
  expect_identical(formals(bootstrap_allocation)$attempts, 399)
  expect_error(bootstrap_allocation(NULL, NULL, NULL, seed = 5L, attempts = 1L),
               "attempts must be a whole number from 199 to 1,000,000", fixed = TRUE)
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
  # The facility-bootstrap vignette loads a stored 399-attempt result, so this
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

# The note of results for models outside the coverage study (see the last section).
.coverage_note <- paste("Coverage of these intervals was evaluated for Gaussian decay with fitted",
                        "sigma and v0; this model uses the same procedure without a separate",
                        "coverage study.")

# Other kernels, a declared kernel and a fitted supply elasticity ---------------
# The bootstrap takes its parameter names from the model, so it also runs for the
# exponential and power kernels, a declared smooth kernel and a fitted beta. The
# designs share the geometry and demand of alloc_generated_case(); counts are the
# rounded exact CLM utilization at `truth` from clm_oracle(). Runs are short (3 of
# the 399 attempts): they exercise the real engine and its bookkeeping, not the
# interval rule or coverage, and every value is recomputed from the oracle.

# A model, counts and an oracle. `kernel` is a declaration (otherwise `family`
# names a built-in kernel) and `oracle_kernel` its hand-written counterpart, a
# function of (distance, sigma). `scenario_supply` adds a paired scenario model.
.boot_case <- function(family, truth, starts, lower, upper, kernel = NULL,
                       oracle_kernel = NULL, kappa = 1, beta = 1, fit_beta = FALSE,
                       supply = NULL, scenario_supply = NULL) {
  f <- alloc_generated_case()
  base_supply <- if (is.null(supply)) f$supply else supply
  declare <- function(s) {
    inputs <- prepare_allocation(f$demand, s, f$distance, metadata = f$inputs$input_metadata)
    clm_allocation(inputs, family = if (is.null(kernel)) family else kernel,
                   kappa = kappa, beta = beta, fit_v0 = TRUE, fit_beta = fit_beta)
  }
  oracle <- function(theta, supply = base_supply) {
    clm_oracle(f$distance, f$demand, supply, theta[["sigma"]], theta[["v0"]],
      family = family, beta = if (fit_beta) theta[["beta"]] else beta, kappa = kappa,
      kernel = oracle_kernel)
  }
  list(model = declare(base_supply),
       scenario = if (!is.null(scenario_supply)) declare(scenario_supply),
       oracle = oracle, demand = f$demand, supply = base_supply,
       scenario_supply = scenario_supply, y = colSums(round(oracle(truth)$flow)),
       starts = starts, lower = lower, upper = upper,
       parameters = c("sigma", "v0", if (fit_beta) "beta"))
}

# Gaussian decay with a fitted supply elasticity; `lower_beta` and `supply` let a
# test make beta unidentified or pin it on its bound.
.boot_beta_case <- function(supply = NULL, lower_beta = .2, near_beta = 1.1) {
  .boot_case("gaussian", c(sigma = 8, v0 = 25, beta = 1.3),
    starts = rbind(near = c(sigma = 7, v0 = 20, beta = near_beta),
                   far = c(sigma = 12, v0 = 50, beta = 2)),
    lower = c(sigma = 2, v0 = 1, beta = lower_beta),
    upper = c(sigma = 40, v0 = 200, beta = 4), fit_beta = TRUE, supply = supply)
}

# A plateau kernel (full weight within 5 minutes, Gaussian beyond) with a paired
# scenario that raises the supply of one facility. The declaration is made by the
# caller, inside its test block.
.boot_plateau_case <- function(kernel) {
  f <- alloc_generated_case()
  scenario_supply <- f$supply
  scenario_supply[2] <- scenario_supply[2] * 1.2
  .boot_case("gaussian", c(sigma = 8, v0 = 25),
    starts = rbind(near = c(sigma = 7, v0 = 20), far = c(sigma = 12, v0 = 50)),
    lower = c(sigma = 2, v0 = 1), upper = c(sigma = 40, v0 = 200), kernel = kernel,
    oracle_kernel = function(distance, sigma) {
      ifelse(distance <= 5, 1, exp(-(distance - 5)^2 / (2 * sigma^2)))
    },
    scenario_supply = scenario_supply)
}

.boot_run <- function(case, fit, seed, attempts = 3L) {
  .run_allocation_bootstrap(case$model, fit, alloc_bootstrap_observation(), case$scenario,
    seed = seed, attempts = attempts)
}

# Column rules: the parameters take the place `sigma` and `v0` had, right after
# `success` in the draws and after the two gaps in the start table.
.draw_columns <- function(parameters, scenario = FALSE) {
  c("attempt", "seed", "status", "success", parameters, "boundary", "best_start", "loss",
    "score_norm", "regional_rho", if (scenario) "delta_contacts", "regional_A")
}
.start_columns <- function(parameters) {
  c("start", "loss", "eligible", "optim_convergence", "solver_converged", "residual",
    "seconds", "boundary", "status", "optim_message", "solver_message",
    "log_parameter_gap", "loss_gap", parameters, "score_norm", "rank", "warnings",
    "errors", "attempt")
}

# Checks of a short run against the bootstrap's own rules, applied by hand.
.expect_short_run <- function(case, fit, run, seed) {
  p <- case$parameters
  scenario <- !is.null(case$scenario)
  expect_s3_class(run, "ae_allocation_bootstrap")
  # 3 of 399 attempts: incomplete, so no interval can be reported.
  expect_identical(run$status, "incomplete")
  expect_identical(run$reasons, "fewer than 399 completed attempts")
  expect_identical(c(run$planned, run$attempted), c(399L, 3L))
  # The original fit is eligible only when its rank equals the number of fitted
  # parameters (two, or three with a fitted beta).
  expect_identical(run$original$status, "eligible")
  expect_equal(run$original$rank, length(p))
  expect_identical(names(run$original$theta), p)
  expect_equal(run$original$theta, stats::coef(fit))

  # Every one of these models lies outside the coverage study.
  expect_identical(run$coverage_note, .coverage_note)
  expect_identical(tail(names(run), 1L), "coverage_note")
  expect_identical(names(run$draws), .draw_columns(p, scenario))
  expect_identical(run$draws$attempt, 1:3)
  expect_identical(run$draws$seed, seed + 1:3)
  ok <- run$draws$success
  expect_gt(sum(ok), 0)
  expect_equal(run$successful, sum(ok))
  expect_true(all(run$draws$status[ok] == "selected"))
  expect_true(all(is.na(run$draws[!ok, c(p, "loss")])))

  est <- run$intervals
  expect_identical(est$quantity, c(p, "regional_rho", if (scenario) "delta_contacts",
                                   "regional_A"))
  regular <- est$quantity != "regional_A"
  expect_true(all(est$status[regular] == "incomplete_bootstrap"))
  expect_true(all(is.na(est$lower[regular]) & is.na(est$upper[regular])))
  expect_identical(est$status[!regular], "fixed_input_identity")
  expect_true(all(est$attempted == 3L & est$planned == 399L & est$successful == sum(ok)))
  # Point estimates: the saved fit's parameters and regional values from the oracle.
  theta <- stats::coef(fit)
  mu <- case$oracle(theta)$utilization
  at <- function(q) est$estimate[est$quantity == q]
  expect_equal(est$estimate[match(p, est$quantity)], unname(theta[p]))
  expect_equal(at("regional_rho"), sum(mu) / sum(case$demand), tolerance = 1e-10)
  expect_equal(at("regional_A"), sum(case$supply) / sum(case$demand), tolerance = 1e-12)
  if (scenario) {
    changed <- sum(case$oracle(theta, case$scenario_supply)$utilization)
    expect_equal(at("delta_contacts"), changed - sum(mu), tolerance = 1e-9)
  }

  expect_identical(names(run$starts), .start_columns(p))
  expect_identical(run$starts$attempt, rep(1:3, each = nrow(case$starts)))
  expect_identical(run$starts$start, rep(rownames(case$starts), 3))

  bounds <- fit$bounds
  for (i in which(ok)) {
    draw <- run$draws[i, ]
    theta_i <- unlist(draw[p])
    w <- run$multiplicities[i, ]
    mu_i <- case$oracle(theta_i)$utilization
    positive <- w > 0 & mu_i > 0
    # The weighted Poisson objective, regional values and scenario change.
    expect_equal(draw$loss, sum(w[positive] * (mu_i[positive] -
      case$y[positive] * log(mu_i[positive]))), tolerance = 1e-8)
    expect_equal(draw$regional_rho, sum(mu_i) / sum(case$demand), tolerance = 1e-10)
    expect_equal(draw$regional_A, sum(case$supply) / sum(case$demand), tolerance = 1e-12)
    if (scenario) {
      changed <- sum(case$oracle(theta_i, case$scenario_supply)$utilization)
      expect_equal(draw$delta_contacts, changed - sum(mu_i), tolerance = 1e-9)
    }
    # Boundary rule on the log scale: within 1e-6 plus 0.1% of the log search span
    # of a bound, for every fitted parameter.
    eta <- log(theta_i); lo <- log(bounds$lower[p]); hi <- log(bounds$upper[p])
    expect_identical(draw$boundary, any(pmin(eta - lo, hi - eta) <= 1e-6 + 1e-3 * (hi - lo)))
    expect_true(all(theta_i >= bounds$lower[p] & theta_i <= bounds$upper[p]))
    expect_lte(draw$score_norm, 1e-3)
  }
  invisible(run)
}

test_that("an exponential kernel with fixed non-unit kappa and beta runs a short bootstrap", {
  case <- .boot_case("exponential", c(sigma = .15, v0 = 20),
    starts = rbind(near = c(sigma = .12, v0 = 15), far = c(sigma = .3, v0 = 40)),
    lower = c(sigma = .02, v0 = 1), upper = c(sigma = 2, v0 = 200), kappa = 1.5, beta = .8)
  fit <- alloc_memo("bootstrap_fit_exponential", alloc_count_fit(case))
  run <- alloc_memo("bootstrap_run_exponential", .boot_run(case, fit, 601L))
  expect_identical(case$model$metadata$spec$family, "exponential")
  .expect_short_run(case, fit, run, 601L)
})

test_that("a power kernel runs a short bootstrap", {
  case <- .boot_case("power", c(sigma = 1.8, v0 = 10),
    starts = rbind(near = c(sigma = 1.5, v0 = 8), far = c(sigma = 2.6, v0 = 20)),
    lower = c(sigma = .3, v0 = .5), upper = c(sigma = 6, v0 = 100))
  fit <- alloc_memo("bootstrap_fit_power", alloc_count_fit(case))
  run <- alloc_memo("bootstrap_run_power", .boot_run(case, fit, 602L))
  expect_identical(case$model$metadata$spec$family, "power")
  .expect_short_run(case, fit, run, 602L)
})

test_that("a declared smooth kernel runs a short bootstrap with a paired scenario", {
  # Declared with braces, so the function carries source references; the stored
  # declaration is plain data and does not depend on how it was written.
  plateau <- decay_kernel(
    function(distance, sigma, plateau = 5) {
      ifelse(distance <= plateau, 1, exp(-(distance - plateau)^2 / (2 * sigma^2)))
    },
    name = "plateau", parameter = "scale", args = list(plateau = 5)
  )
  case <- .boot_plateau_case(plateau)
  fit <- alloc_memo("bootstrap_fit_plateau", alloc_count_fit(case))
  run <- alloc_memo("bootstrap_run_plateau", .boot_run(case, fit, 603L))
  expect_identical(case$model$metadata$spec$kernel, plateau)
  expect_identical(case$scenario$metadata$spec, case$model$metadata$spec)
  .expect_short_run(case, fit, run, 603L)
})

test_that("a fitted supply elasticity adds a third parameter after v0", {
  case <- .boot_beta_case()
  fit <- alloc_memo("bootstrap_fit_beta", alloc_count_fit(case))
  run <- alloc_memo("bootstrap_run_beta", .boot_run(case, fit, 604L))
  expect_identical(case$model$theta$names, c("sigma", "v0", "beta"))
  .expect_short_run(case, fit, run, 604L)
  # Three parameters need rank three; the saved-result methods are generic.
  expect_equal(run$original$rank, 3)
  expect_identical(as.data.frame(run)$quantity,
                   c("sigma", "v0", "beta", "regional_rho", "regional_A"))
  expect_identical(summary(run)$estimates, run$intervals)
  expect_output(print(run), "beta")
  expect_true(all(is.finite(as.data.frame(run, type = "draws")$beta[run$draws$success])))
})

# Singular values of the weighted log-parameter Jacobian of the oracle means, with
# unit multiplicities and central differences: the identification rule by hand.
.hand_singular_values <- function(case, theta) {
  mu <- case$oracle(theta)$utilization
  h <- 1e-5
  jacobian <- vapply(seq_along(theta), function(k) {
    up <- down <- theta
    up[k] <- up[k] * exp(h)
    down[k] <- down[k] * exp(-h)
    (case$oracle(up)$utilization - case$oracle(down)$utilization) / (2 * h)
  }, numeric(length(mu)))
  svd(jacobian / sqrt(mu), nu = 0, nv = 0)$d
}

test_that("the rank rule compares the rank with the number of fitted parameters", {
  f <- alloc_generated_case()
  unit <- stats::setNames(rep(1, length(f$supply)), names(f$supply))
  two <- .boot_case("gaussian", c(sigma = 8, v0 = 25),
    starts = f$starts, lower = f$lower, upper = f$upper)
  three <- .boot_beta_case()
  # With equal supply everywhere, beta moves attractiveness exactly as v0 does:
  # only two of the three directions are identified.
  flat <- .boot_beta_case(supply = stats::setNames(rep(6, length(f$supply)), names(f$supply)))
  flat_fit <- alloc_memo("bootstrap_fit_equal_supply", alloc_count_fit(flat))
  models <- list(
    two = list(case = two, fit = alloc_generated_fit(), rank = 2L),
    three = list(case = three, fit = alloc_memo("bootstrap_fit_beta", alloc_count_fit(three)),
                 rank = 3L),
    flat = list(case = flat, fit = flat_fit, rank = 2L))
  for (name in names(models)) {
    m <- models[[name]]
    theta <- stats::coef(m$fit)
    sv <- .hand_singular_values(m$case, theta)
    expect_equal(sum(sv > 1e-4 * max(sv)), m$rank, info = name)
    d <- .allocation_bootstrap_diagnostic(m$case$model, theta, m$case$y, unit,
      m$fit$bounds$lower, m$fit$bounds$upper)
    expect_equal(d$rank, m$rank, info = name)
    if (name == "flat") {
      expect_lt(min(sv) / max(sv), 1e-6)
      expect_lt(d$singular_ratio, 1e-6)
    } else {
      # The ratio of the smallest to the largest singular value, over every
      # parameter (it used to be reported only for two).
      expect_equal(d$singular_ratio, min(sv) / max(sv), tolerance = 1e-6, info = name)
      expect_gt(d$singular_ratio, 1e-2)
    }
  }
  testthat::local_mocked_bindings(.allocation_case_fit = function(...) {
    stop("unexpected bootstrap refit")
  })
  result <- bootstrap_allocation(flat$model, flat_fit, alloc_bootstrap_observation(), seed = 44L)
  expect_identical(result$status, "refused")
  expect_identical(result$original$status, "original_unidentified")
  expect_equal(result$original$rank, 2)
  expect_equal(result$attempted, 0)
  expect_identical(result$intervals$quantity, c("sigma", "v0", "beta", "regional_rho", "regional_A"))
  regular <- result$intervals$quantity != "regional_A"
  expect_true(all(result$intervals$status[regular] == "original_unidentified"))
  expect_true(all(is.na(result$intervals$lower) & is.na(result$intervals$upper)))
  # The empty draw table still has one column per fitted parameter.
  expect_identical(names(result$draws), .draw_columns(c("sigma", "v0", "beta")))
  expect_equal(nrow(result$draws), 0)
})

test_that("an original fit with beta on its search bound refuses intervals", {
  case <- .boot_beta_case(lower_beta = 1.5, near_beta = 1.6)
  fit <- alloc_memo("bootstrap_fit_beta_pinned", alloc_count_fit(case))
  testthat::local_mocked_bindings(.allocation_case_fit = function(...) {
    stop("unexpected bootstrap refit")
  })
  theta <- stats::coef(fit)
  expect_equal(theta[["beta"]], 1.5)
  # Boundary rule on the log scale: within 1e-6 plus 0.1% of the log search span.
  eta <- log(theta); lo <- log(fit$bounds$lower); hi <- log(fit$bounds$upper)
  near_bound <- pmin(eta - lo, hi - eta) <= 1e-6 + 1e-3 * (hi - lo)
  expect_identical(near_bound, c(sigma = FALSE, v0 = FALSE, beta = TRUE))
  result <- bootstrap_allocation(case$model, fit, alloc_bootstrap_observation(), seed = 44L)
  expect_identical(result$status, "refused")
  expect_identical(result$original$status, "original_mean_boundary")
  expect_true(result$original$boundary)
  expect_equal(result$attempted, 0)
  regular <- result$intervals$quantity != "regional_A"
  expect_true(all(result$intervals$status[regular] == "original_mean_boundary"))
})

test_that("failed fits and attempts keep one missing value per fitted parameter", {
  case <- .boot_beta_case()
  p <- case$parameters
  fit <- alloc_memo("bootstrap_fit_beta", alloc_count_fit(case))
  observation <- alloc_bootstrap_observation()
  na_theta <- c(sigma = NA_real_, v0 = NA_real_, beta = NA_real_)

  # The original fit failed: a refused result with a named missing estimate.
  failed_fit <- fit_allocation(case$model, case$y, case$starts, case$lower, case$upper,
    loss = function(predicted, observed) stop("injected loss failure"))
  refused <- bootstrap_allocation(case$model, failed_fit, observation, seed = 44L)
  expect_identical(refused$status, "refused")
  expect_identical(refused$original$status, "original_fit_failed")
  expect_identical(refused$original$theta, na_theta)
  expect_identical(refused$intervals$quantity, c(p, "regional_rho", "regional_A"))
  expect_true(all(is.na(refused$intervals$estimate[refused$intervals$quantity != "regional_A"])))
  expect_identical(names(refused$draws), .draw_columns(p))
  # The point placeholder carries the model's parameter names, with or without a
  # paired scenario (the model itself serves as a scenario that changes nothing).
  for (scenario in list(NULL, case$model)) {
    context <- .allocation_bootstrap_prepare(case$model, failed_fit, observation, scenario)
    expect_identical(names(context$point), c(p, "regional_rho",
      if (!is.null(scenario)) "delta_contacts", "regional_A"))
    expect_true(all(is.na(context$point[names(context$point) != "regional_A"])))
  }

  # A weighted refit with no eligible start: its theta names every parameter.
  start_ids <- rownames(case$starts)
  search <- list(
    table = data.frame(start = start_ids, loss = NA_real_, eligible = FALSE,
      status = "not_converged", boundary = FALSE, stringsAsFactors = FALSE),
    theta = matrix(NA_real_, 2L, 3L, dimnames = list(start_ids, p)),
    warnings = list(character(), character()), errors = list(NULL, "no convergence"))
  unit <- stats::setNames(rep(1, length(case$supply)), names(case$supply))
  none <- testthat::with_mocked_bindings(
    .allocation_case_fit(case$model, case$y, case$starts, case$lower, case$upper, unit),
    .fit_problem_multistart = function(...) search)
  expect_false(none$success)
  expect_identical(none$status, "fit_failed")
  expect_identical(none$theta, na_theta)
  expect_identical(names(none$table), c("start", "loss", "eligible", "status", "boundary",
    p, "score_norm", "rank", "warnings", "errors"))
  run <- testthat::with_mocked_bindings(.boot_run(case, fit, 606L, attempts = 2L),
    .allocation_case_fit = function(...) none)
  expect_identical(run$draws$status, rep("fit_failed", 2))
  expect_identical(names(run$draws), .draw_columns(p))
  expect_true(all(is.na(run$draws[, p])) && all(!run$draws$success))
  expect_true(all(is.na(run$starts[, p])))
  expect_equal(c(run$attempted, run$successful), c(2, 0))

  # A fit that raises an error becomes a completed failed attempt.
  errored <- testthat::with_mocked_bindings(.boot_run(case, fit, 607L, attempts = 2L),
    .allocation_case_fit = function(...) stop("injected fit error"))
  expect_true(all(startsWith(errored$draws$status, "fit_error: injected fit error")))
  expect_identical(names(errored$draws), .draw_columns(p))
  expect_true(all(is.na(errored$draws[, p])) && all(!errored$draws$success))
  expect_identical(names(errored$starts), c("start", "loss", "eligible", "optim_convergence",
    "solver_converged", "residual", "seconds", "boundary", "status", "optim_message",
    "solver_message", "loss_gap", "log_parameter_gap", "sigma", "v0", "beta", "score_norm",
    "rank", "warnings", "errors", "attempt"))
  expect_true(all(is.na(errored$starts[, p])))
  expect_true(all(errored$starts$errors == "injected fit error"))
})

test_that("the gate admits built-in and smooth declared kernels with a fitted outside mass", {
  f <- alloc_generated_case(); fit <- alloc_generated_fit()
  observation <- alloc_bootstrap_observation()
  smooth <- decay_kernel(function(distance, sigma) {
    exp(-distance / sigma)
  }, name = "scale-exponential", parameter = "scale")
  rough <- decay_kernel(function(distance, sigma) {
    ifelse(distance <= sigma, 1, 0)
  }, name = "cutoff", parameter = "radius", smooth = FALSE)
  declare <- function(...) clm_allocation(f$inputs, ...)
  admitted <- list(
    gaussian = declare(fit_v0 = TRUE),
    exponential = declare(family = "exponential", fit_v0 = TRUE),
    power = declare(family = "power", fit_v0 = TRUE),
    builtin_declaration = declare(family = decay_kernel("gaussian"), fit_v0 = TRUE),
    smooth_declaration = declare(family = smooth, fit_v0 = TRUE),
    fitted_beta = declare(fit_v0 = TRUE, fit_beta = TRUE),
    positive_kappa_beta = declare(kappa = 2, beta = .7, fit_v0 = TRUE),
    fitted_beta_with_kappa = declare(kappa = 2, fit_v0 = TRUE, fit_beta = TRUE))
  for (name in names(admitted)) {
    expect_no_error(.allocation_bootstrap_model(admitted[[name]]))
  }
  # The provider is rebuilt from the specification, declared kernel included.
  tampered <- admitted$smooth_declaration
  tampered$bind <- function(theta) stop("replaced provider")
  rebuilt <- .allocation_bootstrap_model(tampered)
  theta <- c(sigma = 6, v0 = 12)
  expected <- clm_oracle(f$distance, f$demand, f$supply, 6, 12,
                         kernel = function(distance, sigma) exp(-distance / sigma))
  expect_equal(as.numeric(.solve_problem(rebuilt, theta, keep_history = FALSE, warn = FALSE,
    diagnostics = FALSE, requested_outputs = "utilization")$outputs$utilization),
    expected$utilization, tolerance = 1e-10)

  # Each refusal names its reason, before any refit; the public function agrees.
  testthat::local_mocked_bindings(.allocation_case_fit = function(...) {
    stop("unexpected bootstrap refit")
  })
  refused <- list(
    "smooth in sigma" = declare(family = rough, fit_v0 = TRUE),
    "fitted outside weight" = declare(v0 = 25),
    "fit_v0 = TRUE" = declare(family = "exponential", v0 = 25, fit_beta = TRUE))
  for (reason in names(refused)) {
    expect_error(.run_allocation_bootstrap(refused[[reason]], fit, observation, seed = 1L,
      attempts = 1L), reason, fixed = TRUE)
    expect_error(bootstrap_allocation(refused[[reason]], fit, observation, seed = 1L),
                 reason, fixed = TRUE)
  }
  # A model whose parameters do not match its specification, or whose family is
  # neither built in nor declared, is not admitted either.
  mutated <- admitted$gaussian
  mutated$theta$names <- c("sigma", "beta")
  expect_error(.allocation_bootstrap_model(mutated), "match its specification")
  internal <- admitted$gaussian
  internal$metadata$spec$family <- "none"
  expect_error(.allocation_bootstrap_model(internal), "smooth in sigma")
})

test_that("an interrupted run with a declared kernel resumes after the model and fit are saved and read", {
  # Declared with braces, as in the short run above.
  plateau <- decay_kernel(
    function(distance, sigma, plateau = 5) {
      ifelse(distance <= plateau, 1, exp(-(distance - plateau)^2 / (2 * sigma^2)))
    },
    name = "plateau", parameter = "scale", args = list(plateau = 5)
  )
  case <- .boot_plateau_case(plateau)
  fit <- alloc_memo("bootstrap_fit_plateau", alloc_count_fit(case))
  uninterrupted <- alloc_memo("bootstrap_run_plateau", .boot_run(case, fit, 603L))
  observation <- alloc_bootstrap_observation()
  path <- withr::local_tempfile(fileext = ".rds")
  # The planned number of attempts belongs to the checkpoint contract, so a run
  # is interrupted (as an operator would) rather than shortened: attempts 1 and 2
  # complete, and the third is interrupted before it finishes.
  real_fit <- .allocation_case_fit
  calls <- 0L
  first <- testthat::with_mocked_bindings(
    .run_allocation_bootstrap(case$model, fit, observation, case$scenario, seed = 603L,
      checkpoint = path, attempts = 3L),
    .allocation_case_fit = function(...) {
      calls <<- calls + 1L
      if (calls == 3L) stop(structure(list(message = "test interruption", call = NULL),
        class = c("interrupt", "condition")))
      real_fit(...)
    })
  expect_identical(first$status, "incomplete")
  expect_identical(first$attempted, 2L)
  expect_equal(length(readRDS(path)$records), 2)
  # The checkpoint keeps the declaration as plain data.
  expect_identical(readRDS(path)$contract$spec$kernel, plateau)

  # The model, the paired scenario and the fit go through the disk.
  bundle <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(model = case$model, scenario = case$scenario, fit = fit), bundle)
  reloaded <- readRDS(bundle)
  expect_identical(reloaded$model$metadata$spec, case$model$metadata$spec)
  # The fit is used as read back, with the Poisson loss function it saved.
  resumed <- .run_allocation_bootstrap(reloaded$model, reloaded$fit, observation,
    reloaded$scenario, seed = 603L, checkpoint = path, attempts = 3L)
  expect_identical(resumed$attempted, 3L)
  expect_equal(length(readRDS(path)$records), 3)
  # Equal to an uninterrupted run; the first two attempts come from the checkpoint.
  for (field in setdiff(names(uninterrupted), "starts")) {
    expect_identical(resumed[[field]], uninterrupted[[field]], info = field)
  }
  untimed <- function(x) x$starts[setdiff(names(x$starts), "seconds")]
  expect_identical(untimed(resumed), untimed(uninterrupted))
  expect_identical(resumed$draws[1:2, ], first$draws)

  # Another declaration under the same name is a different contract.
  wide <- decay_kernel(
    function(distance, sigma, plateau = 5) {
      ifelse(distance <= plateau, 1, exp(-(distance - plateau)^2 / (2 * sigma^2)))
    },
    name = "plateau", parameter = "scale", args = list(plateau = 6)
  )
  other <- .boot_plateau_case(wide)
  other_fit <- alloc_memo("bootstrap_fit_plateau_wide", alloc_count_fit(other))
  expect_error(.run_allocation_bootstrap(other$model, other_fit, observation, other$scenario,
    seed = 603L, checkpoint = path, attempts = 3L), "checkpoint does not match")
})

test_that("a (sigma, v0) model keeps the names and order of its tables", {
  f <- alloc_generated_case(); fit <- alloc_generated_fit()
  observation <- alloc_bootstrap_observation()
  supply <- f$supply
  supply[2] <- supply[2] * 1.2
  scenario <- clm_allocation(prepare_allocation(f$demand, supply, f$distance,
    metadata = f$inputs$input_metadata), fit_v0 = TRUE)
  run <- .run_allocation_bootstrap(f$model, fit, observation, scenario, seed = 11L, attempts = 2L)
  # The studied model gets no coverage note: its result has no new field.
  expect_false("coverage_note" %in% names(run))
  # The column vectors the bootstrap produced before parameters came from the model.
  expect_identical(names(run$draws), c("attempt", "seed", "status", "success", "sigma", "v0",
    "boundary", "best_start", "loss", "score_norm", "regional_rho", "delta_contacts",
    "regional_A"))
  expect_identical(names(run$starts), c("start", "loss", "eligible", "optim_convergence",
    "solver_converged", "residual", "seconds", "boundary", "status", "optim_message",
    "solver_message", "log_parameter_gap", "loss_gap", "sigma", "v0", "score_norm", "rank",
    "warnings", "errors", "attempt"))
  expect_identical(names(run$original$theta), c("sigma", "v0"))
  expect_identical(run$intervals$quantity,
                   c("sigma", "v0", "regional_rho", "delta_contacts", "regional_A"))

  refused <- bootstrap_allocation(f$model, alloc_failed_search(f, f$y), observation, seed = 44L)
  expect_identical(refused$original$theta, c(sigma = NA_real_, v0 = NA_real_))
  expect_identical(names(refused$draws), c("attempt", "seed", "status", "success", "sigma",
    "v0", "boundary", "best_start", "loss", "score_norm", "regional_rho", "regional_A"))
  expect_equal(nrow(refused$draws), 0)

  errored <- testthat::with_mocked_bindings(
    .run_allocation_bootstrap(f$model, fit, observation, seed = 12L, attempts = 1L),
    .allocation_case_fit = function(...) stop("injected fit error"))
  expect_identical(names(errored$draws), names(refused$draws))
  expect_identical(errored$draws$sigma, NA_real_)
  expect_identical(errored$draws$v0, NA_real_)
  expect_identical(names(errored$starts), c("start", "loss", "eligible", "optim_convergence",
    "solver_converged", "residual", "seconds", "boundary", "status", "optim_message",
    "solver_message", "loss_gap", "log_parameter_gap", "sigma", "v0", "score_norm", "rank",
    "warnings", "errors", "attempt"))
})

# Declared bounds: finite differences and the original diagnostic ----------------
# A declared kernel bounds sigma from below. Where the optimum sits on that bound,
# the finite-difference step of the sensitivities used to leave the bounds: the
# analytic gradient of the fit was abandoned without a message, and the
# bootstrap stopped (original fit) or lost its draws on the bound (replicates).
# The built-in Gaussian, with the same search bounds, is the reference.

.gaussian_copy <- function(lower, upper = 40) {
  decay_kernel(function(distance, sigma) exp(-(distance^2) / (2 * sigma^2)),
               name = "gcopy", parameter = "travel scale", lower = lower, upper = upper)
}

# Calls of the fallback to a numerical gradient made by `code`.
.count_gradient_fallbacks <- function(code) {
  count <- 0L
  real <- .nfxp_fd_log_gradient
  value <- testthat::with_mocked_bindings(code, .nfxp_fd_log_gradient = function(...) {
    count <<- count + 1L
    real(...)
  })
  list(value = value, count = count)
}

test_that("a declared kernel whose optimum is on its bound fits like the built-in Gaussian", {
  f <- alloc_generated_case()
  # Truth is sigma = 8, so with a lower bound of 10 the Poisson optimum is the bound.
  # The declaration has the bounds of the search.
  starts <- rbind(a = c(sigma = 12, v0 = 20), b = c(sigma = 20, v0 = 50))
  lower <- c(sigma = 10, v0 = 1)
  declared <- clm_allocation(f$inputs, family = .gaussian_copy(10, 40), fit_v0 = TRUE)
  fit_with <- function(model) {
    .count_gradient_fallbacks(fit_allocation(model, f$y, starts, lower, f$upper,
      loss = "poisson", gradient = TRUE, control = list(maxit = 300, factr = 10, pgtol = 1e-8)))
  }
  builtin <- fit_with(f$model)
  copy <- fit_with(declared)
  expect_equal(coef(copy$value)[["sigma"]], 10, tolerance = 1e-6)
  expect_equal(coef(copy$value), coef(builtin$value), tolerance = 1e-6)
  expect_equal(copy$value$best$loss, builtin$value$best$loss, tolerance = 1e-6)
  expect_identical(copy$value$status, builtin$value$status)
  # The analytic gradient is used throughout, with one-sided steps at the bound:
  # it never falls back to numerical differences of the objective.
  expect_identical(c(builtin$count, copy$count), c(0L, 0L))
  expect_true(copy$value$best$gradient)
  # The bootstrap gives the declared model the refusal of the built-in model: the
  # original optimum is on the bound. It used to stop with an error.
  observation <- alloc_bootstrap_observation()
  testthat::local_mocked_bindings(.allocation_case_fit = function(...) {
    stop("unexpected bootstrap refit")
  })
  refused <- bootstrap_allocation(f$model, builtin$value, observation, seed = 44L)
  result <- bootstrap_allocation(declared, copy$value, observation, seed = 44L)
  expect_identical(refused$original$status, "original_mean_boundary")
  expect_identical(result$original$status, refused$original$status)
  expect_identical(result$status, "refused")
  expect_true(result$original$boundary)
  expect_equal(result$attempted, 0)
  expect_equal(result$original$theta, refused$original$theta, tolerance = 1e-6)
  regular <- result$intervals$quantity != "regional_A"
  expect_true(all(result$intervals$status[regular] == "original_mean_boundary"))
})

test_that("replicates on a declared bound are kept, as for the built-in Gaussian", {
  f <- alloc_generated_case()
  # Generated with sigma = 10.2 (v0 = 25), whose Poisson estimate is 10.16: close
  # enough to the lower bound of 10 that replicates often fall on it. The search
  # and the declaration share the bound. (A bound that exp(log()) cannot reach
  # exactly, such as 7.6, is tested in the next block.)
  y <- colSums(round(clm_oracle(f$distance, f$demand, f$supply, 10.2, 25)$flow))
  starts <- rbind(a = c(sigma = 11.2, v0 = 20), b = c(sigma = 14.2, v0 = 50))
  lower <- c(sigma = 10, v0 = 1)
  run <- function(model) {
    fit <- fit_allocation(model, y, starts, lower, f$upper, loss = "poisson", gradient = TRUE,
      control = list(maxit = 300, factr = 10, pgtol = 1e-8))
    .run_allocation_bootstrap(model, fit, alloc_bootstrap_observation(), seed = 5L,
                              attempts = 12L)
  }
  builtin <- run(f$model)
  declared <- run(clm_allocation(f$inputs, family = .gaussian_copy(10, 40), fit_v0 = TRUE))
  for (result in list(builtin, declared)) {
    expect_identical(result$original$status, "eligible")
    expect_equal(c(result$attempted, result$successful), c(12, 12))
    expect_identical(result$draws$status, rep("selected", 12))
  }
  on_bound <- function(result) which(result$draws$sigma <= 10 * (1 + 1e-6))
  # Draws on the bound are inner boundary fits that stay in the distribution.
  expect_gt(length(on_bound(builtin)), 0)
  expect_identical(on_bound(declared), on_bound(builtin))
  expect_identical(declared$draws$boundary, builtin$draws$boundary)
  expect_true(all(declared$draws$boundary[on_bound(declared)]))
  expect_equal(declared$draws$sigma, builtin$draws$sigma, tolerance = 1e-5)
  expect_equal(declared$draws$v0, builtin$draws$v0, tolerance = 1e-5)
  expect_equal(summary(declared)$diagnostics$inner_boundary_attempts,
               summary(builtin)$diagnostics$inner_boundary_attempts)
})

test_that("a declared bound that exp(log()) cannot reach exactly still holds boundary draws", {
  f <- alloc_generated_case()
  expect_lt(exp(log(7.6)), 7.6)  # the case this block is about
  y <- colSums(round(clm_oracle(f$distance, f$demand, f$supply, 8, 25)$flow))
  starts <- rbind(a = c(sigma = 9, v0 = 20), b = c(sigma = 12, v0 = 50))
  lower <- c(sigma = 7.6, v0 = 1)
  upper <- c(sigma = 40, v0 = 200)
  run <- function(model) {
    fit <- fit_allocation(model, y, starts, lower, upper, loss = "poisson", gradient = TRUE,
      control = list(maxit = 300, factr = 10, pgtol = 1e-8))
    .run_allocation_bootstrap(model, fit, alloc_bootstrap_observation(), seed = 5L,
                              attempts = 10L)
  }
  builtin <- run(f$model)
  declared <- run(clm_allocation(f$inputs, family = .gaussian_copy(7.6, 40), fit_v0 = TRUE))
  # Trial values and estimates stay inside the declared bound, so replicates on it
  # are selected and kept, as for the built-in Gaussian.
  expect_identical(declared$draws$status, builtin$draws$status)
  expect_equal(declared$successful, builtin$successful)
  on_bound <- function(result) which(result$draws$sigma <= 7.6 * (1 + 1e-6))
  expect_gt(length(on_bound(builtin)), 0)
  expect_identical(on_bound(declared), on_bound(builtin))
  expect_true(all(declared$draws$sigma >= 7.6))
})

test_that("a built-in fit on a search bound that exp(log()) misses is refused as a boundary", {
  f <- alloc_generated_case()
  y <- colSums(round(clm_oracle(f$distance, f$demand, f$supply, 8, 25)$flow))
  lower <- c(sigma = 10.000002, v0 = 1)
  expect_lt(exp(log(lower[["sigma"]])), lower[["sigma"]])
  fit <- fit_allocation(f$model, y, rbind(a = c(sigma = 12, v0 = 20), b = c(sigma = 15, v0 = 50)),
    lower, c(sigma = 40, v0 = 200), loss = "poisson", gradient = TRUE,
    control = list(maxit = 300, factr = 10, pgtol = 1e-8))
  # The estimate is on the bound, not a unit in the last place below it.
  expect_identical(coef(fit)[["sigma"]], lower[["sigma"]])
  result <- .run_allocation_bootstrap(f$model, fit, alloc_bootstrap_observation(), seed = 5L,
                                      attempts = 1L)
  expect_identical(result$original$status, "original_mean_boundary")
})

test_that("an original fit whose score cannot be computed is refused, not an error", {
  f <- alloc_generated_case(); fit <- alloc_generated_fit()
  testthat::local_mocked_bindings(
    .allocation_bootstrap_diagnostic = function(...) stop("injected diagnostic failure"),
    .allocation_case_fit = function(...) stop("unexpected bootstrap refit"))
  result <- .run_allocation_bootstrap(f$model, fit, alloc_bootstrap_observation(), seed = 1L,
                                      attempts = 1L)
  expect_identical(result$status, "refused")
  expect_identical(result$original$status, "original_score_failed")
  expect_identical(result$reasons, "original_score_failed")
  expect_equal(result$attempted, 0)
  # The other fields of the original fit are as for a fit that was not scored.
  expect_equal(result$original$theta, stats::coef(fit))
  expect_true(is.na(result$original$score_norm) && is.na(result$original$rank) &&
                is.na(result$original$boundary))
  regular <- result$intervals$quantity != "regional_A"
  expect_true(all(result$intervals$status[regular] == "original_score_failed"))
  expect_true(all(is.na(result$intervals$lower) & is.na(result$intervals$upper)))
  expect_identical(result$intervals$status[!regular], "fixed_input_identity")
  # The public function agrees, and the refusal prints and summarizes.
  public <- bootstrap_allocation(f$model, fit, alloc_bootstrap_observation(), seed = 1L)
  expect_identical(public$original$status, "original_score_failed")
  expect_output(print(public), "original_score_failed|original fit score failed")
})

# Coverage note -------------------------------------------------------------------
# The coverage study covers built-in Gaussian decay with fitted sigma and v0 and
# kappa = beta = 1. Results for any other model say that they use the same
# procedure without a separate study; results for the studied model are unchanged.

# A refused result of a model, from a search in which every start fails.
.refused_bootstrap <- function(case) {
  failed <- alloc_failed_search(case, observed = case$y)
  bootstrap_allocation(case$model, failed, alloc_bootstrap_observation(), seed = 44L)
}

.gaussian_case <- function(...) {
  f <- alloc_generated_case()
  .boot_case("gaussian", c(sigma = 8, v0 = 25), starts = f$starts, lower = f$lower,
             upper = f$upper, ...)
}

# Printed text with wrapped lines joined.
.printed <- function(x) gsub("\\s+", " ", paste(utils::capture.output(print(x)), collapse = " "))

test_that("a model outside the coverage study gets a coverage note, last in the result", {
  copy <- .gaussian_copy(0)
  cases <- list(
    "another kernel" = .boot_case("exponential", c(sigma = .15, v0 = 20),
      starts = rbind(near = c(sigma = .12, v0 = 15), far = c(sigma = .3, v0 = 40)),
      lower = c(sigma = .02, v0 = 1), upper = c(sigma = 2, v0 = 200)),
    "a declared kernel" = .gaussian_case(kernel = copy,
      oracle_kernel = function(distance, sigma) exp(-(distance^2) / (2 * sigma^2))),
    "a fitted supply elasticity" = .boot_beta_case(),
    "a fixed kappa" = .gaussian_case(kappa = 1.5),
    "a fixed beta" = .gaussian_case(beta = .8))
  for (label in names(cases)) {
    result <- .refused_bootstrap(cases[[label]])
    expect_identical(result$status, "refused", info = label)
    expect_identical(result$coverage_note, .coverage_note, info = label)
    expect_identical(tail(names(result), 1L), "coverage_note", info = label)
    expect_identical(summary(result)$coverage_note, .coverage_note, info = label)
    expect_match(.printed(result), .coverage_note, fixed = TRUE, info = label)
    expect_match(.printed(summary(result)), .coverage_note, fixed = TRUE, info = label)
    # The settings are the checkpoint contract and carry no note.
    expect_identical(result$settings, .allocation_bootstrap_settings(), info = label)
  }
})

test_that("results for the studied model are unchanged and carry no note", {
  fields <- c("status", "reasons", "original", "intervals", "draws", "starts", "multiplicities",
              "settings", "observation", "seed", "planned", "attempted", "successful",
              "input_metadata", "facility_ids", "capacity_support")
  # Built-in Gaussian decay, fitted sigma and v0, kappa = beta = 1, also declared
  # as a built-in kernel (the declaration is the string).
  for (case in list(.gaussian_case(), .gaussian_case(kernel = decay_kernel("gaussian")))) {
    result <- .refused_bootstrap(case)
    expect_identical(names(result), fields)
    expect_false("coverage_note" %in% names(summary(result)))
    expect_false(grepl("Coverage of these intervals", .printed(result), fixed = TRUE))
    expect_false(grepl("Coverage of these intervals", .printed(summary(result)), fixed = TRUE))
  }
  # A completed run of the shared design (with mocked refits) has the same fields.
  run <- alloc_bootstrap_run()
  expect_identical(names(run), fields)
  expect_null(run$coverage_note)
  expect_identical(.allocation_bootstrap_coverage_note(alloc_generated_case()$model), NULL)
  expect_identical(.allocation_bootstrap_coverage_note(
    clm_allocation(alloc_generated_case()$inputs, family = "exponential", fit_v0 = TRUE)),
    .coverage_note)
})

# The number of attempts ---------------------------------------------------------
# 399 by default; another number from 199 needs 95% successes (rounded up), is
# recorded on the result and noted as outside the coverage study, and a checkpoint
# cannot be resumed with another number. The engine itself takes any planned size,
# which keeps these tests short.

test_that("the success rule is 95% of the attempts, rounded up: 380 of 399", {
  expect_identical(.allocation_bootstrap_minimum(c(99L, 399L, 999L, 1000L)),
                   c(95L, 380L, 950L, 950L))
  point <- c(sigma = 8, v0 = 25, regional_rho = .6, regional_A = 2)
  original <- list(status = "eligible")
  draws <- data.frame(success = rep(TRUE, 99L), sigma = seq(6, 10, length.out = 99L),
    v0 = seq(20, 30, length.out = 99L), regional_rho = seq(.5, .7, length.out = 99L),
    regional_A = rep(2, 99L))
  draws$success[96:99] <- FALSE
  tab <- .allocation_bootstrap_intervals(original, point, draws, planned = 99L)
  expect_identical(tab$status[tab$quantity == "sigma"], "conditional_pointwise_bootstrap")
  expect_true(all(tab$planned == 99L))
  draws$success[95] <- FALSE
  tab <- .allocation_bootstrap_intervals(original, point, draws, planned = 99L)
  expect_identical(tab$status[tab$quantity == "sigma"], "insufficient_bootstrap_success")
  # the default run keeps the settings of earlier versions exactly
  expect_null(.allocation_bootstrap_settings()$planned)
  expect_identical(.allocation_bootstrap_settings()$minimum_successful, 380L)
  expect_identical(.allocation_bootstrap_settings(999L)$minimum_successful, 950L)
  expect_identical(.allocation_bootstrap_settings(999L)$planned, 999L)
})

test_that("a run of another number of attempts is complete, recorded and noted", {
  case <- alloc_generated_case()
  fit <- alloc_generated_fit()
  boot <- testthat::with_mocked_bindings(
    .run_allocation_bootstrap(case$model, fit, alloc_bootstrap_observation(),
                              seed = 31L, planned = 99L),
    .allocation_case_fit = function(...) alloc_mock_case_fit(case))
  expect_identical(boot$status, "complete")
  expect_identical(boot$planned, 99L)
  expect_equal(boot$attempted, 99)
  expect_identical(boot$settings$minimum_successful, 95L)
  expect_true(all(is.finite(boot$intervals$lower[boot$intervals$quantity != "regional_A"])))
  expect_match(boot$coverage_note, "Coverage was evaluated with 399 attempts; this run used 99.",
               fixed = TRUE)
  # a default run carries no note for the studied model
  expect_null(.allocation_bootstrap_coverage_note(case$model, 399L))
})

test_that("bootstrap_fit() takes the number of attempts, from 199 to 1,000,000", {
  case <- alloc_generated_case()
  fit <- alloc_generated_fit()
  passed <- NULL
  testthat::with_mocked_bindings(
    bootstrap_fit(case$model, fit, alloc_bootstrap_observation(), seed = 1L,
                  attempts = 199),
    .run_allocation_bootstrap = function(..., attempts, planned) {
      passed <<- c(attempts = attempts, planned = planned)
    })
  expect_identical(passed, c(attempts = 199L, planned = 199L))
  testthat::with_mocked_bindings(
    bootstrap_allocation(case$model, fit, alloc_bootstrap_observation(), seed = 1L),
    .run_allocation_bootstrap = function(..., attempts, planned) {
      passed <<- c(attempts = attempts, planned = planned)
    })
  expect_identical(passed, c(attempts = 399L, planned = 399L))
  message <- "attempts must be a whole number from 199 to 1,000,000"
  for (bad in list(198, 250.5, NA_real_, NULL, c(399, 999), "399", 1e6 + 1, 1e10)) {
    expect_error(bootstrap_fit(case$model, fit, alloc_bootstrap_observation(), seed = 1L,
                               attempts = bad), message, fixed = TRUE)
  }
})

test_that("a checkpoint cannot be extended to more attempts", {
  case <- alloc_generated_case()
  fit <- alloc_generated_fit()
  path <- withr::local_tempfile(fileext = ".rds")
  run <- function(planned) testthat::with_mocked_bindings(
    .run_allocation_bootstrap(case$model, fit, alloc_bootstrap_observation(),
                              seed = 31L, checkpoint = path, planned = planned),
    .allocation_case_fit = function(...) alloc_mock_case_fit(case))
  expect_identical(run(99L)$status, "complete")
  expect_error(run(199L), "bootstrap checkpoint does not match")
  # the same run resumes from its own checkpoint
  expect_identical(run(99L)$status, "complete")
})

test_that("checkpoints of different sizes never resume each other, 399 and 400 included", {
  case <- alloc_generated_case()
  fit <- alloc_generated_fit()
  # 399 and 400 attempts need the same 380 successes; only the planned number differs
  expect_identical(.allocation_bootstrap_minimum(400L), 380L)
  partial <- function(path, planned) testthat::with_mocked_bindings(
    .run_allocation_bootstrap(case$model, fit, alloc_bootstrap_observation(),
                              seed = 31L, checkpoint = path, attempts = 2L, planned = planned),
    .allocation_case_fit = function(...) alloc_mock_case_fit(case))
  for (sizes in list(c(400L, 399L), c(399L, 400L), c(199L, 250L), c(250L, 199L))) {
    path <- withr::local_tempfile(fileext = ".rds")
    expect_identical(partial(path, sizes[1])$status, "incomplete")
    expect_error(partial(path, sizes[2]),
                 "does not match inputs, settings, number of attempts", info = toString(sizes))
  }
  # an interrupted run of another size resumes with its own size, without new fits
  path <- withr::local_tempfile(fileext = ".rds")
  first <- partial(path, 250L)
  fits <- 0L
  again <- testthat::with_mocked_bindings(
    .run_allocation_bootstrap(case$model, fit, alloc_bootstrap_observation(),
                              seed = 31L, checkpoint = path, attempts = 2L, planned = 250L),
    .allocation_case_fit = function(...) {
      fits <<- fits + 1L
      alloc_mock_case_fit(case)
    })
  expect_identical(fits, 0L)
  expect_identical(again$draws, first$draws)
})

test_that("the seed leaves room for the planned attempts, and the note says how it differs", {
  case <- alloc_generated_case()
  fit <- alloc_generated_fit()
  expect_error(.run_allocation_bootstrap(case$model, fit, alloc_bootstrap_observation(),
                                         seed = .Machine$integer.max - 249L, planned = 250L),
               "seed must be an explicit integer between 0 and integer.max - 250", fixed = TRUE)
  fewer <- .allocation_bootstrap_coverage_note(case$model, 250L)
  expect_match(fewer, "this run used 250. Fewer attempts give noisier intervals", fixed = TRUE)
  more <- .allocation_bootstrap_coverage_note(case$model, 999L)
  expect_identical(more, "Coverage was evaluated with 399 attempts; this run used 999.")
})

