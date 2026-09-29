.prediction_fixture <- function(family = "gaussian", fitted = FALSE,
                                supply = c(b = 6, a = 4, c = 2), v0 = 4) {
  D <- c(o3 = 3, o1 = 5, o7 = 7)
  M <- matrix(c(1, 3, 2, 4, 1, 3, 2, 4, 1), 3, 3,
              dimnames = list(names(D), names(supply)))
  prepared <- .prepare_allocation(D, supply, M, metadata = list(
    demand_units = "eligible events", demand_period = "one period",
    supply_units = "service slots", supply_period = "one period", travel_units = "minutes"))
  p <- .huff_problem(prepared, family = family, kappa = .65, beta = 1.3,
                     v0 = v0, fit_v0 = fitted, fit_beta = fitted)
  theta <- c(sigma = switch(family, gaussian = 2, exponential = .3, power = 1.8),
             if (fitted) c(v0 = 3.5, beta = 1.15))
  list(problem = p, theta = theta, distance = M, demand = D, supply = supply)
}

.prediction_oracle <- function(f, distance = f$distance, demand = NULL, loads = NULL) {
  spec <- f$problem$metadata$spec; th <- f$theta
  k <- switch(spec$family, gaussian = exp(-distance^2 / (2 * th[["sigma"]]^2)),
              exponential = exp(-th[["sigma"]] * distance), power = distance^(-th[["sigma"]]))
  beta <- if (spec$fit_beta) th[["beta"]] else spec$beta
  v0 <- if (spec$fit_v0) th[["v0"]] else spec$v0
  score <- sweep(k, 2, (.65 * f$supply)^beta, "*")
  den <- rowSums(score) + v0
  P <- score * ifelse(den > 0, 1 / den, 0)
  if (is.null(loads)) loads <- as.numeric(crossprod(P, f$demand))
  r <- rep(0, length(loads)); positive <- loads > 0
  r[positive] <- f$supply[positive] / loads[positive]
  rho <- rowSums(P); A <- as.numeric(P %*% r)
  list(allocation = P, flow = if (is.null(demand)) NULL else P * as.numeric(demand),
       rho = unname(rho), A = A, abar = ifelse(rho > 0, A / rho, NA_real_),
       outside_share = v0 * ifelse(den > 0, 1 / den, 0), utilization = loads)
}

.prediction_fit <- function(f) {
  .fit_problem_nfxp(f$problem, .prediction_oracle(f, demand = f$demand)$flow,
    init = f$theta, lower = f$theta / 4, upper = f$theta * 4,
    output = "flow", loss = .poisson_loss, loss_grad = .poisson_gradient,
    loss_args = list(), gradient = TRUE, control = list(maxit = 20))
}

test_that("fixed and fitted snapshots preserve nondefault CLM equations and overlap", {
  for (family in c("gaussian", "exponential", "power")) {
    for (fitted in c(FALSE, TRUE)) {
      f <- .prediction_fixture(family, fitted)
      oracle <- .prediction_oracle(f, demand = f$demand)
      solved <- .solve_problem(f$problem, f$theta, requested_outputs = "flow")
      fit <- .prediction_fit(f)
      for (source in list(solved, fit)) {
        x <- .predict_clm(source, f$distance, demand = f$demand,
          outputs = c("allocation", "flow", "rho", "outside_share", "A", "abar"))
        for (name in names(x$outputs)) {
          expect_equal(unname(x$outputs[[name]]), unname(oracle[[name]]), tolerance = 1e-7)
        }
        expect_true(all(x$validity$probability_valid))
        expect_true(all(x$validity$capacity_valid))
        expect_identical(x$units$travel_units, "minutes")
        expect_identical(source$prediction_snapshot$spec$kappa, .65)
        expect_identical(source$prediction_snapshot$spec$beta, 1.3)
      }
      expect_equal(solved$prediction_snapshot$utilization, oracle$utilization, tolerance = 1e-12)
      expect_equal(fit$prediction_snapshot$theta, fit$theta)
      expect_identical(fit$prediction_snapshot, fit$equilibrium$prediction_snapshot)
      plain <- function(x) if (is.list(x)) all(vapply(x, plain, logical(1))) else is.atomic(x)
      expect_true(plain(solved$prediction_snapshot))
      expect_false(any(c("problem", "distance", "allocation", "template", "origin_ids") %in%
                       names(solved$prediction_snapshot)))
    }
  }
})

test_that("requested axes and demand align independently without affecting saved loads", {
  f <- .prediction_fixture()
  source <- .solve_problem(f$problem, f$theta)
  saved <- source
  M <- f$distance; rownames(M) <- c("new3", "new1", "new7")
  D <- c(new3 = 2, new1 = 0, new7 = NA_real_)
  outputs <- c("allocation", "flow", "rho", "A", "abar")
  x <- .predict_clm(source, M, demand = D, outputs = outputs)
  z <- .predict_clm(source, M[c(3, 1, 2), c(2, 3, 1)], origin_ids = rownames(M),
                    demand = D[c(2, 3, 1)], outputs = outputs)
  expect_identical(x, z)
  reverse <- .predict_clm(source, M, origin_ids = rev(rownames(M)),
                          demand = D, outputs = outputs)
  expect_equal(reverse$outputs$allocation, x$outputs$allocation[3:1, ])
  expect_identical(reverse$origin_ids, rev(rownames(M)))
  expect_identical(source, saved)
  expect_identical(x$validity$demand_status, c("positive", "zero", "unknown"))
  expect_true(all(x$outputs$flow[2, ] == 0))
  expect_true(all(is.na(x$outputs$flow[3, ])))
  huge <- .predict_clm(source, M, demand = rep(1e8, 3), outputs = outputs)
  expect_identical(huge$outputs$allocation, x$outputs$allocation)
  expect_identical(huge$facilities$utilization, source$prediction_snapshot$utilization)
  expect_error(.predict_clm(source, M[, -1]), "matrix")
  bad <- M; colnames(bad)[1] <- "other"
  expect_error(.predict_clm(source, bad), "exactly match")
  bad <- M; colnames(bad)[1] <- colnames(bad)[2]
  expect_error(.predict_clm(source, bad), "unique")
  expect_error(.predict_clm(source, unname(M), origin_ids = rownames(M)), "column IDs")
  expect_error(.predict_clm(source, M, origin_ids = c("x", "x", "z")), "unique")
  expect_error(.predict_clm(source, M, demand = c(1, -1, 2)), "demand")
  expect_error(.predict_clm(source, M, demand = c(1, Inf, 2)), "demand")
  expect_error(.predict_clm(source, M, demand = c(wrong = 1, new1 = 2, new7 = 3)), "exactly match")
})

test_that("unknown travel and guarded zero denominators remain distinct", {
  f <- .prediction_fixture()
  source <- .solve_problem(f$problem, f$theta)
  M <- rbind(unknown = c(1, NA, 2), absent = c(Inf, Inf, Inf), known = c(1, 2, 3))
  colnames(M) <- names(f$supply)
  out <- c("rho", "outside_share", "A", "abar", "allocation", "flow")
  x <- .predict_clm(source, M, demand = c(0, NA, 0), outputs = out)
  expect_identical(x$validity$status, c("unknown_travel", "known_absence", "valid"))
  expect_identical(x$validity$probability_valid, c(FALSE, TRUE, TRUE))
  expect_true(all(is.na(x$outputs$allocation[1, ])))
  expect_true(all(is.na(x$outputs$flow[1:2, ])))
  expect_true(all(x$outputs$flow[3, ] == 0))
  expect_equal(unname(x$outputs$rho[2]), 0)
  expect_equal(unname(x$outputs$outside_share[2]), 1)
  expect_true(is.na(x$outputs$abar[2]))
  imputed <- .predict_clm(source, M, demand = c(0, 1, 2), outputs = out,
                          missing_travel = "legacy_zero")
  expect_true(imputed$validity$unknown_travel[1])
  expect_true(imputed$validity$legacy_imputed[1])
  expect_true(imputed$validity$probability_valid[1])
  expect_true(all(imputed$outputs$flow[1, ] == 0))
  closed <- .prediction_fixture(v0 = 0)
  y <- .predict_clm(.solve_problem(closed$problem, closed$theta), M[2, , drop = FALSE],
                    demand = 5, outputs = out)
  expect_identical(y$validity$status, "undefined_choice_denominator")
  expect_false(y$validity$probability_valid)
  expect_false(y$validity$capacity_valid)
  expect_equal(as.numeric(y$outputs$allocation), c(0, 0, 0))
  expect_equal(as.numeric(y$outputs$flow), c(0, 0, 0))
  expect_equal(unname(y$outputs$rho + y$outputs$outside_share), 0)
  expect_true(is.na(y$outputs$abar))
  M[1, 1] <- -Inf
  expect_error(.predict_clm(source, M, missing_travel = "legacy_zero"), "nonnegative")
})

test_that("new contact with a zero-load facility leaves capacity explicitly unsupported", {
  f <- .prediction_fixture()
  M <- f$distance; M[, 3] <- Inf
  p <- .huff_problem(.prepare_allocation(f$demand, f$supply, M), kappa = .65, beta = 1.3, v0 = 4)
  source <- .solve_problem(p, f$theta)
  request <- matrix(c(1, 2, 1), 1, dimnames = list("new", names(f$supply)))
  x <- .predict_clm(source, request, demand = 10,
    outputs = c("rho", "A", "abar", "A_supported", "abar_supported", "allocation", "flow", "unsupported_contact"))
  expect_true(x$validity$probability_valid)
  expect_false(x$validity$capacity_valid)
  expect_identical(x$validity$status, "unsupported_capacity")
  expect_gt(x$outputs$unsupported_contact, 0)
  expect_equal(unname(x$outputs$unsupported_contact), unname(x$outputs$allocation[, 3]))
  expect_true(is.na(x$outputs$A)); expect_true(is.na(x$outputs$abar))
  expect_true(is.finite(x$outputs$A_supported))
  expect_equal(x$outputs$abar_supported, x$outputs$A_supported / x$outputs$rho)
  expect_true(all(is.finite(x$outputs$flow)))
  expect_equal(source$prediction_snapshot$utilization[3], 0)
})

test_that("scenario snapshots remain coherent and reject edited redundant records", {
  f <- .prediction_fixture(fitted = TRUE)
  fit <- .prediction_fit(f)
  base <- .predict_clm(fit, f$distance)
  scenario <- .prediction_fixture(fitted = TRUE, supply = c(b = 9, a = 4, c = 2))
  solved <- .solve_problem(scenario$problem, f$theta)
  changed <- .predict_clm(solved, f$distance)
  ref <- .prediction_oracle(scenario)
  expect_equal(unname(changed$outputs$rho), ref$rho)
  expect_equal(unname(changed$outputs$A), ref$A)
  expect_false(isTRUE(all.equal(changed$outputs$rho, base$outputs$rho)))
  expect_identical(solved$prediction_snapshot$theta, fit$theta)
  for (field in c("theta", "theta_hat")) {
    bad <- fit; bad[[field]][1] <- bad[[field]][1] * 2
    expect_error(.predict_clm(bad, f$distance), "conflicts")
  }
  bad <- fit; bad$coverage_meta$supply[1] <- 99
  expect_error(.predict_clm(bad, f$distance), "conflicts")
  bad <- fit; bad$coverage_meta$facility_ids <- rev(bad$coverage_meta$facility_ids)
  expect_error(.predict_clm(bad, f$distance), "conflicts")
  bad <- fit; bad$outputs$utilization[1] <- 99
  expect_error(.predict_clm(bad, f$distance), "conflicts")
  bad <- fit; bad$prediction_snapshot <- solved$prediction_snapshot
  expect_error(.predict_clm(bad, f$distance), "conflicts")
  bad <- solved; bad$utilization[1] <- 99
  expect_error(.predict_clm(bad, f$distance), "conflicts")
  for (side in c("prediction_snapshot", "evaluated_clm")) {
    bad <- solved; bad[[side]]$supply[1] <- 99
    expect_error(.predict_clm(bad, f$distance), "conflicts")
    bad <- solved; bad[[side]]$spec$v0 <- 99
    expect_error(.predict_clm(bad, f$distance), "conflicts")
    bad <- solved; bad[[side]]$facility_ids <- rev(bad[[side]]$facility_ids)
    expect_error(.predict_clm(bad, f$distance), "conflicts")
  }
  bad <- fit; bad$prediction_snapshot$theta[["beta"]] <- 0
  expect_error(.predict_clm(bad, f$distance), "beta must be positive")
  bad <- fit; bad$prediction_snapshot$schema <- "old"
  expect_error(.predict_clm(bad, f$distance), "regenerate")
  bad <- fit; bad$prediction_snapshot <- NULL
  expect_error(.predict_clm(bad, f$distance), "regenerate")
  bad <- fit; bad$equilibrium$converged <- FALSE
  expect_error(.predict_clm(bad, f$distance), "successful")
  fit$convergence <- 99L
  expect_no_error(.predict_clm(fit, f$distance))
  expect_error(.predict_clm(f$problem, f$distance), "solved or fitted")
  legacy <- .huff_problem(.prepare_allocation(f$demand, f$supply, f$distance), allocation = "huff_decay")
  expect_error(.predict_clm(.solve_problem(legacy, c(sigma = 2)), f$distance), "regenerate")
})

test_that("legacy missing travel needs explicit imputation and domain guards are visible", {
  f <- .prediction_fixture(); M <- f$distance; M[1, 2] <- NA_real_
  d <- terra::rast(nrows = 1, ncols = 3); terra::values(d) <- f$demand
  travel <- c(d, d, d); terra::values(travel) <- M; names(travel) <- names(f$supply)
  p <- .huff_problem(d, f$supply, travel, kappa = .65, beta = 1.3, v0 = 4)
  source <- .solve_problem(p, f$theta)
  pred <- .predict_clm(source, M, outputs = "allocation")
  expect_true(all(is.na(pred$outputs$allocation[1, ])))
  legacy <- .predict_clm(source, M, outputs = "allocation", missing_travel = "legacy_zero")
  expect_equal(unname(legacy$outputs$allocation), unname(source$outputs$allocation))
  expect_true(legacy$validity$unknown_travel[1]); expect_true(legacy$validity$legacy_imputed[1])
  power <- .prediction_fixture("power")
  strict <- .solve_problem(power$problem, power$theta)
  zeros <- f$distance; zeros[1, 1] <- 0
  expect_error(.predict_clm(strict, zeros, missing_travel = "legacy_zero"), "strictly positive")
  legacy_power <- .solve_problem(.huff_problem(d, f$supply, travel, family = "power"), c(sigma = 2))
  expect_error(.predict_clm(legacy_power, zeros), "strictly positive")
  guarded <- .predict_clm(legacy_power, zeros, missing_travel = "legacy_zero")
  expect_true(guarded$validity$legacy_kernel_imputed[1])
})

test_that("vector requests do not solve load data or materialize matrix outputs", {
  f <- .prediction_fixture(); source <- .solve_problem(f$problem, f$theta)
  testthat::local_mocked_bindings(.bind_theta = function(...) stop("unexpected bind"),
    .solve_problem = function(...) stop("unexpected solve"),
    .fit_problem_nfxp = function(...) stop("unexpected fit"),
    .huff_state = function(...) stop("unexpected rich evaluation"),
    .clm_flow = function(...) stop("unexpected flow"),
    .clm_prediction_flow = function(...) stop("unexpected flow"),
    .rewrap_cells = function(...) stop("unexpected map"))
  testthat::local_mocked_bindings(values = function(...) stop("unexpected raster read"),
    extract = function(...) stop("unexpected raster extraction"), .package = "terra")
  x <- .predict_clm(source, f$distance)
  expect_named(x$outputs, c("rho", "A", "abar"))
  expect_false(any(vapply(x$outputs, is.matrix, logical(1))))
  expect_null(x$spatial$template)
  expect_error(.prediction_surface(x), "surface metadata")
})

test_that("normalization order preserves finite counts and capacity at extreme scales", {
  M <- matrix(1, 1, 1, dimnames = list("old", "f"))
  p <- .huff_problem(.prepare_allocation(c(old = 1), c(f = 1), M), v0 = 1e-308)
  source <- .solve_problem(p, c(sigma = 1))
  request <- matrix(sqrt(-2 * log(1e-308)), 1, 1, dimnames = list("new", "f"))
  flow <- .predict_clm(source, request, demand = 10, outputs = "flow")
  both <- .predict_clm(source, request, demand = 10, outputs = c("allocation", "flow"))
  expect_true(is.finite(flow$outputs$flow[1]))
  expect_equal(flow$outputs$flow, both$outputs$flow, tolerance = 1e-12)
  expect_equal(as.numeric(flow$outputs$flow), 5, tolerance = 1e-10)
  expect_false("allocation" %in% names(flow$outputs))

  # score*(S/U) would overflow, but normalized (P*S)/U is finite.
  p <- .huff_problem(.prepare_allocation(c(old = 1e-100), c(f = 1e200), M),
                     beta = 1, v0 = 1)
  source <- .solve_problem(p, c(sigma = 1))
  x <- .predict_clm(source, M, outputs = c("rho", "A", "abar"))
  stable <- ((exp(-.5) * 1e200) / (exp(-.5) * 1e200 + 1) * 1e200) /
    source$prediction_snapshot$utilization[1]
  expect_equal(as.numeric(x$outputs$A), stable, tolerance = 1e-12)
  expect_true(x$validity$capacity_valid)
  expect_false(x$validity$capacity_overflow)

  # When the final capacity itself cannot fit in a double, probability remains valid.
  p <- .huff_problem(.prepare_allocation(c(old = 1e-200), c(f = 1e200), M), v0 = 1)
  source <- .solve_problem(p, c(sigma = 1))
  probability <- .predict_clm(source, M, demand = 10, outputs = c("rho", "allocation", "flow"))
  expect_true(probability$validity$probability_valid)
  expect_false(probability$validity$capacity_valid)
  expect_true(probability$validity$capacity_overflow)
  expect_identical(probability$validity$status, "capacity_numerical_overflow")
  expect_true(all(is.finite(probability$outputs$allocation)))
  expect_true(all(is.finite(probability$outputs$flow)))
  capacity <- .predict_clm(source, M, outputs = c("A", "abar", "A_supported", "abar_supported"))
  expect_true(all(is.na(unlist(capacity$outputs))))

  # Multiplying P*S first would underflow, while P*(S/U) is representable.
  p <- .huff_problem(.prepare_allocation(c(old = 1e-300), c(f = 1e-200), M),
                     kappa = 1e200, v0 = 1)
  source <- .solve_problem(p, c(sigma = 1))
  request <- matrix(sqrt(-2 * log(1e-200)), 1, 1, dimnames = list("new", "f"))
  x <- .predict_clm(source, request, outputs = c("rho", "A", "abar"))
  kernel <- exp(-request[1]^2 / 2)
  stable <- (kernel / (kernel + 1)) * (1e-200 / source$prediction_snapshot$utilization[1])
  expect_gt(x$outputs$A, 0)
  expect_equal(as.numeric(x$outputs$A), stable, tolerance = 1e-12)
  expect_true(x$validity$capacity_valid)

  # An unrepresentable ratio may still have a finite requested contribution.
  p <- .huff_problem(.prepare_allocation(c(old = 1e-300), c(f = 1e100), M),
                     kappa = 1e-100, v0 = 1)
  source <- .solve_problem(p, c(sigma = 1))
  x <- .predict_clm(source, request, outputs = c("rho", "A", "abar"))
  stable <- exp(log(kernel / (kernel + 1)) + log(1e100) -
                  log(source$prediction_snapshot$utilization[1]))
  expect_equal(as.numeric(x$outputs$A), stable, tolerance = 1e-12)
  expect_true(x$validity$capacity_valid)
  expect_false(x$validity$capacity_overflow)
  expect_true(x$validity$conditional_capacity_overflow)
  expect_false(x$validity$conditional_capacity_valid)
  expect_true(is.na(x$outputs$abar))

  # Normalized P itself can underflow while count and capacity stay finite.
  p <- .huff_problem(.prepare_allocation(c(old = 1e-100), c(f = 1e100), M),
                     kappa = 1e-100, v0 = 1e200)
  source <- .solve_problem(p, c(sigma = 1))
  x <- .predict_clm(source, request, demand = 1e308,
                    outputs = c("rho", "A", "allocation", "flow"))
  log_probability <- log(kernel) - log(kernel + 1e200)
  expected_A <- exp(log_probability + log(1e100) - log(source$prediction_snapshot$utilization[1]))
  expect_true(x$validity$probability_underflow)
  expect_equal(as.numeric(x$outputs$allocation), 0)
  expect_equal(as.numeric(x$outputs$A), expected_A, tolerance = 1e-12)
  expect_equal(as.numeric(x$outputs$flow), exp(log_probability + log(1e308)), tolerance = 1e-12)
  flow_only <- .predict_clm(source, request, demand = 1e308, outputs = "flow")
  expect_identical(flow_only$outputs$flow, x$outputs$flow)
  expect_false(x$validity$conditional_capacity_valid)
})

test_that("legacy power sigma zero cannot silently change known absent-edge semantics", {
  d <- terra::rast(nrows = 1, ncols = 1); terra::values(d) <- 1
  travel <- d; terra::values(travel) <- Inf; names(travel) <- "f"
  source <- .solve_problem(.huff_problem(d, c(f = 1), travel, family = "power"), c(sigma = 0))
  M <- matrix(Inf, 1, 1, dimnames = list("new", "f"))
  expect_error(.predict_clm(source, M), "checked numeric preparation or positive sigma")
  expect_error(.predict_clm(source, M, missing_travel = "legacy_zero"), "checked numeric")
  numeric_source <- .solve_problem(.huff_problem(.prepare_allocation(c(new = 1), c(f = 1), M),
    family = "power", v0 = 2), c(sigma = 0))
  predicted <- .predict_clm(numeric_source, M, outputs = c("rho", "outside_share"))
  expect_equal(unname(predicted$outputs$rho), 0)
  expect_equal(unname(predicted$outputs$outside_share), 1)
  expect_true(predicted$validity$probability_valid)
})

test_that("ordinary and guarded prediction agree with algebra across every family", {
  flow <- .clm_prediction_flow; certificates <- logical()
  testthat::local_mocked_bindings(.clm_prediction_flow = function(score, denominator, demand,
                                                                 ordinary = FALSE) {
    certificates <<- c(certificates, ordinary)
    flow(score, denominator, demand, ordinary)
  })
  for (family in c("gaussian", "exponential", "power")) {
    f <- .prediction_fixture(family)
    source <- .solve_problem(f$problem, f$theta)
    D <- c(2, 0, NA_real_)
    x <- .predict_clm(source, f$distance, demand = D,
                       outputs = c("rho", "A", "abar", "allocation", "flow"))
    expected <- .prediction_oracle(f, demand = D)
    expect_true(tail(certificates, 1))
    for (key in names(x$outputs)) {
      expect_equal(unname(x$outputs[[key]]), unname(expected[[key]]), tolerance = 1e-12)
    }
    # Large request counts use stable column normalization despite ordinary
    # source ranges; zero and missing demand retain their distinct masks.
    huge <- .predict_clm(source, f$distance, demand = c(1e101, 0, NA_real_), outputs = "flow")
    expect_equal(huge$outputs$flow, expected$allocation * c(1e101, 0, NA_real_), tolerance = 1e-12)
    tiny_distance <- switch(family,
      gaussian = f$theta[["sigma"]] * sqrt(-2 * log(1e-110)),
      exponential = -log(1e-110) / f$theta[["sigma"]],
      power = (1e-110)^(-1 / f$theta[["sigma"]]))
    request <- matrix(tiny_distance, 1, 3, dimnames = list("tiny", names(f$supply)))
    tiny <- .predict_clm(source, request, demand = 2, outputs = c("rho", "A", "flow"))
    tiny_expected <- .prediction_oracle(f, distance = request, demand = 2,
      loads = source$prediction_snapshot$utilization)
    expect_false(tail(certificates, 1))
    expect_gt(tiny$outputs$A, 0)
    for (key in names(tiny$outputs)) {
      expect_equal(unname(tiny$outputs[[key]]), unname(tiny_expected[[key]]), tolerance = 1e-12)
    }
    # Small baseline demand gives a finite extreme S/U, forcing the guarded
    # capacity path without changing the model's allocation probabilities.
    f$demand <- f$demand * 1e-110
    f$problem <- .huff_problem(.prepare_allocation(f$demand, f$supply, f$distance),
      family = family, kappa = .65, beta = 1.3, v0 = 4)
    guarded <- .predict_clm(.solve_problem(f$problem, f$theta), f$distance, demand = D,
                            outputs = c("rho", "A", "abar", "allocation", "flow"))
    expected <- .prediction_oracle(f, demand = D)
    expect_false(tail(certificates, 1))
    for (key in names(guarded$outputs)) {
      expect_equal(unname(guarded$outputs[[key]]), unname(expected[[key]]), tolerance = 1e-12)
    }
  }
  f <- .prediction_fixture(); baseline <- f$distance; baseline[, 3] <- Inf
  p <- .huff_problem(.prepare_allocation(f$demand, f$supply, baseline),
    kappa = .65, beta = 1.3, v0 = 4)
  unsupported <- .predict_clm(.solve_problem(p, f$theta), f$distance,
    demand = f$demand, outputs = c("A", "flow", "unsupported_contact"))
  expect_true(tail(certificates, 1))
  expect_true(all(unsupported$validity$unsupported_capacity))
  expect_true(all(!unsupported$validity$capacity_valid))
  expect_true(all(is.na(unsupported$outputs$A)))
  expect_true(all(unsupported$outputs$unsupported_contact > 0))
  for (outside in c(0, 4)) {
    zero <- .prediction_fixture(supply = c(b = 0, a = 0, c = 0), v0 = outside)
    x <- .predict_clm(.solve_problem(zero$problem, zero$theta), zero$distance,
      demand = zero$demand, outputs = c("rho", "A", "abar", "flow"))
    expect_false(tail(certificates, 1))
    expect_equal(unname(x$outputs$rho), rep(0, 3))
    expect_equal(unname(x$outputs$A), rep(0, 3))
    expect_true(all(is.na(x$outputs$abar)))
    expect_true(all(x$outputs$flow == 0))
    expect_identical(x$validity$probability_valid, rep(outside > 0, 3))
  }
})

test_that("sparse nonfinite travel handling preserves original finite kernel domains", {
  for (family in c("gaussian", "exponential", "power")) {
    f <- .prediction_fixture(family); solved <- .solve_problem(f$problem, f$theta)
    mixed <- rbind(unknown = c(1, NA_real_, Inf), absent = rep(Inf, 3), finite_zero = c(0, 1, 2))
    colnames(mixed) <- names(f$supply)
    if (family == "power") {
      expect_error(.predict_clm(solved, mixed), "strictly positive")
      expect_error(.predict_clm(solved, mixed, missing_travel = "legacy_zero"), "strictly positive")
      mixed <- mixed[1:2, , drop = FALSE]
    }
    x <- .predict_clm(solved, mixed, outputs = c("rho", "outside_share"))
    expect_true(is.na(x$outputs$rho[1]))
    expect_equal(unname(x$outputs$rho[2]), 0)
    expect_equal(unname(x$outputs$outside_share[2]), 1)
    expect_identical(x$validity$unknown_travel, c(TRUE, rep(FALSE, nrow(mixed) - 1L)))
    expect_true(all(!x$validity$legacy_kernel_imputed))
    if (family != "power") expect_gt(x$outputs$rho[3], 0)
  }
  d <- terra::rast(nrows = 1, ncols = 1); terra::values(d) <- 1
  travel <- d; names(travel) <- "f"
  source <- .solve_problem(.huff_problem(d, c(f = 1), travel, v0 = 1), c(sigma = 0))
  request <- matrix(c(0, NA_real_, Inf, 1), ncol = 1,
                    dimnames = list(c("finite_zero", "unknown", "absent", "known"), "f"))
  expect_error(.predict_clm(source, request), "positive sigma")
  x <- .predict_clm(source, request, demand = c(1, 0, NA, 2),
                    outputs = c("rho", "outside_share", "flow"), missing_travel = "legacy_zero")
  expect_identical(x$validity$legacy_kernel_imputed, c(TRUE, FALSE, FALSE, FALSE))
  expect_identical(x$validity$unknown_travel, c(FALSE, TRUE, FALSE, FALSE))
  expect_equal(unname(x$outputs$rho), rep(0, 4))
  expect_equal(unname(x$outputs$outside_share), rep(1, 4))
  expect_equal(as.numeric(x$outputs$flow), c(0, 0, NA, 0))
  M <- matrix(1, 1, 1, dimnames = list("old", "f"))
  strict <- .solve_problem(.huff_problem(.prepare_allocation(c(old = 1), c(f = 1), M),
    family = "power", v0 = 1), c(sigma = 2))
  tiny <- M; tiny[] <- 1e-200
  expect_error(.predict_clm(strict, tiny), "kernel is invalid on finite")
  tiny[] <- -Inf
  expect_error(.predict_clm(strict, tiny, missing_travel = "legacy_zero"), "nonnegative")
})

test_that("rounded unsupported mass cannot certify capacity support", {
  baseline <- matrix(c(Inf, 1), 1, 2, dimnames = list("old", c("unused", "used")))
  supply <- c(unused = 1e-200, used = 1e200)
  source <- .solve_problem(.huff_problem(.prepare_allocation(c(old = 1), supply, baseline),
    v0 = 1), c(sigma = 1))
  request <- matrix(1, 1, 2, dimnames = list("new", names(supply)))
  x <- .predict_clm(source, request,
    outputs = c("rho", "A", "abar", "A_supported", "unsupported_contact", "allocation"))
  expect_equal(unname(source$prediction_snapshot$utilization[1]), 0)
  expect_equal(unname(x$outputs$unsupported_contact), 0)
  expect_equal(unname(x$outputs$allocation[1, 1]), 0)
  expect_true(x$validity$probability_underflow)
  expect_true(x$validity$unsupported_capacity)
  expect_true(x$validity$probability_valid)
  expect_false(x$validity$capacity_valid)
  expect_false(x$validity$conditional_capacity_valid)
  expect_identical(x$validity$status, "unsupported_capacity")
  expect_true(is.na(x$outputs$A))
  expect_true(is.na(x$outputs$abar))
  expect_equal(unname(x$outputs$rho), 1)
  expect_equal(unname(x$outputs$A_supported), supply[["used"]] /
    source$prediction_snapshot$utilization[2], tolerance = 1e-12)
})

test_that("requested maps preserve distinct cell indices and portable geometry", {
  f <- .prediction_fixture(); source <- .solve_problem(f$problem, f$theta)
  template <- terra::rast(nrows = 2, ncols = 4, xmin = 20, xmax = 24, ymin = 0, ymax = 2)
  cells <- c(o3 = 7, o1 = 2, o7 = 5)
  x <- .predict_clm(source, f$distance, spatial = list(template = template, cell_index = cells[c(3, 1, 2)]))
  map <- .prediction_surface(x)
  expect_equal(terra::values(map)[cells, 1], unname(x$outputs$A), ignore_attr = TRUE)
  expect_true(all(is.na(terra::values(map)[-cells, 1])))
  expect_s3_class(x$spatial$template, "ae_spatial_template")
  for (bad in list(c(1, 1, 2), c(0, 2, 3), c(2, 3, 9), c(2.5, 3, 4))) {
    expect_error(.predict_clm(source, f$distance,
      spatial = list(template = template, cell_index = bad)), "unique valid")
  }
  expect_error(.predict_clm(source, f$distance,
    spatial = list(template = template, cell_index = c(wrong = 1, o1 = 2, o7 = 3))), "exactly match")
  P <- .predict_clm(source, f$distance, outputs = "allocation",
                    spatial = list(template = template, cell_index = cells))
  expect_error(.prediction_surface(P, "allocation"), "not an origin-side")
  expect_error(.prediction_surface(x, "flow"), "requested output")
})

test_that("fresh source or installed processes predict from saved plain snapshots", {
  cases <- list()
  for (family in c("gaussian", "exponential", "power")) for (fitted in c(FALSE, TRUE)) {
    f <- .prediction_fixture(family, fitted)
    request <- f$distance[c(3, 1), ]; rownames(request) <- c("newA", "newB")
    for (source in list(.solve_problem(f$problem, f$theta), .prediction_fit(f))) {
      cases[[length(cases) + 1L]] <- list(source = source, request = request,
        expected = .predict_clm(source, request, outputs = c("rho", "A", "abar", "flow")))
    }
  }
  f <- .prediction_fixture()
  template <- terra::rast(nrows = 2, ncols = 4, xmin = 20, xmax = 24, ymin = 0, ymax = 2)
  mapped <- .predict_clm(.solve_problem(f$problem, f$theta), f$distance,
    spatial = list(template = template, cell_index = c(o3 = 7, o1 = 2, o7 = 5)))
  original_map <- .prediction_surface(mapped)
  saved <- tempfile(fileext = ".rds"); result <- tempfile(fileext = ".rds")
  script <- tempfile(fileext = ".R")
  on.exit(unlink(c(saved, result, script)), add = TRUE)
  saveRDS(list(cases = cases, mapped = mapped), saved)
  package_path <- getNamespaceInfo(asNamespace("spax"), "path")
  package_mode <- if (pkgload::is_dev_package("spax")) "source" else "installed"
  writeLines(c("a <- commandArgs(TRUE)",
    "if (a[4] == 'source') pkgload::load_all(a[1], quiet = TRUE) else library('spax', lib.loc = dirname(a[1]))",
    "x <- readRDS(a[2])",
    "z <- lapply(x$cases, function(case) spax:::.predict_clm(case$source, case$request, outputs=c('rho','A','abar','flow')))",
    "m <- spax:::.prediction_surface(x$mapped)",
    "saveRDS(list(predictions=z, values=terra::values(m), extent=as.vector(terra::ext(m)), crs=terra::crs(m)),a[3])"), script)
  status <- system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(script), shQuote(package_path), shQuote(saved), shQuote(result), package_mode),
    stdout = TRUE, stderr = TRUE)
  expect_null(attr(status, "status"), info = paste(status, collapse = "\n"))
  restored <- readRDS(result)
  expect_identical(restored$predictions, lapply(cases, `[[`, "expected"))
  expect_equal(restored$values, terra::values(original_map))
  expect_equal(restored$extent, as.vector(terra::ext(template)))
  expect_identical(restored$crs, terra::crs(template))
})
