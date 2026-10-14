# Requested-location prediction from saved static CLM snapshots
# (predict_allocation() and its private engine .predict_clm()). Sources built
# from legacy raster inputs are tested in test-06-legacy_providers.R; reload
# in a fresh process is tested in test-90-allocation_portability.R.

test_that("fixed and fitted snapshots preserve nondefault CLM equations", {
  outputs <- c("allocation", "flow", "rho", "outside_share", "A", "abar")
  for (entry in alloc_prediction_sources()) {
    case <- entry$case
    oracle <- alloc_prediction_oracle(case, demand = case$demand)
    for (source in entry[c("solved", "fit")]) {
      x <- predict_allocation(source, case$distance, demand = case$demand, outputs = outputs)
      expect_equal(lapply(x$outputs, unname), lapply(oracle[outputs], unname), tolerance = 1e-7)
      expect_true(all(x$validity$probability_valid & x$validity$capacity_valid))
      expect_identical(source$prediction_snapshot$spec[c("kappa", "beta")],
                       list(kappa = .65, beta = 1.3))
    }
    expect_equal(entry$solved$prediction_snapshot$utilization, oracle$utilization,
                 tolerance = 1e-12)
  }
  expect_identical(x$units$travel_units, "minutes")
  # Snapshot content, on the Gaussian sources with fitted v0 and beta.
  entry <- alloc_prediction_sources()[["gaussian fitted"]]
  expect_equal(entry$fit$prediction_snapshot$theta, entry$fit$theta)
  expect_identical(entry$fit$prediction_snapshot, entry$fit$equilibrium$prediction_snapshot)
  plain <- function(x) if (is.list(x)) all(vapply(x, plain, logical(1))) else is.atomic(x)
  snapshot <- entry$solved$prediction_snapshot
  expect_true(plain(snapshot))
  expect_false(any(c("problem", "distance", "allocation", "template", "origin_ids") %in%
                   names(snapshot)))
})

test_that("requested axes and demand align independently without affecting saved loads", {
  source <- alloc_prediction_sources()[["gaussian fixed"]]$solved
  saved <- source
  M <- alloc_prediction_case()$distance; rownames(M) <- c("new3", "new1", "new7")
  D <- c(new3 = 2, new1 = 0, new7 = NA_real_)
  outputs <- c("allocation", "flow", "rho", "A", "abar")
  x <- predict_allocation(source, M, demand = D, outputs = outputs)
  z <- predict_allocation(source, M[c(3, 1, 2), c(2, 3, 1)], origin_ids = rownames(M),
                          demand = D[c(2, 3, 1)], outputs = outputs)
  expect_identical(x, z)
  reverse <- predict_allocation(source, M, origin_ids = rev(rownames(M)),
                                demand = D, outputs = outputs)
  expect_equal(reverse$outputs$allocation, x$outputs$allocation[3:1, ])
  expect_identical(reverse$origin_ids, rev(rownames(M)))
  expect_identical(source, saved)
  expect_identical(x$validity$demand_status, c("positive", "zero", "unknown"))
  expect_true(all(x$outputs$flow[2, ] == 0))
  expect_true(all(is.na(x$outputs$flow[3, ])))
  huge <- predict_allocation(source, M, demand = rep(1e8, 3), outputs = outputs)
  expect_identical(huge$outputs$allocation, x$outputs$allocation)
  expect_identical(huge$facilities$utilization, source$prediction_snapshot$utilization)
  expect_error(predict_allocation(source, M[, -1]), "matrix")
  bad <- M; colnames(bad)[1] <- "other"
  expect_error(predict_allocation(source, bad), "exactly match")
  bad <- M; colnames(bad)[1] <- colnames(bad)[2]
  expect_error(predict_allocation(source, bad), "unique")
  expect_error(predict_allocation(source, unname(M), origin_ids = rownames(M)), "column IDs")
  expect_error(predict_allocation(source, M, origin_ids = c("x", "x", "z")), "unique")
  expect_error(predict_allocation(source, M, demand = c(1, -1, 2)), "demand")
  expect_error(predict_allocation(source, M, demand = c(1, Inf, 2)), "demand")
  expect_error(predict_allocation(source, M, demand = c(wrong = 1, new1 = 2, new7 = 3)),
               "exactly match")
  expect_error(predict_allocation(source, M, outputs = "utilization"), "unsupported prediction outputs")
})

test_that("unknown travel, known absence and zero denominators remain distinct", {
  source <- alloc_prediction_sources()[["gaussian fixed"]]$solved
  M <- rbind(unknown = c(1, NA, Inf), absent = c(Inf, Inf, Inf), known = c(1, 2, 3))
  colnames(M) <- c("b", "a", "c")
  out <- c("rho", "outside_share", "A", "abar", "allocation", "flow")
  x <- predict_allocation(source, M, demand = c(0, NA, 0), outputs = out)
  expect_identical(x$validity$status, c("unknown_travel", "known_absence", "valid"))
  expect_identical(x$validity$probability_valid, c(FALSE, TRUE, TRUE))
  expect_identical(x$validity$unknown_travel, c(TRUE, FALSE, FALSE))
  expect_false(any(x$validity$legacy_kernel_imputed))
  expect_true(is.na(x$outputs$rho[1]))
  expect_true(all(is.na(x$outputs$allocation[1, ])))
  expect_true(all(is.na(x$outputs$flow[1:2, ])))
  expect_true(all(x$outputs$flow[3, ] == 0))
  expect_equal(unname(x$outputs$rho[2]), 0)
  expect_equal(unname(x$outputs$outside_share[2]), 1)
  expect_true(is.na(x$outputs$abar[2]))
  # Explicit legacy_zero imputation on a checked source only omits the unknown edge.
  imputed <- predict_allocation(source, M, demand = c(0, 1, 2), outputs = out,
                                missing_travel = "legacy_zero")
  expect_true(imputed$validity$unknown_travel[1])
  expect_true(imputed$validity$legacy_imputed[1])
  expect_true(imputed$validity$probability_valid[1])
  expect_true(all(imputed$outputs$flow[1, ] == 0))
  # No outside option and no inside opportunity keep the zero-mass guard.
  closed <- alloc_prediction_case(v0 = 0)
  y <- predict_allocation(.solve_problem(closed$model, closed$theta), M[2, , drop = FALSE],
                          demand = 5, outputs = out)
  expect_identical(y$validity$status, "undefined_choice_denominator")
  expect_false(y$validity$probability_valid)
  expect_false(y$validity$capacity_valid)
  expect_equal(as.numeric(y$outputs$allocation), c(0, 0, 0))
  expect_equal(as.numeric(y$outputs$flow), c(0, 0, 0))
  expect_equal(unname(y$outputs$rho + y$outputs$outside_share), 0)
  expect_true(is.na(y$outputs$abar))
  # Finite zero travel is known contact except in the power kernel's domain.
  zero <- M[3, , drop = FALSE]; zero[1, 1] <- 0
  for (family in c("gaussian", "exponential")) {
    solved <- alloc_prediction_sources()[[paste(family, "fixed")]]$solved
    expect_gt(predict_allocation(solved, zero)$outputs$rho, 0)
  }
  power <- alloc_prediction_sources()[["power fixed"]]$solved
  expect_error(predict_allocation(power, zero), "strictly positive")
  expect_error(predict_allocation(power, zero, missing_travel = "legacy_zero"), "strictly positive")
  one <- matrix(1, 1, 1, dimnames = list("old", "f"))
  strict <- .solve_problem(clm_allocation(prepare_allocation(c(old = 1), c(f = 1), one),
                                          family = "power", v0 = 1), c(sigma = 2))
  expect_error(predict_allocation(strict, one * 1e-200), "kernel is invalid on finite")
  expect_error(predict_allocation(strict, one * -Inf, missing_travel = "legacy_zero"), "nonnegative")
  # A checked power source keeps +Inf as known absence even at sigma = 0.
  absent <- matrix(Inf, 1, 1, dimnames = list("new", "f"))
  numeric_source <- .solve_problem(clm_allocation(prepare_allocation(c(new = 1), c(f = 1), absent),
                                                  family = "power", v0 = 2), c(sigma = 0))
  predicted <- predict_allocation(numeric_source, absent, outputs = c("rho", "outside_share"))
  expect_equal(unname(predicted$outputs$rho), 0)
  expect_equal(unname(predicted$outputs$outside_share), 1)
  expect_true(predicted$validity$probability_valid)
})

test_that("scenario snapshots remain coherent and edited records are rejected", {
  entry <- alloc_prediction_sources()[["gaussian fitted"]]
  case <- entry$case; fit <- entry$fit
  base <- predict_allocation(fit, case$distance)
  scenario <- alloc_prediction_case(fitted = TRUE, supply = c(b = 9, a = 4, c = 2))
  solved <- .solve_problem(scenario$model, case$theta)
  changed <- predict_allocation(solved, case$distance)
  ref <- alloc_prediction_oracle(scenario)
  expect_equal(unname(changed$outputs$rho), ref$rho)
  expect_equal(unname(changed$outputs$A), ref$A)
  expect_false(isTRUE(all.equal(changed$outputs$rho, base$outputs$rho)))
  expect_identical(solved$prediction_snapshot$theta, fit$theta)
  # Redundant small records must agree with the snapshot.
  bad <- fit; bad$theta[1] <- bad$theta[1] * 2
  expect_error(predict_allocation(bad, case$distance), "conflicts")
  bad <- fit; bad$coverage_meta$facility_ids <- rev(bad$coverage_meta$facility_ids)
  expect_error(predict_allocation(bad, case$distance), "conflicts")
  bad <- fit; bad$prediction_snapshot <- solved$prediction_snapshot
  expect_error(predict_allocation(bad, case$distance), "conflicts")
  bad <- solved; bad$utilization[1] <- 99
  expect_error(predict_allocation(bad, case$distance), "conflicts")
  bad <- solved; bad$evaluated_clm$spec$v0 <- 99
  expect_error(predict_allocation(bad, case$distance), "conflicts")
  # Each snapshot field is validated on its own.
  edits <- list(
    "snapshot specification" = function(s) { s$spec$allocation <- "huff_decay"; s },
    "snapshot kappa" = function(s) { s$spec$kappa <- -1; s },
    "parameter flags" = function(s) { s$spec$fit_v0 <- NA; s },
    "snapshot theta" = function(s) { names(s$theta)[1] <- "scale"; s },
    "beta must be positive" = function(s) { s$theta[["beta"]] <- 0; s },
    "must be strings" = function(s) { s$facility_ids <- factor(s$facility_ids); s },
    "snapshot supply" = function(s) { s$supply[1] <- -1; s },
    "input metadata" = function(s) { s$input_metadata$travel_units <- NULL; s },
    "input policy" = function(s) { s$input_policy$source <- "other"; s },
    "guard policy" = function(s) { s$provenance$converged <- FALSE; s },
    "regenerate" = function(s) { s$schema <- "old"; s })
  for (message in names(edits)) {
    bad <- solved; bad$prediction_snapshot <- edits[[message]](bad$prediction_snapshot)
    expect_error(predict_allocation(bad, case$distance), message)
  }
  bad <- fit; bad$prediction_snapshot <- NULL
  expect_error(predict_allocation(bad, case$distance), "regenerate")
  bad <- solved; bad$evaluated_clm <- NULL
  expect_error(predict_allocation(bad, case$distance), "evaluated CLM identity")
  bad <- solved; bad$converged <- FALSE
  expect_error(predict_allocation(bad, case$distance), "successful")
  bad <- fit; bad$equilibrium$converged <- FALSE
  expect_error(predict_allocation(bad, case$distance), "successful")
  # Outer optimizer status does not invalidate a successful final evaluation.
  fit$convergence <- 99L
  expect_no_error(predict_allocation(fit, case$distance))
  expect_error(predict_allocation(case$model, case$distance), "solved or fitted")
})

test_that("vector requests do not solve load data or materialize matrix outputs", {
  entry <- alloc_prediction_sources()[["gaussian fixed"]]
  testthat::local_mocked_bindings(
    .solve_problem = function(...) stop("unexpected solve"),
    .fit_problem_nfxp = function(...) stop("unexpected fit"))
  testthat::local_mocked_bindings(values = function(...) stop("unexpected raster read"),
    extract = function(...) stop("unexpected raster extraction"), .package = "terra")
  x <- predict_allocation(entry$solved, entry$case$distance)
  expect_named(x$outputs, c("rho", "A", "abar"))
  expect_false(any(vapply(x$outputs, is.matrix, logical(1))))
  expect_null(x$spatial$template)
  expect_error(.prediction_surface(x), "surface metadata")
})

test_that("normalization order preserves finite counts and capacity at extreme scales", {
  M <- matrix(1, 1, 1, dimnames = list("old", "f"))
  system <- function(demand, supply, ...) {
    clm_allocation(prepare_allocation(c(old = demand), c(f = supply), M), ...)
  }
  source <- .solve_problem(system(1, 1, v0 = 1e-308), c(sigma = 1))
  request <- matrix(sqrt(-2 * log(1e-308)), 1, 1, dimnames = list("new", "f"))
  flow <- predict_allocation(source, request, demand = 10, outputs = "flow")
  both <- predict_allocation(source, request, demand = 10, outputs = c("allocation", "flow"))
  expect_true(is.finite(flow$outputs$flow[1]))
  expect_equal(flow$outputs$flow, both$outputs$flow, tolerance = 1e-12)
  expect_equal(as.numeric(flow$outputs$flow), 5, tolerance = 1e-10)
  expect_false("allocation" %in% names(flow$outputs))

  # score*(S/U) would overflow, but normalized (P*S)/U is finite.
  source <- .solve_problem(system(1e-100, 1e200, v0 = 1), c(sigma = 1))
  x <- predict_allocation(source, M, outputs = c("rho", "A", "abar"))
  stable <- ((exp(-.5) * 1e200) / (exp(-.5) * 1e200 + 1) * 1e200) /
    source$prediction_snapshot$utilization[1]
  expect_equal(as.numeric(x$outputs$A), stable, tolerance = 1e-12)
  expect_true(x$validity$capacity_valid)
  expect_false(x$validity$capacity_overflow)

  # When the final capacity itself cannot fit in a double, probability remains valid.
  source <- .solve_problem(system(1e-200, 1e200, v0 = 1), c(sigma = 1))
  probability <- predict_allocation(source, M, demand = 10, outputs = c("rho", "allocation", "flow"))
  expect_true(probability$validity$probability_valid)
  expect_false(probability$validity$capacity_valid)
  expect_true(probability$validity$capacity_overflow)
  expect_identical(probability$validity$status, "capacity_numerical_overflow")
  expect_true(all(is.finite(probability$outputs$allocation)))
  expect_true(all(is.finite(probability$outputs$flow)))
  capacity <- predict_allocation(source, M, outputs = c("A", "abar", "A_supported", "abar_supported"))
  expect_true(all(is.na(unlist(capacity$outputs))))

  # Multiplying P*S first would underflow, while P*(S/U) is representable.
  source <- .solve_problem(system(1e-300, 1e-200, kappa = 1e200, v0 = 1), c(sigma = 1))
  request <- matrix(sqrt(-2 * log(1e-200)), 1, 1, dimnames = list("new", "f"))
  x <- predict_allocation(source, request, outputs = c("rho", "A", "abar"))
  kernel <- exp(-request[1]^2 / 2)
  stable <- (kernel / (kernel + 1)) * (1e-200 / source$prediction_snapshot$utilization[1])
  expect_gt(x$outputs$A, 0)
  expect_equal(as.numeric(x$outputs$A), stable, tolerance = 1e-12)
  expect_true(x$validity$capacity_valid)

  # An unrepresentable ratio may still have a finite requested contribution.
  source <- .solve_problem(system(1e-300, 1e100, kappa = 1e-100, v0 = 1), c(sigma = 1))
  x <- predict_allocation(source, request, outputs = c("rho", "A", "abar"))
  stable <- exp(log(kernel / (kernel + 1)) + log(1e100) -
                  log(source$prediction_snapshot$utilization[1]))
  expect_equal(as.numeric(x$outputs$A), stable, tolerance = 1e-12)
  expect_true(x$validity$capacity_valid)
  expect_false(x$validity$capacity_overflow)
  expect_true(x$validity$conditional_capacity_overflow)
  expect_false(x$validity$conditional_capacity_valid)
  expect_true(is.na(x$outputs$abar))

  # Normalized P itself can underflow while count and capacity stay finite.
  source <- .solve_problem(system(1e-100, 1e100, kappa = 1e-100, v0 = 1e200), c(sigma = 1))
  x <- predict_allocation(source, request, demand = 1e308,
                          outputs = c("rho", "A", "allocation", "flow"))
  log_probability <- log(kernel) - log(kernel + 1e200)
  expected_A <- exp(log_probability + log(1e100) - log(source$prediction_snapshot$utilization[1]))
  expect_true(x$validity$probability_underflow)
  expect_equal(as.numeric(x$outputs$allocation), 0)
  expect_equal(as.numeric(x$outputs$A), expected_A, tolerance = 1e-12)
  expect_equal(as.numeric(x$outputs$flow), exp(log_probability + log(1e308)), tolerance = 1e-12)
  flow_only <- predict_allocation(source, request, demand = 1e308, outputs = "flow")
  expect_identical(flow_only$outputs$flow, x$outputs$flow)
  expect_false(x$validity$conditional_capacity_valid)

  # Finite scores whose requested row sum overflows are rejected.
  two <- matrix(30, 1, 2, dimnames = list("old", c("f1", "f2")))
  source <- .solve_problem(clm_allocation(prepare_allocation(c(old = 1), c(f1 = 1e307, f2 = 1e307),
    two), kappa = 10, v0 = 1), c(sigma = 1))
  expect_error(predict_allocation(source, two * 0), "denominator overflowed")
})

test_that("ordinary and guarded prediction agree with algebra across every family", {
  outputs <- c("rho", "A", "abar", "allocation", "flow")
  D <- c(2, 0, NA_real_)
  for (family in c("gaussian", "exponential", "power")) {
    entry <- alloc_prediction_sources()[[paste(family, "fixed")]]
    case <- entry$case; source <- entry$solved
    x <- predict_allocation(source, case$distance, demand = D, outputs = outputs)
    expected <- alloc_prediction_oracle(case, demand = D)
    expect_equal(lapply(x$outputs, unname), lapply(expected[outputs], unname),
                 tolerance = 1e-12, info = family)
    # Large request counts use stable column normalization despite ordinary
    # source ranges; zero and missing demand retain their distinct masks.
    huge <- predict_allocation(source, case$distance, demand = c(1e101, 0, NA_real_),
                               outputs = "flow")
    expect_equal(unname(huge$outputs$flow), unname(expected$allocation * c(1e101, 0, NA_real_)),
                 tolerance = 1e-12, info = family)
    # A tiny requested kernel forces the guarded capacity path.
    tiny_distance <- switch(family,
      gaussian = case$theta[["sigma"]] * sqrt(-2 * log(1e-110)),
      exponential = -log(1e-110) / case$theta[["sigma"]],
      power = (1e-110)^(-1 / case$theta[["sigma"]]))
    request <- matrix(tiny_distance, 1, 3, dimnames = list("tiny", names(case$supply)))
    tiny <- predict_allocation(source, request, demand = 2, outputs = c("rho", "A", "flow"))
    tiny_expected <- alloc_prediction_oracle(case, distance = request, demand = 2,
                                             loads = source$prediction_snapshot$utilization)
    expect_gt(tiny$outputs$A, 0)
    expect_equal(lapply(tiny$outputs, unname), lapply(tiny_expected[c("rho", "A", "flow")], unname),
                 tolerance = 1e-12, info = family)
    # Small baseline demand gives a finite extreme S/U, forcing the guarded
    # capacity path without changing the model's allocation probabilities.
    small <- case; small$demand <- case$demand * 1e-110
    small$model <- clm_allocation(prepare_allocation(small$demand, case$supply, case$distance),
                                  family = family, kappa = .65, beta = 1.3, v0 = 4)
    guarded <- predict_allocation(.solve_problem(small$model, case$theta), case$distance,
                                  demand = D, outputs = outputs)
    expect_equal(lapply(guarded$outputs, unname),
                 lapply(alloc_prediction_oracle(small, demand = D)[outputs], unname),
                 tolerance = 1e-12, info = family)
  }
  # New contact with a positive-supply, zero-load facility keeps probabilities
  # valid; capacity uses the zero-contribution convention only when asked.
  case <- alloc_prediction_case(); baseline <- case$distance; baseline[, 3] <- Inf
  source <- .solve_problem(clm_allocation(prepare_allocation(case$demand, case$supply, baseline),
                                          kappa = .65, beta = 1.3, v0 = 4), case$theta)
  x <- predict_allocation(source, case$distance, demand = case$demand,
    outputs = c("rho", "A", "abar", "A_supported", "abar_supported", "allocation", "flow",
                "unsupported_contact"))
  expected <- alloc_request_oracle(case$distance, baseline, case$demand, case$supply,
    case$theta[["sigma"]], v0 = 4, beta = 1.3, kappa = .65, demand = case$demand)
  expect_equal(source$prediction_snapshot$utilization[3], 0)
  expect_identical(x$validity$status, rep("unsupported_capacity", 3))
  expect_true(all(x$validity$probability_valid & !x$validity$capacity_valid))
  expect_true(all(is.na(x$outputs$A) & is.na(x$outputs$abar)))
  expect_true(all(x$outputs$unsupported_contact > 0))
  expect_equal(unname(x$outputs$unsupported_contact), unname(expected$allocation[, 3]),
               tolerance = 1e-12)
  expect_equal(lapply(x$outputs[c("rho", "allocation", "flow")], unname),
               lapply(expected[c("rho", "allocation", "flow")], unname), tolerance = 1e-12)
  expect_equal(unname(x$outputs$A_supported), expected$A, tolerance = 1e-12)
  expect_equal(unname(x$outputs$abar_supported), expected$abar, tolerance = 1e-12)
  # Zero supply gives no contact; only an outside option keeps choice defined.
  for (outside in c(0, 4)) {
    zero <- alloc_prediction_case(supply = c(b = 0, a = 0, c = 0), v0 = outside)
    x <- predict_allocation(.solve_problem(zero$model, zero$theta), zero$distance,
                            demand = zero$demand, outputs = c("rho", "A", "abar", "flow"))
    expect_equal(unname(x$outputs$rho), rep(0, 3))
    expect_equal(unname(x$outputs$A), rep(0, 3))
    expect_true(all(is.na(x$outputs$abar)))
    expect_true(all(x$outputs$flow == 0))
    expect_identical(x$validity$probability_valid, rep(outside > 0, 3))
  }
})

test_that("rounded unsupported mass cannot certify capacity support", {
  baseline <- matrix(c(Inf, 1), 1, 2, dimnames = list("old", c("unused", "used")))
  supply <- c(unused = 1e-200, used = 1e200)
  source <- .solve_problem(clm_allocation(prepare_allocation(c(old = 1), supply, baseline),
                                          v0 = 1), c(sigma = 1))
  request <- matrix(1, 1, 2, dimnames = list("new", names(supply)))
  x <- predict_allocation(source, request,
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

test_that("requested maps preserve distinct cell indices and refuse matrix surfaces", {
  entry <- alloc_prediction_sources()[["gaussian fixed"]]
  template <- terra::rast(nrows = 2, ncols = 4, xmin = 20, xmax = 24, ymin = 0, ymax = 2)
  cells <- c(o3 = 7, o1 = 2, o7 = 5)
  x <- predict_allocation(entry$solved, entry$case$distance,
                          spatial = list(template = template, cell_index = cells[c(3, 1, 2)]))
  map <- .prediction_surface(x)
  expect_equal(terra::values(map)[cells, 1], unname(x$outputs$A), ignore_attr = TRUE)
  expect_true(all(is.na(terra::values(map)[-cells, 1])))
  expect_s3_class(x$spatial$template, "ae_spatial_template")
  P <- predict_allocation(entry$solved, entry$case$distance, outputs = "allocation",
                          spatial = list(template = template, cell_index = cells))
  expect_error(.prediction_surface(P, "allocation"), "not an origin-side")
  expect_error(.prediction_surface(x, "flow"), "requested output")
  expect_error(.prediction_surface(x, c("A", "rho")), "one nonmissing name")
})
