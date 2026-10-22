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
  expect_error(predict_allocation(source, M, outputs = "utilization"),
               "unsupported prediction outputs")
})

test_that("unknown travel, known absence and zero denominators remain distinct", {
  M <- rbind(unknown = c(1, NA, Inf), absent = c(Inf, Inf, Inf), known = c(1, 2, 3))
  colnames(M) <- c("b", "a", "c")
  out <- c("rho", "outside_share", "A", "abar", "allocation", "flow")
  # Nonfinite travel never reaches any kernel, including the power kernel.
  for (family in c("gaussian", "exponential", "power")) {
    source <- alloc_prediction_sources()[[paste(family, "fixed")]]$solved
    x <- predict_allocation(source, M, demand = c(0, NA, 0), outputs = out)
    expect_identical(x$validity[c("status", "probability_valid", "unknown_travel")],
      data.frame(status = c("unknown_travel", "known_absence", "valid"),
                 probability_valid = c(FALSE, TRUE, TRUE),
                 unknown_travel = c(TRUE, FALSE, FALSE), row.names = rownames(M)),
      info = family)
    expect_false(any(x$validity$legacy_kernel_imputed), info = family)
    expect_equal(unname(c(x$outputs$rho[1:2], x$outputs$outside_share[2], x$outputs$abar[2])),
                 c(NA, 0, 1, NA), info = family)
    expect_true(all(is.na(x$outputs$allocation[1, ])) && all(is.na(x$outputs$flow[1:2, ])) &&
                  all(x$outputs$flow[3, ] == 0), info = family)
  }
  source <- alloc_prediction_sources()[["gaussian fixed"]]$solved
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
  expect_error(predict_allocation(power, zero, missing_travel = "legacy_zero"),
               "strictly positive")
  one <- matrix(1, 1, 1, dimnames = list("old", "f"))
  strict <- .solve_problem(clm_allocation(prepare_allocation(c(old = 1), c(f = 1), one),
                                          family = "power", v0 = 1), c(sigma = 2))
  expect_error(predict_allocation(strict, one * 1e-200), "kernel is invalid on finite")
  expect_error(predict_allocation(strict, one * -Inf, missing_travel = "legacy_zero"),
               "nonnegative")
  # A checked power source keeps +Inf as known absence even at sigma = 0.
  absent <- matrix(Inf, 1, 1, dimnames = list("new", "f"))
  numeric_source <- .solve_problem(clm_allocation(
    prepare_allocation(c(new = 1), c(f = 1), absent), family = "power", v0 = 2), c(sigma = 0))
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
  # Each redundant small record must agree with the snapshot on its own.
  bump <- function(v) { v[1] <- v[1] * 2 + 1; v }
  tampers <- list(
    list(fit, "theta", bump), list(fit, "theta_hat", bump),
    list(fit, c("outputs", "utilization"), bump),
    list(fit, c("coverage_meta", "supply"), bump),
    list(fit, c("coverage_meta", "facility_ids"), rev),
    list(solved, "utilization", bump),
    list(solved, c("evaluated_clm", "spec", "v0"), bump),
    list(solved, c("evaluated_clm", "theta"), bump),
    list(solved, c("evaluated_clm", "facility_ids"), rev),
    list(solved, c("evaluated_clm", "supply"), bump))
  for (tamper in tampers) {
    bad <- tamper[[1]]
    bad[[tamper[[2]]]] <- tamper[[3]](bad[[tamper[[2]]]])
    expect_error(predict_allocation(bad, case$distance), "conflicts",
                 info = paste(tamper[[2]], collapse = "$"))
  }
  bad <- fit; bad$prediction_snapshot <- solved$prediction_snapshot
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
    .fit_problem_nfxp = function(...) stop("unexpected fit"),
    .clm_prediction_flow = function(...) stop("unexpected flow"))
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
  probability <- predict_allocation(source, M, demand = 10,
                                    outputs = c("rho", "allocation", "flow"))
  expect_true(probability$validity$probability_valid)
  expect_false(probability$validity$capacity_valid)
  expect_true(probability$validity$capacity_overflow)
  expect_identical(probability$validity$status, "capacity_numerical_overflow")
  expect_true(all(is.finite(probability$outputs$allocation)))
  expect_true(all(is.finite(probability$outputs$flow)))
  capacity <- predict_allocation(source, M,
                                 outputs = c("A", "abar", "A_supported", "abar_supported"))
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

# Declared decay kernels (SPAX-052) -------------------------------------------------
# Snapshots and prediction for models whose kernel was declared with
# decay_kernel(). Expected values come from clm_oracle() with the kernel written
# out again as plain algebra, never from the package's own kernel code.

# A plateau kernel: full weight within 1.5 minutes, Gaussian decay beyond. Its
# bounds on sigma are finite so that out-of-bounds snapshots can be built.
plateau_kernel <- function() {
  decay_kernel(
    function(distance, sigma, plateau = 1.5) {
      # Full weight within the plateau, Gaussian decay beyond it.
      ifelse(distance <= plateau, 1, exp(-(distance - plateau)^2 / (2 * sigma^2)))
    },
    name = "plateau", parameter = "scale", lower = 0.25, upper = 40,
    args = list(plateau = 1.5)
  )
}
plateau_algebra <- function(distance, sigma) exp(-pmax(distance - 1.5, 0)^2 / (2 * sigma^2))

# The prediction case of alloc_prediction_case() with the plateau kernel;
# `fitted` makes v0 and beta parameters. Fixed constants are kappa .65, beta 1.3
# and v0 4; the generating parameters are those of the Gaussian case.
plateau_case <- function(fitted = FALSE) {
  case <- alloc_prediction_case(fitted = fitted)
  case$model <- clm_allocation(case$model$substrate, family = plateau_kernel(),
    kappa = .65, beta = 1.3, v0 = 4, fit_v0 = fitted, fit_beta = fitted)
  case$family <- "plateau"
  case
}

# A fixed evaluation and a multistart flow fit of each plateau case.
plateau_sources <- function() {
  alloc_memo("plateau_sources", {
    out <- list()
    for (fitted in c(FALSE, TRUE)) {
      case <- plateau_case(fitted)
      flow <- clm_oracle(case$distance, case$demand, case$supply, case$theta[["sigma"]],
        v0 = case$v0, beta = case$beta, kappa = case$kappa, kernel = plateau_algebra)$flow
      out[[if (fitted) "fitted" else "fixed"]] <- list(case = case,
        solved = evaluate_allocation(case$model, case$theta),
        fit = fit_allocation(case$model, flow, rbind(truth = case$theta),
          case$theta / 4, case$theta * 4, output = "flow", loss = "poisson",
          gradient = TRUE, control = list(maxit = 20)))
    }
    out
  })
}

# Requested origins: mixed contact (n1), finite zero travel and travel exactly at
# the plateau edge (n2), an absent edge (n3) and an unknown travel time (n4).
plateau_request <- function() {
  request <- rbind(n1 = c(1, 2, 3), n2 = c(0, 1.5, 9), n3 = c(Inf, 4, 6), n4 = c(2, NA, 1))
  colnames(request) <- c("b", "a", "c")
  request
}

# Requested-location algebra for a plateau-kernel system at (sigma, v0, beta):
# probabilities from the requested travel, capacity ratios from the loads of the
# saved system. Rows with unknown travel are NA.
plateau_prediction_oracle <- function(case, sigma, v0, beta, request, demand) {
  choice <- function(distance, demand) clm_oracle(distance, demand, case$supply, sigma,
    v0 = v0, beta = beta, kappa = case$kappa, kernel = plateau_algebra)
  loads <- choice(case$distance, case$demand)$utilization
  ratio <- ifelse(loads > 0, case$supply / loads, 0)
  known <- rowSums(is.na(request)) == 0
  requested <- choice(request[known, , drop = FALSE], rep(0, sum(known)))
  A <- as.vector(requested$probability %*% ratio)
  expand <- function(x) {
    full <- if (is.matrix(x)) matrix(NA_real_, nrow(request), ncol(x)) else
      rep(NA_real_, nrow(request))
    if (is.matrix(x)) full[known, ] <- x else full[known] <- x
    full
  }
  list(allocation = expand(unname(requested$probability)), rho = expand(unname(requested$rho)),
       outside_share = expand(unname(requested$outside_share)), A = expand(A),
       abar = expand(ifelse(requested$rho > 0, A / requested$rho, NA_real_)),
       flow = expand(unname(requested$probability * demand[known])), utilization = loads)
}

# The parameters a source holds: sigma, and v0 and beta when fitted, else the
# constants of its case.
plateau_parameters <- function(case, theta) {
  list(sigma = theta[["sigma"]], v0 = if ("v0" %in% names(theta)) theta[["v0"]] else case$v0,
       beta = if ("beta" %in% names(theta)) theta[["beta"]] else case$beta)
}

test_that("built-in snapshots keep the v1 record and a declared kernel writes v2", {
  fields <- c("schema", "kind", "spec", "theta", "facility_ids", "supply", "utilization",
              "input_metadata", "input_policy", "provenance")
  spec_names <- c("family", "allocation", "kappa", "beta", "v0", "fit_beta", "fit_v0")
  # Built-in kernels: the v1 record, with no kernel in the specification.
  for (entry in alloc_prediction_sources()) for (source in entry[c("solved", "fit")]) {
    snapshot <- source$prediction_snapshot
    expect_identical(snapshot$schema, "spax_clm_prediction_v1")
    expect_identical(snapshot$kind, "builtin_static_clm")
    expect_identical(names(snapshot), fields)
    expect_identical(names(snapshot$spec), spec_names)
  }
  # A declared kernel: v2, the same fields, and the declaration appended last.
  reference <- alloc_prediction_sources()[["gaussian fixed"]]$solved$prediction_snapshot
  plain <- function(x) if (is.list(x)) all(vapply(x, plain, logical(1))) else is.atomic(x)
  for (entry in plateau_sources()) {
    for (snapshot in list(entry$solved$prediction_snapshot, entry$fit$best$prediction_snapshot)) {
      expect_identical(snapshot$schema, "spax_clm_prediction_v2")
      expect_identical(snapshot$kind, "declared_kernel_static_clm")
      expect_identical(names(snapshot), fields)
      expect_identical(names(snapshot$spec), c(spec_names, "kernel"))
      expect_identical(snapshot$spec$family, "plateau")
      expect_identical(snapshot$spec$allocation, "clm")
      expect_identical(snapshot$spec$kernel, plateau_kernel())
      # The declaration is plain data and survives serialization unchanged.
      expect_true(plain(snapshot))
      expect_identical(unserialize(serialize(snapshot, NULL)), snapshot)
      # The record of the same inputs differs only in kernel-specific fields.
      expect_identical(snapshot[c("facility_ids", "supply", "input_metadata", "input_policy",
                                  "provenance")],
                       reference[c("facility_ids", "supply", "input_metadata", "input_policy",
                                   "provenance")])
    }
    # The redundant evaluation identity carries the declaration as well.
    expect_identical(entry$solved$evaluated_clm$spec, entry$solved$prediction_snapshot$spec)
    expect_identical(entry$fit$best$prediction_snapshot,
                     entry$fit$best$equilibrium$prediction_snapshot)
  }
})

test_that("predictions from declared-kernel sources equal the choice algebra", {
  request <- plateau_request()
  demand <- c(n1 = 5, n2 = 0, n3 = NA, n4 = 4)
  outputs <- c("rho", "outside_share", "A", "abar", "allocation", "flow")
  for (entry in plateau_sources()) {
    for (source in entry[c("solved", "fit")]) {
      theta <- if (inherits(source, "ae_multistart_fit")) coef(source) else entry$case$theta
      p <- plateau_parameters(entry$case, theta)
      oracle <- plateau_prediction_oracle(entry$case, p$sigma, p$v0, p$beta, request, demand)
      x <- predict_allocation(source, request, demand = demand, outputs = outputs)
      for (name in outputs) {
        expect_equal(unname(x$outputs[[name]]), oracle[[name]], tolerance = 1e-12, info = name)
      }
      expect_equal(x$facilities$utilization, oracle$utilization, tolerance = 1e-12)
      expect_identical(x$validity$status, c("valid", "valid", "valid", "unknown_travel"))
      expect_identical(x$validity$demand_status, c("positive", "zero", "unknown", "positive"))
      # The plateau edge and the zero travel time are known contact.
      expect_true(all(x$outputs$rho[1:3] > 0))
      expect_true(all(is.na(x$outputs$allocation[4, ])) && all(is.na(x$outputs$flow[3:4, ])))
      expect_true(all(x$outputs$flow[2, ] == 0))
      expect_identical(unname(x$outputs$allocation[3, 1]), 0)
    }
  }
  # Requested rows, columns and demand still align by name, whatever their order.
  fixed <- plateau_sources()$fixed$solved
  shuffled <- predict_allocation(fixed, request[c(4, 2, 1, 3), c(3, 1, 2)],
    origin_ids = rownames(request), demand = demand[c(4, 2, 1, 3)], outputs = outputs)
  expect_identical(shuffled, predict_allocation(fixed, request, demand = demand, outputs = outputs))
})

test_that("saved declared-kernel results predict identically after a save and reload", {
  request <- plateau_request()
  demand <- c(n1 = 5, n2 = 0, n3 = 2, n4 = 4)
  outputs <- c("rho", "A", "abar", "allocation", "flow")
  for (entry in plateau_sources()) for (source in entry[c("solved", "fit")]) {
    path <- withr::local_tempfile(fileext = ".rds")
    saveRDS(source, path)
    restored <- readRDS(path)
    expect_identical(predict_allocation(restored, request, demand = demand, outputs = outputs),
                     predict_allocation(source, request, demand = demand, outputs = outputs))
  }
  # The snapshot is a plain record: it reads back unchanged, with its code as text.
  fixed <- plateau_sources()$fixed$solved
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(fixed$prediction_snapshot, path)
  expect_identical(readRDS(path), fixed$prediction_snapshot)
  expect_type(readRDS(path)$spec$kernel$code, "character")
})

test_that("tampered v2 snapshots are refused, and their code is not run to refuse them", {
  fixed <- plateau_sources()$fixed
  solved <- fixed$solved
  request <- plateau_request()
  other <- decay_kernel(function(distance, sigma) exp(-distance / sigma), name = "plateau",
                        parameter = "scale", lower = 0.25, upper = 40)
  edits <- list(
    # The kernel declaration must be a valid custom declaration that names the family.
    "unparseable code" = list("invalid CLM snapshot specification",
      function(s) { s$spec$kernel$code <- "function(distance, sigma) {"; s }),
    "two expressions" = list("invalid CLM snapshot specification",
      function(s) {
        s$spec$kernel$code <- paste(s$spec$kernel$code, "function(distance, sigma) 1", sep = "\n")
        s
      }),
    "an expression that is not a function" = list("invalid CLM snapshot specification",
      function(s) { s$spec$kernel$code <- "stop('this must not run')"; s }),
    "a function hidden in a block" = list("invalid CLM snapshot specification",
      function(s) {
        s$spec$kernel$code <- "{ stop('this must not run'); function(distance, sigma) 1 }"
        s
      }),
    "a family other than the kernel name" = list("invalid CLM snapshot specification",
      function(s) { s$spec$family <- "other"; s }),
    "a built-in family name" = list("invalid CLM snapshot specification",
      function(s) { s$spec$family <- "gaussian"; s }),
    "a built-in declaration" = list("invalid CLM snapshot specification",
      function(s) { s$spec$kernel <- decay_kernel("gaussian"); s$spec$family <- "gaussian"; s }),
    "a declaration that fails its own checks" = list("invalid CLM snapshot specification",
      function(s) { s$spec$kernel$lower <- -1; s }),
    "no declaration" = list("invalid CLM snapshot specification",
      function(s) { s$spec$kernel <- NULL; s }),
    "an extra specification field" = list("invalid CLM snapshot specification",
      function(s) { s$spec$extra <- 1; s }),
    "a declaration under a v1 record" = list("invalid CLM snapshot specification",
      function(s) { s$schema <- "spax_clm_prediction_v1"; s$kind <- "builtin_static_clm"; s }),
    # sigma must lie within the declared bounds.
    "sigma above the kernel bounds" = list("invalid CLM snapshot theta",
      function(s) { s$theta[["sigma"]] <- 41; s }),
    "sigma below the kernel bounds" = list("invalid CLM snapshot theta",
      function(s) { s$theta[["sigma"]] <- 0.2; s }),
    # Any other schema or kind is unsupported.
    "an unknown schema" = list("unsupported prediction snapshot; regenerate",
      function(s) { s$schema <- "spax_clm_prediction_v3"; s }),
    "a v2 schema with the built-in kind" = list("unsupported prediction snapshot",
      function(s) { s$kind <- "builtin_static_clm"; s }),
    "a v1 schema with the declared kind" = list("unsupported prediction snapshot",
      function(s) { s$schema <- "spax_clm_prediction_v1"; s }),
    "an unknown kind" = list("unsupported prediction snapshot",
      function(s) { s$kind <- "other_static_clm"; s }),
    "a missing field" = list("unsupported prediction snapshot",
      function(s) { s$provenance <- NULL; s }))
  for (label in names(edits)) {
    bad <- solved
    bad$prediction_snapshot <- edits[[label]][[2]](bad$prediction_snapshot)
    error <- tryCatch(predict_allocation(bad, request), error = conditionMessage)
    expect_match(error, edits[[label]][[1]], fixed = TRUE, info = label)
    expect_false(grepl("must not run", error, fixed = TRUE), info = label)
  }
  # The same refusals hold for the snapshot of a fit.
  fit <- plateau_sources()$fitted$fit
  bad <- fit
  bad$best$prediction_snapshot$spec$kernel$code <- "function(distance, sigma) {"
  expect_error(predict_allocation(bad, request), "invalid CLM snapshot specification")
  bad <- fit
  bad$best$prediction_snapshot$theta[["sigma"]] <- 41
  expect_error(predict_allocation(bad, request), "invalid CLM snapshot theta")
  # A well-formed declaration that differs from the evaluated source is a conflict.
  bad <- solved
  bad$prediction_snapshot$spec$kernel <- other
  expect_error(predict_allocation(bad, request), "conflicts")
  bad <- fit
  bad$best$prediction_snapshot$spec$kernel <- other
  expect_error(predict_allocation(bad, request), "conflicts")
  # Unmodified sources still predict.
  expect_no_error(predict_allocation(solved, request))
  expect_no_error(predict_allocation(fit, request))
})

test_that("a kernel declared smooth = FALSE is evaluated and predicts", {
  case <- alloc_prediction_case()
  cutoff <- decay_kernel(
    function(distance, sigma) {
      # Weight one within the cutoff and zero beyond it.
      0 + (distance <= sigma)
    },
    name = "cutoff", parameter = "cutoff", smooth = FALSE
  )
  model <- clm_allocation(case$model$substrate, family = cutoff, kappa = .65, beta = 1.3, v0 = 4)
  solved <- evaluate_allocation(model, c(sigma = 2.5))
  expect_identical(solved$prediction_snapshot$schema, "spax_clm_prediction_v2")
  expect_false(solved$prediction_snapshot$spec$kernel$smooth)
  # n2 reaches no facility within the cutoff; n3 has an absent edge.
  request <- rbind(n1 = c(1, 2.5, 3), n2 = c(4, 5, 6), n3 = c(2, Inf, 3))
  colnames(request) <- c("b", "a", "c")
  demand <- c(n1 = 2, n2 = 3, n3 = 4)
  step <- function(distance, sigma) 0 + (distance <= sigma)
  choice <- function(distance, demand) clm_oracle(distance, demand, case$supply, 2.5, v0 = 4,
    beta = 1.3, kappa = .65, kernel = step)
  loads <- choice(case$distance, case$demand)$utilization
  expect_true(all(loads > 0))
  requested <- choice(request, rep(0, 3))
  A <- as.vector(requested$probability %*% (case$supply / loads))
  x <- predict_allocation(solved, request, demand = demand,
    outputs = c("rho", "A", "abar", "allocation", "flow"))
  expect_equal(unname(x$outputs$allocation), unname(requested$probability), tolerance = 1e-12)
  expect_equal(unname(x$outputs$rho), unname(requested$rho), tolerance = 1e-12)
  expect_equal(unname(x$outputs$A), A, tolerance = 1e-12)
  expect_equal(unname(x$outputs$flow), unname(requested$probability * demand), tolerance = 1e-12)
  expect_identical(unname(x$outputs$rho[2]), 0)
  expect_true(is.na(x$outputs$abar[2]))
  expect_identical(x$validity$status, c("valid", "known_absence", "valid"))
  # The same model is still refused by fit_allocation().
  expect_error(fit_allocation(model, case$supply, rbind(a = c(sigma = 2)), c(sigma = 0.5),
                              c(sigma = 4)), "smooth = FALSE")
})

test_that("prediction applies the domain rules of the declared kernel", {
  case <- alloc_prediction_case()
  hyperbolic <- decay_kernel(function(distance, sigma) 1 / (distance * sigma),
                             name = "hyperbolic", parameter = "scale", zero_distance = FALSE)
  late <- decay_kernel(
    function(distance, sigma) ifelse(distance > 2000, NA_real_, exp(-distance / sigma)),
    name = "late-failure", parameter = "scale")
  declare <- function(kernel, inputs = case$model$substrate) {
    evaluate_allocation(clm_allocation(inputs, family = kernel, kappa = .65, beta = 1.3, v0 = 4),
                        c(sigma = 2))
  }
  # The shared fixture is first used before any mock is installed.
  fixed_snapshot <- plateau_sources()$fixed$solved$prediction_snapshot
  legacy_inputs <- case$model$substrate
  legacy_inputs$input_policy$kernel_domain <- "legacy_zero"
  strict <- declare(hyperbolic)
  legacy <- declare(hyperbolic, legacy_inputs)
  declared_late <- declare(late)
  expect_identical(legacy$prediction_snapshot$input_policy$kernel_domain, "legacy_zero")
  zero <- case$distance
  zero[1, 1] <- 0
  # A kernel declared undefined at zero refuses finite zero travel, but not an
  # absent or unknown edge, and not positive travel.
  message <- "hyperbolic decay requires strictly positive finite travel distances"
  expect_error(predict_allocation(strict, zero), message)
  expect_error(predict_allocation(strict, zero, missing_travel = "legacy_zero"), message)
  for (value in c(Inf, NA)) {
    request <- case$distance
    request[1, 1] <- value
    expect_no_error(predict_allocation(strict, request))
  }
  oracle <- clm_oracle(case$distance, rep(0, 3), case$supply, 2, v0 = 4, beta = 1.3, kappa = .65,
                       kernel = function(distance, sigma) 1 / (distance * sigma))
  expect_equal(unname(predict_allocation(strict, case$distance,
    outputs = "allocation")$outputs$allocation), unname(oracle$allocation), tolerance = 1e-12)
  # Only an explicit legacy request on a legacy source accepts it: the infinite
  # weight at zero travel is then zeroed and flagged, as for the built-in kernels.
  expect_error(predict_allocation(legacy, zero), message)
  imputed <- predict_allocation(legacy, zero, missing_travel = "legacy_zero",
                                outputs = c("rho", "allocation"))
  zeroed <- clm_oracle(zero, rep(0, 3), case$supply, 2, v0 = 4, beta = 1.3, kappa = .65,
                       kernel = function(distance, sigma) 1 / (distance * sigma))
  expect_identical(imputed$validity$legacy_kernel_imputed, c(TRUE, FALSE, FALSE))
  expect_identical(unname(imputed$outputs$allocation[1, 1]), 0)
  expect_equal(unname(imputed$outputs$allocation), unname(zeroed$allocation), tolerance = 1e-12)
  # A kernel that fails only beyond its test grid is refused on strict inputs.
  far <- case$distance
  far[2, 3] <- 3000
  expect_error(predict_allocation(declared_late, far),
               "decay kernel is invalid on finite requested travel")
  expect_no_error(predict_allocation(declared_late, case$distance))
  # The engine checks sigma against the declared bounds itself, so a source that
  # skipped validation is refused.
  testthat::local_mocked_bindings(.clm_prediction_source = function(source) source)
  request <- plateau_request()[1:3, ]
  expect_no_error(.predict_clm(fixed_snapshot, request))
  for (sigma in c(0.2, 41)) {
    bad <- fixed_snapshot
    bad$theta[["sigma"]] <- sigma
    expect_error(.predict_clm(bad, request), "plateau decay requires sigma from 0.25 to 40",
                 info = sigma)
  }
})

test_that("prediction and saved snapshots need a positive sigma for a declared kernel", {
  case <- alloc_prediction_case()
  # Lower bound zero: sigma = 0 is inside the bounds but is not a valid scale.
  free <- decay_kernel(function(distance, sigma) exp(-distance / sigma), name = "free-exp",
                       parameter = "scale")
  model <- clm_allocation(case$model$substrate, family = free, kappa = .65, beta = 1.3, v0 = 4)
  solved <- evaluate_allocation(model, c(sigma = 2))
  small <- evaluate_allocation(model, c(sigma = 0.05))
  request <- case$distance[1:2, ]
  expect_no_error(predict_allocation(solved, request))
  expect_no_error(predict_allocation(small, request))
  # Reading a saved snapshot with sigma = 0 is refused before anything is run.
  bad <- solved
  bad$prediction_snapshot$theta[["sigma"]] <- 0
  expect_error(predict_allocation(bad, request),
               "invalid CLM snapshot theta: free-exp decay requires positive sigma", fixed = TRUE)
  # A negative sigma is refused as an invalid theta.
  negative <- solved$prediction_snapshot
  negative$theta[["sigma"]] <- -1
  expect_error(.validate_clm_prediction_snapshot(negative), "invalid CLM snapshot theta")
  # The prediction engine checks sigma itself, for a source that skipped validation.
  testthat::local_mocked_bindings(.clm_prediction_source = function(source) source)
  expect_error(.predict_clm(bad$prediction_snapshot, request),
               "free-exp decay requires positive sigma")
  expect_no_error(.predict_clm(small$prediction_snapshot, request))
})

test_that("a fit of a declared kernel prints and summarizes the meaning of sigma", {
  fit <- plateau_sources()$fitted$fit
  label <- "plateau (sigma = scale)"
  expect_true(paste("family:", label) %in% utils::capture.output(print(fit$best)))
  expect_identical(summary(fit$best)$family_label, label)
  expect_identical(summary(fit$best)$family, "plateau")
  expect_true(paste("family:", label) %in% utils::capture.output(print(summary(fit$best))))
  expect_true(paste("family:", label) %in% utils::capture.output(print(summary(fit))))
  # The built-in label does not depend on a saved declaration.
  gaussian <- alloc_prediction_sources()[["gaussian fitted"]]$fit
  expect_true("family: gaussian (sigma = travel scale)" %in% utils::capture.output(print(gaussian)))
})

test_that("a prediction prints its origins, demand, travel rule and outputs", {
  case <- alloc_prediction_case()
  solved <- .solve_problem(case$model, case$theta, requested_outputs = "flow")
  request <- case$distance
  request[2, 3] <- NA
  range_text <- function(v, digits = 4) {
    known <- v[!is.na(v)]
    paste0(format(min(known), digits = digits), " to ", format(max(known), digits = digits),
           " (", length(known), " of ", length(v), " origins)")
  }
  pred <- predict_allocation(solved, request, outputs = c("rho", "allocation"))
  lines <- utils::capture.output(shown <- withVisible(print(pred)))
  expect_false(shown$visible)
  expect_identical(shown$value, pred)
  expect_identical(lines, c(
    "<allocation prediction> 3 origins x 3 facilities",
    "  origins     2 valid, 1 unknown_travel",
    "  demand      not given",
    "  travel      an unknown travel time makes the origin unknown",
    "  outputs",
    sprintf("    %-10s  %s", "rho", range_text(pred$outputs$rho)),
    sprintf("    %-10s  %s", "allocation", "3 origins x 3 facilities"),
    "  values in $outputs; the status of each origin in $validity"))
  # Pairs with an unknown time left out ("omit", stored under its older name), given
  # demand with a zero and an unknown value, and fewer digits.
  omit <- predict_allocation(solved, request, demand = c(o3 = 3, o1 = 0, o7 = NA),
                             outputs = c("A", "flow"), missing_travel = "omit")
  expect_identical(omit$policy$missing_travel, "legacy_zero")
  lines <- utils::capture.output(print(omit, digits = 2))
  expect_identical(lines[2:4], c(
    "  origins     2 valid, 1 legacy_imputed",
    "  demand      1 positive, 1 zero, 1 unknown",
    "  travel      pairs with an unknown travel time left out (\"omit\")"))
  expect_identical(lines[6:7], c(sprintf("    %-4s  %s", "A", range_text(omit$outputs$A, 2)),
                                 sprintf("    %-4s  %s", "flow", "3 origins x 3 facilities")))
  # An output without a value on any origin says so.
  none <- pred
  none$outputs$rho[] <- NA_real_
  expect_true(sprintf("    %-10s  %s", "rho", "no values") %in% utils::capture.output(print(none)))
})
