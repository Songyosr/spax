.saved_bootstrap_fixture <- function(refused = FALSE) {
  d <- data.frame(attempt = 1:3, seed = 101:103,
    status = c("selected", "fit_failed", "selected"), success = c(TRUE, FALSE, TRUE),
    sigma = c(10, NA, 12), v0 = c(20, NA, 24), boundary = c(FALSE, TRUE, TRUE),
    best_start = c("near", NA, "far"), loss = c(10, NA, 11),
    score_norm = c(1e-6, NA, 1e-7), regional_rho = c(.6, NA, .7), regional_A = c(2, NA, 2))
  estimates <- data.frame(quantity = c("sigma", "regional_rho", "regional_A"),
    estimate = c(11, .65, 2), lower = NA_real_, upper = NA_real_,
    status = c("incomplete_bootstrap", "incomplete_bootstrap", "fixed_input_identity"),
    successful = 2L, attempted = 3L, planned = 399L)
  starts <- data.frame(attempt = rep(1:3, each = 2), start = rep(c("near", "far"), 3),
    eligible = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE),
    message = c("", "", "failed", "failed", "", ""))
  if (refused) {
    d <- d[FALSE, ]; starts <- starts[FALSE, ]
    estimates$status[1:2] <- "original_unidentified"
    estimates$attempted <- estimates$successful <- 0L
  }
  structure(list(status = if (refused) "refused" else "incomplete",
    reasons = if (refused) "original_unidentified" else character(), original = list(theta = c(sigma = 11, v0 = 22)),
    intervals = estimates, draws = d, starts = starts, planned = 399L,
    attempted = nrow(d), successful = sum(d$success), seed = 1L,
    observation = list(event = "requests", period = "period1", sampling = "independent_facilities"),
    settings = list(evidence_scope = "Conditional generated-study scope",
      fixed_inputs = "demand, supply, travel and network",
      capacity_identity = "regional_A is attributed supply per retained demand"),
    facility_ids = c("f1", "f2", "f3"),
    capacity_support = list(supported_facility_ids = c("f1", "f2"),
      unsupported_facility_ids = "f3", unsupported_supply = c(f3 = 3),
      total_supply = 12, attributed_supply = 9),
    input_metadata = list(demand_units = "requests", supply_units = "slots")),
    class = "ae_allocation_bootstrap")
}

test_that("saved bootstrap tables retain failures, boundaries and denominators", {
  x <- .saved_bootstrap_fixture(); before <- x
  testthat::local_mocked_bindings(
    .solve_problem = function(...) stop("inspection attempted a solve"),
    .fit_problem_nfxp = function(...) stop("inspection attempted a fit"),
    .allocation_case_fit = function(...) stop("inspection attempted a weighted fit"),
    .run_allocation_bootstrap = function(...) stop("inspection attempted resampling"))
  expect_identical(as.data.frame(x), x$intervals)
  expect_identical(as.data.frame(x, type = "draws"), x$draws)
  expect_identical(as.data.frame(x, type = "starts"), x$starts)
  s <- summary(x)
  expect_s3_class(s, "summary.ae_allocation_bootstrap")
  expect_identical(s$estimates, x$intervals)
  expect_equal(s$counts, data.frame(planned = 399L, attempted = 3L, successful = 2L))
  expect_equal(s$diagnostics$failed_attempts, 1)
  expect_equal(s$diagnostics$inner_boundary_attempts, 1)
  expect_identical(s$original, x$original)
  expect_identical(s$capacity_support, x$capacity_support)
  expect_identical(s$facility_ids, x$facility_ids)
  expect_identical(s$settings$capacity_identity, x$settings$capacity_identity)
  expect_output(print(x), "399")
  expect_output(print(s), "independent_facilities")
  expect_output(print(s), "total: 12.*attributed: 9.*unsupported: 3")
  expect_output(print(s), "positive supply without support: 1")
  expect_identical(x, before)
})

test_that("refused bootstrap results remain inspectable without attempts", {
  x <- .saved_bootstrap_fixture(TRUE)
  expect_equal(nrow(as.data.frame(x, type = "draws")), 0)
  expect_equal(nrow(as.data.frame(x, type = "starts")), 0)
  expect_equal(summary(x)$counts$attempted, 0)
  expect_identical(summary(x)$reasons, "original_unidentified")
  expect_output(print(x), "original_unidentified")
  expect_error(as.data.frame(x, type = "unknown"), "arg")
})

test_that("bootstrap plots inspect saved ranges, refusals and retained draws", {
  x <- .saved_bootstrap_fixture(); path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({grDevices::dev.off(); unlink(path)}, add = TRUE)
  expect_identical(plot(x, quantity = "regional_rho"), x)
  expect_identical(plot(x, quantity = "regional_rho", type = "draws"), x)
  expect_identical(plot(x, quantity = "regional_A"), x)
  x$intervals$lower[1] <- 9; x$intervals$upper[1] <- 13
  expect_identical(plot(x, quantity = "sigma", main = "Saved interval"), x)
  refused <- .saved_bootstrap_fixture(TRUE)
  expect_identical(plot(refused, type = "draws"), refused)
  refused$intervals$estimate[1] <- NA_real_
  expect_identical(plot(refused), refused)
  expect_error(plot(x, quantity = "absent"), "saved estimate")
})

test_that("saved stale bounds never make refused or incomplete ranges reportable", {
  x <- .saved_bootstrap_fixture()
  x$intervals$lower[1] <- 9; x$intervals$upper[1] <- 13
  x$intervals$status[1] <- "conditional_pointwise_bootstrap"
  x$status <- "complete"
  segments <- 0L; captions <- character()
  testthat::local_mocked_bindings(
    segments = function(...) { segments <<- segments + 1L; invisible(NULL) },
    mtext = function(text, ...) { captions <<- c(captions, paste(text, collapse = "\n")); invisible(NULL) },
    .package = "graphics")
  pdf <- tempfile(fileext = ".pdf"); saved <- tempfile(fileext = ".rds")
  grDevices::pdf(pdf)
  on.exit({grDevices::dev.off(); unlink(c(pdf, saved))}, add = TRUE)
  plot(x, quantity = "sigma")
  expect_equal(segments, 1L)
  expect_output(print(x), "conditional bootstrap intervals")
  expect_match(tail(captions, 1), "quantity: conditional interval eligible")
  expect_false(grepl("conditional_pointwise_bootstrap", tail(captions, 1), fixed = TRUE))
  expect_match(tail(captions, 1), "\n2/3 finite successful values; 399 planned")
  expected <- c(refused = "refused", incomplete = "incomplete",
    insufficient_success = "insufficient successful draws")
  for (status in c("refused", "incomplete", "insufficient_success")) {
    x$status <- status
    saveRDS(x, saved)
    restored <- readRDS(saved)
    plot(restored, quantity = "sigma")
    expect_equal(segments, 1L)
    expect_match(tail(captions, 1), paste0("run: ", expected[[status]]))
    expect_identical(as.data.frame(restored), x$intervals)
  }
  x$status <- "complete"
  for (status in c("original_score_failed", "incomplete_bootstrap", "fixed_input_identity")) {
    x$intervals$status[1] <- status
    plot(x, quantity = "sigma")
    expect_equal(segments, 1L)
  }
  expect_match(tail(captions, 1), "quantity: fixed input identity")
  x$status <- "incomplete"
  plot(x, quantity = "sigma", type = "draws")
  expect_match(tail(captions, 1), "run: incomplete")
})

test_that("fit parameter tables and multistart summary preserve saved fields", {
  fit <- structure(list(theta = c(sigma = 3, v0 = 8)), class = "ae_problem_nfxp_fit")
  x <- structure(list(best = fit, best_start = "first", status = "selected",
    table = data.frame(start = "first", eligible = TRUE, loss = 1), boundary = matrix(FALSE, 1, 2),
    loss_disagreement = FALSE, parameter_disagreement = FALSE,
    tolerances = list(loss = 1e-6, log_parameter = .001), warnings = list(character()), errors = list(character())),
    class = "ae_multistart_fit")
  testthat::local_mocked_bindings(summary.ae_problem_nfxp_fit = function(object, ...) list(theta = object$theta))
  expect_equal(as.data.frame(fit), data.frame(parameter = c("sigma", "v0"), estimate = c(3, 8)))
  expect_identical(as.data.frame(x), as.data.frame(fit))
  expect_identical(as.data.frame(fit, type = "parameters"), as.data.frame(fit))
  expect_identical(as.data.frame(x, type = "parameters"), as.data.frame(x))
  expect_error(as.data.frame(fit, type = "starts"), "arg")
  expect_error(as.data.frame(x, type = "starts"), "arg")
  s <- summary(x)
  expect_s3_class(s, "summary.ae_multistart_fit")
  expect_identical(names(s), c("status", "selected_start", "selected", "starts", "boundary", "disagreement", "warnings", "errors"))
  expect_identical(s$starts, x$table)
  expect_identical(s$boundary, x$boundary)
  expect_identical(s$warnings, x$warnings)
  x$best <- x$best_start <- NULL; x$status <- "no_valid_fit"
  expect_null(summary(x)$selected)
  expect_output(print(summary(x)), "none")
  expect_error(as.data.frame(x), "no eligible allocation fit")
  expect_error(plot(x), "no eligible allocation fit")
})

test_that("multistart plotting delegates without changing selected diagnostics", {
  fit <- structure(list(theta = c(sigma = 3)), class = "ae_problem_nfxp_fit")
  x <- structure(list(best = fit), class = "ae_multistart_fit")
  received <- NULL
  testthat::local_mocked_bindings(plot.ae_problem_nfxp_fit = function(x, type, ...) {
    received <<- list(x = x, type = type, dots = list(...)); invisible("saved diagnostic")
  })
  expect_identical(plot(x, type = "state", breaks = 4), "saved diagnostic")
  expect_identical(received, list(x = fit, type = "state", dots = list(breaks = 4)))
})

test_that("coverage extraction retains support, IDs, raw masks and metadata", {
  x <- structure(list(regional = data.frame(rho = .4, A = 2, abar = NA_real_, abar_raw = 5),
    cells = data.frame(origin_id = c("b", "a"), registry_index = c(2L, 7L),
      demand = c(3, 5), rho = c(0, .4), A = c(0, 2), abar = c(NA, 5), abar_raw = c(NA, 5)),
    support = "fitted positive-demand origins", input_metadata = list(supply_units = "slots"),
    settings = list(rho_floor = .2)), class = "ae_coverage")
  z <- as.data.frame(x, type = "cells")
  expect_identical(z$origin_id, c("b", "a"))
  expect_identical(z$registry_index, c(2L, 7L))
  expect_identical(z$abar, x$cells$abar)
  expect_identical(attr(z, "support"), x$support)
  expect_identical(attr(z, "input_metadata"), x$input_metadata)
  expect_identical(attr(z, "reporting_settings"), x$settings)
  expect_identical(as.data.frame(x)$abar_raw, x$regional$abar_raw)
  expect_true(is.na(as.data.frame(x)$abar))
  path <- tempfile(fileext = ".rds"); on.exit(unlink(path), add = TRUE)
  saveRDS(x, path)
  expect_identical(as.data.frame(readRDS(path), type = "cells"), z)
})
