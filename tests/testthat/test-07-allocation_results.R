# Saved-result inspection (as.data.frame, summary, print and plot) for
# allocation fits, coverage and bootstrap results. These methods read saved
# values and never solve, fit, resample or build maps.

test_that("saved bootstrap tables retain failures, boundaries and denominators", {
  x <- saved_bootstrap_stub(); before <- x
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
  printed <- paste(utils::capture.output(print(s)), collapse = "\n")
  for (key in c("independent_facilities", "slots", "total: 12", "attributed: 9",
                "unsupported: 3", "without support: 1")) {
    expect_match(printed, key, fixed = TRUE)
  }
  undeclared <- x; undeclared$input_metadata$supply_units <- NA_character_
  expect_output(print(undeclared), "raw supply units")
  expect_identical(x, before)
})

test_that("refused bootstrap results remain inspectable without attempts", {
  x <- saved_bootstrap_stub(TRUE)
  expect_equal(nrow(as.data.frame(x, type = "draws")), 0)
  expect_equal(nrow(as.data.frame(x, type = "starts")), 0)
  expect_equal(summary(x)$counts$attempted, 0)
  expect_identical(summary(x)$reasons, "original_unidentified")
  expect_output(print(x), "original_unidentified")
  expect_error(as.data.frame(x, type = "unknown"), "arg")
})

test_that("bootstrap plots draw an interval only for an eligible quantity of a complete run", {
  x <- saved_bootstrap_stub()
  segments <- 0L; captions <- character()
  testthat::local_mocked_bindings(
    segments = function(...) { segments <<- segments + 1L; invisible(NULL) },
    mtext = function(text, ...) {
      captions <<- c(captions, paste(text, collapse = "\n")); invisible(NULL)
    },
    .package = "graphics")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  saved <- withr::local_tempfile(fileext = ".rds")
  # Saved views return their input; an incomplete run draws no interval.
  expect_identical(plot(x, quantity = "regional_rho"), x)
  expect_identical(plot(x, quantity = "regional_rho", type = "draws"), x)
  expect_identical(plot(x, quantity = "regional_A"), x)
  refused <- saved_bootstrap_stub(TRUE)
  expect_identical(plot(refused, type = "draws"), refused)
  refused$intervals$estimate[1] <- NA_real_
  expect_identical(plot(refused), refused)
  expect_equal(segments, 0L)
  expect_error(plot(x, quantity = "absent"), "saved estimate")
  # Stale saved bounds are drawn only when both the run and quantity permit.
  x$intervals$lower[1] <- 9; x$intervals$upper[1] <- 13
  x$intervals$status[1] <- "conditional_pointwise_bootstrap"
  x$status <- "complete"
  plot(x, quantity = "sigma", main = "Saved interval")
  expect_equal(segments, 1L)
  expect_output(print(x), "conditional bootstrap intervals")
  expect_match(tail(captions, 1), "conditional interval eligible")
  expect_false(grepl("conditional_pointwise_bootstrap", tail(captions, 1), fixed = TRUE))
  expect_match(tail(captions, 1), "2/3 finite successful values")
  expected <- c(refused = "refused", incomplete = "incomplete",
    insufficient_success = "insufficient successful draws")
  for (status in names(expected)) {
    x$status <- status
    saveRDS(x, saved)
    restored <- readRDS(saved)
    plot(restored, quantity = "sigma")
    expect_equal(segments, 1L)
    expect_match(tail(captions, 1), expected[[status]])
    expect_identical(as.data.frame(restored), x$intervals)
  }
  x$status <- "complete"
  for (status in c("original_score_failed", "incomplete_bootstrap", "fixed_input_identity")) {
    x$intervals$status[1] <- status
    plot(x, quantity = "sigma")
    expect_equal(segments, 1L)
  }
  expect_match(tail(captions, 1), "fixed input identity")
  x$status <- "incomplete"
  plot(x, quantity = "sigma", type = "draws")
  expect_match(tail(captions, 1), "incomplete")
})

test_that("fit parameter tables name every saved parameter", {
  fit <- structure(list(theta = c(sigma = 3, v0 = 8)), class = "ae_problem_nfxp_fit")
  expect_equal(as.data.frame(fit), data.frame(parameter = c("sigma", "v0"), estimate = c(3, 8)))
  expect_identical(as.data.frame(fit, type = "parameters"), as.data.frame(fit))
  expect_error(as.data.frame(fit, type = "starts"), "arg")
  multistart <- alloc_small_fit()
  expect_identical(as.data.frame(multistart), as.data.frame(multistart$best))
  expect_identical(as.data.frame(multistart, type = "parameters"), as.data.frame(multistart))
  expect_error(as.data.frame(multistart, type = "starts"), "arg")
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
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(x, path)
  expect_identical(as.data.frame(readRDS(path), type = "cells"), z)
  missing <- x; missing$cells <- NULL
  expect_error(as.data.frame(missing, type = "cells"), "unavailable")
})

test_that("inspection methods run on a real fit and a real bootstrap run", {
  fit <- alloc_small_fit()
  run <- alloc_bootstrap_run()
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(plot(fit))
  expect_identical(plot(run), run)
  expect_identical(plot(run, quantity = "regional_rho", type = "draws"), run)
  expect_output(print(fit), "selected start")
  expect_output(print(run), "incomplete")
  expect_equal(summary(run)$counts$attempted, 3)
  expect_identical(as.data.frame(run, type = "draws"), run$draws)
})

# The title and axis labels a plot draws, read where graphics::title() is called
# (plot.default() and the histogram method draw their labels with it), on a null device.
.drawn_labels <- function(code) {
  drawn <- list()
  record <- function() {
    frame <- parent.frame()
    drawn[[length(drawn) + 1L]] <<- list(main = frame$main, xlab = frame$xlab,
                                         ylab = frame$ylab)
  }
  # A call holding the function itself: trace() evaluates it in the frame of title().
  suppressMessages(trace("title", tracer = as.call(list(record)), print = FALSE,
                         where = asNamespace("graphics")))
  on.exit(suppressMessages(untrace("title", where = asNamespace("graphics"))), add = TRUE)
  withr::local_pdf(NULL)
  force(code)
  drawn
}

test_that("fit plots have plain titles that arguments of the call replace", {
  skip_on_cran()
  fit <- alloc_small_fit()
  expect_identical(.drawn_labels(plot(fit)), list(list(
    main = "Observed and fitted values", xlab = "Observed", ylab = "Fitted")))
  expect_identical(.drawn_labels(plot(fit, type = "convergence")), list(list(
    main = "Solver convergence at the fitted values", xlab = "Iteration", ylab = "Error")))
  expect_identical(.drawn_labels(plot(fit, type = "state"))[[1]][c("main", "xlab")],
                   list(main = "Attractiveness at the fitted values", xlab = "Attractiveness"))
  # main, xlab and ylab of the call replace the defaults (they were matched twice
  # before), on the fit and on its selected fit, and other arguments pass through
  own <- list(list(main = "Practices", xlab = "Registered", ylab = "Modelled"))
  for (x in list(fit, fit$best)) {
    for (type in c("fit", "convergence")) {
      expect_identical(.drawn_labels(plot(x, type = type, main = "Practices",
        xlab = "Registered", ylab = "Modelled", pch = 19, col = "grey40")), own)
    }
    expect_identical(.drawn_labels(plot(x, type = "state", main = "Practices",
      xlab = "Registered", ylab = "Modelled", breaks = 3)), own)
  }
  # what each plot returns
  withr::local_pdf(NULL)
  expect_identical(plot(fit), range(c(fit$best$observed, fit$best$predicted)))
  expect_identical(plot(fit, type = "convergence"), fit$best$equilibrium$history)
  expect_identical(plot(fit, type = "state"), fit$best$state)
})
