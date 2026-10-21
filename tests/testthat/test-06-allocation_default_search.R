# Default starts and bounds in fit_allocation() (SPAX-058, T3) ------------------
# The probe + scan rule of R/06-ae_default_starts.R. Expected values are computed
# in the session (a fit from explicit starts under the same strict controls, the
# kernel and seed algebra written out with clm_oracle()), never taken from a log.
# The Cornwall cases are slow and run off CRAN; the small generated and grouped
# cases always run.

.ds_strict <- list(maxit = 300, factr = 1e3, pgtol = 1e-2)

# A fit with explicit starts, strict controls and analytic gradients.
.ds_reference <- function(model, observed, starts, lower, upper, output = "utilization",
                          loss = "poisson") {
  fit_allocation(model, observed, starts, lower, upper, output = output, loss = loss,
                 gradient = TRUE, control = .ds_strict)
}

# A fit result without its timing fields.
.ds_strip <- function(x) {
  x$seconds <- NULL
  x$table$seconds <- NULL
  x$best$seconds <- NULL
  x
}

# The default-search fit equals the reference: coefficients to a relative
# `tolerance`, the loss to 1e-6. The strict control stops at a projected gradient of
# 0.01 per unit of log parameter, which fixes the coefficients to about 1e-4 relative
# (a negligible slope in likelihood terms), so the default tolerance is 5e-4.
.ds_tol <- 5e-4
.ds_expect_same <- function(default, reference, tolerance = .ds_tol) {
  expect_identical(names(coef(default)), names(coef(reference)))
  expect_lt(max(abs(coef(default) / coef(reference) - 1)), tolerance)
  expect_lt(abs(default$best$loss - reference$best$loss), 1e-6)
}

.ds_message <- function(expr) {
  tryCatch({ expr; NULL }, error = function(e) conditionMessage(e))
}

.ds_wmedian <- function(x, w) {
  o <- order(x)
  unname(x[o][which(cumsum(w[o]) >= 0.5 * sum(w))[1L]])
}

# Cornwall mainland inputs: practices and areas without the Isles of Scilly,
# registered patients as demand, GP FTE excluding training as supply, the
# observed flows as an area-by-practice matrix.
.ds_cornwall <- function() {
  alloc_memo("default_search_cornwall", {
    gp <- sf::st_drop_geometry(cornwall_gp)
    gp <- gp[!gp$scilly, ]
    areas <- sf::st_drop_geometry(cornwall_lsoa)
    areas <- areas[!areas$scilly, ]
    travel <- cornwall_travel[areas$lsoa, gp$practice]
    demand <- stats::setNames(areas$registered, areas$lsoa)
    supply <- stats::setNames(gp$gp_fte_extg, gp$practice)
    flows <- matrix(0, nrow(travel), ncol(travel), dimnames = dimnames(travel))
    keep <- cornwall_flows$lsoa %in% rownames(travel) &
      cornwall_flows$practice %in% colnames(travel)
    flows[cbind(cornwall_flows$lsoa[keep], cornwall_flows$practice[keep])] <-
      cornwall_flows$patients[keep]
    list(demand = demand, supply = supply, travel = travel, flows = flows,
         totals = colSums(flows),
         inputs = prepare_allocation(demand, supply, travel))
  })
}

.ds_plateau <- function() {
  decay_kernel(function(distance, sigma, plateau) {
    ifelse(distance <= plateau, 1, exp(-(distance - plateau)^2 / (2 * sigma^2)))
  }, name = "plateau", parameter = "scale", args = list(plateau = 3))
}

# (a) The default search finds the optimum ----------------------------------------

test_that("the default search equals a fit from bracketing starts on the generated case", {
  case <- alloc_generated_case()
  totals <- fit_allocation(case$model, case$y, loss = "poisson")
  expect_s3_class(totals, "ae_multistart_fit")
  .ds_expect_same(totals, alloc_generated_fit())
  expect_false(any(totals$boundary[totals$best_start, ]))
  flows <- fit_allocation(case$model, case$flow, output = "flow", loss = "poisson")
  .ds_expect_same(flows, alloc_generated_fit(output = "flow"))
  # the three starts are the ones of the record, named scan1 ...
  expect_identical(rownames(totals$starts), paste0("scan", seq_len(nrow(totals$starts))))
  expect_lte(nrow(totals$starts), 3L)
  expect_identical(totals$starts, totals$default_search$starts)
})

test_that("the Cornwall cases of the design evidence table reach the optimum", {
  skip_on_cran()
  cw <- .ds_cornwall()
  plateau <- .ds_plateau()
  cases <- list(
    list("CLM gaussian flows", "gaussian", TRUE, FALSE, "flow",
         rbind(a = c(sigma = 3.5, v0 = 0.05), b = c(sigma = 6.5, v0 = 0.015)),
         c(sigma = 0.5, v0 = 1e-5), c(sigma = 60, v0 = 10)),
    list("CLM gaussian totals", "gaussian", TRUE, FALSE, "utilization",
         rbind(a = c(sigma = 2, v0 = 0.004), b = c(sigma = 3.2, v0 = 0.0012)),
         c(sigma = 0.5, v0 = 1e-6), c(sigma = 60, v0 = 10)),
    list("CLM exponential flows", "exponential", TRUE, FALSE, "flow",
         rbind(a = c(sigma = 0.3, v0 = 0.015), b = c(sigma = 0.55, v0 = 0.004)),
         c(sigma = 0.01, v0 = 1e-6), c(sigma = 5, v0 = 10)),
    list("CLM exponential totals", "exponential", TRUE, FALSE, "utilization",
         rbind(a = c(sigma = 0.0006, v0 = 5), b = c(sigma = 0.0016, v0 = 1.5)),
         c(sigma = 1e-5, v0 = 1e-3), c(sigma = 0.05, v0 = 1e3)),
    list("Huff power flows", "power", FALSE, TRUE, "flow",
         rbind(a = c(sigma = 2, beta = 0.6), b = c(sigma = 2.8, beta = 0.4)),
         c(sigma = 0.1, beta = 0.05), c(sigma = 20, beta = 5)),
    list("Huff power totals", "power", FALSE, TRUE, "utilization",
         rbind(a = c(sigma = 0.8, beta = 0.9), b = c(sigma = 1.4, beta = 0.4)),
         c(sigma = 0.1, beta = 0.05), c(sigma = 20, beta = 5)),
    list("Huff gaussian flows", "gaussian", FALSE, TRUE, "flow",
         rbind(a = c(sigma = 3.5, beta = 0.8), b = c(sigma = 6, beta = 0.35)),
         c(sigma = 0.5, beta = 0.05), c(sigma = 60, beta = 5)),
    list("CLM gaussian+beta flows", "gaussian", TRUE, TRUE, "flow",
         rbind(a = c(sigma = 4, v0 = 0.02, beta = 0.65),
               b = c(sigma = 5.2, v0 = 0.009, beta = 0.45)),
         c(sigma = 0.5, v0 = 1e-5, beta = 0.05), c(sigma = 60, v0 = 10, beta = 5)),
    list("CLM gaussian+beta totals", "gaussian", TRUE, TRUE, "utilization",
         rbind(a = c(sigma = 6.5, v0 = 0.012, beta = 0.75),
               b = c(sigma = 9, v0 = 0.006, beta = 0.6)),
         c(sigma = 0.5, v0 = 1e-5, beta = 0.05), c(sigma = 60, v0 = 10, beta = 5)),
    list("CLM plateau flows", plateau, TRUE, FALSE, "flow",
         rbind(a = c(sigma = 3, v0 = 0.1), b = c(sigma = 5, v0 = 0.02)),
         c(sigma = 0.5, v0 = 1e-5), c(sigma = 60, v0 = 10)))
  for (cs in cases) {
    label <- cs[[1]]
    model <- clm_allocation(cw$inputs, cs[[2]], fit_v0 = cs[[3]], fit_beta = cs[[4]])
    observed <- if (cs[[5]] == "flow") cw$flows else cw$totals
    default <- fit_allocation(model, observed, output = cs[[5]], loss = "poisson")
    reference <- .ds_reference(model, observed, cs[[6]], cs[[7]], cs[[8]], cs[[5]])
    info <- label
    expect_identical(default$status, "selected", info = info)
    expect_false(default$boundary[default$best_start, ] |> any(), info = info)
    expect_identical(names(coef(default)), names(coef(reference)), info = info)
    # The strict control fixes coefficients to about 1e-4 relative (see .ds_tol).
    tolerance <- .ds_tol
    expect_lt(max(abs(coef(default) / coef(reference) - 1)), tolerance, label = info)
    expect_lt(abs(default$best$loss - reference$best$loss), 1e-6, label = info)
  }
})

# (b) Unit invariance ------------------------------------------------------------

test_that("rescaling the travel times rescales sigma and keeps the loss", {
  case <- alloc_generated_case()
  for (family in c("gaussian", "exponential")) {
    for (output in c("utilization", "flow")) {
      observed <- if (output == "flow") case$flow else case$y
      fits <- lapply(c(1, 60, 1 / 60), function(scale) {
        inputs <- prepare_allocation(case$demand, case$supply, case$distance * scale)
        fit_allocation(clm_allocation(inputs, family, fit_v0 = TRUE), observed,
                       output = output, loss = "poisson")
      })
      info <- paste(family, output)
      base <- fits[[1]]
      # a scale (gaussian) grows with the travel times, a rate (exponential) shrinks
      factor <- if (family == "gaussian") c(60, 1 / 60) else c(1 / 60, 60)
      for (k in 2:3) {
        expect_lt(abs(fits[[k]]$best$loss - base$best$loss), 1e-6, label = info)
        expect_lt(abs(coef(fits[[k]])[["sigma"]] / (coef(base)[["sigma"]] * factor[k - 1]) - 1),
                  .ds_tol, label = info)
        expect_lt(abs(coef(fits[[k]])[["v0"]] / coef(base)[["v0"]] - 1), .ds_tol, label = info)
      }
    }
  }
})

test_that("Cornwall fits keep their loss when travel is in seconds or hours", {
  skip_on_cran()
  cw <- .ds_cornwall()
  for (family in c("gaussian", "exponential")) {
    for (output in c("utilization", "flow")) {
      observed <- if (output == "flow") cw$flows else cw$totals
      fits <- lapply(c(1, 60, 1 / 60), function(scale) {
        inputs <- prepare_allocation(cw$demand, cw$supply, cw$travel * scale)
        fit_allocation(clm_allocation(inputs, family, fit_v0 = TRUE), observed,
                       output = output, loss = "poisson")
      })
      info <- paste(family, output)
      factor <- if (family == "gaussian") c(60, 1 / 60) else c(1 / 60, 60)
      for (k in 2:3) {
        expect_lt(abs(fits[[k]]$best$loss - fits[[1]]$best$loss), 1e-6, label = info)
        expect_lt(abs(coef(fits[[k]])[["sigma"]] /
                        (coef(fits[[1]])[["sigma"]] * factor[k - 1]) - 1), .ds_tol, label = info)
      }
    }
  }
  # power decay is scale free
  fits <- lapply(c(1, 60), function(scale) {
    inputs <- prepare_allocation(cw$demand, cw$supply, cw$travel * scale)
    fit_allocation(huff_allocation(inputs), cw$totals, loss = "poisson")
  })
  expect_lt(abs(fits[[2]]$best$loss - fits[[1]]$best$loss), 1e-6)
  expect_lt(max(abs(coef(fits[[2]]) / coef(fits[[1]]) - 1)), 1e-5)
})

# (c) Given starts and bounds run as before --------------------------------------

test_that("given starts, lower and upper give the multistart result unchanged", {
  case <- alloc_generated_case()
  a <- fit_allocation(case$model, case$y, case$starts, case$lower, case$upper,
                      loss = "poisson", gradient = TRUE, control = .ds_strict)
  b <- .fit_problem_multistart(case$model, case$y, case$starts, case$lower, case$upper,
    output = "utilization", loss = .poisson_loss, loss_args = list(eps = 1e-250),
    loss_grad = .poisson_gradient, gradient = TRUE, control = .ds_strict)
  expect_identical(.ds_strip(a), .ds_strip(b))
  expect_null(a$default_search)
  expect_false("default_search" %in% names(a))
  # the loss and gradient defaults of today (weighted SSE, no analytic gradient)
  c1 <- fit_allocation(case$model, case$y, case$starts, case$lower, case$upper)
  c2 <- .fit_problem_multistart(case$model, case$y, case$starts, case$lower, case$upper,
    output = "utilization", loss = .weighted_sse_loss, loss_args = list(),
    loss_grad = .weighted_sse_gradient, gradient = FALSE)
  expect_identical(.ds_strip(c1), .ds_strip(c2))
  # unnamed bounds with one value per parameter are complete, as before
  d1 <- fit_allocation(case$model, case$y, case$starts, unname(case$lower),
                       unname(case$upper), loss = "poisson", gradient = TRUE,
                       control = list(maxit = 20))
  d2 <- fit_allocation(case$model, case$y, case$starts, case$lower, case$upper,
                       loss = "poisson", gradient = TRUE, control = list(maxit = 20))
  expect_identical(.ds_strip(d1), .ds_strip(d2))
  # flow output, with an NA cell
  flow <- case$flow
  flow[2, 3] <- NA
  e1 <- fit_allocation(case$model, flow, case$starts, case$lower, case$upper,
                       output = "flow", loss = "poisson", gradient = TRUE,
                       control = list(maxit = 30))
  e2 <- .fit_problem_multistart(case$model, flow, case$starts, case$lower, case$upper,
    output = "flow", loss = .poisson_loss, loss_args = list(eps = 1e-250),
    loss_grad = .poisson_gradient, gradient = TRUE, control = list(maxit = 30))
  expect_identical(.ds_strip(e1), .ds_strip(e2))
})

test_that("an error from the fit of the default search shows a short call", {
  case <- alloc_generated_case()
  e <- tryCatch(fit_allocation(case$model, case$y, loss = "poisson", bogus = 1),
                error = function(e) e)
  expect_identical(conditionMessage(e),
    "`...` must contain unique named fitting controls, excluding init and bounds")
  # a call to the function by name, with the arguments as symbols and the controls given
  # to the search; not the function or the data written out
  expect_identical(as.character(conditionCall(e)[[1L]]), ".fit_problem_multistart")
  expect_lt(nchar(paste(deparse(conditionCall(e)), collapse = "")), 400L)
  expect_identical(as.character(conditionCall(e)[[2L]]), "model")
  # the same error without a search
  e2 <- tryCatch(fit_allocation(case$model, case$y, case$starts, case$lower, case$upper,
                                loss = "poisson", bogus = 1), error = function(e) e)
  expect_identical(as.character(conditionCall(e2)[[1L]]), ".fit_problem_multistart")
})

test_that("complete calls never run the search", {
  case <- alloc_generated_case()
  expect_no_error(testthat::with_mocked_bindings(
    fit_allocation(case$model, case$y, case$starts, case$lower, case$upper,
                   loss = "poisson", control = list(maxit = 5)),
    .allocation_default_search = function(...) stop("the search ran"),
    .default_scan = function(...) stop("the scan ran")))
})

# (d) The table of given and missing inputs --------------------------------------

test_that("row 2: no starts and no bounds run the full search", {
  case <- alloc_generated_case()
  f <- fit_allocation(case$model, case$y, loss = "poisson")
  ds <- f$default_search
  expect_identical(ds$rule, "probe+scan v1")
  expect_identical(ds$loss, "poisson")
  expect_identical(ds$n_points, 45L)
  expect_identical(ds$n_finite, 45L)
  expect_identical(names(ds$grid), c("point", "sigma", "v0", "loss"))
  expect_identical(nrow(ds$grid), 45L)
  expect_identical(ds$grid$point, rep(1:15, each = 3))
  expect_named(ds$sigma_range, c("lower", "upper"))
  # the grid is 15 log-spaced values over the range, steep to flat (a gaussian
  # scale ascends), the same value at the three v0 seeds
  sigma <- ds$grid$sigma[seq(1, 45, by = 3)]
  range <- ds$sigma_range
  expect_equal(sigma, exp(seq(log(range[["lower"]]), log(range[["upper"]]), length.out = 15)),
               ignore_attr = TRUE)
  expect_identical(ds$grid$sigma, rep(sigma, each = 3))
  expect_identical(ds$bounds$lower[["sigma"]], range[["lower"]])
  expect_identical(ds$bounds$upper[["sigma"]], range[["upper"]])
  expect_identical(ds$bounds, list(lower = f$bounds$lower, upper = f$bounds$upper))
  expect_identical(f$starts, ds$starts)
  # strict controls and gradients were the defaults of the search
  expect_true(f$best$gradient)
  expect_identical(ds$controls$control, list(maxit = 300, factr = 1e3, pgtol = 1e-2))
  expect_true(ds$controls$gradient_default)
  expect_true(ds$controls$control_default)
})

test_that("row 1 and the controls: what the caller gives is kept", {
  case <- alloc_generated_case()
  f <- fit_allocation(case$model, case$y, loss = "poisson", gradient = FALSE,
                      control = list(maxit = 40))
  expect_false(f$best$gradient)
  expect_false(f$default_search$controls$gradient_default)
  expect_false(f$default_search$controls$control_default)
  expect_identical(f$default_search$controls$control, list(maxit = 40))
})

test_that("row 3: no starts with given bounds, entry by entry", {
  case <- alloc_generated_case()
  base <- fit_allocation(case$model, case$y, loss = "poisson")
  # a partial lower and a partial upper replace those entries only
  f <- fit_allocation(case$model, case$y, lower = c(sigma = 3), upper = c(v0 = 100),
                      loss = "poisson")
  ds <- f$default_search
  expect_identical(f$bounds$lower[["sigma"]], 3)
  expect_identical(f$bounds$upper[["v0"]], 100)
  expect_identical(f$bounds$upper[["sigma"]], ds$sigma_range[["upper"]])
  expect_identical(f$bounds$lower[["v0"]], ds$bounds$lower[["v0"]])
  # the grid and the seeds lie inside the merged bounds, the starts are clamped
  expect_true(all(ds$grid$sigma >= 3))
  expect_true(all(ds$grid$v0 <= 100))
  expect_true(all(ds$starts[, "sigma"] >= 3 & ds$starts[, "v0"] <= 100))
  expect_true(ds$sigma_range[["lower"]] >= 3)
  expect_equal(coef(f), coef(base), tolerance = 1e-4)
  # complete given bounds are used as given
  g <- fit_allocation(case$model, case$y, lower = c(sigma = 2, v0 = 1),
                      upper = c(sigma = 40, v0 = 200), loss = "poisson")
  expect_identical(g$bounds$lower, c(sigma = 2, v0 = 1))
  expect_identical(g$bounds$upper, c(sigma = 40, v0 = 200))
  expect_true(all(g$default_search$grid$sigma >= 2 & g$default_search$grid$sigma <= 40))
  expect_true(all(g$default_search$grid$v0 >= 1 & g$default_search$grid$v0 <= 200))
  expect_equal(coef(g), coef(base), tolerance = 1e-4)
  # unnamed complete bounds are read in parameter order
  h <- fit_allocation(case$model, case$y, lower = c(2, 1), upper = c(40, 200), loss = "poisson")
  expect_identical(h$bounds$lower, c(sigma = 2, v0 = 1))
  # a bound that is a Huff beta: the seed 1 is clamped into the merged bounds
  huff <- huff_allocation(case$inputs)
  k <- fit_allocation(huff, case$y, lower = c(beta = 1.2), loss = "poisson")
  expect_true(all(k$default_search$grid$beta == 1.2))
  expect_identical(k$bounds$lower[["beta"]], 1.2)
  expect_identical(k$bounds$upper[["beta"]], 5)
})

test_that("row 3: crossing bounds are an error naming both sources", {
  case <- alloc_generated_case()
  huff <- huff_allocation(case$inputs)
  expect_identical(.ds_message(fit_allocation(huff, case$y, lower = c(beta = 6), loss = "poisson")),
    "lower must be less than upper for beta: 6 (given) and 5 (default)")
  expect_identical(.ds_message(fit_allocation(huff, case$y, upper = c(beta = 0.01),
                                              loss = "poisson")),
    "lower must be less than upper for beta: 0.05 (default) and 0.01 (given)")
  expect_identical(.ds_message(fit_allocation(case$model, case$y, lower = c(sigma = 10),
    upper = c(sigma = 5), loss = "poisson")),
    "lower must be less than upper for sigma: 10 (given) and 5 (given)")
  # given starts and a crossing pair
  expect_identical(.ds_message(fit_allocation(case$model, case$y, case$starts,
    lower = c(sigma = 10), upper = c(sigma = 5))),
    "lower must be less than upper for sigma: 10 (given) and 5 (given)")
  # bounds that are not named positive parameters
  expect_match(.ds_message(fit_allocation(case$model, case$y, lower = c(gamma = 1),
    loss = "poisson")), "`lower` must be a named positive numeric vector", fixed = TRUE)
  expect_match(.ds_message(fit_allocation(case$model, case$y, upper = c(sigma = -1),
    loss = "poisson")), "`upper` must be a named positive numeric vector", fixed = TRUE)
})

test_that("row 4: given starts with missing or partial bounds use default bounds", {
  case <- alloc_generated_case()
  starts <- rbind(a = c(sigma = 6, v0 = 20), b = c(sigma = 9, v0 = 30))
  f <- fit_allocation(case$model, case$y, starts, loss = "poisson")
  # the given starts are used as they are, no record of a search
  expect_identical(f$starts, starts)
  expect_null(f$default_search)
  expect_false(f$best$gradient)
  expect_equal(coef(f), coef(alloc_generated_fit()), tolerance = 1e-3)
  default <- fit_allocation(case$model, case$y, loss = "poisson")$default_search$bounds
  expect_identical(f$bounds, default)
  # given entries replace the default ones, the rest stays
  g <- fit_allocation(case$model, case$y, starts, lower = c(v0 = 2), upper = c(sigma = 50),
                      loss = "poisson")
  expect_identical(g$bounds$lower[["v0"]], 2)
  expect_identical(g$bounds$upper[["sigma"]], 50)
  expect_identical(g$bounds$lower[["sigma"]], default$lower[["sigma"]])
  # the default bound of v0 comes from the scan, whose seeds respect the given bound
  expect_true(is.finite(g$bounds$upper[["v0"]]) && g$bounds$upper[["v0"]] > max(starts[, "v0"]))
  # a default entry widens to include every start
  wide <- rbind(a = c(sigma = 400, v0 = 20), b = c(sigma = 0.1, v0 = 1e5))
  h <- fit_allocation(case$model, case$y, wide, loss = "poisson", control = list(maxit = 3))
  expect_identical(h$bounds$upper[["sigma"]], 400)
  expect_identical(h$bounds$lower[["sigma"]], 0.1)
  expect_identical(h$bounds$upper[["v0"]], 1e5)
  expect_identical(h$bounds$lower[["v0"]], default$lower[["v0"]])
  # a given entry is not widened: the start outside it is refused as before
  expect_match(.ds_message(fit_allocation(case$model, case$y, wide, upper = c(sigma = 50),
    loss = "poisson")), "every start must be inside", fixed = TRUE)
  # a Huff model: beta widens too
  huff <- huff_allocation(case$inputs)
  hs <- rbind(a = c(sigma = 1, beta = 7), b = c(sigma = 2, beta = 0.01))
  k <- fit_allocation(huff, case$y, hs, loss = "poisson", control = list(maxit = 3))
  expect_identical(k$bounds$upper[["beta"]], 7)
  expect_identical(k$bounds$lower[["beta"]], 0.01)
  # given starts keep today's gradient and control defaults; a given gradient is kept
  m <- fit_allocation(case$model, case$y, starts, loss = "poisson", gradient = TRUE)
  expect_true(m$best$gradient)
})

test_that("strict controls apply only to searched starts, each unless the caller gave it", {
  case <- alloc_generated_case()
  captured <- NULL
  capture <- function(problem, observed, starts, lower, upper, ...) {
    captured <<- list(...)
    stop("captured")
  }
  run <- function(...) {
    captured <<- NULL
    try(testthat::with_mocked_bindings(fit_allocation(case$model, case$y, ...),
      .fit_problem_multistart = capture), silent = TRUE)
    captured
  }
  strict <- list(maxit = 300, factr = 1e3, pgtol = 1e-2)
  # starts NULL, neither given: both defaults
  a <- run(loss = "poisson")
  expect_true(a$gradient)
  expect_identical(a$control, strict)
  # starts NULL, one given: only the other defaults
  b <- run(loss = "poisson", gradient = FALSE)
  expect_false(b$gradient)
  expect_identical(b$control, strict)
  c <- run(loss = "poisson", control = list(maxit = 7))
  expect_true(c$gradient)
  expect_identical(c$control, list(maxit = 7))
  d <- run(loss = "poisson", gradient = FALSE, control = list(maxit = 7))
  expect_false(d$gradient)
  expect_identical(d$control, list(maxit = 7))
  # a custom loss without a gradient: none
  e <- run(loss = function(predicted, observed) sum(predicted - observed))
  expect_false(e$gradient)
  expect_identical(e$control, strict)
  # starts given, bounds missing or partial: today's defaults
  f <- run(case$starts, loss = "poisson")
  expect_false(f$gradient)
  expect_null(f$control)
  g <- run(case$starts, lower = c(sigma = 1), loss = "poisson")
  expect_false(g$gradient)
  expect_null(g$control)
  # everything given: untouched
  h <- run(case$starts, case$lower, case$upper, loss = "poisson")
  expect_false(h$gradient)
  expect_null(h$control)
})

test_that("row 4: the scan runs only for a default bound of v0", {
  case <- alloc_generated_case()
  starts <- rbind(a = c(sigma = 6, v0 = 20), b = c(sigma = 9, v0 = 30))
  scanned <- 0L
  count <- function(...) {
    scanned <<- scanned + 1L
    stop("the scan ran")
  }
  # both bounds of v0 given: no scan, whatever else is missing
  expect_no_error(testthat::with_mocked_bindings(
    fit_allocation(case$model, case$y, starts, lower = c(v0 = 1), upper = c(v0 = 200),
                   loss = "poisson", control = list(maxit = 3)),
    .default_scan = count))
  expect_identical(scanned, 0L)
  # a v0 bound missing: the scan finds it
  expect_error(testthat::with_mocked_bindings(
    fit_allocation(case$model, case$y, starts, lower = c(sigma = 1, v0 = 1),
                   upper = c(sigma = 50), loss = "poisson"),
    .default_scan = count), "the scan ran")
  # a Huff model has no v0: starts alone need no scan
  huff <- huff_allocation(case$inputs)
  expect_no_error(testthat::with_mocked_bindings(
    fit_allocation(huff, case$y, rbind(a = c(sigma = 1, beta = 1)), loss = "poisson",
                   control = list(maxit = 3)),
    .default_scan = count))
  expect_identical(scanned, 1L)
})

test_that("the loss must be named when no starts are given (D5)", {
  case <- alloc_generated_case()
  msg <- paste0("choose the loss when starts are not given: loss = \"poisson\" for ",
                "counts, or \"weighted_sse\"")
  expect_identical(.ds_message(fit_allocation(case$model, case$y)), msg)
  expect_identical(.ds_message(fit_allocation(case$model, case$y, lower = c(sigma = 1))), msg)
  # with starts the old default stays: weighted SSE
  expect_no_error(fit_allocation(case$model, case$y, case$starts, case$lower, case$upper,
                                 control = list(maxit = 5)))
  expect_no_error(fit_allocation(case$model, case$y, case$starts, lower = c(sigma = 1),
                                 control = list(maxit = 5)))
})

# (e) Edge cases -----------------------------------------------------------------

test_that("D7: a fully observed target that reaches the demand is an error, near it a warning", {
  case <- alloc_generated_case()
  total <- sum(case$demand)
  scale <- function(share) case$y * (share * total / sum(case$y))
  expect_identical(.ds_message(fit_allocation(case$model, scale(1.01), loss = "poisson")),
    paste0("the observed counts reach the total demand, so the outside weight would go ",
           "to zero; use a model without an outside option (spec_huff() or outside = 0, ",
           "without outside counts; fit_v0 = FALSE in clm_allocation()) or check the targets"))
  # warned once, not at every grid point
  warns <- character()
  fit <- withCallingHandlers(
    fit_allocation(case$model, scale(0.9995), loss = "poisson", control = list(maxit = 20)),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_identical(warns, paste0("the observed counts are within 0.1% of the total ",
    "demand; the outside weight is barely identified"))
  expect_s3_class(fit, "ae_multistart_fit")
  # an outside share of a few per cent is not flagged
  expect_no_warning(fit_allocation(case$model, scale(0.97), loss = "poisson"))
  # flows: the same guard
  expect_match(.ds_message(fit_allocation(case$model, case$flow * (1.01 * total / sum(case$flow)),
    output = "flow", loss = "poisson")), "the observed counts reach the total demand",
    fixed = TRUE)
  # no outside weight to seed: no guard
  expect_no_error(fit_allocation(huff_allocation(case$inputs), scale(1.01), loss = "poisson",
                                 control = list(maxit = 10)))
})

test_that("NA targets run the search with a seed that is consistent with the mask", {
  case <- alloc_generated_case()
  distance <- case$distance
  # flows: cells under 5 are unobserved
  flow <- case$flow
  flow[flow > 0 & flow < 5] <- NA
  expect_gt(sum(is.na(flow)), 0)
  f <- fit_allocation(case$model, flow, output = "flow", loss = "poisson")
  reference <- .ds_reference(case$model, flow, rbind(a = c(sigma = 6, v0 = 15),
    b = c(sigma = 10, v0 = 40)), c(sigma = 1, v0 = 1), c(sigma = 50, v0 = 500), "flow")
  .ds_expect_same(f, reference)
  # the seed at a grid point written out: outside share on the observed cells of
  # the model without an outside option, times the demand-weighted median opportunity
  seed_at <- function(sigma, observed, output) {
    o <- clm_oracle(distance, case$demand, case$supply, sigma, v0 = 0)
    pred0 <- if (output == "flow") o$flow else colSums(o$flow)
    mask <- !is.na(observed)
    s <- min(max(1 - sum(observed[mask]) / sum(pred0[mask]), 1e-6), 1 - 1e-6)
    s / (1 - s) * .ds_wmedian(o$denom, case$demand)
  }
  grid <- f$default_search$grid
  for (row in c(2, 17, 41)) {
    expect_equal(grid$v0[row], seed_at(grid$sigma[row], flow, "flow"), tolerance = 1e-8)
  }
  expect_equal(grid$v0[1], 0.1 * grid$v0[2], tolerance = 1e-12)
  expect_equal(grid$v0[3], 10 * grid$v0[2], tolerance = 1e-12)
  # a naive share over all demand differs: the mask matters
  naive <- 1 - sum(flow, na.rm = TRUE) / sum(case$demand)
  masked <- 1 - sum(flow, na.rm = TRUE) /
    sum(clm_oracle(distance, case$demand, case$supply, grid$sigma[23], v0 = 0)$flow[!is.na(flow)])
  expect_gt(abs(naive - masked), 1e-3)
  # totals with unobserved facilities
  totals <- case$y
  totals[c(2, 7, 11)] <- NA
  g <- fit_allocation(case$model, totals, loss = "poisson")
  gref <- .ds_reference(case$model, totals, rbind(a = c(sigma = 6, v0 = 15),
    b = c(sigma = 10, v0 = 40)), c(sigma = 1, v0 = 1), c(sigma = 50, v0 = 500))
  .ds_expect_same(g, gref)
  grid <- g$default_search$grid
  expect_equal(grid$v0[5], seed_at(grid$sigma[5], totals, "utilization"), tolerance = 1e-8)
  # an unobserved target gets the clamped share, with no D7 guard
  expect_no_error(fit_allocation(case$model, replace(case$y * 1.5, 1, NA), loss = "poisson",
                                 control = list(maxit = 10)))
})

test_that("Cornwall: NA flow cells and the registered_in demand (off CRAN)", {
  skip_on_cran()
  cw <- .ds_cornwall()
  model <- cw$inputs |> clm_allocation(fit_v0 = TRUE)
  flows <- cw$flows
  flows[flows > 0 & flows < 5] <- NA
  expect_gt(sum(is.na(flows)), 100)
  f <- fit_allocation(model, flows, output = "flow", loss = "poisson")
  reference <- .ds_reference(model, flows, rbind(a = c(sigma = 3.5, v0 = 0.05),
    b = c(sigma = 6.5, v0 = 0.015)), c(sigma = 0.5, v0 = 1e-5), c(sigma = 60, v0 = 10), "flow")
  .ds_expect_same(f, reference, tolerance = 1e-4)
  expect_false(any(f$boundary[f$best_start, ]))
  # a seed that ignores the mask would be far off: the mask-consistent seed is the
  # outside share of the observed cells
  grid <- f$default_search$grid
  o <- clm_oracle(cw$travel, cw$demand, cw$supply, grid$sigma[23], v0 = 0)
  mask <- !is.na(flows)
  s <- min(max(1 - sum(flows[mask]) / sum(o$flow[mask]), 1e-6), 1 - 1e-6)
  expect_equal(grid$v0[23], s / (1 - s) * .ds_wmedian(o$denom, cw$demand), tolerance = 1e-8)
  # the observed counts reach the registered_in demand to within 0.1%: D7 flags it
  areas <- sf::st_drop_geometry(cornwall_lsoa)
  areas <- areas[!areas$scilly, ]
  inputs_in <- prepare_allocation(stats::setNames(areas$registered_in, areas$lsoa), cw$supply,
                                  cw$travel)
  model_in <- clm_allocation(inputs_in, fit_v0 = TRUE)
  expect_warning(
    .allocation_default_search(model_in, .bind_problem_target(model_in, cw$flows, "flow"),
      "flow", .poisson_loss, list(eps = 1e-250)),
    "within 0.1% of the total demand", fixed = TRUE)
})

test_that("an all-zero target is an error", {
  case <- alloc_generated_case()
  msg <- paste0("the observed counts are all zero or unobserved, so there is nothing ",
                "to calibrate against")
  expect_identical(.ds_message(fit_allocation(case$model, case$y * 0, loss = "poisson")), msg)
  expect_identical(.ds_message(fit_allocation(case$model, case$flow * 0, output = "flow",
                                              loss = "poisson")), msg)
  flow <- case$flow * 0
  flow[1, 1] <- NA
  expect_identical(.ds_message(fit_allocation(case$model, flow, output = "flow",
                                              loss = "poisson")), msg)
})

test_that("an origin with no finite travel does not enter the travel summary", {
  case <- alloc_generated_case()
  travel <- case$distance
  travel[3, ] <- Inf
  inputs <- prepare_allocation(case$demand, case$supply, travel)
  model <- clm_allocation(inputs, fit_v0 = TRUE)
  without <- clm_allocation(prepare_allocation(case$demand[-3], case$supply,
                                               case$distance[-3, ]), fit_v0 = TRUE)
  expect_identical(.default_travel_summary(.default_search_units(model)),
                   .default_travel_summary(.default_search_units(without)))
  y <- round(evaluate_allocation(model, c(sigma = 8, v0 = 25))$outputs$utilization)
  names(y) <- inputs$facility_ids
  f <- fit_allocation(model, y, loss = "poisson")
  g <- fit_allocation(without, y, loss = "poisson")
  expect_identical(f$default_search$sigma_range, g$default_search$sigma_range)
  .ds_expect_same(f, g)
  # nothing finite at all
  none <- prepare_allocation(case$demand, case$supply, matrix(Inf, nrow(travel), ncol(travel),
    dimnames = dimnames(travel)))
  expect_identical(.ds_message(fit_allocation(clm_allocation(none, fit_v0 = TRUE), case$y,
    loss = "poisson")), "no finite travel times among the active origins")
})

test_that("travel summaries: the nearest facility of each origin sets far", {
  case <- alloc_generated_case()
  travel <- case$distance
  summary <- .default_travel_summary(.default_search_units(case$model))
  expect_identical(summary$d_lo, min(travel[travel > 0]))
  expect_identical(summary$far, max(apply(travel, 1, min)))
  expect_identical(summary$top, max(travel))
  # a zero travel time is not the smallest positive one
  travel[1, 1] <- 0
  zero <- clm_allocation(prepare_allocation(case$demand, case$supply, travel), fit_v0 = TRUE)
  s0 <- .default_travel_summary(.default_search_units(zero))
  expect_identical(s0$d_lo, min(travel[travel > 0]))
  expect_gte(s0$far, s0$d_lo)
})

test_that("power decay with a zero travel time gives the engine's message before any scan", {
  case <- alloc_generated_case()
  travel <- case$distance
  travel[1, 1] <- 0
  inputs <- prepare_allocation(case$demand, case$supply, travel)
  msg <- "power decay requires strictly positive finite travel distances"
  solved <- 0L
  err <- testthat::with_mocked_bindings(
    .ds_message(fit_allocation(huff_allocation(inputs), case$y, loss = "poisson")),
    .solve_problem = function(...) {
      solved <<- solved + 1L
      stop("solved")
    })
  expect_identical(err, msg)
  expect_identical(solved, 0L)
  # a declared kernel undefined at zero: the engine's message for it
  undefined <- decay_kernel(function(distance, sigma) distance^(-sigma), name = "inv",
                            parameter = "exponent", zero_distance = FALSE)
  expect_identical(.ds_message(fit_allocation(clm_allocation(inputs, undefined, fit_v0 = TRUE),
    case$y, loss = "poisson")), "inv decay requires strictly positive finite travel distances")
  # gaussian decay accepts the zero
  expect_no_error(fit_allocation(clm_allocation(inputs, "gaussian", fit_v0 = TRUE), case$y,
                                 loss = "poisson", control = list(maxit = 10)))
})

test_that("power decay works for travel times below one, in any unit", {
  case <- alloc_generated_case()
  fits <- lapply(c(1 / 20, 1 / 1200, 3), function(scale) {
    inputs <- prepare_allocation(case$demand, case$supply, case$distance * scale)
    fit_allocation(huff_allocation(inputs), case$y, loss = "poisson")
  })
  expect_lt(min(case$distance * 1 / 1200), 1)
  for (k in 2:3) {
    expect_lt(abs(fits[[k]]$best$loss - fits[[1]]$best$loss), 1e-6)
    expect_lt(max(abs(coef(fits[[k]]) / coef(fits[[1]]) - 1)), .ds_tol)
  }
  expect_false(any(fits[[1]]$boundary[fits[[1]]$best_start, ]))
  # a power exponent runs from the flattest to the steepest kernel downwards
  range <- fits[[1]]$default_search$sigma_range
  grid <- fits[[1]]$default_search$grid
  expect_true(all(diff(grid$sigma) <= 0))
  expect_equal(grid$sigma[1], range[["upper"]])
})

test_that("the sigma range is the probed range, and grids run steep to flat", {
  case <- alloc_generated_case()
  distance <- case$distance
  d <- c(min(distance[distance > 0]), max(apply(distance, 1, min)), max(distance))
  kernels <- list(
    gaussian = function(x, s) exp(-x^2 / (2 * s^2)),
    exponential = function(x, s) exp(-s * x),
    power = function(x, s) x^(-s))
  for (family in names(kernels)) {
    probes <- 10^seq(-8, 8, by = 1 / 8)
    ok <- vapply(probes, function(s) {
      w <- kernels[[family]](d, s)
      all(is.finite(w)) && w[1] > 0 && w[2] > 0 && w[3] >= 0 &&
        log(w[2]) - log(w[1]) >= -200 && w[3] / w[1] <= 0.99
    }, logical(1))
    model <- clm_allocation(case$inputs, family, fit_v0 = TRUE)
    units <- .default_search_units(model)
    probe <- .default_probe_range(units[[1]], .default_travel_summary(units), 0, Inf)
    expect_equal(probe$range, range(probes[ok]), info = family)
    # the steep end first: a gaussian scale ascends, a rate and an exponent descend
    expect_identical(probe$steep_first, family == "gaussian", info = family)
    f <- fit_allocation(model, case$y, loss = "poisson")
    grid <- f$default_search$grid
    steep <- kernels[[family]](d[2], grid$sigma[seq(1, 45, by = 3)]) /
      kernels[[family]](d[1], grid$sigma[seq(1, 45, by = 3)])
    expect_true(all(diff(steep) >= 0), info = family)
  }
  # given sigma bounds restrict the probe
  model <- clm_allocation(case$inputs, "gaussian", fit_v0 = TRUE)
  units <- .default_search_units(model)
  probe <- .default_probe_range(units[[1]], .default_travel_summary(units), 2, 30)
  expect_true(all(probe$range >= 2 & probe$range <= 30))
  # no qualifying value
  expect_identical(.ds_message(fit_allocation(model, case$y, lower = c(sigma = 1e5),
    loss = "poisson")), paste0("no sigma value makes the gaussian kernel decay over these ",
    "travel times; give lower and upper for sigma"))
})

test_that("a declared kernel works with default bounds", {
  case <- alloc_generated_case()
  plateau <- .ds_plateau()
  model <- clm_allocation(case$inputs, plateau, fit_v0 = TRUE)
  f <- fit_allocation(model, case$y, loss = "poisson")
  expect_identical(f$status, "selected")
  expect_false(any(f$boundary[f$best_start, ]))
  # the range is inside the declaration's bounds, found with the engine's kernel
  bounded <- decay_kernel(function(distance, sigma, plateau) {
    ifelse(distance <= plateau, 1, exp(-(distance - plateau)^2 / (2 * sigma^2)))
  }, name = "plateau2", parameter = "scale", lower = 2, upper = 20, args = list(plateau = 3))
  g <- fit_allocation(clm_allocation(case$inputs, bounded, fit_v0 = TRUE), case$y,
                      loss = "poisson")
  range <- g$default_search$sigma_range
  expect_true(range[["lower"]] >= 2 && range[["upper"]] <= 20)
  expect_true(g$bounds$lower[["sigma"]] >= 2 && g$bounds$upper[["sigma"]] <= 20)
  expect_true(coef(g)[["sigma"]] >= 2 && coef(g)[["sigma"]] <= 20)
  # a reference from starts around the optimum
  ref <- .ds_reference(model, case$y, rbind(a = c(sigma = 5, v0 = 20), b = c(sigma = 9, v0 = 35)),
                       c(sigma = 0.5, v0 = 1), c(sigma = 60, v0 = 300))
  .ds_expect_same(f, ref)
})

test_that("only the count outputs have defaults", {
  case <- alloc_generated_case()
  for (output in c("access", "allocation", "target", "outside_share", "unknown")) {
    expect_identical(.ds_message(fit_allocation(case$model, case$y, output = output,
      loss = "poisson")), paste0("give starts, lower and upper for output '", output, "'"))
  }
  # also when only the bounds are missing
  expect_identical(.ds_message(fit_allocation(case$model, case$y, case$starts, output = "access",
    loss = "poisson")), "give starts, lower and upper for output 'access'")
  # the group output of an ungrouped model is not an output of the model
  expect_identical(.ds_message(fit_allocation(case$model, case$y,
    output = "utilization_by_group", loss = "poisson")),
    "give starts, lower and upper for output 'utilization_by_group'")
})

test_that("the loss decides the gradient and is recorded", {
  case <- alloc_generated_case()
  w <- fit_allocation(case$model, case$y, loss = "weighted_sse")
  expect_true(w$best$gradient)
  expect_identical(w$default_search$loss, "weighted_sse")
  expect_identical(w$default_search$n_finite, 45L)
  expect_identical(w$best$loss_fn, .weighted_sse_loss)
  expect_equal(coef(w), coef(.ds_reference(case$model, case$y, case$starts, case$lower,
    case$upper, loss = "weighted_sse")), tolerance = 1e-4)
  sse <- function(predicted, observed) sum((predicted - observed)^2 / (observed + 1))
  custom <- fit_allocation(case$model, case$y, loss = sse)
  expect_false(custom$best$gradient)
  expect_identical(custom$default_search$loss, "custom")
  expect_false(custom$default_search$controls$gradient)
  with_gradient <- fit_allocation(case$model, case$y, loss = sse, loss_grad = .weighted_sse_gradient)
  expect_true(with_gradient$best$gradient)
  expect_equal(coef(custom), coef(with_gradient), tolerance = 1e-4)
})

test_that("a loss that fails at every grid point gives its own message", {
  case <- alloc_generated_case()
  expect_identical(.ds_message(fit_allocation(case$model, case$y,
    loss = function(predicted, observed) stop("boom"))), "boom")
  expect_identical(.ds_message(fit_allocation(case$model, case$y,
    loss = function(predicted, observed) NaN)),
    "no parameter value in the default search range gives a finite loss; give starts")
})

test_that("no positive outside weight among the grid points is an error", {
  case <- alloc_generated_case()
  target <- .bind_problem_target(case$model, case$y, "utilization")
  scanned <- list(starts = rbind(a = c(sigma = 5, v0 = 10)),
                  best = data.frame(point = 1L, sigma = 5, v0 = 0, loss = 1),
                  record = list())
  expect_identical(.ds_message(testthat::with_mocked_bindings(
    .allocation_default_search(case$model, target, "utilization", .poisson_loss,
                               list(eps = 1e-250)),
    .default_scan = function(...) scanned)),
    "no positive outside weight in the default search; give starts")
})

test_that("the default lower bound of v0 is not below the seed for an outside share of 1e-6", {
  case <- alloc_generated_case()
  # Counts that a model without an outside option produces, 1% too high, with one
  # facility unobserved (so the target is not fully observed): v0 is pushed to zero.
  huff <- clm_oracle(case$distance, case$demand, case$supply, 8, v0 = 0)
  y <- colSums(huff$flow) * 1.01
  y[1L] <- NA
  f <- fit_allocation(case$model, y, loss = "poisson")
  ds <- f$default_search
  # the floor: s / (1 - s), s = 1e-6, times the demand-weighted median opportunity at
  # the grid point with the lowest loss
  top <- ds$grid[which.min(ds$grid$loss), ]
  o <- clm_oracle(case$distance, case$demand, case$supply, top$sigma, v0 = 0)
  floor <- 1e-6 / (1 - 1e-6) * .ds_wmedian(o$denom, case$demand)
  best_v0 <- vapply(split(ds$grid, ds$grid$point), function(x) x$v0[which.min(x$loss)], numeric(1))
  expect_lt(min(best_v0) / 1000, floor)     # the old bound lay far below
  expect_equal(f$bounds$lower[["v0"]], floor, tolerance = 1e-8)
  expect_identical(ds$bounds$lower[["v0"]], f$bounds$lower[["v0"]])
  # the fit ends on that bound, and the boundary is flagged
  expect_true(f$boundary[f$best_start, "v0"])
  expect_equal(coef(f)[["v0"]], floor, tolerance = 1e-6)
  expect_true(f$table$boundary[match(f$best_start, f$table$start)])
  # a fit that does not push v0 to zero: the bound is the larger of the floor and the
  # old rule, and the estimate is interior
  g <- fit_allocation(case$model, case$y, loss = "poisson")
  gs <- g$default_search
  gtop <- gs$grid[which.min(gs$grid$loss), ]
  og <- clm_oracle(case$distance, case$demand, case$supply, gtop$sigma, v0 = 0)
  gfloor <- 1e-6 / (1 - 1e-6) * .ds_wmedian(og$denom, case$demand)
  gbest <- vapply(split(gs$grid, gs$grid$point), function(x) x$v0[which.min(x$loss)], numeric(1))
  expect_equal(g$bounds$lower[["v0"]], max(min(gbest) / 1000, gfloor), tolerance = 1e-8)
  expect_false(g$boundary[g$best_start, "v0"])
  # a bound the caller gives wins, whatever the floor
  h <- fit_allocation(case$model, y, loss = "poisson", lower = c(v0 = 1e-9))
  expect_identical(h$bounds$lower[["v0"]], 1e-9)
})

test_that("Cornwall: an exponential decay with a fitted beta on totals ends on the floor of v0", {
  skip_on_cran()
  cw <- .ds_cornwall()
  model <- clm_allocation(cw$inputs, "exponential", fit_v0 = TRUE, fit_beta = TRUE)
  f <- fit_allocation(model, cw$totals, output = "utilization", loss = "poisson")
  expect_identical(f$status, "selected")
  i <- match(f$best_start, f$table$start)
  expect_true(f$boundary[i, "v0"])
  expect_equal(coef(f)[["v0"]], f$bounds$lower[["v0"]], tolerance = 1e-3)
  # the same loss as the Huff model with exponential decay and a fitted beta (v0 = 0)
  huff <- clm_allocation(cw$inputs, "exponential", fit_v0 = FALSE, fit_beta = TRUE)
  h <- fit_allocation(huff, cw$totals, output = "utilization", loss = "poisson")
  expect_lt(abs(f$best$loss - h$best$loss), 0.5)
})

test_that("kappa and a held beta enter the seed as the engine's attractiveness", {
  case <- alloc_generated_case()
  model <- clm_allocation(case$inputs, kappa = 2, beta = 0.5, fit_v0 = TRUE)
  f <- fit_allocation(model, case$y, loss = "poisson")
  expect_identical(f$status, "selected")
  ref <- .ds_reference(model, case$y, rbind(a = c(sigma = 5, v0 = 8), b = c(sigma = 9, v0 = 15)),
                       c(sigma = 0.5, v0 = 0.1), c(sigma = 60, v0 = 100))
  .ds_expect_same(f, ref)
  seed_at <- function(sigma) {
    o <- clm_oracle(case$distance, case$demand, case$supply, sigma, v0 = 0, kappa = 2, beta = 0.5)
    s <- 1 - sum(case$y) / sum(o$flow)
    s / (1 - s) * .ds_wmedian(o$denom, case$demand)
  }
  grid <- f$default_search$grid
  for (row in c(2, 23, 44)) {
    expect_equal(grid$v0[row], seed_at(grid$sigma[row]), tolerance = 1e-8)
  }
  # a fitted beta is 1 at every grid point
  both <- clm_allocation(case$inputs, kappa = 2, fit_v0 = TRUE, fit_beta = TRUE)
  g <- fit_allocation(both, case$y, loss = "poisson")
  expect_true(all(g$default_search$grid$beta == 1))
  expect_identical(names(coef(g)), c("sigma", "v0", "beta"))
})

# (f) Demand groups --------------------------------------------------------------

.ds_group_ids <- c("0-15", "16-64", "65+")
.ds_group_truth <- c("sigma.0-15" = 5, "sigma.16-64" = 8, "sigma.65+" = 12, v0 = 25)

# The SPAX-057 case of test-06-allocation_groups.R: the generated case in three
# age bands (shares varying over the origins), group sigma 5 / 8 / 12, shared v0 25.
.ds_group_case <- function() {
  alloc_memo("default_search_group_case", {
    base <- alloc_generated_case()
    ids <- rownames(base$distance)
    t <- seq(0, 1, length.out = length(ids))
    share <- cbind(0.15 + 0.5 * t, 0.5 - 0.3 * t, 0.35 - 0.2 * t)
    share <- share / rowSums(share)
    demand <- stats::setNames(lapply(seq_along(.ds_group_ids), function(k) {
      stats::setNames(20 * base$demand * share[, k], ids)
    }), .ds_group_ids)
    inputs <- prepare_allocation(demand, base$supply, base$distance)
    model <- clm_allocation(inputs, by_group = "sigma", fit_v0 = TRUE)
    expected <- evaluate_allocation(model, .ds_group_truth,
      requested_outputs = "utilization_by_group")$outputs$utilization_by_group
    counts <- withr::with_seed(11, {
      x <- expected
      x[] <- stats::rpois(length(x), x)
      x
    })
    list(base = base, inputs = inputs, model = model, expected = expected, counts = counts)
  })
}

test_that("grouped: the SPAX-057 case recovers its values from the default search", {
  case <- .ds_group_case()
  f <- fit_allocation(case$model, case$counts, output = "utilization_by_group", loss = "poisson")
  expect_identical(f$status, "selected")
  expect_true(all(f$table$eligible))
  expect_false(any(f$boundary))
  est <- coef(f)
  expect_identical(names(est), names(.ds_group_truth))
  # the tolerance of the SPAX-057 test
  expect_true(all(abs(est / .ds_group_truth - 1) < 0.15))
  expect_true(est[["sigma.0-15"]] < est[["sigma.16-64"]] && est[["sigma.16-64"]] < est[["sigma.65+"]])
  # the grids of the group sigmas walk together
  ds <- f$default_search
  expect_identical(names(ds$sigma_range), c("sigma.0-15", "sigma.16-64", "sigma.65+"))
  expect_identical(names(ds$grid), c("point", names(.ds_group_truth), "loss"))
  expect_identical(nrow(ds$grid), 45L)
  expect_identical(ds$grid$point, rep(1:15, each = 3))
  # one v0 for all groups: three points per sigma point, not 3^G
  expect_identical(ds$n_points, 45L)
  # a fit from explicit starts gives the same estimate
  ref <- .ds_reference(case$model, case$counts,
    rbind(centre = c(rep(7, 3), 20), spread = c(4, 12, 20, 50)) |> `colnames<-`(names(.ds_group_truth)),
    stats::setNames(rep(1, 4), names(.ds_group_truth)),
    stats::setNames(c(rep(40, 3), 200), names(.ds_group_truth)), "utilization_by_group")
  .ds_expect_same(f, ref, tolerance = 1e-4)
  # the exact expectations are recovered
  e <- fit_allocation(case$model, case$expected, output = "utilization_by_group",
                      loss = "poisson")
  expect_equal(coef(e), .ds_group_truth, tolerance = 1e-3)
})

test_that("grouped: a v0 by group is walked together over three points", {
  case <- .ds_group_case()
  model <- clm_allocation(case$inputs, by_group = c("sigma", "v0"), fit_v0 = TRUE)
  f <- fit_allocation(model, case$counts, output = "utilization_by_group", loss = "poisson")
  expect_identical(f$status, "selected")
  expect_identical(names(coef(f)), c("sigma.0-15", "sigma.16-64", "sigma.65+",
                                     "v0.0-15", "v0.16-64", "v0.65+"))
  expect_identical(f$default_search$n_points, 45L)
  grid <- f$default_search$grid
  # the three multipliers move every group's v0 together
  at <- grid[grid$point == 7, ]
  v0 <- as.matrix(at[, c("v0.0-15", "v0.16-64", "v0.65+")])
  expect_equal(v0[1, ] * 10, v0[2, ], tolerance = 1e-12)
  expect_equal(v0[3, ] / 10, v0[2, ], tolerance = 1e-12)
  expect_true(all(abs(coef(f)[4:6] / 25 - 1) < 0.25))
  # the seed of a group: its own outside share and opportunity
  o <- lapply(.ds_group_ids, function(g) {
    clm_oracle(case$base$distance, case$inputs$D_groups[, g], case$base$supply,
               at$`sigma.0-15`[1], v0 = 0)
  })
  names(o) <- .ds_group_ids
  s <- 1 - sum(case$counts[, "0-15"]) / sum(colSums(o[["0-15"]]$flow))
  expect_equal(at$`v0.0-15`[2], s / (1 - s) *
                 .ds_wmedian(o[["0-15"]]$denom, case$inputs$D_groups[, "0-15"]), tolerance = 1e-8)
})

test_that("the floor of v0 is set for each group when v0 differs by group", {
  case <- .ds_group_case()
  model <- clm_allocation(case$inputs, by_group = c("sigma", "v0"), fit_v0 = TRUE)
  f <- fit_allocation(model, case$counts, output = "utilization_by_group", loss = "poisson")
  grid <- f$default_search$grid
  top <- grid[which.min(grid$loss), ]
  for (g in .ds_group_ids) {
    o <- clm_oracle(case$base$distance, case$inputs$D_groups[, g], case$base$supply,
                    top[[paste0("sigma.", g)]], v0 = 0)
    floor <- 1e-6 / (1 - 1e-6) * .ds_wmedian(o$denom, case$inputs$D_groups[, g])
    lower <- f$bounds$lower[[paste0("v0.", g)]]
    expect_gte(lower, floor * (1 - 1e-8))
    best <- vapply(split(grid, grid$point), function(x) x[[paste0("v0.", g)]][which.min(x$loss)],
                   numeric(1))
    expect_equal(lower, max(min(best) / 1000, floor), tolerance = 1e-8)
  }
})

test_that("grouped: per-group families and a fixed group sigma", {
  case <- .ds_group_case()
  fam <- clm_allocation(case$inputs, fit_v0 = TRUE, family = list(
    "0-15" = "gaussian", "16-64" = "exponential", "65+" = "gaussian"))
  truth <- c("sigma.0-15" = 5, "sigma.16-64" = 0.15, "sigma.65+" = 12, v0 = 25)
  expected <- evaluate_allocation(fam, truth,
    requested_outputs = "utilization_by_group")$outputs$utilization_by_group
  f <- fit_allocation(fam, expected, output = "utilization_by_group", loss = "poisson")
  expect_equal(coef(f), truth, tolerance = 1e-2)
  # each group's range is that of its own kernel
  ranges <- f$default_search$sigma_range
  expect_identical(ranges[["sigma.0-15"]], ranges[["sigma.65+"]])
  expect_false(identical(ranges[["sigma.16-64"]], ranges[["sigma.0-15"]]))
  # an exponential rate is bounded by the travel times: weight kept at the far
  # travel (rate <= 200 / (far - nearest)) and decay over the range (>= 0.01 / (top - nearest))
  distance <- case$base$distance
  d <- c(min(distance[distance > 0]), max(apply(distance, 1, min)), max(distance))
  rate <- ranges[["sigma.16-64"]]
  expect_true(rate[["upper"]] <= 200 / (d[2] - d[1]) && rate[["lower"]] >= 0.01 / (d[3] - d[1]))
  # a fixed group sigma keeps its value and has no grid
  fixed <- clm_allocation(case$inputs, by_group = "sigma", fit_v0 = TRUE,
                          fixed = c("sigma.0-15" = 5, "sigma.65+" = 12))
  g <- fit_allocation(fixed, case$expected, output = "utilization_by_group", loss = "poisson")
  expect_identical(names(coef(g)), c("sigma.16-64", "v0"))
  expect_equal(coef(g), c("sigma.16-64" = 8, v0 = 25), tolerance = 1e-3)
  expect_identical(names(g$default_search$grid), c("point", "sigma.16-64", "v0", "loss"))
  expect_named(g$default_search$sigma_range, c("lower", "upper"))
  # every sigma fixed: only v0 is scanned, at the three seeds
  all_fixed <- clm_allocation(case$inputs, by_group = "sigma", fit_v0 = TRUE,
    fixed = c("sigma.0-15" = 5, "sigma.16-64" = 8, "sigma.65+" = 12))
  h <- fit_allocation(all_fixed, case$expected, output = "utilization_by_group", loss = "poisson")
  expect_equal(coef(h), c(v0 = 25), tolerance = 1e-3)
  expect_identical(h$default_search$n_points, 3L)
  expect_identical(h$default_search$sigma_range, c(NA_real_, NA_real_))
})

test_that("grouped: shared sigma over groups with their own travel uses their union", {
  case <- .ds_group_case()
  # the second group travels twice as long
  travel <- list("0-15" = case$base$distance, "16-64" = 2 * case$base$distance,
                 "65+" = case$base$distance)
  demand <- lapply(.ds_group_ids, function(g) case$inputs$D_groups[, g])
  names(demand) <- .ds_group_ids
  inputs <- prepare_allocation(demand, case$base$supply, travel)
  model <- clm_allocation(inputs, fit_v0 = TRUE)
  units <- .default_search_units(model)
  union <- .default_travel_summary(units)
  single <- lapply(units, .default_unit_travel)
  expect_identical(union$d_lo, min(vapply(single, `[[`, 0, "d_lo")))
  expect_identical(union$far, max(vapply(single, `[[`, 0, "far")))
  expect_identical(union$top, max(vapply(single, `[[`, 0, "top")))
  expect_identical(union$top, 2 * max(case$base$distance))
})

test_that("no free sigma: the v0 seeds at the held sigma are the starts", {
  case <- .ds_group_case()
  model <- clm_allocation(case$inputs, fit_v0 = TRUE, fixed = c(sigma = 8))
  expect_identical(model$theta$names, "v0")
  expected <- evaluate_allocation(model, c(v0 = 25),
    requested_outputs = "utilization_by_group")$outputs$utilization_by_group
  f <- fit_allocation(model, expected, output = "utilization_by_group", loss = "poisson")
  expect_equal(coef(f), c(v0 = 25), tolerance = 1e-4)
  ds <- f$default_search
  expect_identical(ds$sigma_range, c(NA_real_, NA_real_))
  expect_identical(ds$n_points, 3L)
  expect_identical(names(ds$grid), c("point", "v0", "loss"))
  expect_identical(ds$grid$point, c(1L, 1L, 1L))
  expect_identical(rownames(f$starts), paste0("scan", 1:3))
  expect_identical(colnames(f$starts), "v0")
  # the seed: outside share on the observed counts of the model without an
  # outside option, times the demand-weighted median opportunity at sigma 8
  D <- case$inputs$D_active
  o <- clm_oracle(case$base$distance, D, case$base$supply, 8, v0 = 0)
  s <- 1 - sum(expected) / sum(o$flow)
  seed <- s / (1 - s) * .ds_wmedian(o$denom, D)
  expect_equal(unname(f$starts[, "v0"]), c(0.1, 1, 10) * seed, tolerance = 1e-8)
  expect_identical(ds$grid$v0, unname(f$starts[, "v0"]))
  # bounds as for v0 elsewhere: a thousandth to a thousand times the best seed
  expect_identical(names(f$bounds$lower), "v0")
  expect_true(f$bounds$lower[["v0"]] < min(f$starts) && f$bounds$upper[["v0"]] > max(f$starts))
  expect_true(f$best$gradient)
  expect_identical(ds$controls$control, list(maxit = 300, factr = 1e3, pgtol = 1e-2))
  # v0 by group: the three multipliers walk together, 3 points not 3^G
  by_group <- clm_allocation(case$inputs, by_group = "v0", fit_v0 = TRUE,
                             fixed = c(sigma = 8))
  expect_identical(by_group$theta$names, c("v0.0-15", "v0.16-64", "v0.65+"))
  expected_by <- evaluate_allocation(by_group, c("v0.0-15" = 20, "v0.16-64" = 25, "v0.65+" = 30),
    requested_outputs = "utilization_by_group")$outputs$utilization_by_group
  g <- fit_allocation(by_group, expected_by, output = "utilization_by_group", loss = "poisson")
  expect_equal(coef(g), c("v0.0-15" = 20, "v0.16-64" = 25, "v0.65+" = 30), tolerance = 1e-3)
  expect_identical(g$default_search$n_points, 3L)
  expect_identical(g$default_search$sigma_range, c(NA_real_, NA_real_))
  start <- g$starts
  expect_equal(start[1, ] * 10, start[2, ], tolerance = 1e-12)
  expect_equal(start[3, ] / 10, start[2, ], tolerance = 1e-12)
  # a fitted beta is 1 at every point
  with_beta <- clm_allocation(case$inputs, fit_v0 = TRUE, fit_beta = TRUE, fixed = c(sigma = 8))
  expect_identical(with_beta$theta$names, c("v0", "beta"))
  b <- fit_allocation(with_beta, expected, output = "utilization_by_group", loss = "poisson",
                      control = list(maxit = 20))
  expect_true(all(b$default_search$grid$beta == 1))
  expect_identical(b$default_search$n_points, 3L)
  # the NA mask works as elsewhere
  masked <- expected
  masked[c(1, 5), 2] <- NA
  expect_no_error(fit_allocation(model, masked, output = "utilization_by_group",
                                 loss = "poisson", control = list(maxit = 20)))
  # and so does the D7 guard
  expect_match(.ds_message(fit_allocation(model, expected * (1.01 * sum(D) / sum(expected)),
    output = "utilization_by_group", loss = "poisson")), "reach the total demand", fixed = TRUE)
})

test_that("no free sigma: with only beta free the starts are beta 0.5, 1 and 2", {
  case <- .ds_group_case()
  model <- clm_allocation(case$inputs, fit_beta = TRUE, fixed = c(sigma = 8))
  expect_identical(model$theta$names, "beta")
  expected <- evaluate_allocation(model, c(beta = 0.7),
    requested_outputs = "utilization_by_group")$outputs$utilization_by_group
  f <- fit_allocation(model, expected, output = "utilization_by_group", loss = "poisson")
  expect_equal(coef(f), c(beta = 0.7), tolerance = 1e-3)
  ds <- f$default_search
  expect_identical(ds$sigma_range, c(NA_real_, NA_real_))
  expect_identical(ds$n_points, 3L)
  expect_identical(rownames(f$starts), paste0("scan", 1:3))
  expect_identical(unname(f$starts[, "beta"]), c(0.5, 1, 2))
  expect_identical(names(ds$grid), c("point", "beta", "loss"))
  expect_identical(ds$grid$beta, c(0.5, 1, 2))
  expect_true(all(is.finite(ds$grid$loss)))
  expect_identical(f$bounds, list(lower = c(beta = 0.05), upper = c(beta = 5)))
  expect_true(f$best$gradient)
  # a given bound clamps the starts, and duplicates after clamping are dropped
  g <- fit_allocation(model, expected, lower = c(beta = 0.8), upper = c(beta = 0.9),
                      output = "utilization_by_group", loss = "poisson",
                      control = list(maxit = 20))
  expect_identical(unname(g$starts[, "beta"]), c(0.8, 0.9))
  expect_identical(rownames(g$starts), c("scan1", "scan2"))
  expect_identical(g$default_search$n_points, 3L)
  # crossing bounds are still an error
  expect_identical(.ds_message(fit_allocation(model, expected, lower = c(beta = 6),
    output = "utilization_by_group", loss = "poisson")),
    "lower must be less than upper for beta: 6 (given) and 5 (default)")
})

# (g) The record and its print line -----------------------------------------------

test_that("the default_search field and its print line", {
  case <- alloc_generated_case()
  f <- fit_allocation(case$model, case$y, loss = "poisson")
  out <- capture.output(print(f))
  line <- "starts: default search, 45 of 45 grid points, loss poisson"
  expect_identical(sum(out == line), 1L)
  expect_identical(out[1:3], c("<ae_multistart_fit>", "eligible starts: 3 / 3",
                               paste0("selected start: ", f$best_start)))
  expect_identical(out[4], line)
  # the record
  ds <- f$default_search
  expect_named(ds, c("rule", "sigma_range", "n_points", "n_finite", "grid", "starts",
                     "bounds", "controls", "loss"))
  expect_s3_class(ds$grid, "data.frame")
  expect_true(all(is.finite(ds$grid$loss)))
  expect_identical(ds$starts, f$starts)
  # finite points are counted
  failing <- fit_allocation(case$model, case$y,
    loss = function(predicted, observed) if (any(predicted > 150)) NaN else sum(predicted - observed))
  expect_lt(failing$default_search$n_finite, failing$default_search$n_points)
  expect_match(capture.output(print(failing))[4], "^starts: default search, \\d+ of 45 grid points, loss custom$")
  # fits without the field print as before
  old <- f
  old$default_search <- NULL
  old_out <- capture.output(print(old))
  expect_identical(old_out, out[out != line])
  expect_false(any(startsWith(old_out, "starts:")))
  small <- capture.output(print(alloc_small_fit()))
  expect_false(any(startsWith(small, "starts:")))
  expect_identical(small[1:2], c("<ae_multistart_fit>", "eligible starts: 2 / 2"))
  expect_match(small[3], "^selected start: ")
  expect_match(small[4], "^loss disagreement: ")
  # the summary and the methods do not need the field
  expect_equal(coef(old), coef(f))
  expect_s3_class(summary(f), "summary.ae_multistart_fit")
})

test_that("the default search leaves the other helpers and exports untouched", {
  # a fit with a default search is a fit like any other
  case <- alloc_generated_case()
  f <- fit_allocation(case$model, case$y, loss = "poisson")
  expect_equal(coef(f), alloc_generated_fit()$best$theta, tolerance = .ds_tol)
  e <- evaluate_allocation(case$model, coef(f))
  expect_equal(e$outputs$utilization, as.numeric(fitted(f)), tolerance = 1e-6,
               ignore_attr = TRUE)
  expect_identical(.default_bounds_cover(c(a = 1, b = 2), c("a", "b")), TRUE)
  expect_identical(.default_bounds_cover(c(a = 1), c("a", "b")), FALSE)
  expect_identical(.default_bounds_cover(c(1, 2), c("a", "b")), TRUE)
  expect_identical(.default_bounds_cover(NULL, c("a", "b")), FALSE)
  expect_equal(.default_weighted_median(c(3, 1, 2), c(1, 1, 5)), 2)
})
