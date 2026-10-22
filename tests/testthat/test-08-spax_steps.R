# The step functions accept the result of spax() (SPAX-058, T6). Each step, given a
# result, must do what it does with the same objects given one by one: predict reads
# x$fit (fit mode) or x$evaluation (evaluate mode), check and bootstrap read x$model and
# x$fit, and compare reads each result's fit and model. The result is unbundled before
# the step's class and group checks. The cases are small and synthetic; the bootstrap
# runs the real engine with mocked facility-case refits (399 real refits take about half
# a minute).

# Fixtures -------------------------------------------------------------------------

.step_cache <- new.env(parent = emptyenv())
.step_memo <- function(key, code) {
  if (is.null(.step_cache[[key]])) assign(key, code, envir = .step_cache)
  .step_cache[[key]]
}

.step_error <- function(expr) {
  tryCatch({ expr; NULL }, error = function(e) conditionMessage(e))
}

# The generated case of helper-allocation.R: 18 origins, 24 facilities, truth sigma 8
# and v0 25.
.step_case <- function() {
  .step_memo("case", {
    case <- alloc_generated_case()
    case$data <- prep_data(case$demand, case$supply, case$distance)
    case$totals <- obs_totals(case$y)
    case
  })
}

# Results of spax(): Gaussian and exponential conditional logit and a Huff model fitted to
# the facility totals, a conditional logit fitted to the flows, and the evaluation of a
# Gaussian conditional logit.
.step_result <- function(kind) {
  .step_memo(kind, {
    case <- .step_case()
    switch(kind,
      gauss = spax(case$data, spec_clm(), observed = case$totals, starts = case$starts,
                   lower = case$lower, upper = case$upper),
      expo = spax(case$data, spec_clm(decay_exponential()), observed = case$totals),
      huff = spax(case$data, spec_huff(), observed = case$totals),
      flow = spax(case$data, spec_clm(), observed = obs_flows(case$flow),
                  starts = case$starts, lower = case$lower, upper = case$upper),
      evaluated = spax(case$data, spec_clm(decay_gaussian(8), outside = 25)),
      # every start stops at one iteration, so none converges
      failed = suppressWarnings(spax(case$data, spec_clm(), observed = case$totals,
                                     starts = case$starts, lower = case$lower,
                                     upper = case$upper, control = list(maxit = 1))))
  })
}

# The grouped case of SPAX-057 (test-08-spax_run.R): the generated case split into three
# age bands, with Poisson counts of the group totals. A fit by group sigma from given
# starts, and an evaluation.
.step_group_ids <- c("0-15", "16-64", "65+")
.step_grouped <- function() {
  .step_memo("grouped", {
    base <- alloc_generated_case()
    ids <- rownames(base$distance)
    t <- seq(0, 1, length.out = length(ids))
    share <- cbind(0.15 + 0.5 * t, 0.5 - 0.3 * t, 0.35 - 0.2 * t)
    share <- share / rowSums(share)
    demand <- stats::setNames(lapply(seq_along(.step_group_ids), function(k) {
      stats::setNames(20 * base$demand * share[, k], ids)
    }), .step_group_ids)
    data <- prep_data(demand, base$supply, base$distance)
    truth <- c("sigma.0-15" = 5, "sigma.16-64" = 8, "sigma.65+" = 12, v0 = 25)
    expected <- evaluate_allocation(clm_allocation(data, by_group = "sigma", fit_v0 = TRUE),
      truth, requested_outputs = "utilization_by_group")$outputs$utilization_by_group
    counts <- withr::with_seed(11, {
      expected[] <- stats::rpois(length(expected), expected)
      expected
    })
    starts <- rbind(centre = c(rep(7, 3), 20), spread = c(4, 12, 20, 50))
    colnames(starts) <- names(truth)
    list(data = data, distance = base$distance,
         fit = spax(data, spec_clm(by_group = "sigma"), observed = obs_totals(counts),
                    starts = starts, lower = stats::setNames(rep(1, 4), names(truth)),
                    upper = stats::setNames(c(rep(40, 3), 200), names(truth))),
         evaluated = spax(data, spec_clm(decay_gaussian(5), outside = 0.1)))
  })
}

.step_refit <- function() {
  alloc_mock_case_fit(.step_case())
}

# predict_allocation() ------------------------------------------------------------------

test_that("predict_allocation() on a result equals predicting from its fit or evaluation", {
  case <- .step_case()
  request <- case$distance[1:3, ]
  demand <- c(10, 20, 30)
  # fit mode: the fit, with the default outputs and with the others
  x <- .step_result("gauss")
  expect_identical(predict_allocation(x, request), predict_allocation(x$fit, request))
  expect_identical(predict_allocation(source = x, distance = request, demand = demand,
                                      outputs = c("allocation", "flow", "outside_share")),
                   predict_allocation(x$fit, request, demand = demand,
                                      outputs = c("allocation", "flow", "outside_share")))
  expect_s3_class(predict_allocation(x, request), "ae_clm_prediction")
  # the Huff model and a fit to flows
  for (kind in c("huff", "flow")) {
    r <- .step_result(kind)
    expect_identical(predict_allocation(r, request), predict_allocation(r$fit, request),
                     label = kind)
  }
  # evaluate mode: the evaluation
  e <- .step_result("evaluated")
  expect_identical(predict_allocation(e, request), predict_allocation(e$evaluation, request))
  expect_identical(predict_allocation(source = e, distance = request, demand = demand,
                                      outputs = c("allocation", "outside_share")),
                   predict_allocation(e$evaluation, request, demand = demand,
                                      outputs = c("allocation", "outside_share")))
  # the method of the result and the function agree
  # (predict() defaults to the outputs "allocation" and "outside_share")
  outputs <- c("allocation", "outside_share")
  expect_identical(predict(x, request), predict_allocation(x, request, outputs = outputs))
  expect_identical(predict(e, request), predict_allocation(e, request, outputs = outputs))
})

test_that("predict_allocation() unbundles a result before any other check", {
  # a result with no selected fit: the message of the result, before the other arguments
  failed <- .step_result("failed")
  request <- .step_case()$distance[1:2, ]
  msg <- "no fit was selected; see summary(x)"
  expect_identical(.step_error(predict_allocation(failed, request)), msg)
  expect_identical(.step_error(predict_allocation(failed, request, type = "nonsense")), msg)
  expect_identical(.step_error(predict_allocation(failed, request, outputs = "nope",
                                                  type = "raster")), msg)
  # a grouped result gives the grouped refusal, not a class error
  grouped <- .step_grouped()
  refusal <- "predict_allocation() is not available for grouped models yet"
  expect_identical(.step_error(predict_allocation(grouped$fit, grouped$distance[1:2, ])),
                   refusal)
  expect_identical(.step_error(predict_allocation(grouped$evaluated, grouped$distance[1:2, ])),
                   refusal)
  # with a plain source nothing changes
  expect_identical(.step_error(predict_allocation(grouped$fit$fit, grouped$distance[1:2, ])),
                   refusal)
})

# check_allocation() --------------------------------------------------------------------

test_that("check_allocation() on a result equals checking its model and fit", {
  x <- .step_result("gauss")
  expect_identical(check_allocation(x), check_allocation(x$model, x$fit))
  expect_identical(check_allocation(model = x), check_allocation(x$model, x$fit))
  # the other arguments pass through
  expect_identical(check_allocation(x, trip_breaks = c(10, 20)),
                   check_allocation(x$model, x$fit, trip_breaks = c(10, 20)))
  expect_s3_class(check_allocation(x), "ae_allocation_check")
  # the Huff model, and a fit to flows
  for (kind in c("huff", "flow")) {
    r <- .step_result(kind)
    expect_identical(check_allocation(r), check_allocation(r$model, r$fit), label = kind)
  }
})

test_that("check_allocation() refuses what a result cannot supply, before the class checks", {
  x <- .step_result("gauss")
  e <- .step_result("evaluated")
  # an evaluation has no fit
  expect_identical(.step_error(check_allocation(e)), "check_allocation() needs a fitted model")
  # a result comes alone: the spec does not say what a given fit would mean
  alone <- "give a result from spax() alone, without fit"
  expect_identical(.step_error(check_allocation(x, x$fit)), alone)
  expect_identical(.step_error(check_allocation(x, fit = x$fit)), alone)
  expect_identical(.step_error(check_allocation(e, x$fit)), alone)
  expect_identical(.step_error(check_allocation(x, NULL)), alone)
  # a result with no selected fit: the refusal that check_allocation() gives the fit
  failed <- .step_result("failed")
  expect_identical(.step_error(check_allocation(failed)),
                   .step_error(check_allocation(failed$model, failed$fit)))
  expect_match(.step_error(check_allocation(failed)), "no eligible allocation fit")
  # a grouped result gives the grouped refusal, not a class error
  grouped <- .step_grouped()
  expect_identical(.step_error(check_allocation(grouped$fit)),
                   "check_allocation() is not available for grouped models yet")
  expect_identical(.step_error(check_allocation(grouped$evaluated)),
                   "check_allocation() needs a fitted model")
  # plain objects keep their own class errors
  expect_match(.step_error(check_allocation(x$fit, x$fit)), "model")
  expect_match(.step_error(check_allocation(x$model, x$model)), "fit")
})

# compare_allocations() -----------------------------------------------------------------

test_that("compare_allocations() on results equals comparing their fits and models", {
  a <- .step_result("gauss")
  b <- .step_result("expo")
  h <- .step_result("huff")
  plain <- compare_allocations(list(a = a$fit, b = b$fit),
                               models = list(a = a$model, b = b$model))
  expect_identical(compare_allocations(list(a = a, b = b)), plain)
  expect_true(all(as.data.frame(plain)$comparable))
  # the other arguments pass through
  expect_identical(compare_allocations(list(a = a, b = b), reference = "b"),
                   compare_allocations(list(a = a$fit, b = b$fit), reference = "b",
                                       models = list(a = a$model, b = b$model)))
  expect_identical(compare_allocations(list(a = a, b = b), reference = 2L,
                                       coverage_args = list(norm = 1)),
                   compare_allocations(list(a = a$fit, b = b$fit), reference = 2L,
                                       coverage_args = list(norm = 1),
                                       models = list(a = a$model, b = b$model)))
  zones <- data.frame(origin_id = rownames(.step_case()$distance),
                      zone = rep(c("north", "south"), each = 9))
  expect_identical(compare_allocations(list(a = a, b = b), zones = zones),
                   compare_allocations(list(a = a$fit, b = b$fit), zones = zones,
                                       models = list(a = a$model, b = b$model)))
  # three results, one of them a Huff model
  expect_identical(compare_allocations(list(a = a, b = b, h = h)),
                   compare_allocations(list(a = a$fit, b = b$fit, h = h$fit),
                                       models = list(a = a$model, b = b$model,
                                                     h = h$model)))
  # results with plain fits: models for the plain ones only
  expect_identical(compare_allocations(list(a = a, b = b$fit), models = list(b = b$model)),
                   plain)
  expect_identical(compare_allocations(list(a = a$fit, b = b), models = list(a = a$model)),
                   plain)
  mixed <- list(a = a, b = b$fit, h = h)
  expect_identical(compare_allocations(mixed, models = list(b = b$model)),
                   compare_allocations(list(a = a$fit, b = b$fit, h = h$fit),
                                       models = list(a = a$model, b = b$model,
                                                     h = h$model)))
  # the order of the user's models does not matter
  expect_identical(
    compare_allocations(list(a = a, b = b$fit, c = h$fit),
                        models = list(c = h$model, b = b$model)),
    compare_allocations(list(a = a$fit, b = b$fit, c = h$fit),
                        models = list(a = a$model, b = b$model, c = h$model)))
})

test_that("compare_allocations() asks for the models that results do not bring", {
  a <- .step_result("gauss")
  b <- .step_result("expo")
  h <- .step_result("huff")
  # a plain fit without a model next to a result
  expect_identical(.step_error(compare_allocations(list(a = a, b = b$fit))),
                   "give models for b")
  # the names come in the order of fits, comma separated
  expect_identical(.step_error(compare_allocations(list(a = a, b = b$fit, c = h$fit))),
                   "give models for b, c")
  expect_identical(.step_error(compare_allocations(list(c = h$fit, a = a, b = b$fit))),
                   "give models for c, b")
  # only some of the plain fits have one
  expect_identical(.step_error(compare_allocations(list(a = a, b = b$fit, c = h$fit),
                                                   models = list(b = b$model))),
                   "give models for c")
  expect_identical(.step_error(compare_allocations(list(a = a, b = b$fit, c = h$fit),
                                                   models = list(c = h$model))),
                   "give models for b")
  # models that match no fit are left to the checks of compare_allocations()
  expect_identical(.step_error(compare_allocations(list(a = a, b = b),
                                                   models = list(zzz = a$model))),
                   "models must be a list with the same names as fits")
  expect_identical(.step_error(compare_allocations(list(a = a, b = b), models = list(a$model))),
                   "models must be a list with the same names as fits")
  # without results nothing changes: no models is allowed, and so is a partial set
  # of models in the old message
  expect_s3_class(compare_allocations(list(a = a$fit, b = b$fit)), "ae_allocation_comparison")
  expect_identical(.step_error(compare_allocations(list(a = a$fit, b = b$fit),
                                                   models = list(a = a$model))),
                   "models must be a list with the same names as fits")
})

test_that("a result in compare_allocations() carries its own model", {
  a <- .step_result("gauss")
  b <- .step_result("expo")
  both <- list(a = a$model, b = b$model)
  expect_identical(.step_error(compare_allocations(list(a = a, b = b), models = both)),
                   "a is a result and carries its own model")
  expect_identical(.step_error(compare_allocations(list(a = a, b = b), models = both["b"])),
                   "b is a result and carries its own model")
  expect_identical(.step_error(compare_allocations(list(a = a, b = b$fit), models = both)),
                   "a is a result and carries its own model")
  # the first result named in fits order
  expect_identical(.step_error(compare_allocations(list(b = b, a = a), models = both)),
                   "b is a result and carries its own model")
  # the plain element may have its model
  expect_s3_class(compare_allocations(list(a = a, b = b$fit), models = both["b"]),
                  "ae_allocation_comparison")
})

test_that("compare_allocations() refuses a result that was evaluated, not fitted", {
  a <- .step_result("gauss")
  e <- .step_result("evaluated")
  expect_identical(.step_error(compare_allocations(list(a = a, e = e))),
                   "e was evaluated, not fitted; compare_allocations() needs fits")
  expect_identical(.step_error(compare_allocations(list(e = e, a = a))),
                   "e was evaluated, not fitted; compare_allocations() needs fits")
  # before the models are looked at
  expect_identical(.step_error(compare_allocations(list(a = a, e = e),
                                                   models = list(e = e$model))),
                   "e was evaluated, not fitted; compare_allocations() needs fits")
  expect_identical(.step_error(compare_allocations(list(a = a, e = e, f = .step_result("expo")$fit))),
                   "e was evaluated, not fitted; compare_allocations() needs fits")
})

test_that("compare_allocations() keeps its own checks on the list of fits", {
  a <- .step_result("gauss")
  b <- .step_result("expo")
  fits_message <- "fits must be a named list of at least two allocation fits with unique names"
  # a single result, or a list that is not named, is refused as before
  expect_identical(.step_error(compare_allocations(list(a = a))), fits_message)
  expect_identical(.step_error(compare_allocations(list(a, b))), fits_message)
  expect_identical(.step_error(compare_allocations(list(a = a, a = b))), fits_message)
  expect_identical(.step_error(compare_allocations(a)), fits_message)
  # two results given as fits and models, by position
  expect_identical(.step_error(compare_allocations(a, b)), fits_message)
  # a result with no selected fit is refused as its fit would be
  failed <- .step_result("failed")
  expect_identical(.step_error(compare_allocations(list(a = a, f = failed))),
                   .step_error(compare_allocations(list(a = a$fit, f = failed$fit),
                                                   models = list(a = a$model,
                                                                 f = failed$model))))
  expect_match(.step_error(compare_allocations(list(a = a, f = failed))),
               "no eligible allocation fit")
  # an unknown reference is refused as before
  expect_identical(.step_error(compare_allocations(list(a = a, b = b), reference = "z")),
                   "reference must name a fit")
})

# bootstrap_allocation() ----------------------------------------------------------------

test_that("bootstrap_allocation() on a result equals bootstrapping its model and fit", {
  x <- .step_result("gauss")
  observation <- alloc_bootstrap_observation()
  # the real engine, with mocked facility-case refits
  testthat::local_mocked_bindings(.allocation_case_fit = function(...) .step_refit())
  via_result <- bootstrap_allocation(x, observation = observation, seed = 31L)
  via_parts <- bootstrap_allocation(x$model, x$fit, observation, seed = 31L)
  expect_s3_class(via_result, "ae_allocation_bootstrap")
  expect_identical(via_result$status, "complete")
  expect_identical(via_result$attempted, 399L)
  expect_identical(via_result, via_parts)
  expect_identical(bootstrap_allocation(model = x, observation = observation, seed = 31L),
                   via_parts)
  # a paired scenario passes through
  case <- .step_case()
  scenario <- clm_allocation(
    prep_data(case$demand, case$supply * rep(c(2, 1, 1), length.out = length(case$supply)),
              case$distance), fit_v0 = TRUE)
  expect_identical(bootstrap_allocation(x, observation = observation, scenario = scenario,
                                        seed = 31L),
                   bootstrap_allocation(x$model, x$fit, observation, scenario = scenario,
                                        seed = 31L))
})

test_that("bootstrap_allocation() keeps its refusals for a result", {
  observation <- alloc_bootstrap_observation()
  huff <- .step_result("huff")
  flow <- .step_result("flow")
  failed <- .step_result("failed")
  testthat::local_mocked_bindings(.allocation_case_fit = function(...) {
    stop("unexpected bootstrap refit")
  })
  # a Huff model has no fitted outside weight
  from_result <- .step_error(bootstrap_allocation(huff, observation = observation, seed = 1L))
  expect_identical(from_result, paste0("bootstrap requires a fitted outside weight ",
                                       "(outside = \"fit\"; fit_v0 = TRUE in clm_allocation())"))
  expect_identical(from_result, .step_error(bootstrap_allocation(huff$model, huff$fit,
                                                                 observation, seed = 1L)))
  # a fit to flows is outside the method
  from_flow <- .step_error(bootstrap_allocation(flow, observation = observation, seed = 1L))
  expect_identical(from_flow, "bootstrap requires an unweighted Poisson facility-utilization fit")
  expect_identical(from_flow, .step_error(bootstrap_allocation(flow$model, flow$fit,
                                                               observation, seed = 1L)))
  # no selected fit: the refused result of the fit given separately
  refused <- bootstrap_allocation(failed, observation = observation, seed = 1L)
  expect_identical(refused$status, "refused")
  expect_identical(refused$original$status, "original_fit_failed")
  expect_identical(refused, bootstrap_allocation(failed$model, failed$fit, observation,
                                                 seed = 1L))
  # an evaluation has no fit; a given fit is refused; the observation is checked as before
  e <- .step_result("evaluated")
  expect_identical(.step_error(bootstrap_allocation(e, observation = observation, seed = 1L)),
                   "bootstrap_allocation() needs a fitted model")
  x <- .step_result("gauss")
  alone <- "give a result from spax() alone, without fit"
  expect_identical(.step_error(bootstrap_allocation(x, x$fit, observation, seed = 1L)), alone)
  expect_identical(.step_error(bootstrap_allocation(x, fit = x$fit, observation = observation,
                                                    seed = 1L)), alone)
  # a result and the observation given by position: the observation takes the place of fit
  expect_identical(.step_error(bootstrap_allocation(x, observation, seed = 1L)), alone)
  expect_match(.step_error(bootstrap_allocation(x, observation = list(), seed = 1L)),
               "observation must declare")
  expect_match(.step_error(bootstrap_allocation(x, observation = observation)), "seed")
  # a grouped result gives the grouped refusal, not a class error
  grouped <- .step_grouped()
  expect_identical(.step_error(bootstrap_allocation(grouped$fit, observation = observation,
                                                    seed = 1L)),
                   "bootstrap_allocation() is not available for grouped models yet")
  expect_identical(.step_error(bootstrap_allocation(grouped$evaluated, observation = observation,
                                                    seed = 1L)),
                   "bootstrap_allocation() needs a fitted model")
})

# check_fit(), compare_fits() and bootstrap_fit() (SPAX-060, V4) -------------------------
# The general names of the three steps are the same functions as the allocation names; they
# differ only in the name that a message gives for the step. FCA indices are computed, not
# fitted, so every one of the six names refuses an FCA result.

# FCA results of the three kinds: the raster route of spax(), the one-shot, numeric data.
.step_fca <- function(kind) {
  .step_memo(paste0("fca_", kind), {
    td <- fca_toy()
    switch(kind,
      raster = spax(prep_data(td$demand, td$supply_df, td$distance, id_col = "id",
                              supply_cols = "doctors"),
                    spec_e2sfca(decay_gaussian(2))),
      one_shot = spax_e2sfca(td$demand, td$supply_df, td$distance, id_col = "id",
                             supply_cols = "doctors",
                             decay_params = list(method = "gaussian", sigma = 2)),
      numeric = spax(.step_case()$data, spec_e2sfca(decay_gaussian(8))))
  })
}

test_that("the general step names have the formals of the allocation names", {
  expect_identical(formals(check_fit), formals(check_allocation))
  expect_identical(formals(compare_fits), formals(compare_allocations))
  expect_identical(formals(bootstrap_fit), formals(bootstrap_allocation))
  expect_identical(names(formals(check_fit)), c("model", "fit", "trip_breaks"))
  expect_identical(names(formals(compare_fits)),
                   c("fits", "models", "reference", "zones", "coverage_args"))
  expect_identical(names(formals(bootstrap_fit)),
                   c("model", "fit", "observation", "scenario", "seed", "checkpoint",
                     "attempts"))
})

test_that("check_fit() gives the result of check_allocation()", {
  x <- .step_result("gauss")
  expect_identical(check_fit(x), check_allocation(x))
  expect_identical(check_fit(x$model, x$fit), check_allocation(x$model, x$fit))
  expect_identical(check_fit(model = x, trip_breaks = c(10, 20)),
                   check_allocation(x$model, x$fit, trip_breaks = c(10, 20)))
  expect_s3_class(check_fit(x), "ae_allocation_check")
  # a Huff model and a fit to flows, as for the allocation name
  for (kind in c("huff", "flow")) {
    r <- .step_result(kind)
    expect_identical(check_fit(r), check_allocation(r), label = kind)
  }
})

test_that("compare_fits() gives the result of compare_allocations()", {
  a <- .step_result("gauss")
  b <- .step_result("expo")
  h <- .step_result("huff")
  expect_identical(compare_fits(list(a = a, b = b)), compare_allocations(list(a = a, b = b)))
  expect_identical(compare_fits(list(a = a, b = b), reference = "b"),
                   compare_allocations(list(a = a, b = b), reference = "b"))
  zones <- data.frame(origin_id = rownames(.step_case()$distance),
                      zone = rep(c("x", "y"), length.out = nrow(.step_case()$distance)))
  expect_identical(compare_fits(list(a = a, b = b), zones = zones),
                   compare_allocations(list(a = a, b = b), zones = zones))
  expect_identical(compare_fits(list(a = a$fit, b = b$fit, h = h$fit),
                                models = list(a = a$model, b = b$model, h = h$model)),
                   compare_allocations(list(a = a$fit, b = b$fit, h = h$fit),
                                       models = list(a = a$model, b = b$model, h = h$model)))
  expect_s3_class(compare_fits(list(a = a, b = b)), "ae_allocation_comparison")
})

test_that("bootstrap_fit() gives the result of bootstrap_allocation()", {
  x <- .step_result("gauss")
  observation <- alloc_bootstrap_observation()
  testthat::local_mocked_bindings(.allocation_case_fit = function(...) .step_refit())
  via_new <- bootstrap_fit(x, observation = observation, seed = 31L)
  expect_s3_class(via_new, "ae_allocation_bootstrap")
  expect_identical(via_new, bootstrap_allocation(x, observation = observation, seed = 31L))
  expect_identical(bootstrap_fit(x$model, x$fit, observation, seed = 31L), via_new)
})

test_that("the messages name the step the user called", {
  e <- .step_result("evaluated")
  x <- .step_result("gauss")
  observation <- alloc_bootstrap_observation()
  expect_identical(.step_error(check_fit(e)), "check_fit() needs a fitted model")
  expect_identical(.step_error(check_allocation(e)), "check_allocation() needs a fitted model")
  expect_identical(.step_error(bootstrap_fit(e, observation = observation, seed = 1L)),
                   "bootstrap_fit() needs a fitted model")
  expect_identical(.step_error(bootstrap_allocation(e, observation = observation, seed = 1L)),
                   "bootstrap_allocation() needs a fitted model")
  expect_identical(.step_error(compare_fits(list(x = x, e = e))),
                   "e was evaluated, not fitted; compare_fits() needs fits")
  expect_identical(.step_error(compare_allocations(list(x = x, e = e))),
                   "e was evaluated, not fitted; compare_allocations() needs fits")
  # the grouped refusal
  grouped <- .step_grouped()
  expect_identical(.step_error(check_fit(grouped$fit)),
                   "check_fit() is not available for grouped models yet")
  expect_identical(.step_error(check_allocation(grouped$fit)),
                   "check_allocation() is not available for grouped models yet")
  expect_identical(.step_error(bootstrap_fit(grouped$fit, observation = observation, seed = 1L)),
                   "bootstrap_fit() is not available for grouped models yet")
  expect_identical(.step_error(bootstrap_allocation(grouped$fit, observation = observation,
                                                    seed = 1L)),
                   "bootstrap_allocation() is not available for grouped models yet")
  # what the steps share is unchanged
  alone <- "give a result from spax() alone, without fit"
  expect_identical(.step_error(check_fit(x, x$fit)), alone)
  expect_identical(.step_error(bootstrap_fit(x, observation, seed = 1L)), alone)
  fits_message <- "fits must be a named list of at least two allocation fits with unique names"
  expect_identical(.step_error(compare_fits(list(x = x))), fits_message)
  expect_identical(.step_error(compare_fits(list(a = x, b = .step_result("expo")),
                                            reference = "z")), "reference must name a fit")
})

test_that("an FCA index has no fit to check, compare or bootstrap, under either name", {
  observation <- alloc_bootstrap_observation()
  for (kind in c("raster", "one_shot", "numeric")) {
    f <- .step_fca(kind)
    expect_true(inherits(f, c("spax", "spax_access_values")), label = kind)
    expect_identical(.step_error(check_fit(f)), "FCA indices have no fit to check", label = kind)
    expect_identical(.step_error(check_allocation(f)), "FCA indices have no fit to check",
                     label = kind)
    expect_identical(.step_error(bootstrap_fit(f, observation = observation, seed = 1L)),
                     "FCA indices have no fit to bootstrap", label = kind)
    expect_identical(.step_error(bootstrap_allocation(f, observation = observation, seed = 1L)),
                     "FCA indices have no fit to bootstrap", label = kind)
    # a fit given as well does not change the refusal
    x <- .step_result("gauss")
    expect_identical(.step_error(check_fit(f, x$fit)), "FCA indices have no fit to check",
                     label = kind)
    # given as fits, or as an element of fits
    expect_identical(.step_error(compare_fits(f)), "FCA indices have no fit to compare",
                     label = kind)
    expect_identical(.step_error(compare_allocations(f)), "FCA indices have no fit to compare",
                     label = kind)
    expected <- "idx is an FCA index: FCA indices have no fit to compare"
    expect_identical(.step_error(compare_fits(list(a = x, idx = f))), expected, label = kind)
    expect_identical(.step_error(compare_allocations(list(a = x, idx = f))), expected,
                     label = kind)
  }
})

test_that("compare_fits() refuses an FCA index element by element, before its other checks", {
  x <- .step_result("gauss")
  f <- .step_fca("raster")
  g <- .step_fca("numeric")
  # the first FCA element is named, in any position
  expect_identical(.step_error(compare_fits(list(f = f, a = x))),
                   "f is an FCA index: FCA indices have no fit to compare")
  expect_identical(.step_error(compare_fits(list(a = x, f = f, g = g))),
                   "f is an FCA index: FCA indices have no fit to compare")
  expect_identical(.step_error(compare_fits(list(a = x, g = g, f = f))),
                   "g is an FCA index: FCA indices have no fit to compare")
  # an element without a name is named by its position
  expect_identical(.step_error(compare_fits(list(x, f))),
                   "fits[[2]] is an FCA index: FCA indices have no fit to compare")
  # before the checks of the list: one element, duplicated names, models, a reference
  expect_identical(.step_error(compare_fits(list(f = f))),
                   "f is an FCA index: FCA indices have no fit to compare")
  expect_identical(.step_error(compare_fits(list(a = f, a = x))),
                   "a is an FCA index: FCA indices have no fit to compare")
  expect_identical(.step_error(compare_fits(list(a = x, f = f), models = list(nope = 1),
                                            reference = "z")),
                   "f is an FCA index: FCA indices have no fit to compare")
  # an FCA index among results that were evaluated: the index is named first
  e <- .step_result("evaluated")
  expect_identical(.step_error(compare_fits(list(e = e, f = f))),
                   "f is an FCA index: FCA indices have no fit to compare")
})

test_that("check_fit() and bootstrap_fit() refuse an FCA index before any other check", {
  f <- .step_fca("raster")
  observation <- alloc_bootstrap_observation()
  expect_identical(.step_error(check_fit(f, trip_breaks = -1)),
                   "FCA indices have no fit to check")
  expect_identical(.step_error(bootstrap_fit(f)), "FCA indices have no fit to bootstrap")
  expect_identical(.step_error(bootstrap_fit(f, observation = list(), seed = "a")),
                   "FCA indices have no fit to bootstrap")
})
