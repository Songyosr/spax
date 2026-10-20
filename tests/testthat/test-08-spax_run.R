# spax(), the one-shots and the spax_allocation result (SPAX-058, T5). A fit through
# spax() must be the fit the step workflow gives for the same arguments (bind,
# align the observations, fit_allocation() under the strict controls); evaluate mode
# must be evaluate_allocation(). Printed output is compared as exact lines, not with
# snapshots. The synthetic cases always run; the Cornwall cases are skipped on CRAN.

# Fixtures -------------------------------------------------------------------------

.run_strict <- list(maxit = 300, factr = 1e3, pgtol = 1e-8)
.run_no_fit <- "no fit was selected; see summary(x)"

# A small memo for fits: each is computed once per run of this file.
.run_cache <- new.env(parent = emptyenv())
.run_memo <- function(key, code) {
  if (is.null(.run_cache[[key]])) assign(key, code, envir = .run_cache)
  .run_cache[[key]]
}

# A fit result without its timing fields.
.run_strip <- function(x) {
  x$seconds <- NULL
  x$table$seconds <- NULL
  x$best$seconds <- NULL
  x
}

.run_error <- function(expr) {
  tryCatch({ expr; NULL }, error = function(e) conditionMessage(e))
}

# The bound model equals another field by field (the bind closures differ by their
# environments).
.run_expect_same_model <- function(a, b) {
  for (field in c("model", "substrate", "state", "theta")) {
    expect_identical(a[[field]], b[[field]], label = field)
  }
  expect_identical(a$metadata$spec, b$metadata$spec)
}

# The generated case: 18 origins, 24 facilities, truth sigma 8 and v0 25.
.run_case <- function() {
  .run_memo("case", {
    case <- alloc_generated_case()
    case$data <- prep_data(case$demand, case$supply, case$distance)
    case
  })
}

# Sixteen origins and three facilities with the demand split into three groups.
.run_groups <- c("young", "mid", "old")
.run_grouped <- function(per_group_travel = FALSE) {
  f <- alloc_huff_case()
  demand <- list(young = f$demand * 0.2, mid = f$demand * 0.5, old = f$demand * 0.3)
  travel <- if (per_group_travel) {
    list(young = f$distance, mid = f$distance * 1.1, old = f$distance * 1.3)
  } else {
    f$distance
  }
  prep_data(demand, f$supply, travel)
}

# The SPAX-057 grouped case of test-06-allocation_groups.R: the generated case split
# into three age bands (true sigma 5 / 8 / 12, shared v0 25), group demand 20 times the
# generated demand, Poisson counts of the group totals from seed 11.
.run_group_ids <- c("0-15", "16-64", "65+")
.run_group_truth <- c("sigma.0-15" = 5, "sigma.16-64" = 8, "sigma.65+" = 12, v0 = 25)
.run_group_case <- function() {
  .run_memo("group_case", {
    base <- alloc_generated_case()
    ids <- rownames(base$distance)
    t <- seq(0, 1, length.out = length(ids))
    share <- cbind(0.15 + 0.5 * t, 0.5 - 0.3 * t, 0.35 - 0.2 * t)
    share <- share / rowSums(share)
    demand <- stats::setNames(lapply(seq_along(.run_group_ids), function(k) {
      stats::setNames(20 * base$demand * share[, k], ids)
    }), .run_group_ids)
    data <- prep_data(demand, base$supply, base$distance)
    model <- clm_allocation(data, by_group = "sigma", fit_v0 = TRUE)
    expected <- evaluate_allocation(model, .run_group_truth,
      requested_outputs = "utilization_by_group")$outputs$utilization_by_group
    counts <- withr::with_seed(11, {
      expected[] <- stats::rpois(length(expected), expected)
      expected
    })
    list(data = data, model = model, counts = counts,
         starts = rbind(centre = c(rep(7, 3), 20), spread = c(4, 12, 20, 50)) |>
           `colnames<-`(names(.run_group_truth)),
         lower = stats::setNames(rep(1, 4), names(.run_group_truth)),
         upper = stats::setNames(c(rep(40, 3), 200), names(.run_group_truth)))
  })
}

# The fit the step workflow gives for a specification, observations and arguments:
# bind, align the observations, fit_allocation() with the strict controls.
.run_steps <- function(data, spec, obs, starts = NULL, lower = NULL, upper = NULL) {
  bound <- .bind_spax_spec(spec, data)
  target <- .canonical_observed(obs, data, outside_option = .spec_has_outside(spec))
  fit_allocation(bound$problem, target, starts, lower, upper, output = obs$type,
                 loss = obs$loss, loss_args = obs$loss_args, gradient = TRUE,
                 control = .run_strict)
}

# Spec: the strict controls, the checks and the refusals ---------------------------------

test_that("a fit equals the step workflow with the same arguments", {
  case <- .run_case()
  totals <- obs_totals(case$y)
  x <- spax(case$data, spec_clm(), observed = totals, starts = case$starts,
            lower = case$lower, upper = case$upper)
  ref <- .run_steps(case$data, spec_clm(), totals, case$starts, case$lower, case$upper)
  expect_s3_class(x, c("spax_allocation", "spax_result"), exact = TRUE)
  expect_identical(x$mode, "fit")
  # the fit table without its timings, the coefficients and the whole fit
  expect_identical(x$fit$table[setdiff(names(x$fit$table), "seconds")],
                   ref$table[setdiff(names(ref$table), "seconds")])
  expect_identical(coef(x), coef(ref))
  expect_identical(x$theta, coef(ref))
  expect_identical(.run_strip(x$fit), .run_strip(ref))
  expect_true(x$fit$best$gradient)
  # the model is the one the step functions give
  .run_expect_same_model(x$model, clm_allocation(case$data, fit_v0 = TRUE))
  expect_identical(x$spec, spec_clm())
  expect_identical(x$observed$spec, totals)
  expect_identical(x$observed$output, "utilization")
  expect_identical(x$observed$target,
                   .canonical_observed(totals, case$data, outside_option = TRUE))
  expect_null(x$evaluation)

  # flows, and the Huff model
  flows <- obs_flows(case$flow)
  xf <- spax(case$data, spec_clm(), observed = flows, starts = case$starts,
             lower = case$lower, upper = case$upper)
  reff <- .run_steps(case$data, spec_clm(), flows, case$starts, case$lower, case$upper)
  expect_identical(.run_strip(xf$fit), .run_strip(reff))
  expect_identical(xf$observed$output, "flow")
  huff_starts <- rbind(a = c(sigma = 1, beta = 0.5), b = c(sigma = 3, beta = 1.5))
  huff_lower <- c(sigma = 0.1, beta = 0.05)
  huff_upper <- c(sigma = 10, beta = 5)
  xh <- spax(case$data, spec_huff(), observed = totals, starts = huff_starts,
             lower = huff_lower, upper = huff_upper)
  refh <- .run_steps(case$data, spec_huff(), totals, huff_starts, huff_lower, huff_upper)
  expect_identical(.run_strip(xh$fit), .run_strip(refh))
  .run_expect_same_model(xh$model, huff_allocation(case$data))
  # the loss comes with the observation
  sse <- obs_totals(case$y, loss = "weighted_sse")
  xs <- spax(case$data, spec_clm(), observed = sse, starts = case$starts,
             lower = case$lower, upper = case$upper)
  expect_identical(.run_strip(xs$fit),
                   .run_strip(.run_steps(case$data, spec_clm(), sse, case$starts,
                                         case$lower, case$upper)))
  expect_identical(xs$fit$best$loss_fn, .weighted_sse_loss)
  expect_identical(x$fit$best$loss_fn, .poisson_loss)
})

test_that("the default search gives the fit of explicit starts on the generated case", {
  case <- .run_case()
  x <- spax(case$data, spec_clm(), observed = obs_totals(case$y))
  ref <- alloc_generated_fit()
  expect_identical(names(coef(x)), names(coef(ref)))
  expect_lt(max(abs(coef(x) / coef(ref) - 1)), 1e-5)
  expect_lt(abs(x$fit$best$loss - ref$best$loss), 1e-6)
  expect_identical(x$fit$default_search$rule, "probe+scan v1")
  expect_identical(x$fit$default_search$loss, "poisson")
  expect_true(x$fit$best$gradient)
  expect_false(any(x$fit$boundary[x$fit$best_start, ]))
  # flows
  xf <- spax(case$data, spec_clm(), observed = obs_flows(case$flow))
  reff <- alloc_generated_fit(output = "flow")
  expect_lt(max(abs(coef(xf) / coef(reff) - 1)), 1e-5)
  expect_lt(abs(xf$fit$best$loss - reff$best$loss), 1e-6)
  # raw observed counts: a named vector or a matrix is read like the declarations
  expect_equal(spax(case$data, spec_clm(), observed = case$y)$theta, coef(x), tolerance = 1e-10)
  expect_equal(spax(case$data, spec_clm(), observed = case$flow)$theta, coef(xf),
               tolerance = 1e-10)
})

test_that("the fits to Cornwall by the default search equal fits from explicit starts", {
  skip_on_cran()
  areas <- sf::st_drop_geometry(cornwall_lsoa[!cornwall_lsoa$scilly, ])
  gp <- sf::st_drop_geometry(cornwall_gp[!cornwall_gp$scilly, ])
  travel <- cornwall_travel[areas$lsoa, gp$practice]
  supply <- stats::setNames(gp$gp_fte_extg, gp$practice)
  flows <- obs_flows(cornwall_flows, origin = "lsoa", facility = "practice",
                     count = "patients")
  same <- function(x, ref) {
    expect_identical(names(coef(x)), names(coef(ref)))
    expect_lt(max(abs(coef(x) / coef(ref) - 1)), 1e-5)
    expect_lt(abs(x$fit$best$loss - ref$best$loss), 1e-6)
    expect_false(any(x$fit$boundary[x$fit$best_start, ]))
  }

  # Huff: power decay and fitted beta, no outside option
  d_huff <- prep_data(stats::setNames(areas$registered_in, areas$lsoa), supply, travel)
  huff <- suppressMessages(spax(d_huff, spec_huff(), observed = flows))
  ref_huff <- suppressMessages(.run_steps(d_huff, spec_huff(), flows,
    starts = rbind(a = c(sigma = 1, beta = 0.5), b = c(sigma = 3, beta = 1.5)),
    lower = c(sigma = 0.1, beta = 0.05), upper = c(sigma = 10, beta = 5)))
  same(huff, ref_huff)
  expect_equal(unname(coef(huff)), c(2.393, 0.4863), tolerance = 1e-3)

  # conditional logit: Gaussian decay and a fitted outside weight
  d_clm <- prep_data(stats::setNames(areas$registered, areas$lsoa), supply, travel)
  clm <- suppressMessages(spax(d_clm, spec_clm(), observed = flows))
  ref_clm <- suppressMessages(.run_steps(d_clm, spec_clm(), flows,
    starts = rbind(a = c(sigma = 4, v0 = 0.02), b = c(sigma = 6, v0 = 0.05)),
    lower = c(sigma = 1, v0 = 1e-4), upper = c(sigma = 30, v0 = 5)))
  same(clm, ref_clm)
  expect_equal(unname(coef(clm)), c(4.591, 0.02734), tolerance = 1e-3)
  expect_identical(clm$observed$output, "flow")
  expect_identical(dim(clm$observed$target), c(336L, 54L))
})

test_that("spax() always passes the strict controls unless gradient or control is given", {
  case <- .run_case()
  seen <- NULL
  testthat::local_mocked_bindings(fit_allocation = function(model, observed, starts = NULL,
      lower = NULL, upper = NULL, output = "utilization", loss = "weighted_sse",
      loss_args = list(), ...) {
    seen <<- list(starts = starts, lower = lower, upper = upper, output = output,
                  loss = loss, loss_args = loss_args, dots = list(...))
    stop("stop after the call")
  })
  call_spax <- function(...) {
    seen <<- NULL
    try(spax(case$data, spec_clm(), observed = obs_totals(case$y), ...), silent = TRUE)
    seen
  }
  # without starts and with them
  for (given in list(list(), list(starts = case$starts, lower = case$lower,
                                  upper = case$upper))) {
    s <- do.call(call_spax, given)
    expect_identical(s$dots$gradient, TRUE)
    expect_identical(s$dots$control, .run_strict)
    expect_identical(s$output, "utilization")
    expect_identical(s$loss, "poisson")
    expect_identical(s$loss_args, list())
    expect_identical(s$starts, given$starts)
  }
  # each default is dropped when `...` gives that argument
  s <- call_spax(control = list(maxit = 5))
  expect_identical(s$dots$control, list(maxit = 5))
  expect_identical(s$dots$gradient, TRUE)
  s <- call_spax(gradient = FALSE)
  expect_identical(s$dots$gradient, FALSE)
  expect_identical(s$dots$control, .run_strict)
  # other arguments of fit_allocation() pass through; the output follows the kind of
  # observation and the loss comes from it
  s <- call_spax(keep_fits = FALSE, tol = 1e-6)
  expect_identical(s$dots$keep_fits, FALSE)
  expect_identical(s$dots$tol, 1e-6)
  seen <- NULL
  try(spax(case$data, spec_clm(), observed = obs_flows(case$flow, loss = "weighted_sse",
                                                       loss_args = list(eta = 2))),
      silent = TRUE)
  expect_identical(seen$output, "flow")
  expect_identical(seen$loss, "weighted_sse")
  expect_identical(seen$loss_args, list(eta = 2))
})

test_that("the strict controls reach a real fit from given starts", {
  case <- .run_case()
  x <- spax(case$data, spec_clm(), observed = obs_totals(case$y), starts = case$starts,
            lower = case$lower, upper = case$upper, gradient = FALSE)
  expect_false(x$fit$best$gradient)
  expect_true(spax(case$data, spec_clm(), observed = obs_totals(case$y),
                   starts = case$starts, lower = case$lower,
                   upper = case$upper)$fit$best$gradient)
})

test_that("the checks run in order and the messages are exact", {
  case <- .run_case()
  d <- case$data
  totals <- obs_totals(case$y)
  # 1. data, before the model
  expect_identical(.run_error(spax(list(), "huff")), "data must come from prep_data()")
  expect_identical(.run_error(spax(case$demand, spec_huff())),
                   "data must come from prep_data()")
  # 2. model, before the refusals and the binding
  model_message <- "model must be a model specification such as spec_huff() or spec_clm()"
  expect_identical(.run_error(spax(d, "huff", loss = "poisson")), model_message)
  expect_identical(.run_error(spax(d, list(kind = "huff"))), model_message)
  expect_identical(.run_error(spax(d, decay_gaussian())), model_message)
  # 3. the refusals in `...`, before the binding and in both modes
  refusal <- paste0("the loss is set in obs_flows() or obs_totals() (loss = ), and the ",
                    "output by the kind of observation")
  expect_identical(.run_error(spax(d, spec_clm(), observed = totals, loss = "poisson")),
                   refusal)
  expect_identical(.run_error(spax(d, spec_clm(), observed = totals, output = "flow")),
                   refusal)
  expect_identical(.run_error(spax(d, spec_clm(), observed = totals, loss_args = list())),
                   refusal)
  expect_identical(.run_error(spax(d, spec_clm(decay_gaussian(8), outside = 5),
                                   loss = "poisson")), refusal)
  expect_identical(.run_error(spax(d, spec_clm(by_group = "sigma"), loss = "poisson")),
                   refusal)
  # binding errors come before the observed checks
  expect_identical(.run_error(spax(d, spec_clm(by_group = "sigma"))),
                   paste0("by_group needs data with demand groups (prep_data() with demand ",
                          "as a named list by group)"))
  expect_match(.run_error(spax(alloc_huff_case()$inputs, spec_clm(decay_gaussian(5)))),
               "in this version sigma can be held only when nothing else is fitted",
               fixed = TRUE)
  # fit mode: observed is needed, naming the free parameters
  expect_identical(.run_error(spax(d, spec_clm())),
                   "observed counts are needed to fit sigma, v0; or give their values")
  expect_identical(.run_error(spax(d, spec_huff())),
                   "observed counts are needed to fit sigma, beta; or give their values")
  expect_identical(.run_error(spax(d, spec_clm(beta = "fit"))),
                   "observed counts are needed to fit sigma, v0, beta; or give their values")
  # raw and declared observations go through their own checks
  expect_identical(.run_error(spax(d, spec_clm(), observed = data.frame(a = 1))),
                   "wrap a table in obs_flows() or obs_totals() to name its columns")
  expect_match(.run_error(spax(d, spec_clm(), observed = unname(case$y))),
               "observed must be named by origin and facility IDs")
  # evaluate mode: nothing to fit
  fixed <- spec_clm(decay_gaussian(8), outside = 25)
  expect_identical(.run_error(spax(d, fixed, observed = totals)),
                   "nothing to fit: every parameter has a value; omit observed")
  bounds_message <- "starts and bounds apply only when something is fitted"
  expect_identical(.run_error(spax(d, fixed, starts = case$starts)), bounds_message)
  expect_identical(.run_error(spax(d, fixed, lower = case$lower)), bounds_message)
  expect_identical(.run_error(spax(d, fixed, upper = case$upper)), bounds_message)
  # observed is checked before starts
  expect_identical(.run_error(spax(d, fixed, observed = totals, starts = case$starts)),
                   "nothing to fit: every parameter has a value; omit observed")
})

test_that("an evaluation equals evaluate_allocation(), ungrouped", {
  case <- .run_case()
  d <- case$data
  x <- spax(d, spec_clm(decay_gaussian(8), outside = 25))
  ref <- evaluate_allocation(clm_allocation(d, v0 = 25), c(sigma = 8))
  expect_identical(x$mode, "evaluate")
  expect_s3_class(x$evaluation, "ae_equilibrium")
  expect_identical(x$evaluation$outputs, ref$outputs)
  expect_identical(x$evaluation$x_star, ref$x_star)
  expect_identical(x$evaluation$coverage, ref$coverage)
  expect_identical(x$theta, c(sigma = 8))
  expect_identical(x$held, c(v0 = 25, beta = 1, kappa = 1))
  expect_null(x$fit)
  expect_null(x$observed)
  .run_expect_same_model(x$model, clm_allocation(d, v0 = 25))
  # the Huff model, with beta given, and with kappa
  h <- spax(d, spec_huff(decay_power(2), beta = 1))
  expect_identical(h$evaluation$outputs,
                   evaluate_allocation(huff_allocation(d, fit_beta = FALSE), c(sigma = 2))$outputs)
  expect_identical(h$held, c(v0 = 0, beta = 1, kappa = 1))
  k <- spax(d, spec_clm(decay_exponential(0.2), outside = 3, beta = 0.5, kappa = 2))
  expect_identical(k$evaluation$outputs,
    evaluate_allocation(clm_allocation(d, family = "exponential", kappa = 2, beta = 0.5,
                                       v0 = 3), c(sigma = 0.2))$outputs)
  expect_identical(k$held, c(v0 = 3, beta = 0.5, kappa = 2))
  # `...` goes to evaluate_allocation()
  f <- spax(d, spec_clm(decay_gaussian(8), outside = 25), requested_outputs = "flow")
  expect_identical(f$evaluation$outputs$flow, ref_flow <-
    evaluate_allocation(clm_allocation(d, v0 = 25), c(sigma = 8),
                        requested_outputs = "flow")$outputs$flow)
  expect_null(x$evaluation$outputs$flow)
  expect_identical(.run_error(spax(d, spec_clm(decay_gaussian(8), outside = 25),
                                   requested_outputs = "nope")),
                   "problem outputs do not include requested output `nope`")
})

test_that("an evaluation equals evaluate_allocation(), grouped", {
  shared <- .run_grouped()
  by_group_travel <- .run_grouped(per_group_travel = TRUE)
  # a shared kernel with its sigma given
  x <- spax(shared, spec_clm(decay_gaussian(5), outside = 0.1))
  ref <- evaluate_allocation(clm_allocation(shared, v0 = 0.1), c(sigma = 5))
  expect_identical(x$evaluation$outputs, ref$outputs)
  expect_identical(x$theta, c(sigma = 5))
  # a decay by group, with the per-group outputs requested
  decay <- list(young = decay_gaussian(5), mid = decay_gaussian(6), old = decay_gaussian(8))
  grouped <- c("utilization_by_group", "access_by_group")
  y <- spax(by_group_travel, spec_clm(decay, outside = 0.1), requested_outputs = grouped)
  family <- list(young = "gaussian", mid = "gaussian", old = "gaussian")
  ref <- evaluate_allocation(
    clm_allocation(by_group_travel, family = family, v0 = 0.1, by_group = "sigma"),
    c(sigma.young = 5, sigma.mid = 6, sigma.old = 8), requested_outputs = grouped)
  expect_identical(y$evaluation$outputs, ref$outputs)
  expect_identical(y$theta, c(sigma.young = 5, sigma.mid = 6, sigma.old = 8))
  expect_identical(y$held, c(v0 = 0.1, beta = 1, kappa = 1))
  # per-group kernels of different families, and a v0 by group
  z <- spax(shared, spec_clm(list(young = decay_gaussian(5), mid = decay_exponential(0.3),
                                  old = decay_power(2)),
                             outside = c(young = 0.1, mid = 0.2, old = 0.3)))
  ref <- evaluate_allocation(
    clm_allocation(shared, family = list(young = "gaussian", mid = "exponential",
                                         old = "power"),
                   v0 = c(young = 0.1, mid = 0.2, old = 0.3), by_group = "sigma"),
    c(sigma.young = 5, sigma.mid = 0.3, sigma.old = 2))
  expect_identical(z$evaluation$outputs, ref$outputs)
  expect_identical(z$held, c(v0.young = 0.1, v0.mid = 0.2, v0.old = 0.3, beta = 1, kappa = 1))
  # the Huff model on grouped data
  h <- spax(shared, spec_huff(decay_power(2), beta = 1))
  expect_identical(h$evaluation$outputs,
                   evaluate_allocation(huff_allocation(shared, fit_beta = FALSE),
                                       c(sigma = 2))$outputs)
  # methods that read outputs work on a grouped evaluation; predict is refused
  expect_identical(nrow(as.data.frame(y, type = "facilities")), 3L)
  expect_identical(as.data.frame(y, type = "origins")$access,
                   as.numeric(y$evaluation$outputs$access))
  expect_identical(.run_error(predict(y, alloc_huff_case()$distance)),
                   "predict_allocation() is not available for grouped models yet")
})

test_that("the one-shots equal spax() on prepared data", {
  case <- .run_case()
  meta <- list(demand_units = "people", travel_units = "minutes")
  # a fit, from starts
  x <- spax_clm(case$demand, case$supply, case$distance, observed = case$y,
                starts = case$starts, lower = case$lower, upper = case$upper,
                data_args = list(metadata = meta))
  ref <- spax(prep_data(case$demand, case$supply, case$distance, metadata = meta),
              spec_clm(), observed = case$y, starts = case$starts, lower = case$lower,
              upper = case$upper)
  .run_expect_same_model(x$model, ref$model)
  expect_identical(.run_strip(x$fit), .run_strip(ref$fit))
  for (field in c("spec", "observed", "theta", "held", "mode", "evaluation")) {
    expect_identical(x[[field]], ref[[field]], label = field)
  }
  expect_identical(x$model$substrate$input_metadata$travel_units, "minutes")
  expect_identical(as.character(x$call[[1L]]), "spax_clm")
  expect_identical(x$call$observed, quote(case$y))
  expect_s3_class(x, c("spax_allocation", "spax_result"), exact = TRUE)
  # the model arguments reach the specification
  y <- spax_clm(case$demand, case$supply, case$distance, observed = case$y,
                decay = "exponential", outside = 0, kappa = 2, beta = 0.5)
  r <- spax(prep_data(case$demand, case$supply, case$distance),
            spec_clm("exponential", outside = 0, kappa = 2, beta = 0.5),
            observed = case$y)
  expect_identical(.run_strip(y$fit), .run_strip(r$fit))
  expect_identical(y$spec, r$spec)
  # Huff, with the default decay and beta, from the default search
  h <- spax_huff(case$demand, case$supply, case$distance, observed = case$flow)
  rh <- spax(prep_data(case$demand, case$supply, case$distance), spec_huff(),
             observed = case$flow)
  expect_identical(.run_strip(h$fit), .run_strip(rh$fit))
  expect_identical(h$spec, spec_huff())
  expect_identical(as.character(h$call[[1L]]), "spax_huff")
  # evaluation
  e <- spax_huff(case$demand, case$supply, case$distance, decay = decay_power(1.5), beta = 1)
  re <- spax(prep_data(case$demand, case$supply, case$distance),
             spec_huff(decay_power(1.5), beta = 1))
  expect_identical(e$evaluation, re$evaluation)
  expect_identical(e$theta, re$theta)
  # grouped data
  g <- alloc_huff_case()
  demand <- list(young = g$demand * 0.2, old = g$demand * 0.8)
  eg <- spax_clm(demand, g$supply, g$distance, decay = list(young = decay_gaussian(3),
                                                           old = decay_gaussian(5)),
                 outside = 0.2)
  expect_identical(eg$evaluation$outputs,
    spax(prep_data(demand, g$supply, g$distance),
         spec_clm(list(young = decay_gaussian(3), old = decay_gaussian(5)),
                  outside = 0.2))$evaluation$outputs)
})

test_that("the one-shots check their arguments in the order of spax()", {
  case <- .run_case()
  # the data first (as spax(prep_data(...), ...) would), then the specification
  expect_match(.run_error(spax_huff(case$demand[-1], case$supply, case$distance,
                                    decay = "other")), "demand|origin")
  expect_identical(.run_error(spax_clm(case$demand, case$supply, case$distance,
                                       outside = "x")),
                   "outside must be \"fit\", a number, or a named list or vector by group")
  expect_identical(.run_error(spax_huff(case$demand, case$supply, case$distance,
                                        decay = "other")),
                   paste0("decay must be decay_gaussian(), decay_exponential(), ",
                          "decay_power(), decay_custom(), a decay_kernel() or one of ",
                          "\"gaussian\", \"exponential\", \"power\""))
  expect_identical(.run_error(spax_huff(case$demand, case$supply, case$distance,
                                        loss = "poisson")),
                   paste0("the loss is set in obs_flows() or obs_totals() (loss = ), and ",
                          "the output by the kind of observation"))
  expect_identical(.run_error(spax_huff(case$demand, case$supply, case$distance)),
                   "observed counts are needed to fit sigma, beta; or give their values")
  expect_identical(.run_error(spax_clm(case$demand, case$supply, case$distance,
                                       decay = decay_gaussian(8), outside = 25,
                                       observed = case$y)),
                   "nothing to fit: every parameter has a value; omit observed")
  # an argument of prep_data() in `...` is refused first, whatever else is wrong
  for (name in c("missing_demand", "metadata", "included", "origin_ids", "facility_ids",
                 "missing_travel", "spatial")) {
    args <- list(case$demand, case$supply, case$distance, 1)
    names(args) <- c("", "", "", name)
    expect_identical(.run_error(do.call(spax_huff, c(args, list(decay = "other")))),
                     paste0(name, " is an argument of prep_data(): give it in data_args = list(",
                            name, " = ...)"))
    expect_identical(.run_error(do.call(spax_clm, c(args, list(outside = "x")))),
                     paste0(name, " is an argument of prep_data(): give it in data_args = list(",
                            name, " = ...)"))
  }
  expect_identical(.run_error(spax_huff(case$demand, case$supply, case$distance,
                                        missing_demand = "exclude", data_args = list("minutes"))),
                   "missing_demand is an argument of prep_data(): give it in data_args = list(missing_demand = ...)")
  # the same argument given as data_args works
  expect_s3_class(spax_clm(case$demand, case$supply, case$distance, decay = decay_gaussian(8),
                           outside = 25, data_args = list(missing_demand = "exclude")),
                  "spax_allocation")
  # data_args are arguments of prep_data() by name
  expect_identical(.run_error(spax_huff(case$demand, case$supply, case$distance,
                                        data_args = list("minutes"))),
                   "data_args must be a named list of arguments of prep_data()")
  expect_identical(.run_error(spax_clm(case$demand, case$supply, case$distance,
                                       data_args = 1)),
                   "data_args must be a named list of arguments of prep_data()")
  expect_error(spax_huff(case$demand, case$supply, case$distance,
                         data_args = list(nope = 1)), "unused argument")
})

# Fit with no selected start --------------------------------------------------------

# Every start stops at one iteration, so none converges: a real search with no
# eligible fit.
.run_failed <- function() {
  .run_memo("failed", {
    case <- .run_case()
    x <- suppressWarnings(spax(case$data, spec_clm(), observed = obs_totals(case$y),
                               starts = case$starts, lower = case$lower, upper = case$upper,
                               control = list(maxit = 1)))
    list(case = case, x = x)
  })
}

test_that("a search with no selected start warns, and methods that need outputs say so", {
  case <- .run_case()
  expect_warning(
    x <- spax(case$data, spec_clm(), observed = obs_totals(case$y), starts = case$starts,
              lower = case$lower, upper = case$upper, control = list(maxit = 1)),
    "no start converged; see summary(x)", fixed = TRUE)
  expect_identical(x$mode, "fit")
  expect_null(x$fit$best)
  expect_null(x$theta)
  expect_null(coef(x))
  expect_identical(x$held, c(beta = 1, kappa = 1))
  expect_s3_class(x, "spax_allocation")
  # the same warning with a mocked fit and no starts
  mocked <- structure(list(best = NULL, best_start = NULL, status = "no_valid_fit",
                           table = data.frame(start = "scan1", eligible = FALSE),
                           boundary = matrix(FALSE, 1, 2, dimnames = list("scan1",
                                                                          c("sigma", "v0")))),
                      class = "ae_multistart_fit")
  testthat::local_mocked_bindings(fit_allocation = function(...) mocked)
  expect_warning(m <- spax(case$data, spec_clm(), observed = obs_totals(case$y)),
                 "no start converged; see summary(x)", fixed = TRUE)
  expect_null(m$theta)
  expect_identical(m$fit, mocked)
})

test_that("a result with no selected start still shows its parameters and starts", {
  x <- .run_failed()$x
  expect_identical(.run_error(fitted(x)), .run_no_fit)
  expect_identical(.run_error(predict(x, .run_case()$distance[1:2, ])), .run_no_fit)
  for (type in c("facilities", "origins", "flows")) {
    expect_identical(.run_error(as.data.frame(x, type = type)), .run_no_fit, label = type)
  }
  expect_identical(.run_error(.result_outputs(x, "utilization")), .run_no_fit)
  # parameters, starts, summary and print do not need outputs
  p <- as.data.frame(x)
  expect_identical(p$parameter, c("sigma", "v0", "beta", "kappa"))
  expect_identical(p$value, c(NA, NA, 1, 1))
  expect_identical(p$status, c("not fitted", "not fitted", "held", "held"))
  starts <- as.data.frame(x, type = "starts")
  expect_identical(starts, x$fit$table)
  expect_identical(starts$eligible, c(FALSE, FALSE))
  s <- summary(x)
  expect_true(all(is.na(s$parameters$value[1:2])))
  expect_identical(s$fit$status, "no_valid_fit")
  lines <- capture.output(print(x))
  expect_identical(lines[length(lines)], "  selected    none: no start converged (2 starts)")
  expect_identical(lines[8:9], c("  sigma      NA     not fitted", "  v0         NA     not fitted"))
  expect_false(any(grepl("boundary|default search", lines)))
  expect_identical(capture.output(print(s))[1L],
                   "<spax allocation summary> conditional logit with an outside option")
})

# Methods ---------------------------------------------------------------------------

.run_totals_fit <- function() {
  .run_memo("totals_fit", {
    case <- .run_case()
    spax(case$data, spec_clm(), observed = obs_totals(case$y), starts = case$starts,
         lower = case$lower, upper = case$upper)
  })
}

# The generated case with an absent edge (infinite travel at origin2, facility3), an
# unobserved cell and a zero count that the model gives a positive flow.
.run_flow_fit <- function() {
  .run_memo("flow_fit", {
    case <- .run_case()
    travel <- case$distance
    travel["origin2", "facility3"] <- Inf
    flows <- case$flow
    flows["origin2", "facility3"] <- 0
    flows["origin5", "facility7"] <- NA
    flows["origin4", "facility9"] <- 0
    data <- prep_data(case$demand, case$supply, travel)
    x <- spax(data, spec_clm(), observed = obs_flows(flows), starts = case$starts,
              lower = case$lower, upper = case$upper)
    list(x = x, flows = flows, travel = travel)
  })
}

test_that("coef() gives the free parameters and held values are on the result", {
  x <- .run_totals_fit()
  case <- .run_case()
  expect_identical(coef(x), x$fit$best$theta)
  expect_identical(names(coef(x)), c("sigma", "v0"))
  expect_identical(x$held, c(beta = 1, kappa = 1))
  expect_true(all(abs(coef(x) / case$truth - 1) < 0.1))
  # Huff: sigma and beta are free, no outside option
  h <- spax(case$data, spec_huff(), observed = obs_totals(case$y))
  expect_identical(names(coef(h)), c("sigma", "beta"))
  expect_identical(h$held, c(v0 = 0, kappa = 1))
  # a held beta and v0, a fitted sigma: the held values are not in coef()
  c1 <- spax(case$data, spec_clm(outside = 25, beta = 1), observed = obs_totals(case$y))
  expect_identical(names(coef(c1)), "sigma")
  expect_identical(c1$held, c(v0 = 25, beta = 1, kappa = 1))
  # an evaluation: sigma is given, the rest is held
  e <- spax(case$data, spec_huff(decay_power(1.5), beta = 0.8))
  expect_identical(coef(e), c(sigma = 1.5))
  expect_identical(e$held, c(v0 = 0, beta = 0.8, kappa = 1))
})

test_that("a grouped model holds its group sigma through the decay list", {
  case <- .run_group_case()
  decay <- list(`0-15` = decay_gaussian(), `16-64` = decay_gaussian(8),
                `65+` = decay_gaussian())
  x <- spax(case$data, spec_clm(decay), observed = obs_totals(case$counts))
  expect_identical(names(coef(x)), c("sigma.0-15", "sigma.65+", "v0"))
  expect_identical(x$held, c("sigma.16-64" = 8, beta = 1, kappa = 1))
  expect_true(all(abs(coef(x) / .run_group_truth[names(coef(x))] - 1) < 0.15))
  p <- as.data.frame(x)
  expect_identical(p$parameter, c("sigma.0-15", "sigma.65+", "v0", "sigma.16-64", "beta", "kappa"))
  expect_identical(p$status, c("fitted", "fitted", "fitted", "held", "held", "held"))
  expect_identical(p$value, c(unname(coef(x)), 8, 1, 1))
})

test_that("a grouped fit to totals by group recovers the SPAX-057 values", {
  case <- .run_group_case()
  totals <- obs_totals(case$counts)
  expect_identical(dim(totals$x), c(24L, 3L))
  # from the starts of that test
  x <- spax(case$data, spec_clm(by_group = "sigma"), observed = totals,
            starts = case$starts, lower = case$lower, upper = case$upper)
  ref <- fit_allocation(case$model, case$counts, case$starts, case$lower, case$upper,
                        output = "utilization_by_group", loss = "poisson", gradient = TRUE,
                        control = .run_strict)
  expect_identical(.run_strip(x$fit), .run_strip(ref))
  expect_identical(names(coef(x)), names(.run_group_truth))
  expect_true(all(x$fit$table$eligible))
  expect_false(any(x$fit$boundary))
  expect_true(all(abs(coef(x) / .run_group_truth - 1) < 0.15))
  est <- coef(x)
  expect_true(est[["sigma.0-15"]] < est[["sigma.16-64"]] && est[["sigma.16-64"]] < est[["sigma.65+"]])
  expect_identical(x$observed$output, "utilization_by_group")
  # and from the default search
  d <- spax(case$data, spec_clm(by_group = "sigma"), observed = totals)
  expect_identical(names(coef(d)), names(.run_group_truth))
  expect_true(all(abs(coef(d) / .run_group_truth - 1) < 0.15))
  expect_equal(coef(d), coef(x), tolerance = 1e-4)
  expect_identical(d$fit$default_search$rule, "probe+scan v1")
  # the totals by group as a long table
  long <- data.frame(practice = rep(rownames(case$counts), 3),
                     age = rep(colnames(case$counts), each = 24), n = as.vector(case$counts))
  expect_equal(coef(spax(case$data, spec_clm(by_group = "sigma"),
                         observed = obs_totals(long), starts = case$starts,
                         lower = case$lower, upper = case$upper)), coef(x), tolerance = 1e-8)
  # a model with a group sigma refuses totals that are not by group
  expect_match(.run_error(spax(case$data, spec_clm(by_group = "sigma"),
                               observed = obs_totals(rowSums(case$counts)))),
               "group-specific parameters (sigma.0-15, sigma.16-64, sigma.65+) need",
               fixed = TRUE)
  expect_s3_class(x, "spax_allocation")
  # methods on a grouped fit: totals by group in the facilities table
  tab <- as.data.frame(x, type = "facilities")
  expect_identical(tab$observed, as.numeric(rowSums(case$counts)))
  expect_identical(.run_error(predict(x, .run_case()$distance[1:2, ])),
                   "predict_allocation() is not available for grouped models yet")
})

test_that("as.data.frame() returns each table", {
  x <- .run_totals_fit()
  case <- .run_case()
  # the default is the parameters
  expect_identical(as.data.frame(x), as.data.frame(x, type = "parameters"))
  p <- as.data.frame(x)
  expect_identical(names(p), c("parameter", "value", "status"))
  expect_identical(p$parameter, c("sigma", "v0", "beta", "kappa"))
  expect_identical(p$value, c(unname(coef(x)), 1, 1))
  expect_identical(p$status, c("fitted", "fitted", "held", "held"))
  expect_identical(nrow(p), 4L)

  # facilities: the data facilities, with the observed totals
  f <- as.data.frame(x, type = "facilities")
  expect_identical(names(f), c("facility_id", "supply", "attractiveness", "utilization",
                               "observed"))
  expect_identical(f$facility_id, colnames(case$distance))
  expect_identical(f$supply, unname(case$supply[f$facility_id]))
  expect_identical(f$attractiveness, f$supply)
  expect_identical(f$utilization, as.numeric(fitted(x)))
  expect_identical(f$observed, unname(case$y[f$facility_id]))
  expect_identical(nrow(f), 24L)
  expect_identical(attr(f, "row.names"), 1:24)

  # origins: the active origins
  o <- as.data.frame(x, type = "origins")
  expect_identical(names(o), c("origin_id", "demand", "access", "outside_share"))
  expect_identical(o$origin_id, rownames(case$distance))
  expect_identical(o$demand, unname(case$demand[o$origin_id]))
  expect_equal(o$access + o$outside_share, rep(1, 18), tolerance = 1e-12)
  expect_gt(min(o$outside_share), 0)
  expect_identical(o$access, as.numeric(x$fit$best$outputs$access))

  # flows of a fit to totals: computed on demand, every cell with a positive flow
  stored <- names(x$fit$best$outputs)
  fl <- as.data.frame(x, type = "flows")
  expect_identical(names(fl), c("origin_id", "facility_id", "flow"))
  ref <- evaluate_allocation(x$model, coef(x), requested_outputs = "flow")$outputs$flow
  expect_identical(nrow(fl), sum(ref > 0))
  expect_identical(fl$flow, as.numeric(ref[cbind(match(fl$origin_id, rownames(ref)),
                                                 match(fl$facility_id, colnames(ref)))]))
  expect_identical(fl$origin_id[1:3], rep("origin1", 3))
  expect_identical(fl$facility_id[1:3], paste0("facility", 1:3))
  expect_equal(sum(fl$flow), sum(case$demand * o$access), tolerance = 1e-8)
  # ... and nothing is stored by asking
  expect_identical(names(x$fit$best$outputs), stored)
  expect_false("flow" %in% names(x$fit$best$outputs))

  # starts
  s <- as.data.frame(x, type = "starts")
  expect_identical(s, x$fit$table)
  expect_identical(nrow(s), 2L)
  expect_identical(s$start, c("short", "wide"))

  # row.names and optional are passed on; an unknown type is refused
  expect_identical(rownames(as.data.frame(x, row.names = c("a", "b", "c", "d"))),
                   c("a", "b", "c", "d"))
  expect_error(as.data.frame(x, type = "regions"), "should be one of")
})

test_that("the flows table keeps cells with a modelled flow or an observed count", {
  fit <- .run_flow_fit()
  x <- fit$x
  fl <- as.data.frame(x, type = "flows")
  expect_identical(names(fl), c("origin_id", "facility_id", "flow", "observed"))
  # the one cell with no modelled flow and a zero count is left out
  expect_identical(nrow(fl), 18L * 24L - 1L)
  expect_false(any(fl$origin_id == "origin2" & fl$facility_id == "facility3"))
  # an unobserved cell keeps its modelled flow; a zero count with a modelled flow stays
  cell <- fl[fl$origin_id == "origin5" & fl$facility_id == "facility7", ]
  expect_true(is.na(cell$observed))
  expect_gt(cell$flow, 0)
  zero <- fl[fl$origin_id == "origin4" & fl$facility_id == "facility9", ]
  expect_identical(zero$observed, 0)
  expect_gt(zero$flow, 0)
  # flows are demand times allocation, read from the fit without another solve
  expect_identical(fl$flow, local({
    m <- x$fit$best$outputs$flow
    as.numeric(m[cbind(match(fl$origin_id, rownames(m)), match(fl$facility_id, colnames(m)))])
  }))
  expect_identical(fl$observed, local({
    m <- fit$flows
    as.numeric(m[cbind(match(fl$origin_id, rownames(m)), match(fl$facility_id, colnames(m)))])
  }))
  # origin order, then facility order
  expect_identical(fl$origin_id[1:25], c(rep("origin1", 24), "origin2"))
  expect_identical(fl$facility_id[25:27], c("facility1", "facility2", "facility4"))
  # a table of facilities has no observed column for a fit to flows
  expect_identical(names(as.data.frame(x, type = "facilities")),
                   c("facility_id", "supply", "attractiveness", "utilization"))
  expect_identical(as.data.frame(x, type = "facilities")$utilization,
                   as.numeric(x$fit$best$outputs$utilization))
})

test_that("flows are computed on demand only when the fit did not save them", {
  flow <- .run_flow_fit()$x
  totals <- .run_totals_fit()
  calls <- 0L
  solve <- .solve_problem
  testthat::local_mocked_bindings(.solve_problem = function(...) {
    calls <<- calls + 1L
    solve(...)
  })
  # a fit to flows has them; everything else is read from the fit
  for (type in c("parameters", "facilities", "origins", "flows", "starts")) {
    as.data.frame(flow, type = type)
  }
  coef(flow); fitted(flow); summary(flow); capture.output(print(flow))
  expect_identical(calls, 0L)
  # a fit to totals computes the flows once per request
  as.data.frame(totals, type = "flows")
  expect_identical(calls, 1L)
  as.data.frame(totals, type = "facilities")
  as.data.frame(totals, type = "origins")
  expect_identical(calls, 1L)
  # .result_outputs(): saved outputs as they are, others by one evaluation
  saved <- .result_outputs(totals, c("utilization", "access"))
  expect_identical(saved, totals$fit$best$outputs[c("utilization", "access")])
  expect_identical(calls, 1L)
  both <- .result_outputs(totals, c("flow", "utilization"))
  expect_identical(calls, 2L)
  expect_identical(names(both), c("flow", "utilization"))
  expect_identical(both$utilization, totals$fit$best$outputs$utilization)
  expect_equal(both$flow, evaluate_allocation(totals$model, coef(totals),
                                              requested_outputs = "flow")$outputs$flow)
})

test_that("a fit does not solve again after fit_allocation()", {
  case <- .run_case()
  solve <- .solve_problem
  count <- function(expr) {
    calls <- 0L
    testthat::local_mocked_bindings(.solve_problem = function(...) {
      calls <<- calls + 1L
      solve(...)
    })
    force(expr)
    calls
  }
  totals <- obs_totals(case$y)
  via_spax <- count(spax(case$data, spec_clm(), observed = totals, starts = case$starts,
                         lower = case$lower, upper = case$upper))
  via_steps <- count(.run_steps(case$data, spec_clm(), totals, case$starts, case$lower,
                                case$upper))
  expect_gt(via_steps, 10L)
  expect_identical(via_spax, via_steps)
  # an evaluation solves once
  expect_identical(count(spax(case$data, spec_clm(decay_gaussian(8), outside = 25))), 1L)
})

test_that("fitted() reads the fit, and names an evaluation by facility", {
  case <- .run_case()
  x <- .run_totals_fit()
  expect_identical(fitted(x), fitted(x$fit))
  expect_identical(names(fitted(x)), colnames(case$distance))
  expect_identical(unname(fitted(x)), x$fit$best$outputs$utilization)
  flows <- .run_flow_fit()$x
  expect_identical(fitted(flows), fitted(flows$fit))
  e <- spax(case$data, spec_clm(decay_gaussian(8), outside = 25))
  expect_identical(names(fitted(e)), colnames(case$distance))
  expect_identical(unname(fitted(e)), e$evaluation$outputs$utilization)
  expect_identical(fitted(e), stats::setNames(
    evaluate_allocation(clm_allocation(case$data, v0 = 25), c(sigma = 8))$outputs$utilization,
    e$model$substrate$facility_ids))
  # the facilities of the data, in the order of the data
  expect_identical(names(fitted(e)), e$model$substrate$facility_ids)
  # the evaluation tables
  f <- as.data.frame(e, type = "facilities")
  expect_identical(f$utilization, unname(fitted(e)))
  expect_identical(names(f), c("facility_id", "supply", "attractiveness", "utilization"))
  p <- as.data.frame(e)
  expect_identical(p$status, c("given", "held", "held", "held"))
  expect_identical(p$parameter, c("sigma", "v0", "beta", "kappa"))
  expect_identical(p$value, c(8, 25, 1, 1))
  expect_identical(nrow(as.data.frame(e, type = "flows")), 18L * 24L)
  expect_identical(nrow(as.data.frame(e, type = "origins")), 18L)
  expect_identical(.run_error(as.data.frame(e, type = "starts")),
                   "no fit was made: the model was evaluated at given values, so there are no starts")
})

test_that("predict() equals predict_allocation() on the saved fit or evaluation", {
  case <- .run_case()
  x <- .run_totals_fit()
  request <- case$distance[c(3, 1, 7), ]
  p <- predict(x, request)
  ref <- predict_allocation(x$fit, distance = request, outputs = c("allocation", "outside_share"))
  expect_identical(p, ref)
  expect_s3_class(p, "ae_clm_prediction")
  expect_identical(p$origin_ids, c("origin3", "origin1", "origin7"))
  expect_identical(dim(p$outputs$allocation), c(3L, 24L))
  # demand, outputs and further arguments pass through
  d <- c(10, 20, 30)
  p2 <- predict(x, request, demand = d, outputs = c("flow", "rho"))
  expect_identical(p2, predict_allocation(x$fit, distance = request, demand = d,
                                          outputs = c("flow", "rho")))
  expect_equal(rowSums(p2$outputs$flow), d * p2$outputs$rho, tolerance = 1e-12)
  unnamed <- request
  rownames(unnamed) <- NULL
  p3 <- predict(x, unnamed, outputs = "rho", origin_ids = c("a", "b", "c"))
  expect_identical(p3$origin_ids, c("a", "b", "c"))
  # an evaluation
  e <- spax(case$data, spec_clm(decay_gaussian(8), outside = 25))
  pe <- predict(e, request)
  expect_identical(pe, predict_allocation(e$evaluation, distance = request,
                                          outputs = c("allocation", "outside_share")))
  expect_equal(pe$outputs$allocation, p$outputs$allocation, tolerance = 0.2)
  # the prediction of the fit equals the prediction from the step workflow's fit
  expect_identical(predict(x, request, outputs = "rho")$outputs$rho,
                   predict_allocation(.run_steps(case$data, spec_clm(), obs_totals(case$y),
                                                 case$starts, case$lower, case$upper),
                                      request, outputs = "rho")$outputs$rho)
  # the S3 method conforms to stats::predict(object, ...)
  expect_identical(names(formals(stats::predict)), c("object", "..."))
  expect_identical(names(formals(predict.spax_allocation))[1:2], c("object", "travel"))
})

test_that("print() shows the specification, the data, the parameters and the fit", {
  case <- .run_case()
  # an evaluation, with exact lines
  e <- spax(case$data, spec_huff(decay_power(1.5), beta = 0.8))
  expect_identical(capture.output(print(e)), c(
    "<spax allocation> Huff model (no outside option)",
    "  decay       power (sigma = exponent): 1.5",
    "  attraction  (kappa * supply)^beta: kappa = 1, beta = 0.8",
    "  outside     none",
    "  data        18 origins x 24 facilities",
    "",
    "  parameter  value  status",
    "  sigma      1.5    given",
    "  v0         0      held",
    "  beta       0.8    held",
    "  kappa      1      held"))
  shown <- NULL
  capture.output(shown <- withVisible(print(e)))
  expect_false(shown$visible)
  expect_identical(shown$value, e)
  # a fit: the lines follow the parameters and the search
  x <- spax(case$data, spec_clm(), observed = obs_totals(case$y))
  est <- vapply(unname(coef(x)), format, character(1), digits = 4, trim = TRUE)
  lines <- capture.output(print(x))
  expect_identical(lines[1:5], c(
    "<spax allocation> conditional logit with an outside option",
    "  decay       gaussian (sigma = travel scale): fit",
    "  attraction  (kappa * supply)^beta: kappa = 1, beta = 1",
    "  outside     v0: fit",
    "  data        18 origins x 24 facilities"))
  width <- max(nchar(c("value", est, "1")))
  row <- function(name, value, status) sprintf("  %-9s  %-*s  %s", name, width, value, status)
  expect_identical(lines[6:11], c("", row("parameter", "value", "status"),
                                  row("sigma", est[1L], "fitted"),
                                  row("v0", est[2L], "fitted"),
                                  row("beta", "1", "held"), row("kappa", "1", "held")))
  expect_identical(lines[12], "")
  n <- nrow(x$fit$table)
  expect_identical(lines[13:15], c(
    sprintf("  selected    %s (start %d of %d); %d of %d starts converged", x$fit$best_start,
            match(x$fit$best_start, x$fit$table$start), n, sum(x$fit$table$eligible), n),
    "  boundary    none",
    sprintf("  starts      default search, %d of %d grid points, loss poisson",
            x$fit$default_search$n_finite, x$fit$default_search$n_points)))
  expect_length(lines, 15L)
  # given starts: no default-search line, and a boundary is named
  g <- spax(case$data, spec_clm(), observed = obs_totals(case$y), starts = case$starts,
            lower = case$lower, upper = case$upper)
  glines <- capture.output(print(g))
  expect_identical(glines[length(glines) - 1L:0L], c(
    sprintf("  selected    %s (start %d of 2); %d of 2 starts converged", g$fit$best_start,
            match(g$fit$best_start, c("short", "wide")), sum(g$fit$table$eligible)),
    "  boundary    none"))
  b <- spax(case$data, spec_clm(), observed = obs_totals(case$y),
            starts = rbind(a = c(sigma = 9, v0 = 20), b = c(sigma = 12, v0 = 50)),
            lower = c(sigma = 7.9, v0 = 1), upper = c(sigma = 40, v0 = 200))
  blines <- capture.output(print(b))
  expect_identical(blines[length(blines)], "  boundary    sigma at a search bound")
  # a grouped model prints its groups
  gp <- spax(.run_grouped(), spec_clm(list(young = decay_gaussian(5), mid = decay_gaussian(6),
                                           old = decay_power(2)), outside = 0.5))
  expect_identical(capture.output(print(gp))[1:5], c(
    "<spax allocation> conditional logit with an outside option",
    "  decay       by group: young gaussian (5), mid gaussian (6), old power (2)",
    "  attraction  (kappa * supply)^beta: kappa = 1, beta = 1",
    "  outside     v0: 0.5",
    "  data        16 origins x 3 facilities"))
})

test_that("print() says when the outside weight is on its lower bound", {
  case <- .run_case()
  said <- paste0("v0 at its lower bound: the outside option adds almost nothing ",
                 "(compare spec_huff())")
  boundary_line <- function(x) {
    lines <- capture.output(print(x))
    lines[grepl("^  boundary", lines)]
  }
  # the default search on counts that need no outside option: v0 ends on its floor
  huff <- clm_oracle(case$distance, case$demand, case$supply, 8, v0 = 0)
  y <- colSums(huff$flow) * 1.01
  y[1L] <- NA
  x <- spax(case$data, spec_clm(), observed = obs_totals(y))
  expect_true(x$fit$boundary[x$fit$best_start, "v0"])
  expect_identical(boundary_line(x), paste0("  boundary    ", said))
  # given bounds: v0 on the lower bound, on the upper bound, and both with sigma
  starts <- rbind(a = c(sigma = 9, v0 = 40), b = c(sigma = 12, v0 = 60))
  lower <- spax(case$data, spec_clm(), observed = obs_totals(case$y), starts = starts,
                lower = c(sigma = 2, v0 = 30), upper = c(sigma = 40, v0 = 200))
  expect_identical(boundary_line(lower), paste0("  boundary    ", said))
  upper <- spax(case$data, spec_clm(), observed = obs_totals(case$y),
                starts = rbind(a = c(sigma = 9, v0 = 10), b = c(sigma = 12, v0 = 15)),
                lower = c(sigma = 2, v0 = 1), upper = c(sigma = 40, v0 = 20))
  expect_identical(boundary_line(upper), "  boundary    v0 at a search bound")
  # a group-specific v0 names the groups; other parameters on a bound come first
  fit <- structure(list(
    best = list(), best_start = "scan1", table = data.frame(start = "scan1", eligible = TRUE),
    boundary = matrix(c(FALSE, TRUE, TRUE, FALSE), 1, 4,
      dimnames = list("scan1", c("sigma", "v0.a", "v0.b", "beta"))),
    theta = matrix(c(5, 1e-3, 40, 1), 1, 4, dimnames = list("scan1", c("sigma", "v0.a", "v0.b", "beta"))),
    bounds = list(lower = c(sigma = 1, v0.a = 1e-3, v0.b = 1e-6, beta = 0.05),
                  upper = c(sigma = 10, v0.a = 100, v0.b = 40, beta = 5))),
    class = "ae_multistart_fit")
  lines <- .result_fit_lines(list(fit = fit))
  expect_identical(lines[2L], paste0("  boundary    v0.b at a search bound; v0.a at its lower bound: ",
    "the outside option adds almost nothing (compare spec_huff())"))
})

test_that("summary() returns one class and prints the specification, the parameters and the fit", {
  case <- .run_case()
  x <- .run_totals_fit()
  s <- summary(x)
  expect_s3_class(s, "summary.spax_allocation", exact = TRUE)
  expect_identical(names(s), c("spec", "parameters", "fit", "mode"))
  expect_identical(s$spec, format(x$spec))
  expect_identical(s$parameters, as.data.frame(x, type = "parameters"))
  expect_identical(s$mode, "fit")
  expect_s3_class(s$fit, "summary.ae_multistart_fit")
  expect_identical(s$fit$starts, x$fit$table)
  # print: the spec block, the parameters table, then the fit summary
  lines <- capture.output(print(s))
  p <- as.data.frame(x)
  expect_identical(lines[1:4], c(
    "<spax allocation summary> conditional logit with an outside option",
    "  decay       gaussian (sigma = travel scale): fit",
    "  attraction  (kappa * supply)^beta: kappa = 1, beta = 1",
    "  outside     v0: fit"))
  expect_identical(lines[5:6], c("", "  parameter  value  status"))
  expect_identical(lines[11:12], c("", "<summary.ae_multistart_fit>"))
  expect_identical(lines[11:length(lines)], c("", capture.output(print(s$fit))))
  expect_identical(capture.output(print(s))[7:8], .result_parameter_lines(p)[2:3])
  expect_identical(capture.output(invisible(print(s))), lines)
  # an evaluation
  e <- spax(case$data, spec_huff(decay_power(1.5), beta = 0.8))
  se <- summary(e)
  expect_s3_class(se, "summary.spax_allocation", exact = TRUE)
  expect_identical(se$mode, "evaluate")
  expect_null(se$fit)
  expect_identical(capture.output(print(se)), c(
    "<spax allocation summary> Huff model (no outside option)",
    "  decay       power (sigma = exponent): 1.5",
    "  attraction  (kappa * supply)^beta: kappa = 1, beta = 0.8",
    "  outside     none",
    "",
    "  parameter  value  status",
    "  sigma      1.5    given",
    "  v0         0      held",
    "  beta       0.8    held",
    "  kappa      1      held",
    "",
    "evaluated at given values"))
})

test_that("the result holds its pieces and the methods are registered", {
  x <- .run_totals_fit()
  expect_identical(names(x), c("model", "spec", "observed", "fit", "evaluation", "theta",
                               "held", "mode", "call"))
  expect_s3_class(x$model, "ae_problem")
  expect_s3_class(x$fit, "ae_multistart_fit")
  expect_identical(names(x$observed), c("spec", "target", "output"))
  expect_s3_class(x$observed$spec, "spax_obs")
  expect_true(is.call(x$call))
  expect_identical(as.character(x$call[[1L]]), "spax")
  expect_true(inherits(x, "spax_result"))
  e <- spax(.run_case()$data, spec_huff(decay_power(1.5), beta = 0.8))
  expect_identical(names(e), names(x))
  expect_null(e$observed)
  expect_null(e$fit)
  expect_s3_class(e$evaluation, "ae_equilibrium")
  expect_identical(e$mode, "evaluate")
  for (generic in c("print", "summary", "coef", "fitted", "as.data.frame", "predict")) {
    expect_true(is.function(getS3method(generic, "spax_allocation")), label = generic)
  }
  expect_true(is.function(getS3method("print", "summary.spax_allocation")))
  # the formals of the generics
  expect_identical(names(formals(as.data.frame.spax_allocation)),
                   c("x", "row.names", "optional", "type", "..."))
  expect_identical(eval(formals(as.data.frame.spax_allocation)$type),
                   c("parameters", "facilities", "origins", "flows", "starts"))
  expect_identical(names(formals(spax)), c("data", "model", "observed", "starts", "lower",
                                           "upper", "..."))
})
