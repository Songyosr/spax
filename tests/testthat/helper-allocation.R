# Shared fixtures for the allocation workflow tests: test-06-ae_prediction.R,
# test-06-ae_preparation.R, test-06-ae_workflow.R, test-06-allocation_clm.R,
# test-06-legacy_providers.R, test-07-allocation_*.R and
# test-90-allocation_portability.R. Expected values come from clm_oracle()
# (helper-clm.R) rather than from package internals.

# Memoised fixtures ------------------------------------------------------------
# Expensive fits are computed once per test run and shared across blocks and
# files. Every read checks that the shared value is byte-identical to the value
# first stored, so a block that modifies it (for example through an enclosed
# environment) makes the next reader fail instead of leaking state.
.alloc_memo_store <- new.env(parent = emptyenv())

alloc_memo <- function(key, code) {
  entry <- .alloc_memo_store[[key]]
  if (is.null(entry)) {
    value <- code
    entry <- list(value = value, bytes = serialize(value, NULL))
    assign(key, entry, envir = .alloc_memo_store)
  } else if (!identical(serialize(entry$value, NULL), entry$bytes)) {
    stop("shared allocation fixture '", key, "' was modified after it was stored")
  }
  entry$value
}

# Small public-workflow case ---------------------------------------------------
# Five origins (with known zero and unknown demand) and three facilities in
# non-alphabetical order. Counts are exact CLM expectations at `truth`.
alloc_small_case <- function() {
  demand <- c(o1 = 40, o2 = 0, o3 = 55, o4 = NA, o5 = 35)
  supply <- c(f2 = 18, f1 = 25, f3 = 15)
  travel <- matrix(c(1, 5, 3, 4, 6, 6, 2, 2, 3, 4, 3, 4, 6, 1, 2), 5, 3,
                   dimnames = list(names(demand), names(supply)))
  inputs <- prepare_allocation(demand, supply, travel, missing_demand = "exclude",
    metadata = list(demand_units = "requests", demand_period = "period1",
      supply_units = "slots", supply_period = "period1", travel_units = "minutes"))
  truth <- c(sigma = 3, v0 = 10)
  active <- which(is.finite(demand) & demand > 0)
  oracle <- clm_oracle(travel[active, , drop = FALSE], demand[active], supply,
                       truth[["sigma"]], truth[["v0"]])
  list(inputs = inputs, model = clm_allocation(inputs, fit_v0 = TRUE),
       demand = demand, supply = supply, travel = travel, truth = truth,
       active = active, probability = oracle$probability, flow = oracle$flow,
       observed = colSums(oracle$flow),
       starts = rbind(near = c(sigma = 3.2, v0 = 9), far = c(sigma = 5, v0 = 20)),
       lower = c(sigma = 1, v0 = 1), upper = c(sigma = 8, v0 = 40))
}

# Poisson multistart fit of the small case; the target is supplied in reverse
# facility order to exercise named alignment.
alloc_small_fit <- function() {
  alloc_memo("small_fit", {
    f <- alloc_small_case()
    fit_allocation(f$model, rev(f$observed), f$starts, f$lower, f$upper,
      loss = "poisson", gradient = TRUE, control = list(maxit = 100))
  })
}

# A real search in which every start fails, so no fit is selected.
alloc_failed_search <- function(case = alloc_small_case(), observed = case$observed) {
  fit_allocation(case$model, observed, case$starts, case$lower, case$upper,
    loss = function(predicted, observed) stop("injected loss failure"))
}

# Static-Huff grid -------------------------------------------------------------
# The 4 x 4 demand grid and three facilities of the original static-Huff tests,
# as checked numeric inputs with origins in raster cell order.
alloc_huff_case <- function() {
  demand <- stats::setNames(c(10, 20, 30, 40, 15, 25, 35, 45,
                              12, 22, 32, 42, 18, 28, 38, 48), paste0("o", 1:16))
  distance <- cbind(facility1 = c(1, 2, 3, 4, 2, 3, 4, 5, 3, 4, 5, 6, 4, 5, 6, 7),
                    facility2 = c(7, 6, 5, 4, 6, 5, 4, 3, 5, 4, 3, 2, 4, 3, 2, 1),
                    facility3 = c(4, 3, 2, 3, 3, 2, 3, 4, 2, 3, 4, 5, 3, 4, 5, 6))
  rownames(distance) <- names(demand)
  supply <- c(facility1 = 30, facility2 = 45, facility3 = 20)
  list(demand = demand, supply = supply, distance = distance,
       inputs = prepare_allocation(demand, supply, distance))
}

# Prediction case ----------------------------------------------------------------
# Three origins and facilities with nondefault kappa/beta. With `fitted`, v0
# and beta are parameters rather than constants.
alloc_prediction_case <- function(family = "gaussian", fitted = FALSE,
                                  supply = c(b = 6, a = 4, c = 2), v0 = 4) {
  demand <- c(o3 = 3, o1 = 5, o7 = 7)
  distance <- matrix(c(1, 3, 2, 4, 1, 3, 2, 4, 1), 3, 3,
                     dimnames = list(names(demand), names(supply)))
  inputs <- prepare_allocation(demand, supply, distance, metadata = list(
    demand_units = "eligible events", demand_period = "one period",
    supply_units = "service slots", supply_period = "one period", travel_units = "minutes"))
  model <- clm_allocation(inputs, family = family, kappa = .65, beta = 1.3,
                          v0 = v0, fit_v0 = fitted, fit_beta = fitted)
  theta <- c(sigma = switch(family, gaussian = 2, exponential = .3, power = 1.8),
             if (fitted) c(v0 = 3.5, beta = 1.15))
  list(model = model, theta = theta, distance = distance, demand = demand,
       supply = supply, family = family, kappa = .65,
       beta = if (fitted) 1.15 else 1.3, v0 = if (fitted) 3.5 else v0)
}

# Flow-calibrated fit started at the generating parameters.
alloc_prediction_fit <- function(case) {
  observed <- alloc_prediction_oracle(case, demand = case$demand)$flow
  .fit_problem_nfxp(case$model, observed, init = case$theta,
    lower = case$theta / 4, upper = case$theta * 4, output = "flow",
    loss = .poisson_loss, loss_grad = .poisson_gradient, loss_args = list(),
    gradient = TRUE, control = list(maxit = 20))
}

# The 12 saved sources: three kernels, fixed or fitted v0/beta, and a solved
# evaluation or a fit of each.
alloc_prediction_sources <- function() {
  alloc_memo("prediction_sources", {
    out <- list()
    for (family in c("gaussian", "exponential", "power")) for (fitted in c(FALSE, TRUE)) {
      case <- alloc_prediction_case(family, fitted)
      out[[paste(family, if (fitted) "fitted" else "fixed")]] <- list(case = case,
        solved = .solve_problem(case$model, case$theta, requested_outputs = "flow"),
        fit = alloc_prediction_fit(case))
    }
    out
  })
}

# Requested-location algebra: probabilities from the requested travel, capacity
# ratios from the loads of the saved system (by default the case's own system).
alloc_request_oracle <- function(request, system_distance, system_demand, supply,
                                 sigma, v0 = 0, family = "gaussian", beta = 1,
                                 kappa = 1, demand = NULL, loads = NULL) {
  choice <- function(distance, demand) clm_oracle(distance, demand, supply, sigma,
    v0 = v0, family = family, beta = beta, kappa = kappa)
  if (is.null(loads)) loads <- choice(system_distance, system_demand)$utilization
  requested <- choice(request, rep(0, nrow(request)))
  P <- requested$probability
  ratio <- ifelse(loads > 0, supply / loads, 0)
  A <- as.vector(P %*% ratio)
  list(allocation = P, flow = if (!is.null(demand)) P * as.numeric(demand),
       rho = unname(requested$rho), outside_share = unname(requested$outside_share), A = A,
       abar = unname(ifelse(requested$rho > 0, A / requested$rho, NA_real_)),
       utilization = loads)
}

alloc_prediction_oracle <- function(case, distance = case$distance, demand = NULL,
                                    loads = NULL) {
  alloc_request_oracle(distance, case$distance, case$demand, case$supply,
    case$theta[["sigma"]], v0 = case$v0, family = case$family, beta = case$beta,
    kappa = case$kappa, demand = demand, loads = loads)
}

# Generated bootstrap/inference design --------------------------------------------
# Truth sigma = 8, v0 = 25; whole counts are rounded exact CLM flows. With
# `colocated`, every facility sits at the same place, so totals identify only
# one local direction.
alloc_generated_case <- function(n = 18L, m = 24L, colocated = FALSE) {
  distance <- sqrt(outer(seq(2, 38, length.out = n), seq(3, 37, length.out = m), "-")^2 +
    outer(5 * sin(seq_len(n)), 4 * cos(seq_len(m)), "-")^2) + 1
  if (colocated) distance <- matrix(rep(seq(2, 38, length.out = n), m), n, m)
  dimnames(distance) <- list(paste0("origin", seq_len(n)), paste0("facility", seq_len(m)))
  demand <- stats::setNames(150 + 20 * sin(seq_len(n)), rownames(distance))
  supply <- stats::setNames(rep(c(5, 8, 6), length.out = m), colnames(distance))
  inputs <- prepare_allocation(demand, supply, distance, metadata = list(
    demand_units = "registered people", demand_period = "reference date",
    supply_units = "sessions", supply_period = "week", travel_units = "minutes"))
  truth <- c(sigma = 8, v0 = 25)
  flow <- round(clm_oracle(distance, demand, supply, truth[["sigma"]], truth[["v0"]])$flow)
  list(inputs = inputs, model = clm_allocation(inputs, fit_v0 = TRUE),
       demand = demand, supply = supply, distance = distance, truth = truth,
       flow = flow, y = colSums(flow),
       starts = rbind(short = c(sigma = 7, v0 = 20), wide = c(sigma = 12, v0 = 50)),
       lower = c(sigma = 2, v0 = 1), upper = c(sigma = 40, v0 = 200))
}

# Precise Poisson fit with the bootstrap's numerical controls. `model` and
# `lower` allow fits of modified systems; those are not memoised.
alloc_count_fit <- function(case, observed = case$y, output = "utilization",
                            model = case$model, starts = case$starts,
                            lower = case$lower) {
  fit_allocation(model, observed, starts, lower, case$upper, output = output,
    loss = "poisson", gradient = TRUE,
    control = list(maxit = 300, factr = 10, pgtol = 1e-8))
}

alloc_generated_fit <- function(colocated = FALSE, output = "utilization") {
  alloc_memo(paste("generated_fit", colocated, output), {
    case <- alloc_generated_case(colocated = colocated)
    alloc_count_fit(case, if (output == "flow") case$flow else case$y, output)
  })
}

alloc_bootstrap_observation <- function() {
  list(event = "registered people", period = "reference date unresolved",
       sampling = "independent_facilities")
}

# A facility-case refit result with the fields the bootstrap engine reads, for
# runs that should not optimise.
alloc_mock_case_fit <- function(case, theta = case$truth, success = TRUE,
                                boundary = FALSE) {
  parameters <- if (success) theta else stats::setNames(rep(NA_real_, 2), names(theta))
  table <- data.frame(start = rownames(case$starts), sigma = parameters[[1]],
    v0 = parameters[[2]], loss = if (success) c(1, 2) else NA_real_,
    success = success, eligible = success, boundary = boundary,
    convergence = if (success) 0L else NA_integer_,
    optim_convergence = if (success) 0L else NA_integer_,
    solver_converged = success, residual = if (success) 0 else NA_real_,
    score_norm = if (success) 0 else NA_real_, kkt_score = if (success) 0 else NA_real_,
    rank = if (success) 2L else NA_integer_,
    status = if (success) "eligible" else "fit_failed", warnings = "", errors = "")
  list(success = success, status = if (success) "eligible" else "fit_failed",
    theta = parameters, loss = if (success) 1 else NA_real_,
    best_start = if (success) rownames(case$starts)[1] else NA_character_,
    boundary = boundary, score_norm = if (success) 0 else NA_real_,
    rank = if (success) 2L else NA_integer_, table = table)
}

# A short incomplete run of the real engine on the shared fit, with mocked
# facility-case refits.
alloc_bootstrap_run <- function() {
  alloc_memo("bootstrap_run", {
    case <- alloc_generated_case()
    fit <- alloc_generated_fit()
    testthat::with_mocked_bindings(
      .run_allocation_bootstrap(case$model, fit, alloc_bootstrap_observation(),
        seed = 31L, attempts = 3L),
      .allocation_case_fit = function(...) alloc_mock_case_fit(case))
  })
}

# Saved bootstrap result -----------------------------------------------------------
# A hand-built saved result with one failed and one inner-boundary attempt, or
# a refused run without attempts.
saved_bootstrap_stub <- function(refused = FALSE) {
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
    reasons = if (refused) "original_unidentified" else character(),
    original = list(theta = c(sigma = 11, v0 = 22)),
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

# Fresh R process ----------------------------------------------------------------
# Saves `payload` as `x`, runs `code` (which must assign `result`) in a fresh
# Rscript that loads this package the same way as the current session (source
# via pkgload, or installed), and returns `result`.
run_in_fresh_r <- function(payload, code) {
  dir <- withr::local_tempdir()
  paths <- file.path(dir, c("input.rds", "output.rds", "script.R"))
  saveRDS(payload, paths[1])
  package_path <- getNamespaceInfo(asNamespace("spax"), "path")
  mode <- if (pkgload::is_dev_package("spax")) "source" else "installed"
  writeLines(c("a <- commandArgs(TRUE)",
    "if (a[4] == 'source') pkgload::load_all(a[1], quiet = TRUE) else",
    "  library('spax', lib.loc = dirname(a[1]))",
    "x <- readRDS(a[2])", code, "saveRDS(result, a[3])"), paths[3])
  status <- system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(paths[3]), shQuote(package_path), shQuote(paths[1]),
      shQuote(paths[2]), mode), stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(status, "status"))) {
    stop("fresh R process failed:\n", paste(status, collapse = "\n"))
  }
  readRDS(paths[2])
}
