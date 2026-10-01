# Conditional facility-case bootstrap for the experimental CLM workflow ------

.allocation_bootstrap_settings <- function() {
  list(schema = "spax_pcase_v1", method = "Poisson facility-case bootstrap",
    control = list(maxit = 300, factr = 10, pgtol = 1e-8),
    score_tol = 1e-3, rank_tol = 1e-4, fd_eps = 1e-6,
    boundary_tol = c(absolute = 1e-6, log_span = 1e-3),
    projected_bound_tol = 1e-7, minimum_successful = 380L,
    probabilities = c(.025, .975), quantile_type = 7L,
    fixed_inputs = "demand, supply, travel, included network and support policy",
    capacity_identity = paste("regional_A is supported raw supply per retained need;",
      "positive supply with no finite edge on retained demand support is excluded",
      "from attributed capacity and disclosed separately"),
    evidence_scope = paste("Conditional percentile intervals; coverage evidence is",
      "limited to the SPAX-048 fixed 80-origin/40-facility generated design.",
      "Facility independence and the empirical observation law are assumptions.",
      "No simultaneous-map or causal coverage claim."))
}

.allocation_case_values <- function(predicted, observed, weights, eps) {
  if (!is.numeric(predicted) || !is.null(dim(predicted)) ||
      !is.numeric(observed) || !is.null(dim(observed)) ||
      !is.numeric(weights) || !is.null(dim(weights)) ||
      length(predicted) != length(observed) || length(weights) != length(observed) ||
      any(!is.finite(predicted) | predicted < 0) ||
      any(!is.finite(observed) | observed < 0) ||
      any(!is.finite(weights) | weights < 0)) stop("invalid case-weighted count values")
  .chck_positive_scalar(eps, "eps")
  use <- weights > 0
  if (any(use & predicted == 0 & observed > 0)) stop("positive count at zero model mean")
  if (any(use & predicted > 0 & predicted <= eps)) stop("likelihood-floor contamination")
  list(mu = as.numeric(predicted), y = as.numeric(observed), w = as.numeric(weights),
       positive = use & predicted > 0)
}

.allocation_case_loss <- function(predicted, observed, weights, eps = 1e-9) {
  z <- .allocation_case_values(predicted, observed, weights, eps)
  k <- z$positive
  sum(z$w[k] * (z$mu[k] - z$y[k] * log(z$mu[k])))
}

.allocation_case_gradient <- function(predicted, observed, sensitivity, weights, eps = 1e-9) {
  z <- .allocation_case_values(predicted, observed, weights, eps)
  if (!is.matrix(sensitivity) || !is.numeric(sensitivity) ||
      nrow(sensitivity) != length(z$mu) || any(!is.finite(sensitivity))) {
    stop("case sensitivity must have one finite row per facility")
  }
  k <- z$positive
  as.numeric(crossprod(sensitivity[k, , drop = FALSE],
    z$w[k] * (1 - z$y[k] / z$mu[k])))
}

.allocation_bootstrap_model <- function(model) {
  .check_allocation_model(model)
  spec <- model$metadata$spec
  if (!identical(spec$family, "gaussian") || !isTRUE(spec$fit_v0) ||
      isTRUE(spec$fit_beta) || !identical(model$theta$names, c("sigma", "v0")) ||
      spec$kappa != 1 || spec$beta != 1) {
    stop("bootstrap requires Gaussian static CLM with fitted sigma/v0 and kappa = beta = 1")
  }
  s <- model$substrate
  .chck_class(s, "ae_prepared_inputs", "model inputs")
  if (!length(s$D_active) || any(!is.finite(s$D_active) | s$D_active <= 0) ||
      !is.matrix(s$distance_active) || anyNA(s$distance_active) ||
      any(s$distance_active < 0) ||
      !identical(dim(s$distance_active), c(length(s$D_active), length(s$facility_ids))) ||
      !is.matrix(s$S) || ncol(s$S) != 1L || nrow(s$S) != length(s$facility_ids) ||
      any(!is.finite(s$S) | s$S < 0)) stop("invalid fixed prepared bootstrap inputs")
  # Reconstruct the built-in provider from the declared inputs. A manually
  # replaced/captured closure cannot override the supported model equations.
  model$bind <- .compile_huff_map(spec, s)$bind
  model
}

.allocation_bootstrap_observation <- function(observation) {
  expected <- c("event", "period", "sampling")
  if (!is.list(observation) || is.null(names(observation)) ||
      anyDuplicated(names(observation)) || !setequal(names(observation), expected) ||
      !all(vapply(observation, function(x) is.character(x) && length(x) == 1L &&
        !is.na(x) && nzchar(trimws(x)), logical(1))) ||
      !identical(observation$sampling, "independent_facilities")) {
    stop("observation must declare event, period and sampling = 'independent_facilities'")
  }
  observation[expected]
}

.allocation_bootstrap_align <- function(x, ids, name) {
  if (!is.numeric(x) || !is.null(dim(x)) || length(x) != length(ids)) {
    stop(name, " must have one value per facility")
  }
  x <- x[.allocation_order(names(x), ids, name)]
  stats::setNames(as.numeric(x), ids)
}

.allocation_bootstrap_diagnostic <- function(model, theta, observed, weights, lower, upper,
                                            eps = 1e-9) {
  ids <- model$substrate$facility_ids
  y <- .allocation_bootstrap_align(observed, ids, "observed")
  w <- .allocation_bootstrap_align(weights, ids, "weights")
  if (any(!is.finite(y) | y < 0) || any(!is.finite(w) | w < 0) || sum(w) <= 0) {
    stop("invalid diagnostic counts or multiplicities")
  }
  theta <- .coerce_problem_theta(theta, model$theta)
  lower <- .coerce_fit_theta(lower, names(theta), "lower")
  upper <- .coerce_fit_theta(upper, names(theta), "upper")
  eta <- log(theta); lo <- log(lower); hi <- log(upper)
  boundary <- any(pmin(eta - lo, hi - eta) <= 1e-6 + 1e-3 * (hi - lo))
  base <- .solve_problem(model, theta, keep_history = FALSE, warn = FALSE,
    diagnostics = FALSE, requested_outputs = "utilization")
  mu <- stats::setNames(as.numeric(base$outputs$utilization), ids)
  J <- .problem_output_sensitivity(model, theta, base$x_star,
    output = "utilization", fd_eps = 1e-6)
  J <- sweep(J, 2, theta, "*")
  colnames(J) <- names(theta); rownames(J) <- ids
  result <- list(mean = mu, jacobian_log = J, kkt_score = NA_real_, rank = NA_integer_,
    boundary = boundary, singular_ratio = NA_real_, status = "invalid_numeric_result")
  if (!isTRUE(base$converged) || any(!is.finite(mu) | mu < 0) ||
      any(!is.finite(J))) return(result)
  if (any(mu == 0 & y > 0)) {
    result$status <- "incompatible_zero_mean"
    return(result)
  }
  possible <- as.numeric(model$substrate$S) > 0 &
    colSums(is.finite(model$substrate$distance_active)) > 0
  if (any(mu > 0 & mu <= eps) || any(mu == 0 & possible) ||
      any(J[mu == 0, , drop = FALSE] != 0)) {
    result$status <- "nonregular_mean_or_loss_floor"
    return(result)
  }
  positive <- mu > 0
  JW <- J[positive, , drop = FALSE] * sqrt(w[positive] / mu[positive])
  singular <- if (nrow(JW)) svd(JW, nu = 0, nv = 0)$d else numeric()
  largest <- if (length(singular)) max(singular) else 0
  result$rank <- if (largest > 0) sum(singular > largest * 1e-4) else 0L
  result$singular_ratio <- if (length(singular) == 2L && largest > 0)
    min(singular) / largest else 0
  gradient <- as.numeric(crossprod(J[positive, , drop = FALSE],
    w[positive] * (1 - y[positive] / mu[positive])))
  projected <- gradient
  projected[eta <= lo + 1e-7 & gradient > 0] <- 0
  projected[eta >= hi - 1e-7 & gradient < 0] <- 0
  scale <- sqrt(colSums(JW^2))
  result$kkt_score <- sqrt(sum((projected / pmax(scale, .Machine$double.eps))^2))
  result$status <- if (is.finite(result$kkt_score) && result$kkt_score <= 1e-3)
    "eligible" else "score_failed"
  result
}

.allocation_case_fit <- function(model, observed, starts, lower, upper, weights, eps = 1e-9) {
  ids <- model$substrate$facility_ids
  observed <- .allocation_bootstrap_align(observed, ids, "observed")
  weights <- .allocation_bootstrap_align(weights, ids, "weights")
  search <- .fit_problem_multistart(model, observed, starts, lower, upper,
    output = "utilization", loss = .allocation_case_loss,
    loss_grad = .allocation_case_gradient, loss_args = list(weights = as.numeric(weights), eps = eps),
    gradient = TRUE, control = .allocation_bootstrap_settings()$control)
  table <- search$table
  table$sigma <- search$theta[, "sigma"]; table$v0 <- search$theta[, "v0"]
  table$score_norm <- NA_real_; table$rank <- NA_integer_
  table$warnings <- vapply(search$warnings, paste, character(1), collapse = " | ")
  table$errors <- vapply(search$errors, function(x) if (is.null(x)) "" else x, character(1))
  for (i in seq_len(nrow(table))) {
    if (!table$eligible[i]) next
    d <- tryCatch(.allocation_bootstrap_diagnostic(model, search$theta[i, ], observed,
      weights, lower, upper, eps), error = function(e) e)
    if (inherits(d, "error")) {
      table$eligible[i] <- FALSE; table$status[i] <- "diagnostic_failed"
      table$errors[i] <- conditionMessage(d)
    } else {
      table$score_norm[i] <- d$kkt_score; table$rank[i] <- d$rank
      table$boundary[i] <- d$boundary
      table$eligible[i] <- identical(d$status, "eligible")
      table$status[i] <- d$status
    }
  }
  eligible <- which(table$eligible)
  if (!length(eligible)) return(list(success = FALSE, status = "fit_failed",
    theta = c(sigma = NA_real_, v0 = NA_real_), loss = NA_real_, best_start = NA_character_,
    boundary = NA, score_norm = NA_real_, rank = NA_integer_, table = table))
  best <- eligible[which.min(table$loss[eligible])]
  list(success = TRUE, status = "selected", theta = search$theta[best, ],
    loss = table$loss[best], best_start = table$start[best], boundary = table$boundary[best],
    score_norm = table$score_norm[best], rank = table$rank[best], table = table)
}

.allocation_bootstrap_quantities <- function(model, theta, scenario = NULL) {
  base <- .solve_problem(model, theta, keep_history = FALSE, warn = FALSE,
    diagnostics = FALSE, requested_outputs = "utilization")
  if (!isTRUE(base$converged)) stop("baseline evaluation did not converge")
  contacts <- sum(base$outputs$utilization)
  need <- sum(model$substrate$D_active)
  values <- c(sigma = unname(theta["sigma"]), v0 = unname(theta["v0"]),
    regional_rho = contacts / need)
  if (!is.null(scenario)) {
    changed <- .solve_problem(scenario, theta, keep_history = FALSE, warn = FALSE,
      diagnostics = FALSE, requested_outputs = "utilization")
    if (!isTRUE(changed$converged)) stop("scenario evaluation did not converge")
    values <- c(values, delta_contacts = sum(changed$outputs$utilization) - contacts)
  }
  supported <- colSums(is.finite(model$substrate$distance_active)) > 0
  c(values, regional_A = sum(model$substrate$S[supported, , drop = FALSE]) / need)
}

.allocation_bootstrap_intervals <- function(original, point, draws) {
  attempted <- nrow(draws)
  do.call(rbind, lapply(names(point), function(q) {
    use <- if (attempted) draws$success & is.finite(draws[[q]]) else logical()
    successful <- sum(use)
    status <- if (!identical(original$status, "eligible")) original$status else
      if (attempted < 399L) "incomplete_bootstrap" else
      if (successful < 380L) "insufficient_bootstrap_success" else "conditional_pointwise_bootstrap"
    if (q == "regional_A" && is.finite(point[q])) status <- "fixed_input_identity"
    endpoints <- if (status == "conditional_pointwise_bootstrap")
      stats::quantile(draws[[q]][use], c(.025, .975), type = 7, names = FALSE) else c(NA_real_, NA_real_)
    data.frame(quantity = q, estimate = unname(point[q]), lower = endpoints[1], upper = endpoints[2],
      status = status, successful = successful, attempted = attempted, planned = 399L,
      stringsAsFactors = FALSE)
  }))
}

.allocation_bootstrap_same <- function(a, b) {
  identical(dim(a), dim(b)) && identical(is.na(as.numeric(a)), is.na(as.numeric(b))) &&
    all(abs(as.numeric(a) - as.numeric(b)) <= 1e-8 + 1e-8 * abs(as.numeric(b)), na.rm = TRUE)
}

.allocation_bootstrap_prepare <- function(model, fit, observation, scenario) {
  model <- .allocation_bootstrap_model(model)
  observation <- .allocation_bootstrap_observation(observation)
  .chck_class(fit, "ae_multistart_fit", "fit")
  if (!is.null(scenario)) {
    scenario <- .allocation_bootstrap_model(scenario)
    a <- model$substrate; b <- scenario$substrate
    a$S <- b$S <- NULL
    if (!identical(a, b) || !identical(model$metadata$spec, scenario$metadata$spec)) {
      stop("scenario may change supply only; all other prepared inputs and settings must agree")
    }
  }
  settings <- .allocation_bootstrap_settings()
  ids <- model$substrate$facility_ids
  point <- stats::setNames(rep(NA_real_, 3L + !is.null(scenario)),
    c("sigma", "v0", "regional_rho", if (!is.null(scenario)) "delta_contacts"))
  supported <- colSums(is.finite(model$substrate$distance_active)) > 0
  point <- c(point, regional_A = sum(model$substrate$S[supported, , drop = FALSE]) /
    sum(model$substrate$D_active))
  original <- list(status = "original_fit_failed", theta = c(sigma = NA_real_, v0 = NA_real_),
    score_norm = NA_real_, rank = NA_integer_, boundary = NA)
  if (is.null(fit$best)) return(list(model = model, scenario = scenario, observation = observation,
    original = original, point = point, observed = NULL, eps = 1e-9, settings = settings))
  selected <- .selected_allocation_fit(fit)
  arguments_ok <- is.list(selected$loss_args) && (length(selected$loss_args) == 0L ||
    (length(selected$loss_args) == 1L && identical(names(selected$loss_args), "eps") &&
      is.numeric(selected$loss_args$eps) && length(selected$loss_args$eps) == 1L &&
      is.finite(selected$loss_args$eps) && selected$loss_args$eps > 0))
  if (!identical(selected$output, "utilization") ||
      !identical(selected$loss_fn, .poisson_loss) || !arguments_ok) {
    stop("bootstrap requires an unweighted Poisson facility-utilization fit")
  }
  target <- .bind_problem_target(model, selected$observed, "utilization")
  if (!all(target$mask) || any(!is.finite(target$values) | target$values < 0 |
      abs(target$values - round(target$values)) > 1e-8)) {
    stop("bootstrap requires complete nonnegative whole facility counts")
  }
  if (!identical(.fit_observation_identity(selected), target$identity[c("axes", "ids")])) {
    stop("saved fit observation IDs conflict with the supplied model")
  }
  eps <- if (length(selected$loss_args)) selected$loss_args$eps else 1e-9
  theta <- .coerce_problem_theta(selected$theta, model$theta)
  lower <- .coerce_fit_theta(fit$bounds$lower, names(theta), "lower")
  upper <- .coerce_fit_theta(fit$bounds$upper, names(theta), "upper")
  if (any(lower <= 0 | upper <= lower | theta < lower | theta > upper)) stop("invalid saved fit bounds")
  if (!is.matrix(fit$starts) || !is.numeric(fit$starts) || !nrow(fit$starts) ||
      !identical(colnames(fit$starts), names(theta)) || any(!is.finite(fit$starts)) ||
      any(sweep(fit$starts, 2, lower, "<")) || any(sweep(fit$starts, 2, upper, ">"))) {
    stop("invalid saved explicit starts")
  }
  base <- .solve_problem(model, theta, keep_history = FALSE, warn = FALSE,
    diagnostics = FALSE, requested_outputs = "utilization")
  source <- .clm_prediction_source(selected)
  expected_loss <- do.call(.poisson_loss,
    c(list(predicted = as.numeric(base$outputs$utilization), observed = target$values), selected$loss_args))
  if (!identical(base$prediction_snapshot, source) ||
      !.allocation_bootstrap_same(base$outputs$utilization, selected$predicted) ||
      !identical(model$substrate$D_active, selected$coverage_meta$demand) ||
      !identical(as.numeric(base$outputs$allocation), as.numeric(selected$outputs$allocation)) ||
      !is.finite(selected$loss) || abs(expected_loss - selected$loss) > 1e-8 + 1e-10 * abs(expected_loss)) {
    stop("saved fit conflicts with the supplied fixed model; refit the current model")
  }
  observed <- stats::setNames(target$values, ids)
  original$theta <- theta
  point <- .allocation_bootstrap_quantities(model, theta, scenario)
  if (isTRUE(selected$convergence == 0L) && isTRUE(selected$equilibrium$converged)) {
    d <- .allocation_bootstrap_diagnostic(model, theta, observed, rep(1, length(ids)), lower, upper, eps)
    original$score_norm <- d$kkt_score; original$rank <- d$rank; original$boundary <- d$boundary
    original$status <- if (d$status == "incompatible_zero_mean" ||
      d$status == "nonregular_mean_or_loss_floor") d$status else
      if (!identical(d$status, "eligible")) "original_score_failed" else
      if (d$rank < 2L) "original_unidentified" else
      if (d$boundary) "original_mean_boundary" else "eligible"
  }
  list(model = model, scenario = scenario, observation = observation,
    original = original, point = point, observed = observed, eps = eps, settings = settings,
    starts = fit$starts, lower = lower, upper = upper)
}

.allocation_bootstrap_empty_draws <- function(quantities) {
  x <- data.frame(attempt = integer(), seed = integer(), status = character(), success = logical(),
    sigma = numeric(), v0 = numeric(), boundary = logical(), best_start = character(),
    loss = numeric(), score_norm = numeric(), stringsAsFactors = FALSE)
  for (q in setdiff(quantities, c("sigma", "v0"))) x[[q]] <- numeric()
  x
}

.allocation_bootstrap_save <- function(value, path) {
  temporary <- tempfile(pattern = ".spax-pcase-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(value, temporary, version = 3)
  if (!file.rename(temporary, path)) stop("could not atomically replace bootstrap checkpoint")
  invisible(NULL)
}

.allocation_bootstrap_integrity <- function(value) {
  path <- tempfile(pattern = "spax-pcase-integrity-")
  on.exit(unlink(path), add = TRUE)
  writeBin(serialize(value, NULL, version = 3), path)
  unname(tools::md5sum(path))
}

.allocation_bootstrap_failed_table <- function(starts, message) {
  data.frame(start = rownames(starts), loss = NA_real_, eligible = FALSE,
    optim_convergence = NA_integer_, solver_converged = FALSE, residual = NA_real_,
    seconds = NA_real_, boundary = NA, status = "error", optim_message = NA_character_,
    solver_message = NA_character_, loss_gap = NA_real_, log_parameter_gap = NA_real_,
    sigma = NA_real_, v0 = NA_real_, score_norm = NA_real_, rank = NA_integer_,
    warnings = "", errors = message, stringsAsFactors = FALSE)
}

.allocation_bootstrap_record <- function(context, fit, weights, attempt, seed) {
  values <- stats::setNames(rep(NA_real_, length(context$point)), names(context$point))
  status <- fit$status; success <- isTRUE(fit$success)
  if (success) {
    q <- tryCatch(.allocation_bootstrap_quantities(context$model, fit$theta, context$scenario),
      error = function(e) e)
    if (inherits(q, "error")) {
      success <- FALSE; status <- paste0("evaluation_failed: ", conditionMessage(q))
    } else values <- q
  }
  draw <- data.frame(attempt = attempt, seed = seed, status = status, success = success,
    sigma = unname(fit$theta["sigma"]), v0 = unname(fit$theta["v0"]),
    boundary = fit$boundary, best_start = fit$best_start, loss = fit$loss,
    score_norm = fit$score_norm, stringsAsFactors = FALSE)
  for (q in setdiff(names(values), c("sigma", "v0"))) draw[[q]] <- unname(values[q])
  table <- fit$table; table$attempt <- rep(attempt, nrow(table))
  list(draw = draw, starts = table, multiplicities = weights)
}

.allocation_bootstrap_result <- function(context, records, seed, attempts, reason = NULL) {
  draws <- if (length(records)) do.call(rbind, lapply(records, `[[`, "draw")) else
    .allocation_bootstrap_empty_draws(names(context$point))
  starts <- if (length(records)) do.call(rbind, lapply(records, `[[`, "starts")) else data.frame()
  multiplicities <- if (length(records)) do.call(rbind, lapply(records, `[[`, "multiplicities")) else
    matrix(integer(), nrow = 0L, ncol = length(context$model$substrate$facility_ids),
      dimnames = list(NULL, context$model$substrate$facility_ids))
  intervals <- .allocation_bootstrap_intervals(context$original, context$point, draws)
  status <- if (!identical(context$original$status, "eligible")) "refused" else
    if (!is.null(reason) || nrow(draws) < 399L) "incomplete" else
    if (any(intervals$status == "insufficient_bootstrap_success")) "insufficient_success" else "complete"
  reasons <- if (status == "refused") context$original$status else
    if (status == "insufficient_success") "insufficient_bootstrap_success" else
    if (status == "incomplete" && is.null(reason)) "fewer than 399 completed attempts" else reason
  if (status == "incomplete") {
    variable <- intervals$status != "fixed_input_identity"
    intervals$status[variable] <- "incomplete_bootstrap"
    intervals$lower[variable] <- intervals$upper[variable] <- NA_real_
  }
  s <- context$model$substrate
  supported <- colSums(is.finite(s$distance_active)) > 0
  unsupported <- !supported & as.numeric(s$S) > 0
  structure(list(status = status, reasons = reasons, original = context$original,
    intervals = intervals, draws = draws, starts = starts, multiplicities = multiplicities,
    settings = context$settings, observation = context$observation, seed = seed,
    planned = 399L, attempted = nrow(draws), successful = sum(draws$success),
    input_metadata = context$model$substrate$input_metadata,
    facility_ids = s$facility_ids,
    capacity_support = list(supported_facility_ids = s$facility_ids[supported],
      unsupported_facility_ids = s$facility_ids[unsupported],
      unsupported_supply = stats::setNames(as.numeric(s$S)[unsupported], s$facility_ids[unsupported]),
      total_supply = sum(s$S), attributed_supply = sum(s$S[supported, , drop = FALSE]))),
    class = "ae_allocation_bootstrap")
}

.run_allocation_bootstrap <- function(model, fit, observation, scenario = NULL, seed,
                                      checkpoint = NULL, attempts = 399L) {
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) || seed != floor(seed) ||
      seed < 0 || seed > .Machine$integer.max - 399L) stop("seed must be an explicit integer between 0 and integer.max - 399")
  if (!is.numeric(attempts) || length(attempts) != 1L || !is.finite(attempts) ||
      attempts != floor(attempts) || attempts < 1L || attempts > 399L) stop("invalid internal attempt limit")
  seed <- as.integer(seed); attempts <- as.integer(attempts)
  if (!is.null(checkpoint) && (!is.character(checkpoint) || length(checkpoint) != 1L ||
      is.na(checkpoint) || !nzchar(checkpoint) || !dir.exists(dirname(checkpoint)))) {
    stop("checkpoint must name an RDS file in an existing directory")
  }
  context <- .allocation_bootstrap_prepare(model, fit, observation, scenario)
  if (!identical(context$original$status, "eligible")) {
    return(.allocation_bootstrap_result(context, list(), seed, attempts))
  }
  contract <- list(settings = context$settings, model = context$model$substrate,
    spec = context$model$metadata$spec, scenario = if (is.null(context$scenario)) NULL else context$scenario$substrate,
    observed = context$observed, starts = context$starts, lower = context$lower, upper = context$upper,
    original = context$original, point = context$point, eps = context$eps,
    observation = context$observation, seed = seed, attempts = attempts,
    rng = c("Mersenne-Twister", "Inversion", "Rejection"), R_version = as.character(getRversion()))
  records <- list()
  if (!is.null(checkpoint) && file.exists(checkpoint)) {
    saved <- readRDS(checkpoint)
    if (!is.list(saved) || !identical(names(saved), c("contract", "records", "integrity")) ||
        !identical(saved$contract, contract) || !is.list(saved$records) ||
        length(saved$records) > attempts ||
        !identical(saved$integrity, .allocation_bootstrap_integrity(saved[c("contract", "records")]))) {
      stop("bootstrap checkpoint does not match inputs, settings, seed, version or integrity")
    }
    records <- saved$records
  }
  # Generate only small observation multiplicities, with a separate predetermined
  # seed for every attempt. Saving/restoring RNG includes its kind and absence.
  old_kind <- RNGkind(); had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv) else
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  ids <- context$model$substrate$facility_ids
  weights_at <- function(b) {
    set.seed(seed + b)
    stats::setNames(tabulate(sample.int(length(ids), length(ids), replace = TRUE), nbins = length(ids)), ids)
  }
  if (length(records)) for (b in seq_along(records)) {
    record <- records[[b]]
    if (!is.list(record) || !identical(names(record), c("draw", "starts", "multiplicities")) ||
        !is.data.frame(record$draw) || nrow(record$draw) != 1L ||
        !identical(record$draw$attempt, as.integer(b)) || !identical(record$draw$seed, seed + b) ||
        !identical(record$multiplicities, weights_at(b)) || !is.data.frame(record$starts) ||
        !identical(record$starts$start, rownames(context$starts)) ||
        nrow(record$starts) != nrow(context$starts) ||
        !is.logical(record$draw$success) || is.na(record$draw$success) ||
        !all(record$starts$attempt == b)) stop("invalid completed bootstrap checkpoint record")
  }
  reason <- NULL
  if (length(records) < attempts) {
    for (b in seq.int(length(records) + 1L, attempts)) {
      weights <- weights_at(b)
      outcome <- tryCatch(.allocation_case_fit(context$model, context$observed, context$starts,
        context$lower, context$upper, weights, context$eps),
        interrupt = function(e) e, error = function(e) e)
      if (inherits(outcome, "interrupt")) {
        reason <- "interrupted before attempt completed"; break
      }
      if (inherits(outcome, "error")) outcome <- list(success = FALSE,
        status = paste0("fit_error: ", conditionMessage(outcome)), theta = c(sigma = NA_real_, v0 = NA_real_),
        loss = NA_real_, best_start = NA_character_, boundary = NA, score_norm = NA_real_,
        table = .allocation_bootstrap_failed_table(context$starts, conditionMessage(outcome)))
      record <- .allocation_bootstrap_record(context, outcome, weights, as.integer(b), seed + b)
      records[[b]] <- record
      if (!is.null(checkpoint)) {
        saved <- tryCatch({
          payload <- list(contract = contract, records = records)
          payload$integrity <- .allocation_bootstrap_integrity(payload)
          .allocation_bootstrap_save(payload, checkpoint)
        },
          error = function(e) e, interrupt = function(e) e)
        if (inherits(saved, "condition")) {
          reason <- paste0("checkpoint failed: ", conditionMessage(saved)); break
        }
      }
    }
  }
  .allocation_bootstrap_result(context, records, seed, attempts, reason)
}

#' Conditional facility-case bootstrap for a fitted Gaussian allocation
#'
#' This experimental operation runs separately from fitting. It resamples facility
#' observation multiplicities, retaining every physical facility and its supply
#' in the allocation network. Need, travel, capacity and included-network inputs
#' remain fixed. Facility independence is a caller-declared working assumption.
#'
#' Exactly 399 attempts use the original explicit starts and positive bounds,
#' with fixed numerical controls. Failed attempts are retained without replacement.
#' Successful inner boundary estimates remain in the distribution. Original mean
#' boundaries, local rank deficiency or insufficient fit precision refuse intervals;
#' percentile intervals require a complete run of 399 attempts and at least
#' 380 finite successful values per quantity. Incomplete results retain their
#' point estimates and diagnostics but withhold intervals.
#' Coverage validation in generated 80-origin/40-facility designs under tested
#' fixed-input laws is not a universal guarantee of exact 95 percent coverage
#' or validation of an application's observation law.
#'
#' @param model A checked Gaussian static CLM with fitted sigma and v0,
#'   kappa=beta=1, and known travel on its retained demand support.
#' @param fit A matching multistart fit of complete whole facility counts using
#'   the unweighted built-in Poisson objective. Fractional or partial observations,
#'   flow targets, other kernels and custom objectives are outside this method.
#' @param observation Named list declaring nonblank `event`, `period` (including
#'   an explicit unresolved reference date), and `sampling="independent_facilities"`.
#'   Non-overlapping registration alone does not establish independence.
#' @param scenario Optional checked CLM whose prepared system changes supply only.
#'   Every draw evaluates baseline and scenario together at the same parameters.
#' @param seed Required nonnegative integer; attempt b uses seed+b. The caller's
#'   RNG state and RNG kind are restored, including when interrupted.
#' @param checkpoint Optional RDS file in an existing directory. Completed attempts
#'   are saved atomically and resumed after exact input/settings/version checks.
#'   Completed failures are never retried; incomplete runs retain actual counts.
#' @return An `ae_allocation_bootstrap` with compact draws, multiplicities, all
#'   start diagnostics, assumptions, failure reasons and conditional percentile
#'   intervals for sigma, v0, regional contact and optional paired change in modeled
#'   contacts. Regional attributed capacity per need is a fixed-input identity
#'   without an interval: it includes supply with at least one known finite edge
#'   on retained demand support. Unsupported positive supply is disclosed in
#'   `capacity_support`; total supplied capacity can exceed attributed capacity.
#'   Scientific refusals are inspectable results; invalid inputs error.
#' @family experimental allocation workflow
#' @export
bootstrap_allocation <- function(model, fit, observation, scenario = NULL, seed, checkpoint = NULL) {
  .run_allocation_bootstrap(model, fit, observation, scenario, seed, checkpoint, attempts = 399L)
}
