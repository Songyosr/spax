# Private static-CLM observation and first-order uncertainty ------------------
# These helpers have an explicit, bounded statistical contract. The information
# range is a local tangent diagnostic, not a proof of global identification.
# Sampling covariance is conditional on fixed prepared inputs. No public API,
# automatic sampling-law selection or confidence guarantee is implied.

.allocation_observation <- function(model, observed, output = c("utilization", "flow"),
                                    event, exposure_units, period, exposure,
                                    sampling = c("independent_poisson", "independent_groups"),
                                    groups = NULL) {
  .check_allocation_model(model)
  output <- match.arg(output)
  sampling <- match.arg(sampling)
  exposure <- match.arg(exposure, c("poisson_mean", "working_count_mean"))
  declaration <- list(event = event, exposure_units = exposure_units, period = period)
  if (!all(vapply(declaration, function(x) is.character(x) && length(x) == 1L &&
      !is.na(x) && nzchar(trimws(x)), logical(1)))) {
    stop("event, exposure_units and period must be explicit nonblank strings")
  }
  declaration$exposure <- exposure
  declaration$mean_assumption <- "correct marginal mean conditional on fixed inputs and observation selection"
  target <- .bind_problem_target(model, observed, output)
  y <- target$values
  if (any(y < 0 | abs(y - round(y)) > 1e-8)) {
    stop("this observation contract requires nonnegative whole counts")
  }
  ids <- model$substrate$facility_ids
  if (sampling == "independent_groups") {
    if (!is.character(groups) || !is.null(dim(groups)) || is.null(names(groups)) ||
        anyNA(groups) || any(!nzchar(trimws(groups))) || anyNA(names(groups)) ||
        anyDuplicated(names(groups)) || !setequal(names(groups), ids)) {
      stop("groups must explicitly name every facility ID with a nonblank group")
    }
    groups <- groups[match(ids, names(groups))]
    names(groups) <- ids
    labels <- if (output == "flow") rep(groups, each = length(model$substrate$D_active)) else groups
  } else {
    if (!is.null(groups)) stop("groups require independent_groups sampling")
    labels <- NULL
  }
  structure(list(target = target, output = output, sampling = sampling,
    declaration = declaration, groups = groups, labels = labels,
    input_contract = list(spec = model$metadata$spec,
      demand = model$substrate$D_active, supply = model$substrate$S,
      facility_ids = ids, origin_ids = model$substrate$origin_ids,
      metadata = model$substrate$input_metadata, policy = model$substrate$input_policy)),
    class = "allocation_observation")
}

# Only portable O(J) prediction/load values survive the sensitivity evaluations.
# Callbacks request locations through predict_allocation(), rather than retaining
# an origin-facility allocation/flow matrix for every parameter perturbation.
.allocation_compact_solution <- function(x) {
  structure(list(converged = x$converged, prediction_snapshot = x$prediction_snapshot,
    evaluated_clm = x$evaluated_clm, utilization = x$utilization,
    outputs = list(utilization = x$outputs$utilization)), class = "ae_equilibrium")
}

.allocation_information_range <- function(jacobian, mean, rank_tol) {
  weighted <- jacobian / sqrt(mean)
  information <- crossprod(weighted)
  if (any(!is.finite(information))) stop("nonfinite observation information")
  eig <- eigen(information, symmetric = TRUE)
  largest <- max(eig$values, 0)
  keep <- eig$values > largest * rank_tol^2 & eig$values > 0
  vectors <- eig$vectors[, keep, drop = FALSE]
  inverse <- if (any(keep)) sweep(vectors, 2, 1 / eig$values[keep], "*") %*% t(vectors) else
    matrix(0, ncol(jacobian), ncol(jacobian))
  dimnames(information) <- dimnames(inverse) <- list(colnames(jacobian), colnames(jacobian))
  list(information = information, inverse = inverse, rank = sum(keep),
    eigenvalues = pmax(eig$values, 0), range = vectors,
    null = eig$vectors[, !keep, drop = FALSE],
    singular_ratio = if (largest > 0) sqrt(max(min(eig$values), 0) / largest) else 0)
}

.allocation_inference <- function(model, fit, observation, fd_eps = 1e-5,
                                  rank_tol = 1e-4, min_groups = 20L,
                                  group_adjustment = c("HC3", "none")) {
  .check_allocation_model(model)
  .chck_class(fit, "ae_multistart_fit", "fit")
  .chck_class(observation, "allocation_observation", "observation")
  group_adjustment <- match.arg(group_adjustment)
  for (z in list(fd_eps, rank_tol)) {
    if (!is.numeric(z) || length(z) != 1L || !is.finite(z) || z <= 0 || z >= 1) {
      stop("fd_eps and rank_tol must be finite scalars strictly between zero and one")
    }
  }
  if (!is.numeric(min_groups) || length(min_groups) != 1L || !is.finite(min_groups) ||
      min_groups < 2 || min_groups > .Machine$integer.max || min_groups != floor(min_groups)) stop("min_groups must be an integer >= 2")
  selected <- .selected_allocation_fit(fit)
  source <- .clm_prediction_source(selected)
  outside <- if (source$spec$fit_v0) selected$theta[["v0"]] else source$spec$v0
  if (outside <= 0) stop("inference requires a positive outside option")
  if (!isTRUE(selected$convergence == 0L)) stop("inference requires a converged outer fit")
  expected <- .allocation_observation(model, observation$target$observed, observation$output,
    event = observation$declaration$event, exposure_units = observation$declaration$exposure_units,
    period = observation$declaration$period, exposure = observation$declaration$exposure,
    sampling = observation$sampling, groups = observation$groups)
  if (!identical(expected$input_contract, observation$input_contract) ||
      !identical(expected$declaration, observation$declaration) ||
      !identical(expected$target[c("observed", "values", "mask", "identity", "dim", "n_observed", "length")],
        observation$target[c("observed", "values", "mask", "identity", "dim", "n_observed", "length")]) ||
      !identical(expected$labels, observation$labels)) stop("observation contract conflicts with model or target")
  target <- expected$target
  loss_arguments_ok <- is.list(selected$loss_args) &&
    (length(selected$loss_args) == 0L ||
      (length(selected$loss_args) == 1L && identical(names(selected$loss_args), "eps") &&
        is.numeric(selected$loss_args$eps) && length(selected$loss_args$eps) == 1L &&
        is.finite(selected$loss_args$eps) && selected$loss_args$eps > 0))
  if (!identical(selected$output, observation$output) ||
      !identical(.fit_observation_identity(selected), target$identity[c("axes", "ids")]) ||
      !identical(as.numeric(selected$observed), as.numeric(target$observed)) ||
      !identical(dim(selected$observed), dim(target$observed)) ||
      !identical(selected$loss_fn, .poisson_loss) ||
      !loss_arguments_ok) {
    stop("inference requires the same canonical counts and unweighted Poisson fit")
  }
  theta <- selected$theta
  parameters <- model$theta$names
  if (!identical(names(theta), parameters) || any(theta <= 0)) stop("inference requires positive log parameters")
  # Rebind the declared substrate, rather than trusting a closure that could
  # still capture an earlier substrate after unsupported manual model mutation.
  model$bind <- .compile_huff_map(model$metadata$spec, model$substrate)$bind
  evaluate <- function(z) .solve_problem(model, z, keep_history = FALSE,
    warn = FALSE, check = FALSE, diagnostics = FALSE, requested_outputs = observation$output)
  base <- evaluate(theta)
  same <- function(a, b) identical(dim(a), dim(b)) &&
    identical(is.na(as.numeric(a)), is.na(as.numeric(b))) &&
    all(abs(as.numeric(a) - as.numeric(b)) <= 1e-8 + 1e-8 * abs(as.numeric(b)), na.rm = TRUE)
  if (!identical(base$prediction_snapshot, source) ||
      !identical(as.numeric(base$outputs$allocation), as.numeric(selected$outputs$allocation)) ||
      !same(base$utilization, source$utilization) ||
      !identical(model$substrate$D_active, selected$coverage_meta$demand) ||
      !identical(model$substrate$input_policy, source$input_policy)) {
    stop("fitted source conflicts with the prepared CLM; refit the current model")
  }
  mask <- target$mask
  mu <- as.numeric(base$outputs[[observation$output]])[mask]
  y <- target$values
  if (any(!is.finite(mu) | mu < 0)) stop("invalid fitted count means")
  if (any(mu == 0 & y > 0)) stop("positive observed count at zero model mean")
  expected_loss <- do.call(.poisson_loss, c(list(predicted = mu, observed = y), selected$loss_args))
  if (!same(base$outputs[[observation$output]], selected$predicted) ||
      !is.finite(selected$loss) || abs(expected_loss - selected$loss) > 1e-8 + 1e-10 * abs(expected_loss)) {
    stop("fitted counts or loss conflict with the current observation contract")
  }
  jacobian <- matrix(NA_real_, length(mu), length(theta), dimnames = list(NULL, parameters))
  perturbations <- vector("list", length(theta)); names(perturbations) <- parameters
  for (k in seq_along(theta)) {
    plus <- minus <- theta
    plus[k] <- theta[k] * exp(fd_eps); minus[k] <- theta[k] * exp(-fd_eps)
    a <- evaluate(plus); b <- evaluate(minus)
    jacobian[, k] <- (as.numeric(a$outputs[[observation$output]])[mask] -
      as.numeric(b$outputs[[observation$output]])[mask]) / (2 * fd_eps)
    perturbations[[k]] <- list(plus = .allocation_compact_solution(a),
      minus = .allocation_compact_solution(b), theta_plus = plus, theta_minus = minus)
  }
  if (any(!is.finite(jacobian))) stop("nonfinite log-parameter count sensitivity")
  positive <- mu > 0
  # In these kernels finite admissible travel and positive attraction imply
  # positive mathematical mass. Numeric underflow is not a structural absence.
  positive_attraction <- as.numeric(model$substrate$S) > 0
  possible <- if (observation$output == "flow")
    (as.vector(is.finite(model$substrate$distance_active)) &
      rep(positive_attraction, each = length(model$substrate$D_active)))[mask] else
    (positive_attraction & colSums(is.finite(model$substrate$distance_active)) > 0)[mask]
  if (!any(positive)) stop("no positive-mean observation information")
  range <- .allocation_information_range(jacobian[positive, , drop = FALSE], mu[positive], rank_tol)
  if (range$rank == 0L) stop("no locally sensitive positive-mean count information")
  score_rows <- jacobian[positive, , drop = FALSE] * ((y[positive] - mu[positive]) / mu[positive])
  score <- colSums(score_rows)
  covariance <- range$inverse
  degrees <- Inf; groups <- NULL; leverage <- NULL; max_leverage <- NULL
  meat_rank <- NA_integer_; reasons <- character()
  if (observation$sampling == "independent_groups") {
    informative <- rowSums(abs(jacobian[positive, , drop = FALSE])) > 0
    labels <- observation$labels[mask][positive][informative]
    groups <- rowsum(score_rows[informative, , drop = FALSE], labels, reorder = FALSE)
    g <- nrow(groups); degrees <- g - 1
    meat_rank <- qr(groups)$rank
    if (g < min_groups) reasons <- c(reasons, "few_independent_groups")
    covariance <- if (g > 1) range$inverse %*% crossprod(groups) %*% range$inverse * g / (g - 1) else
      matrix(NA_real_, length(theta), length(theta))
    # Q whitens the estimable information range. Group leverage is evaluated
    # in this small space; no observation-by-observation hat matrix is built.
    Q <- sweep(range$range, 2, sqrt(range$eigenvalues[seq_len(range$rank)]), "/")
    adjusted <- matrix(NA_real_, g, range$rank)
    leverage <- max_leverage <- setNames(numeric(g), rownames(groups))
    for (index in seq_len(g)) {
      label <- rownames(groups)[index]
      block <- jacobian[positive, , drop = FALSE][informative, , drop = FALSE][labels == label, , drop = FALSE] /
        sqrt(mu[positive][informative][labels == label])
      H <- crossprod(block %*% Q)
      leverage[index] <- sum(diag(H))
      max_leverage[index] <- max(eigen(H, symmetric = TRUE, only.values = TRUE)$values)
      if (group_adjustment == "HC3" && g >= min_groups) {
        if (max_leverage[index] >= 1 - 1e-8) {
          reasons <- unique(c(reasons, "dominant_group_leverage"))
        } else {
          adjusted[index, ] <- solve(diag(range$rank) - H,
            as.numeric(crossprod(Q, groups[index, ])))
        }
      }
    }
    if (group_adjustment == "HC3" && g >= min_groups) {
      # Weighted block HC3: its (G-1)/G meat normalization cancels the
      # conventional G/(G-1) factor. This is a local linearization, not an
      # exact nonlinear leave-group-out refit or a finite-sample theorem.
      covariance <- if (all(is.finite(adjusted))) Q %*% crossprod(adjusted) %*% t(Q) else
        matrix(NA_real_, length(theta), length(theta))
      meat_rank <- if (all(is.finite(adjusted))) qr(adjusted)$rank else NA_integer_
    }
  }
  boundary <- any(fit$boundary[match(fit$best_start, rownames(fit$boundary)), ])
  if (boundary) reasons <- c(reasons, "parameter_boundary")
  floor <- if ("eps" %in% names(selected$loss_args)) selected$loss_args$eps else 1e-9
  if (any(mu[positive] <= floor) || any(!positive & possible) ||
      any(jacobian[!positive, , drop = FALSE] != 0)) {
    reasons <- c(reasons, "nonregular_mean_or_loss_floor")
  }
  score_norm <- sqrt(max(as.numeric(crossprod(score, range$inverse %*% score)), 0))
  if (score_norm > 1e-3) reasons <- c(reasons, "insufficient_fit_precision")
  # The declared confirmation study failed grouped facility-total coverage.
  # Retain covariance/rank diagnostics, but do not turn this unvalidated design
  # or the uncorrected research comparator into regular reported intervals.
  reasons <- c(reasons, .allocation_interval_scope(observation, group_adjustment))
  dimnames(covariance) <- list(parameters, parameters)
  structure(list(theta = theta, parameters = parameters, observation = observation,
    covariance_log = covariance, information = range, jacobian_log = jacobian,
    fitted_mean = mu, base = .allocation_compact_solution(base), perturbations = perturbations, fd_eps = fd_eps,
    degrees_freedom = degrees, valid_regular_interval = !length(reasons), reasons = reasons,
    diagnostics = list(boundary = boundary, rank = range$rank,
      parameter_count = length(parameters), n_observed = length(mu), n_positive_mean = sum(positive),
      score_norm = score_norm, independent_groups = if (is.null(groups)) NA_integer_ else nrow(groups),
      group_leverage = leverage,
      group_max_leverage = max_leverage,
      group_adjustment = if (is.null(groups)) NA_character_ else group_adjustment,
      group_sizes = if (is.null(groups)) NULL else table(labels),
      raw_score_rank = if (is.null(groups)) NA_integer_ else qr(groups)$rank,
      meat_rank = meat_rank,
      mean_pearson = if (sum(positive) > range$rank)
        sum((y[positive] - mu[positive])^2 / mu[positive]) / (sum(positive) - range$rank) else NA_real_,
      scope = "local first-order information; fixed prepared inputs; pointwise intervals")),
    class = "allocation_inference")
}

.allocation_interval_scope <- function(observation, group_adjustment) {
  reasons <- character()
  if (observation$sampling == "independent_groups") {
    if (observation$output == "utilization") reasons <- c(reasons, "grouped_totals_not_validated")
    if (!identical(group_adjustment, "HC3")) reasons <- c(reasons, "uncorrected_group_comparator")
  }
  reasons
}

.allocation_delta <- function(inference, values, gradient_log,
                              transform = "identity", level = .95,
                              fixed_input = rep(FALSE, length(values)), support_tol = 1e-4) {
  .chck_class(inference, "allocation_inference", "inference")
  ids <- names(values)
  if (!is.numeric(values) || !is.null(dim(values)) || is.null(ids) || anyNA(ids) ||
      anyDuplicated(ids) || any(!nzchar(ids))) stop("values require unique named quantities")
  if (!is.matrix(gradient_log) || !is.numeric(gradient_log) ||
      nrow(gradient_log) != length(values) ||
      !identical(rownames(gradient_log), ids) ||
      !identical(colnames(gradient_log), inference$parameters)) stop("gradient_log must follow quantity and parameter order")
  if (!is.logical(fixed_input) || length(fixed_input) != length(values) || anyNA(fixed_input)) stop("invalid fixed_input declarations")
  if (length(transform) == 1L) transform <- rep(transform, length(values))
  if (!is.character(transform) || length(transform) != length(values) || anyNA(transform) ||
      any(!transform %in% c("identity", "log", "logit"))) stop("invalid quantity transforms")
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1) stop("invalid interval level")
  if (!is.numeric(support_tol) || length(support_tol) != 1L || !is.finite(support_tol) || support_tol <= 0 || support_tol >= 1) stop("invalid support tolerance")
  # Recheck the scope when reading a saved pre-restriction diagnostic object.
  scope <- .allocation_interval_scope(inference$observation, inference$diagnostics$group_adjustment)
  valid <- isTRUE(inference$valid_regular_interval) && !length(scope)
  reasons <- unique(c(inference$reasons, scope))
  critical <- if (!valid) NA_real_ else
    if (is.finite(inference$degrees_freedom))
      stats::qt((1 + level) / 2, inference$degrees_freedom) else stats::qnorm((1 + level) / 2)
  table <- data.frame(quantity = ids, estimate = as.numeric(values), standard_error = NA_real_,
    lower = NA_real_, upper = NA_real_, null_fraction = NA_real_,
    status = "invalid_quantity", transform = transform, stringsAsFactors = FALSE)
  for (i in seq_along(values)) {
    value <- values[i]; gradient <- gradient_log[i, ]
    if (!is.finite(value) || any(!is.finite(gradient))) next
    magnitude <- sqrt(sum(gradient^2))
    null <- inference$information$null
    fraction <- if (magnitude > 0 && ncol(null)) sqrt(sum((gradient %*% null)^2)) / magnitude else 0
    table$null_fraction[i] <- fraction
    if (fixed_input[i]) {
      if (magnitude > support_tol) stop("fixed-input identity has a nonzero parameter gradient")
      table$status[i] <- "fixed_input_identity"
      next
    }
    if (fraction > support_tol) { table$status[i] <- "unsupported_local_direction"; next }
    if (magnitude == 0) { table$status[i] <- "zero_first_order_gradient"; next }
    if (!valid) {
      table$status[i] <- paste(reasons, collapse = ";"); next
    }
    if ((transform[i] == "log" && value <= 0) ||
        (transform[i] == "logit" && (value <= 0 || value >= 1))) {
      table$status[i] <- "transform_boundary"; next
    }
    variance <- as.numeric(crossprod(gradient, inference$covariance_log %*% gradient))
    if (!is.finite(variance) || variance <= 0) { table$status[i] <- "invalid_sampling_variance"; next }
    scale <- switch(transform[i], identity = 1, log = 1 / value, logit = 1 / (value * (1 - value)))
    center <- switch(transform[i], identity = value, log = log(value), logit = stats::qlogis(value))
    endpoints <- center + c(-1, 1) * critical * sqrt(variance) * scale
    endpoints <- switch(transform[i], identity = endpoints, log = exp(endpoints), logit = stats::plogis(endpoints))
    if (any(!is.finite(endpoints))) { table$status[i] <- "nonfinite_interval"; next }
    table$standard_error[i] <- sqrt(variance)
    table$lower[i] <- endpoints[1]; table$upper[i] <- endpoints[2]
    table$status[i] <- "local_pointwise_interval"
  }
  table
}

.allocation_estimands <- function(inference, evaluate, transform = "identity",
                                  fixed_input = NULL, level = .95, support_tol = 1e-4) {
  .chck_class(inference, "allocation_inference", "inference")
  if (!is.function(evaluate)) stop("evaluate must be a fixed-input quantity callback")
  values <- evaluate(inference$theta, inference$base)
  if (!is.numeric(values) || !is.null(dim(values)) || is.null(names(values)) ||
      anyNA(names(values)) || anyDuplicated(names(values)) || any(!nzchar(names(values)))) {
    stop("quantity callback requires a numeric vector with unique named quantities")
  }
  gradient <- matrix(NA_real_, length(values), length(inference$theta),
    dimnames = list(names(values), inference$parameters))
  for (k in seq_along(inference$parameters)) {
    z <- inference$perturbations[[k]]
    a <- evaluate(z$theta_plus, z$plus); b <- evaluate(z$theta_minus, z$minus)
    if (!is.numeric(a) || !is.numeric(b) || !identical(names(a), names(values)) ||
        !identical(names(b), names(values)) || length(a) != length(values) ||
        length(b) != length(values)) stop("quantity callback changed its output identity")
    gradient[, k] <- (a - b) / (2 * inference$fd_eps)
  }
  if (is.null(fixed_input)) fixed_input <- rep(FALSE, length(values))
  list(table = .allocation_delta(inference, values, gradient, transform, level,
    fixed_input, support_tol), gradient_log = gradient)
}
