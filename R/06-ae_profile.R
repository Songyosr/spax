# Private profile fitting -----------------------------------------------------

#' Fix one problem parameter without rebuilding the prepared substrate
#'
#' The returned problem exposes only the nuisance-parameter contract. Its bind
#' closure reconstructs the full theta in the original order and delegates to
#' the original prepared problem. Analytic state-map parameter Jacobians, when
#' present, are reduced to the nuisance columns; output sensitivities continue
#' to use the standard problem machinery.
#' @keywords internal
.fix_problem_parameter <- function(problem, parameter, value) {
  .chck_class(problem, "ae_problem", "problem")
  expected <- problem$theta$names
  if (!is.character(parameter) || length(parameter) != 1L || is.na(parameter) ||
      !parameter %in% expected) {
    stop("`parameter` must name one parameter in the problem theta contract")
  }
  .chck_positive_scalar(value, "value")
  if (value < problem$theta$lower[match(parameter, expected)] ||
      value > problem$theta$upper[match(parameter, expected)]) {
    stop("`value` is outside the problem theta bounds")
  }
  nuisance <- setdiff(expected, parameter)
  if (!length(nuisance)) {
    stop("profile fitting requires at least one nuisance parameter")
  }
  nuisance_index <- match(nuisance, expected)
  contract <- list(
    names = nuisance,
    lower = problem$theta$lower[nuisance_index],
    upper = problem$theta$upper[nuisance_index]
  )
  bind <- function(theta) {
    theta <- .coerce_problem_theta(theta, contract)
    full <- stats::setNames(numeric(length(expected)), expected)
    full[[parameter]] <- value
    full[nuisance] <- theta
    step <- .bind_theta(problem, full)
    if (is.function(step$jac_param)) {
      original_jac_param <- step$jac_param
      step$jac_param <- function(x) {
        jac <- original_jac_param(x)
        if (is.list(jac)) {
          if (is.null(jac$jac_param)) {
            stop("analytic parameter Jacobian list must include `jac_param`")
          }
          jac$jac_param <- jac$jac_param[, nuisance_index, drop = FALSE]
          return(jac)
        }
        jac[, nuisance_index, drop = FALSE]
      }
    }
    step$theta <- theta
    step
  }
  metadata <- problem$metadata
  metadata$profile <- list(parameter = parameter, value = as.numeric(value),
                           nuisance = nuisance)
  .new_problem(problem$model, problem$substrate, problem$state, contract, bind,
               metadata = metadata)
}

#' Profile one parameter of a prepared AE problem
#'
#' Each supplied positive `values` point fixes `parameter` and optimizes every
#' remaining nuisance parameter through `.fit_problem_nfxp()`. The prepared
#' substrate is shared. Each point owns fresh solver/reuse state; `warm_start`
#' carries only the preceding eligible nuisance estimate into the next outer
#' optimization. Supplied order is retained.
#'
#' `init`, `lower`, and `upper` name the complete original theta. Named `...`
#' arguments are ordinary `.fit_problem_nfxp()` controls. Failed, nonconverged,
#' invalid and boundary points remain visible. The lowest eligible loss wins;
#' ties retain the first supplied point. Rich fits are retained only for the
#' winner unless `keep_fits = TRUE`.
#'
#' This returns likelihood or loss profile values only. A confidence interval
#' additionally requires a justified loss scale, threshold and degrees of
#' freedom; none is inferred here. Fixing a nuisance value to satisfy an
#' external moment or total is a different estimator and is not this helper.
#' @keywords internal
.profile_problem <- function(problem, observed, parameter, values,
                             init, lower, upper, ...,
                             warm_start = TRUE, keep_fits = FALSE) {
  .chck_class(problem, "ae_problem", "problem")
  expected <- problem$theta$names
  if (!is.character(parameter) || length(parameter) != 1L || is.na(parameter) ||
      !parameter %in% expected) {
    stop("`parameter` must name one parameter in the problem theta contract")
  }
  nuisance <- setdiff(expected, parameter)
  if (!length(nuisance)) {
    stop("profile fitting requires at least one nuisance parameter")
  }
  if (!is.numeric(values) || !length(values) || any(!is.finite(values)) ||
      any(values <= 0) || anyDuplicated(values)) {
    stop("`values` must contain distinct finite positive profile points")
  }
  value_names <- names(values)
  values <- stats::setNames(as.numeric(values), value_names)
  init <- .coerce_fit_theta(init, expected, "init")
  lower <- .coerce_fit_theta(lower, expected, "lower")
  upper <- .coerce_fit_theta(upper, expected, "upper")
  .coerce_problem_theta(init, problem$theta)
  .coerce_problem_theta(lower, problem$theta)
  .coerce_problem_theta(upper, problem$theta)
  if (any(init <= 0 | lower <= 0 | upper <= 0)) {
    stop("`init`, `lower`, and `upper` must be positive")
  }
  if (any(lower >= upper)) stop("`lower` must be less than `upper`")
  if (any(init < lower | init > upper)) {
    stop("`init` must be inside [`lower`, `upper`]")
  }
  if (any(values < lower[[parameter]] | values > upper[[parameter]])) {
    stop("every profile point must be inside [`lower`, `upper`]")
  }
  for (x in list(warm_start, keep_fits)) {
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
      stop("`warm_start` and `keep_fits` must be TRUE or FALSE")
    }
  }
  args <- list(...)
  allowed <- setdiff(names(formals(.fit_problem_nfxp)),
                     c("problem", "observed", "init", "lower", "upper"))
  if (length(args) && (is.null(names(args)) || anyNA(names(args)) ||
      any(!nzchar(names(args))) || anyDuplicated(names(args)) ||
      any(!names(args) %in% allowed))) {
    stop("`...` must contain unique named fitting controls, excluding init and bounds")
  }
  target <- .fit_target_meta(observed, "observed")
  n <- length(values)
  ids <- names(values)
  if (is.null(ids)) ids <- paste0("point", seq_len(n))
  if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
    stop("profile point names must be nonempty and unique")
  }
  theta <- matrix(NA_real_, n, length(expected), dimnames = list(ids, expected))
  boundary <- matrix(FALSE, n, length(expected), dimnames = list(ids, expected))
  warnings <- errors <- stats::setNames(vector("list", n), ids)
  fits <- if (keep_fits) stats::setNames(vector("list", n), ids) else NULL
  table <- data.frame(point = ids, value = values, loss = NA_real_, eligible = FALSE,
    optim_convergence = NA_integer_, solver_converged = FALSE,
    residual = NA_real_, seconds = NA_real_, boundary = FALSE,
    status = "error", optim_message = NA_character_, solver_message = NA_character_,
    stringsAsFactors = FALSE)
  best <- NULL
  best_index <- NA_integer_
  next_init <- init[nuisance]
  edge_tol <- 1e-6 + 1e-3 * (log(upper) - log(lower))
  started <- proc.time()[["elapsed"]]
  for (i in seq_len(n)) {
    subproblem <- .fix_problem_parameter(problem, parameter, values[i])
    messages <- character()
    error <- NULL
    start_time <- proc.time()[["elapsed"]]
    fit <- tryCatch(withCallingHandlers(
      do.call(.fit_problem_nfxp, c(list(problem = subproblem, observed = observed,
        init = next_init, lower = lower[nuisance], upper = upper[nuisance]), args)),
      warning = function(w) {
        messages <<- c(messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }), error = function(e) {
        error <<- conditionMessage(e)
        NULL
      })
    table$seconds[i] <- proc.time()[["elapsed"]] - start_time
    warnings[[i]] <- messages
    errors[i] <- list(error)
    if (is.null(fit)) next
    full <- init
    full[[parameter]] <- values[i]
    full[nuisance] <- fit$theta[nuisance]
    fit$theta_hat <- fit$theta <- full
    fit$profile <- list(parameter = parameter, value = values[i], nuisance = nuisance)
    theta[i, ] <- full
    table$loss[i] <- fit$loss
    table$optim_convergence[i] <- fit$convergence
    table$solver_converged[i] <- isTRUE(fit$equilibrium$converged)
    table$residual[i] <- fit$equilibrium$residual_norm
    if (!is.null(fit$message)) table$optim_message[i] <- fit$message
    if (!is.null(fit$equilibrium$message)) table$solver_message[i] <- fit$equilibrium$message
    finite <- all(is.finite(full)) && all(full > 0) && is.finite(fit$loss) &&
      is.finite(table$residual[i]) &&
      all(is.finite(as.numeric(fit$predicted)[target$mask]))
    table$eligible[i] <- finite && isTRUE(fit$convergence == 0L) &&
      table$solver_converged[i]
    table$status[i] <- if (!isTRUE(fit$convergence == 0L) ||
      !table$solver_converged[i]) "not_converged" else
      if (!finite) "invalid_result" else "eligible"
    boundary[i, ] <- log(full) <= log(lower) + edge_tol |
      log(full) >= log(upper) - edge_tol
    table$boundary[i] <- any(boundary[i, ])
    if (keep_fits) fits[i] <- list(fit)
    if (table$eligible[i] && (is.null(best) || fit$loss < best$loss)) {
      best <- fit
      best_index <- i
    }
    if (warm_start && table$eligible[i]) next_init <- full[nuisance]
    fit <- NULL
  }
  elapsed <- proc.time()[["elapsed"]] - started
  table <- cbind(table, as.data.frame(theta, check.names = FALSE))
  structure(list(best = best,
    best_point = if (is.null(best)) NULL else ids[best_index],
    status = if (is.null(best)) "no_valid_fit" else "selected",
    parameter = parameter, values = stats::setNames(values, ids), nuisance = nuisance,
    table = table, theta = theta, boundary = boundary, warnings = warnings,
    errors = errors, fits = fits, seconds = unname(elapsed), warm_start = warm_start,
    bounds = list(lower = lower, upper = upper)), class = "ae_profile_fit")
}
