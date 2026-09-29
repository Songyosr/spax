#' Fit a prepared AE problem from explicit independent starting points
#'
#' `starts` is a finite positive numeric matrix, one row per start, with exactly
#' the problem's named parameter columns. Rows run serially in supplied order;
#' each owns fresh NFXP warm-state/reuse storage. The prepared problem is shared.
#' Inputs and callback/model behavior must remain fixed across starts for losses
#' to be comparable. Named `...` arguments pass unchanged to `.fit_problem_nfxp`.
#' Observations are bound to canonical output IDs once before the start loop;
#' every start uses that same target and observation mask.
#'
#' Selection requires finite loss, parameters and observed predictions, outer
#' optimizer convergence, and inner convergence with finite residual. Boundary
#' fits remain eligible and are flagged. The lowest eligible loss wins; exact
#' ties retain the first start. If none qualifies, `best` and `best_start` are
#' NULL and `status` is `no_valid_fit`; inspect the per-start table and messages.
#' Warnings are captured in `warnings` instead of emitted repeatedly.
#'
#' `loss_tol` is an absolute loss difference, in the supplied loss's units.
#' `parameter_tol` is the maximum absolute log parameter ratio to the winner.
#' These are descriptive disagreement thresholds, not statistical tests. Fewer
#' than two eligible starts give NA disagreement flags. Raw differences remain
#' in the table. Boundary flags use the runner's log search-span tolerance.
#'
#' Only the best rich fit is retained unless `keep_fits = TRUE`; other results
#' retain compact parameters and diagnostics. This is a finite search over the
#' supplied starts, not a guarantee of a global optimum or identified parameters.
#' @keywords internal
.fit_problem_multistart <- function(problem, observed, starts, lower, upper, ...,
                                    loss_tol = 1e-6, parameter_tol = 1e-3,
                                    keep_fits = FALSE) {
  .chck_class(problem, "ae_problem", "problem")
  expected <- problem$theta$names
  if (!is.matrix(starts) || !is.numeric(starts) || !nrow(starts) ||
      ncol(starts) != length(expected) || any(!is.finite(starts)) ||
      any(starts <= 0) || is.null(colnames(starts)) ||
      anyDuplicated(colnames(starts)) || !setequal(colnames(starts), expected)) {
    stop("`starts` must be a finite positive matrix with exactly the named parameters")
  }
  starts <- starts[, expected, drop = FALSE]
  if (anyDuplicated(starts)) stop("`starts` must contain distinct rows")
  ids <- rownames(starts)
  if (is.null(ids)) ids <- paste0("start", seq_len(nrow(starts)))
  if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
    stop("start row names must be nonempty and unique")
  }
  rownames(starts) <- ids
  lower <- .coerce_fit_theta(lower, expected, "lower")
  upper <- .coerce_fit_theta(upper, expected, "upper")
  .coerce_problem_theta(lower, problem$theta)
  .coerce_problem_theta(upper, problem$theta)
  if (any(lower <= 0 | upper <= 0 | lower >= upper)) {
    stop("positive `lower` must be less than `upper`")
  }
  for (i in seq_len(nrow(starts))) {
    .coerce_problem_theta(starts[i, ], problem$theta)
    if (any(starts[i, ] < lower | starts[i, ] > upper)) {
      stop("every start must be inside [`lower`, `upper`]")
    }
  }
  for (value in list(loss_tol, parameter_tol)) {
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < 0) {
      stop("disagreement tolerances must be finite nonnegative scalars")
    }
  }
  if (!is.logical(keep_fits) || length(keep_fits) != 1L || is.na(keep_fits)) {
    stop("`keep_fits` must be TRUE or FALSE")
  }
  args <- list(...)
  allowed <- setdiff(names(formals(.fit_problem_nfxp)),
                     c("problem", "observed", "init", "lower", "upper"))
  if (length(args) && (is.null(names(args)) || anyNA(names(args)) ||
      any(!nzchar(names(args))) || anyDuplicated(names(args)) ||
      any(!names(args) %in% allowed))) {
    stop("`...` must contain unique named fitting controls, excluding init and bounds")
  }
  output <- if ("output" %in% names(args)) args$output else "utilization"
  target <- .bind_problem_target(problem, observed, output)

  n <- nrow(starts)
  theta <- matrix(NA_real_, n, length(expected), dimnames = list(ids, expected))
  boundary <- matrix(FALSE, n, length(expected), dimnames = list(ids, expected))
  warnings <- errors <- stats::setNames(vector("list", n), ids)
  fits <- if (keep_fits) stats::setNames(vector("list", n), ids) else NULL
  table <- data.frame(start = ids, loss = NA_real_, eligible = FALSE,
    optim_convergence = NA_integer_, solver_converged = FALSE,
    residual = NA_real_, seconds = NA_real_, boundary = FALSE,
    status = "error", optim_message = NA_character_, solver_message = NA_character_,
    stringsAsFactors = FALSE)
  best <- NULL
  best_index <- NA_integer_
  # Read the clock without forcing collection between independently fitted starts.
  started <- proc.time()[["elapsed"]]
  for (i in seq_len(n)) {
    fit <- NULL
    messages <- character()
    error <- NULL
    start_time <- proc.time()[["elapsed"]]
    fit <- tryCatch(withCallingHandlers(
      do.call(.fit_problem_nfxp, c(list(problem = problem, observed = target,
        init = stats::setNames(starts[i, ], expected), lower = lower, upper = upper), args)),
      warning = function(w) {
        messages <<- c(messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }), error = function(e) {
        error <<- conditionMessage(e)
        NULL
      })
    warnings[[i]] <- messages
    errors[i] <- list(error)
    table$seconds[i] <- proc.time()[["elapsed"]] - start_time
    if (is.null(fit)) next
    theta[i, ] <- fit$theta[expected]
    table$loss[i] <- fit$loss
    table$optim_convergence[i] <- fit$convergence
    table$solver_converged[i] <- isTRUE(fit$equilibrium$converged)
    table$residual[i] <- fit$equilibrium$residual_norm
    if (!is.null(fit$message)) table$optim_message[i] <- fit$message
    if (!is.null(fit$equilibrium$message)) table$solver_message[i] <- fit$equilibrium$message
    finite <- all(is.finite(theta[i, ])) && all(theta[i, ] > 0) &&
      all(log(theta[i, ]) >= log(lower) - 1e-12 &
          log(theta[i, ]) <= log(upper) + 1e-12) &&
      is.finite(fit$loss) && is.finite(table$residual[i]) &&
      all(is.finite(as.numeric(fit$predicted)[target$mask]))
    table$eligible[i] <- finite && isTRUE(fit$convergence == 0L) &&
      table$solver_converged[i]
    table$status[i] <- if (!isTRUE(fit$convergence == 0L) ||
      !table$solver_converged[i]) "not_converged" else
      if (!finite) "invalid_result" else "eligible"
    if (all(is.finite(theta[i, ])) && all(theta[i, ] > 0)) {
      edge_tol <- 1e-6 + 1e-3 * (log(upper) - log(lower))
      boundary[i, ] <- log(theta[i, ]) <= log(lower) + edge_tol |
        log(theta[i, ]) >= log(upper) - edge_tol
      table$boundary[i] <- any(boundary[i, ])
    }
    if (keep_fits) fits[i] <- list(fit)
    if (table$eligible[i] && (is.null(best) || fit$loss < best$loss)) {
      best <- fit
      best_index <- i
    }
    fit <- NULL
  }
  elapsed <- proc.time()[["elapsed"]] - started
  table$loss_gap <- table$log_parameter_gap <- rep(NA_real_, n)
  valid <- which(table$eligible)
  loss_disagreement <- parameter_disagreement <- NA
  if (length(valid)) {
    table$loss_gap[valid] <- table$loss[valid] - best$loss
    table$log_parameter_gap[valid] <- apply(abs(sweep(log(theta[valid, , drop = FALSE]),
      2, log(best$theta[expected]), "-")), 1, max)
    if (length(valid) >= 2L) {
      loss_disagreement <- any(table$loss_gap[valid] > loss_tol)
      parameter_disagreement <- any(table$log_parameter_gap[valid] > parameter_tol)
    }
  }
  structure(list(best = best, best_start = if (length(valid)) ids[best_index] else NULL,
    status = if (length(valid)) "selected" else "no_valid_fit",
    table = table, starts = starts, theta = theta, boundary = boundary,
    warnings = warnings, errors = errors, fits = fits, seconds = unname(elapsed),
    loss_disagreement = loss_disagreement, parameter_disagreement = parameter_disagreement,
    tolerances = list(loss = loss_tol, log_parameter = parameter_tol),
    bounds = list(lower = lower, upper = upper)), class = "ae_multistart_fit")
}

#' Print the outcome of a private AE multistart search
#' @method print ae_multistart_fit
#' @keywords internal
#' @export
print.ae_multistart_fit <- function(x, ...) {
  cat("<ae_multistart_fit>\n")
  cat("eligible starts: ", sum(x$table$eligible), " / ", nrow(x$table), "\n", sep = "")
  cat("selected start: ", if (is.null(x$best_start)) "none" else x$best_start, "\n", sep = "")
  cat("loss disagreement: ", x$loss_disagreement,
      "; parameter disagreement: ", x$parameter_disagreement, "\n", sep = "")
  print(x$table, row.names = FALSE)
  if (any(lengths(x$warnings)) || any(lengths(x$errors))) {
    cat("Per-start messages are available in $warnings and $errors.\n")
  }
  invisible(x)
}
