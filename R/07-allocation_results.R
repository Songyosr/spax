# Saved allocation-result inspection ---------------------------------------

#' Inspect saved allocation result tables
#'
#' These methods read saved values without solving, fitting, resampling or
#' constructing maps. Fit tables use `type = "parameters"` and contain parameter
#' names and estimates. Other selectors are rejected. A
#' multistart fit uses the selected fit; when none is eligible, use `summary()`
#' to inspect all supplied starts instead.
#'
#' Coverage tables preserve the recorded support, input-unit declarations and
#' reporting settings as attributes. `type = "cells"` returns retained positive
#' demand origins, including their canonical IDs and any existing spatial cell
#' index; it does not expand to unknown or zero-demand prediction locations.
#' Reported and raw conditional capacity remain separate saved columns.
#'
#' Bootstrap tables contain saved point/interval estimates, every attempted draw,
#' or every attempted start. Failed draws, inner boundaries and refusal statuses
#' are retained. Extraction neither creates intervals nor changes eligibility.
#' @param x A fitted allocation result, compact coverage, or bootstrap result.
#' @param row.names,optional Standard `as.data.frame()` arguments.
#' @param type Saved table to extract; choices depend on the result class.
#' @param ... Additional generic arguments.
#' @return A data frame of saved values. Coverage tables additionally carry
#'   `support`, `input_metadata` and `reporting_settings` attributes.
#' @name allocation-result-tables
#' @exportS3Method base::as.data.frame
as.data.frame.ae_problem_nfxp_fit <- function(x, row.names = NULL, optional = FALSE,
                                             type = c("parameters"), ...) {
  .chck_class(x, "ae_problem_nfxp_fit", "x")
  type <- match.arg(type)
  theta <- x$theta
  parameters <- names(theta)
  if (is.null(parameters)) parameters <- paste0("theta", seq_along(theta))
  .allocation_saved_table(data.frame(parameter = parameters, estimate = as.numeric(theta),
    stringsAsFactors = FALSE), row.names, optional, ...)
}

#' @rdname allocation-result-tables
#' @exportS3Method base::as.data.frame
as.data.frame.ae_multistart_fit <- function(x, row.names = NULL, optional = FALSE,
                                           type = c("parameters"), ...) {
  .chck_class(x, "ae_multistart_fit", "x")
  type <- match.arg(type)
  as.data.frame.ae_problem_nfxp_fit(.selected_allocation_fit(x), row.names, optional,
    type = type, ...)
}

#' @rdname allocation-result-tables
#' @exportS3Method base::as.data.frame
as.data.frame.ae_coverage <- function(x, row.names = NULL, optional = FALSE,
                                     type = c("regional", "cells"), ...) {
  .chck_class(x, "ae_coverage", "x")
  type <- match.arg(type)
  out <- .allocation_saved_table(x[[type]], row.names, optional, ...)
  attr(out, "support") <- x$support
  attr(out, "input_metadata") <- x$input_metadata
  attr(out, "reporting_settings") <- x$settings
  out
}

#' @rdname allocation-result-tables
#' @exportS3Method base::as.data.frame
as.data.frame.ae_allocation_bootstrap <- function(x, row.names = NULL, optional = FALSE,
                                                 type = c("estimates", "draws", "starts"), ...) {
  .chck_class(x, "ae_allocation_bootstrap", "x")
  type <- match.arg(type)
  .allocation_saved_table(x[[if (type == "estimates") "intervals" else type]], row.names, optional, ...)
}

.allocation_saved_table <- function(x, row.names, optional, ...) {
  if (!is.data.frame(x)) stop("saved result table is unavailable")
  base::as.data.frame.data.frame(x, row.names = row.names, optional = optional, ...)
}

#' Print a saved allocation multistart summary
#'
#' Preserves all selected-fit, per-start, boundary, disagreement and message
#' fields returned by `summary()`, including searches with no eligible fit.
#' @param x A summary of an allocation multistart fit.
#' @param digits Display precision; saved values are unchanged.
#' @param ... Additional print arguments.
#' @return The summary, invisibly.
#' @export
print.summary.ae_multistart_fit <- function(x, digits = 4, ...) {
  cat("<summary.ae_multistart_fit>\n")
  cat("status: ", x$status, "; selected start: ",
    if (is.null(x$selected_start)) "none" else x$selected_start, "\n", sep = "")
  cat("loss disagreement: ", x$disagreement$loss,
    "; parameter disagreement: ", x$disagreement$parameter, "\n", sep = "")
  cat("Supplied-start diagnostics (not a global-optimum or identification guarantee):\n")
  print.data.frame(.ae_round_data_frame(x$starts, digits), row.names = FALSE)
  if (!is.null(x$selected)) {
    cat("\nSelected fit:\n")
    print(x$selected, digits = digits, ...)
  }
  if (any(lengths(x$warnings)) || any(lengths(x$errors))) {
    cat("Per-start messages remain available in $warnings and $errors.\n")
  }
  invisible(x)
}

#' Plot saved diagnostics from the selected allocation fit
#' @param x An allocation multistart fit.
#' @param type Existing selected-fit diagnostic to display.
#' @param ... Arguments forwarded to the selected-fit plot method.
#' @return The selected diagnostic method's result, invisibly.
#' @exportS3Method graphics::plot
plot.ae_multistart_fit <- function(x, type = c("fit", "convergence", "state"), ...) {
  .chck_class(x, "ae_multistart_fit", "x")
  plot.ae_problem_nfxp_fit(.selected_allocation_fit(x), type = match.arg(type), ...)
}

#' Inspect a saved facility bootstrap
#'
#' These methods display saved results, including refused original fits,
#' unfinished attempts and unavailable intervals. Counts distinguish planned,
#' attempted and successful fits; each quantity retains its own finite-success
#' count and reporting status. Fixed-input identities have no sampling interval.
#'
#' `plot()` displays one named saved quantity. Its default is the first saved
#' quantity. The interval view draws a saved point and available percentile
#' endpoints only when the overall run is complete and that quantity's saved
#' status permits reporting; an unavailable interval is labelled explicitly. This
#' also suppresses stale finite endpoints in refused or incomplete saved objects.
#' The draw view plots
#' finite values from successful attempted fits, retaining inner boundaries and
#' displaying the number plotted. Neither view implies empirical observation-law
#' validation, simultaneous coverage or causal interpretation of a scenario.
#' @param x,object An `ae_allocation_bootstrap` result.
#' @param digits Display precision.
#' @param quantity One saved quantity name; defaults to the first estimate row.
#' @param type Plot a saved interval or successful-draw distribution.
#' @param ... Additional generic or graphics arguments.
#' @return `summary()` returns saved estimates, original fit, status, counts,
#'   diagnostics, capacity support and declarations in a
#'   `summary.ae_allocation_bootstrap` object.
#'   Print and plot methods return their input invisibly.
#' @name allocation-bootstrap-methods
#' @export
print.ae_allocation_bootstrap <- function(x, digits = 4, ...) {
  .chck_class(x, "ae_allocation_bootstrap", "x")
  print(summary(x), digits = digits, ...)
  invisible(x)
}

#' @rdname allocation-bootstrap-methods
#' @exportS3Method base::summary
summary.ae_allocation_bootstrap <- function(object, ...) {
  .chck_class(object, "ae_allocation_bootstrap", "object")
  draws <- object$draws
  structure(list(status = object$status, reasons = object$reasons,
    counts = data.frame(planned = object$planned, attempted = object$attempted,
      successful = object$successful), estimates = object$intervals,
    diagnostics = list(failed_attempts = sum(draws$success %in% FALSE),
      inner_boundary_attempts = sum(draws$success %in% TRUE & draws$boundary %in% TRUE)),
    original = object$original, settings = object$settings,
    observation = object$observation, input_metadata = object$input_metadata,
    facility_ids = object$facility_ids, capacity_support = object$capacity_support,
    seed = object$seed), class = "summary.ae_allocation_bootstrap")
}

#' @rdname allocation-bootstrap-methods
#' @export
print.summary.ae_allocation_bootstrap <- function(x, digits = 4, ...) {
  cat("<ae_allocation_bootstrap>\n")
  cat("status: ", x$status, "\n", sep = "")
  if (length(x$reasons)) cat("reasons: ", paste(x$reasons, collapse = "; "), "\n", sep = "")
  print.data.frame(x$counts, row.names = FALSE)
  cat("failed attempts: ", x$diagnostics$failed_attempts,
    "; retained inner-boundary attempts: ", x$diagnostics$inner_boundary_attempts, "\n", sep = "")
  cat("sampling: ", x$observation$sampling, "; event: ", x$observation$event,
    "; period: ", x$observation$period, "\n", sep = "")
  support <- x$capacity_support
  if (!is.null(support)) {
    supply_units <- x$input_metadata$supply_units
    if (!is.character(supply_units) || length(supply_units) != 1L ||
        is.na(supply_units) || !nzchar(supply_units)) supply_units <- "raw supply units"
    cat("supply (", supply_units, "): total: ", .ae_format_number(support$total_supply, digits),
      "; attributed: ", .ae_format_number(support$attributed_supply, digits),
      "; unsupported: ", .ae_format_number(sum(support$unsupported_supply), digits), "\n", sep = "")
    cat("facilities: total: ", length(x$facility_ids),
      "; with retained-demand support: ", length(support$supported_facility_ids),
      "; positive supply without support: ", length(support$unsupported_facility_ids), "\n", sep = "")
  }
  if (is.character(x$settings$fixed_inputs)) cat("fixed inputs: ", x$settings$fixed_inputs, "\n", sep = "")
  for (label in c("capacity_identity", "evidence_scope")) {
    if (is.character(x$settings[[label]])) cat(paste(strwrap(paste(x$settings[[label]], collapse = " ")), collapse = "\n"), "\n")
  }
  if (!identical(x$status, "complete")) cat("Regular interval reporting is unavailable for this run status.\n")
  if (identical(x$status, "complete") && any(
      x$estimates$status == "conditional_pointwise_bootstrap" &
      is.finite(x$estimates$lower) & is.finite(x$estimates$upper))) {
    cat("Available bounds: conditional bootstrap intervals.\n")
  }
  print.data.frame(.ae_round_data_frame(x$estimates, digits), row.names = FALSE)
  invisible(x)
}

.allocation_bootstrap_status_label <- function(status) {
  labels <- c(conditional_pointwise_bootstrap = "conditional interval eligible",
    fixed_input_identity = "fixed input identity", incomplete_bootstrap = "incomplete bootstrap",
    insufficient_bootstrap_success = "insufficient successful values",
    insufficient_success = "insufficient successful draws",
    original_fit_failed = "original fit failed", original_mean_boundary = "original fit on boundary",
    original_score_failed = "original fit score failed", original_unidentified = "original fit unidentified")
  if (status %in% names(labels)) unname(labels[status]) else gsub("_", " ", status, fixed = TRUE)
}

#' @rdname allocation-bootstrap-methods
#' @exportS3Method graphics::plot
plot.ae_allocation_bootstrap <- function(x, quantity = NULL,
                                         type = c("interval", "draws"), ...) {
  .chck_class(x, "ae_allocation_bootstrap", "x")
  type <- match.arg(type)
  estimates <- x$intervals
  if (is.null(quantity)) quantity <- estimates$quantity[1L]
  if (!is.character(quantity) || length(quantity) != 1L || is.na(quantity) ||
      !quantity %in% estimates$quantity) stop("quantity must name one saved estimate")
  row <- estimates[match(quantity, estimates$quantity), , drop = FALSE]
  caption <- c(strwrap(paste0("run: ", .allocation_bootstrap_status_label(x$status),
    "; quantity: ", .allocation_bootstrap_status_label(row$status)), width = 62),
    paste0(row$successful, "/", row$attempted, " finite successful values; ", row$planned, " planned"))
  args <- list(...)
  empty <- function(message) {
    graphics::plot.new()
    graphics::title(main = if (is.null(args$main)) paste("Bootstrap:", quantity) else args$main)
    graphics::text(.5, .5, message)
  }
  if (type == "draws") {
    values <- x$draws[[quantity]]
    if (is.null(values) && nrow(x$draws)) stop("quantity has no saved draw column")
    values <- values[x$draws$success %in% TRUE & is.finite(values)]
    if (!length(values)) empty("No finite successful draws") else {
      defaults <- list(x = values, xlab = quantity, main = paste("Saved bootstrap draws:", quantity))
      do.call(graphics::hist, utils::modifyList(defaults, args))
    }
  } else if (!is.finite(row$estimate)) {
    empty("No reportable point estimate")
  } else {
    available <- identical(x$status, "complete") &&
      identical(row$status, "conditional_pointwise_bootstrap") &&
      is.finite(row$lower) && is.finite(row$upper)
    limits <- if (available) range(c(row$estimate, row$lower, row$upper)) else
      row$estimate + c(-1, 1) * max(abs(row$estimate) * .05, .5)
    defaults <- list(x = row$estimate, y = 1, xlim = limits, ylim = c(.7, 1.3),
      xlab = quantity, ylab = "", yaxt = "n", pch = 19,
      main = if (available) "Conditional bootstrap interval" else "Point estimate; interval unavailable")
    do.call(graphics::plot, utils::modifyList(defaults, args))
    if (available) graphics::segments(row$lower, 1, row$upper, 1, lwd = 2)
  }
  graphics::mtext(caption, side = 3, line = rev(seq_along(caption) - 1) * .8 - .1, cex = .7)
  invisible(x)
}
