#' Check what fitted facility counts identify in an allocation model
#'
#' Compares a fitted allocation with two simpler allocations on the same loss
#' and observed targets, and summarizes the travel times the allocations imply.
#' The check reports; it never refits, refuses or changes the fit.
#'
#' Facility counts constrain where predicted loads fall but not where users
#' live. When facility capacity closely tracks the observed counts, an
#' allocation that ignores travel time can reproduce the counts as well as, or
#' better than, a distance-decay allocation. The fitted decay then depends on
#' the travel support, such as a catchment limit on the prepared travel times,
#' rather than on the counts. The comparators are:
#'
#' * `fitted`: the selected fit, recomputed at its parameters.
#' * `no_decay`: distance decay switched off within the prepared travel support
#'   (absent edges stay absent). Origins choose among reachable facilities in
#'   proportion to attractiveness, and the outside mass is re-optimized when the
#'   model fits it.
#' * `capacity_only`: facility loads proportional to attractiveness, ignoring
#'   travel entirely, with one fitted scale factor. Available for facility
#'   `utilization` targets only.
#'
#' Implied trips are one-way travel times weighted by expected flows, using the
#' prepared travel times. Flows on absent edges (possible only for
#' `capacity_only`) are excluded from the travel-time quantiles and reported as
#' a share. To see how the fit depends on a catchment limit, prepare the inputs
#' again with travel beyond the limit set to `Inf`, refit, and check each fit.
#'
#' @param model The corrected CLM from [clm_allocation()] that was fitted.
#' @param fit The `ae_multistart_fit` returned by [fit_allocation()] for
#'   `model`. A search with no selected fit cannot be checked.
#' @param trip_breaks Positive travel times (in the prepared travel units) for
#'   which the share of expected trips longer than each value is reported.
#' @return An `ae_allocation_check` with a `comparators` table (loss, loss
#'   difference from the fit, parameters, need-weighted contact and implied
#'   trips), the exact comparator `parameters`, the selected fit's parameter
#'   `boundary` flags, and logical `flags`. Use `print()` or `as.data.frame()`.
#' @family experimental allocation workflow
#' @export
#' @md
check_allocation <- function(model, fit, trip_breaks = c(30, 60, 120)) {
  .check_allocation_model(model)
  .chck_class(fit, "ae_multistart_fit", "fit")
  best <- .selected_allocation_fit(fit)
  if (!is.numeric(trip_breaks) || !length(trip_breaks) || anyNA(trip_breaks) ||
      any(!is.finite(trip_breaks)) || any(trip_breaks <= 0)) {
    stop("trip_breaks must be finite positive travel times")
  }
  trip_breaks <- sort(unique(trip_breaks))
  spec <- model$metadata$spec
  if (!identical(best$family, spec$family) || !identical(best$allocation_form, spec$allocation)) {
    stop("fit does not belong to this model")
  }
  sub <- model$substrate
  output <- best$output
  mask <- if (is.null(best$target_mask)) rep(TRUE, length(best$observed)) else best$target_mask
  observed <- as.numeric(best$observed)[mask]
  loss_of <- function(predicted) {
    value <- do.call(best$loss_fn, c(list(predicted = as.numeric(predicted)[mask],
                                          observed = observed), best$loss_args))
    if (is.numeric(value) && length(value) == 1L && is.finite(value)) value else Inf
  }
  solve_at <- function(theta) {
    .solve_problem(model, theta, keep_history = FALSE, warn = FALSE, check = FALSE,
                   requested_outputs = unique(c(output, "flow", "access")))$outputs
  }
  theta <- best$theta
  fitted_out <- solve_at(theta)
  fitted_loss <- loss_of(fitted_out[[output]])
  if (!isTRUE(all.equal(fitted_loss, best$loss, tolerance = 1e-6))) {
    stop("the fit cannot be reproduced from this model; check that it belongs to the model")
  }

  ## No decay within the travel support: the decay parameter at its flat limit.
  flat <- c(gaussian = 1e8, exponential = 1e-10, power = 1e-10)[[spec$family]]
  flat_theta <- theta
  flat_theta[["sigma"]] <- flat
  flat_eval <- function(log_v0) {
    th <- flat_theta
    th[["v0"]] <- exp(log_v0)
    loss_of(solve_at(th)[[output]])
  }
  if ("v0" %in% names(theta)) {
    grid <- seq(log(1e-8), log(1e8), length.out = 33)
    values <- vapply(grid, flat_eval, numeric(1))
    k <- which.min(values)
    span <- grid[pmax(1L, k - 1L)]
    span <- c(span, grid[pmin(length(grid), k + 1L)])
    opt <- if (diff(span) > 0) stats::optimize(flat_eval, span, tol = 1e-8) else
      list(minimum = grid[k], objective = values[k])
    flat_theta[["v0"]] <- exp(opt$minimum)
  }
  flat_out <- solve_at(flat_theta)
  flat_loss <- loss_of(flat_out[[output]])

  ## Capacity only, ignoring travel: loads proportional to attractiveness.
  beta <- if ("beta" %in% names(theta)) theta[["beta"]] else spec$beta
  attract <- as.numeric(sub$S)^beta
  cap_loss <- NA_real_
  cap_scale <- NA_real_
  if (identical(output, "utilization")) {
    scale_eval <- function(log_c) loss_of(exp(log_c) * attract)
    start <- log(sum(observed) / sum(attract[mask]))
    opt <- stats::optimize(scale_eval, start + c(-5, 5), tol = 1e-10)
    cap_loss <- opt$objective
    cap_scale <- exp(opt$minimum)
  }

  ## Implied trips and contact.
  travel <- sub$distance_active
  demand <- sub$D_active
  trips <- function(flow) {
    flow <- as.matrix(flow)
    finite <- is.finite(travel) & flow > 0
    total <- sum(flow)
    w <- flow[finite]
    t <- travel[finite]
    o <- order(t)
    cw <- cumsum(w[o]) / sum(w)
    q <- function(p) t[o][which(cw >= p)[1L]]
    c(trip_median = q(0.5), trip_p90 = q(0.9),
      stats::setNames(vapply(trip_breaks, function(b) sum(w[t > b]) / sum(w), numeric(1)),
                      paste0("trips_over_", trip_breaks)),
      trips_absent_edge = 1 - sum(w) / total)
  }
  contact <- function(out) sum(demand * as.numeric(out$access)) / sum(demand)
  cap_flow <- outer(demand, attract / sum(attract))
  rows <- list(
    fitted = c(loss = fitted_loss, contact = contact(fitted_out), trips(fitted_out$flow)),
    no_decay = c(loss = flat_loss, contact = contact(flat_out), trips(flat_out$flow)),
    capacity_only = c(loss = cap_loss, contact = NA_real_, trips(cap_flow))
  )
  if (!identical(output, "utilization")) rows$capacity_only[] <- NA_real_
  tab <- data.frame(comparator = names(rows), do.call(rbind, rows), row.names = NULL,
                    check.names = FALSE)
  tab$loss_difference <- tab$loss - fitted_loss
  tab$parameters <- c(
    paste(sprintf("%s=%.4g", names(theta), theta), collapse = ", "),
    paste(sprintf("%s=%.4g", names(flat_theta), flat_theta), collapse = ", "),
    if (is.na(cap_scale)) NA_character_ else sprintf("scale=%.4g", cap_scale))
  tab <- tab[, c("comparator", "loss", "loss_difference", "parameters", "contact",
                 setdiff(names(tab), c("comparator", "loss", "loss_difference",
                                       "parameters", "contact")))]
  boundary <- fit$boundary[fit$best_start, ]
  flags <- c(
    capacity_only_fits_at_least_as_well = isTRUE(cap_loss <= fitted_loss),
    no_decay_fits_at_least_as_well = isTRUE(flat_loss <= fitted_loss),
    decay_parameter_at_bound = isTRUE(boundary[["sigma"]]))
  structure(list(comparators = tab, boundary = boundary, flags = flags,
                 parameters = list(fitted = theta, no_decay = flat_theta,
                                   capacity_scale = cap_scale),
                 family = spec$family, output = output,
                 travel_units = sub$input_metadata$travel_units,
                 trip_breaks = trip_breaks),
            class = "ae_allocation_check")
}

#' @rdname check_allocation
#' @param x An `ae_allocation_check`.
#' @param digits Significant digits for printed values.
#' @param ... Unused.
#' @export
#' @md
print.ae_allocation_check <- function(x, digits = 4, ...) {
  cat("<ae_allocation_check>\n")
  units <- x$travel_units
  if (is.null(units) || is.na(units)) units <- "travel units"
  tab <- x$comparators
  show <- tab[, c("comparator", "loss", "loss_difference", "contact",
                  "trip_median", "trip_p90")]
  print(format(show, digits = digits), row.names = FALSE)
  cat("Trip lengths are one-way, in ", units, ", weighted by expected flows.\n", sep = "")
  if (x$flags[["capacity_only_fits_at_least_as_well"]] ||
      x$flags[["no_decay_fits_at_least_as_well"]]) {
    cat("\nThe observed counts are reproduced at least as well without distance decay",
        if (x$flags[["capacity_only_fits_at_least_as_well"]]) " (capacity shares alone)",
        ".\nThe fitted decay therefore depends on the travel support (for example a",
        " catchment limit),\nnot on the counts. Compare the implied trip lengths",
        " before relying on it.\n", sep = "")
  } else {
    cat("\nThe fitted allocation reproduces the counts better than both comparators.\n")
  }
  if (x$flags[["decay_parameter_at_bound"]]) {
    cat("The selected decay parameter lies at a search bound.\n")
  }
  invisible(x)
}

#' @rdname check_allocation
#' @param row.names,optional Standard `as.data.frame()` arguments; unused.
#' @exportS3Method base::as.data.frame
#' @md
as.data.frame.ae_allocation_check <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$comparators
}
