#' Check what fitted facility counts identify in an allocation model
#'
#' Compares a fitted allocation with two simpler allocations on the same loss
#' and observed targets, and summarizes the travel times the allocations imply.
#' The check reports only: it never changes or replaces the fit.
#'
#' Facility counts constrain where predicted loads fall but not where users
#' live. When facility capacity closely tracks the observed counts, an
#' allocation that ignores travel time can reproduce the counts as well as, or
#' better than, a distance-decay allocation. The fitted decay then depends on
#' the travel support, such as a catchment limit on the prepared travel times,
#' rather than on the counts. The comparators are:
#'
#' * `fitted`: the selected fit, recomputed at its parameters.
#' * `no_decay`: distance decay switched off within the prepared travel support:
#'   every finite travel time gets the same weight, whatever decay kernel the
#'   model uses, and absent edges stay absent. Origins choose among reachable
#'   facilities in proportion to attractiveness, and the outside mass is
#'   re-optimized when the model fits it. Its reported parameters leave out the
#'   decay parameter `sigma`, which this allocation does not use.
#' * `capacity_only`: facility loads proportional to attractiveness, ignoring
#'   travel entirely, with one fitted scale factor. Available for facility
#'   `utilization` targets only.
#'
#' When the model fits the attractiveness exponent `beta`, it is re-optimized in
#' both comparators. Comparator searches start from the fit's own scale and widen
#' while the optimum lies at the search edge; `flags` records any search that
#' still ends at an edge.
#'
#' Implied trips are one-way travel times weighted by expected flows, using the
#' prepared travel times. Flows on absent edges (possible only for
#' `capacity_only`) have no travel time: the quantiles and the shares of trips
#' longer than each break are among flows on reachable edges, and the absent
#' share is reported separately. To see how the fit depends on a catchment limit, prepare the inputs
#' again with travel beyond the limit set to `Inf`, refit, and check each fit.
#'
#' @param model The corrected CLM from [clm_allocation()] that was fitted.
#' @param fit The `ae_multistart_fit` returned by [fit_allocation()] for
#'   `model`. A search with no selected fit cannot be checked.
#' @param trip_breaks Positive travel times (in the prepared travel units) for
#'   which the share of expected trips longer than each value is reported.
#' @return An `ae_allocation_check` with a `comparators` table (loss, loss
#'   difference from the fit, parameters, need-weighted contact and implied
#'   trips), the exact comparator `parameters` (those of `no_decay` are the
#'   re-optimized parameters other than `sigma`, an empty vector when the model
#'   fits only `sigma`), the selected fit's parameter `boundary` flags,
#'   `search_at_edge`, and logical `flags`. Use `print()` or `as.data.frame()`.
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
  ctx <- .allocation_fit_context(model, best)
  spec <- model$metadata$spec
  sub <- model$substrate
  output <- ctx$output
  loss_of <- ctx$loss_of
  theta <- best$theta
  fitted_out <- ctx$fitted_outputs
  fitted_loss <- ctx$fitted_loss
  has_beta <- "beta" %in% names(theta)
  beta_hat <- if (has_beta) theta[["beta"]] else spec$beta
  edges <- c(no_decay = FALSE, capacity_only = FALSE)

  ## No decay within the travel support: the model with the internal no-decay
  ## family (weight 1 on every finite travel time, 0 on absent edges), solved at
  ## the fitted parameters, whose sigma that family ignores. The outside mass
  ## (and beta, when fitted) are re-optimized.
  solve_flat <- .allocation_solver(.no_decay_model(model), output)
  flat_theta <- theta
  flat_loss_at <- function(th) loss_of(solve_flat(th)[[output]])
  if ("v0" %in% names(theta)) {
    v0_eval <- function(log_v0) {
      th <- flat_theta
      th[["v0"]] <- exp(log_v0)
      flat_loss_at(th)
    }
    centre <- if (theta[["v0"]] > 0) log(theta[["v0"]]) else log(sum(as.numeric(sub$S)))
    opt <- .optimize_widen(v0_eval, centre)
    flat_theta[["v0"]] <- exp(opt$par)
    edges[["no_decay"]] <- opt$at_edge
  }
  if (has_beta) {
    free <- intersect(c("v0", "beta"), names(theta))
    joint <- function(par) {
      th <- flat_theta
      th[free] <- exp(par)
      flat_loss_at(th)
    }
    opt <- stats::optim(log(flat_theta[free]), joint, method = if (length(free) > 1L)
      "Nelder-Mead" else "BFGS", control = list(reltol = 1e-12, maxit = 2000))
    if (opt$value < flat_loss_at(flat_theta)) flat_theta[free] <- exp(opt$par)
  }
  flat_out <- solve_flat(flat_theta)
  flat_loss <- loss_of(flat_out[[output]])
  # The decay parameter is not part of this allocation: report the others.
  no_decay_theta <- flat_theta[setdiff(names(flat_theta), "sigma")]

  ## Capacity only, ignoring travel: loads proportional to attractiveness,
  ## with one fitted scale (and beta, when the model fits it).
  supply <- as.numeric(sub$S)
  cap_loss <- NA_real_
  cap_par <- c(scale = NA_real_, beta = beta_hat)
  observed_total <- sum(ctx$observed)
  if (identical(output, "utilization") && observed_total > 0) {
    attract_at <- function(b) supply^b
    scale_eval <- function(log_c) loss_of(exp(log_c) * attract_at(beta_hat))
    centre <- log(observed_total / sum(attract_at(beta_hat)[ctx$mask]))
    opt <- .optimize_widen(scale_eval, centre)
    cap_par[["scale"]] <- exp(opt$par)
    cap_loss <- opt$value
    edges[["capacity_only"]] <- opt$at_edge
    if (has_beta) {
      joint <- function(par) loss_of(exp(par[1]) * attract_at(exp(par[2])))
      opt <- stats::optim(log(cap_par), joint, method = "Nelder-Mead",
                          control = list(reltol = 1e-12, maxit = 2000))
      if (opt$value < cap_loss) {
        cap_par[] <- exp(opt$par)
        cap_loss <- opt$value
      }
    }
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
  cap_attract <- supply^cap_par[["beta"]]
  cap_flow <- outer(demand, cap_attract / sum(cap_attract))
  rows <- list(
    fitted = c(loss = fitted_loss, contact = contact(fitted_out), trips(fitted_out$flow)),
    no_decay = c(loss = flat_loss, contact = contact(flat_out), trips(flat_out$flow)),
    capacity_only = c(loss = cap_loss, contact = NA_real_, trips(cap_flow))
  )
  if (is.na(cap_loss)) rows$capacity_only[] <- NA_real_
  tab <- data.frame(comparator = names(rows), do.call(rbind, rows), row.names = NULL,
                    check.names = FALSE)
  tab$loss_difference <- tab$loss - fitted_loss
  tab$parameters <- c(
    paste(sprintf("%s=%.4g", names(theta), theta), collapse = ", "),
    if (length(no_decay_theta)) {
      paste0("no decay; ", paste(sprintf("%s=%.4g", names(no_decay_theta), no_decay_theta),
                                 collapse = ", "))
    } else "no decay",
    if (is.na(cap_loss)) NA_character_ else
      paste(sprintf("%s=%.4g", names(cap_par)[c(TRUE, has_beta)],
                    cap_par[c(TRUE, has_beta)]), collapse = ", "))
  tab <- tab[, c("comparator", "loss", "loss_difference", "parameters", "contact",
                 setdiff(names(tab), c("comparator", "loss", "loss_difference",
                                       "parameters", "contact")))]
  boundary <- stats::setNames(as.logical(fit$boundary[fit$best_start, ]), colnames(fit$boundary))
  flags <- c(
    capacity_only_fits_at_least_as_well = isTRUE(cap_loss <= fitted_loss),
    no_decay_fits_at_least_as_well = isTRUE(flat_loss <= fitted_loss),
    decay_parameter_at_bound = isTRUE(boundary[["sigma"]]),
    comparator_search_at_edge = any(edges))
  structure(list(comparators = tab, boundary = boundary, flags = flags,
                 parameters = list(fitted = theta, no_decay = no_decay_theta,
                                   capacity_only = cap_par),
                 search_at_edge = edges,
                 family = spec$family, output = output,
                 travel_units = sub$input_metadata$travel_units,
                 trip_breaks = trip_breaks),
            class = "ae_allocation_check")
}

# Solve `model` at a parameter vector and return the outputs the checks read: the
# fit's own output, flow and access.
.allocation_solver <- function(model, output) {
  function(theta) {
    .solve_problem(model, theta, keep_history = FALSE, warn = FALSE, check = FALSE,
                   requested_outputs = unique(c(output, "flow", "access")))$outputs
  }
}

# The model with distance decay switched off: a copy with the internal "none"
# family (weight 1 on every finite travel time, 0 on absent edges), no declared
# kernel and so no prediction snapshot, with its map rebuilt from that
# specification. The parameter contract is unchanged, so the model is solved at a
# full parameter vector whose sigma it ignores.
.no_decay_model <- function(model) {
  spec <- model$metadata$spec
  spec$family <- "none"
  spec[["kernel"]] <- NULL
  model$metadata$spec <- spec
  model$bind <- .compile_huff_map(spec, model$substrate)$bind
  model
}

# Reproduce a selected fit from its model: the fit's own loss on its own masked,
# ID-aligned targets. Errors when the model did not produce the fit. A fit with
# a prediction snapshot must also carry this model's decay kernel declaration
# (none for a built-in kernel), which tells apart two declared kernels that share
# a name. Other constants of the specification are not compared: the loss check
# below catches those that change the fit.
.allocation_fit_context <- function(model, best) {
  spec <- model$metadata$spec
  if (!identical(best$family, spec$family) || !identical(best$allocation_form, spec$allocation) ||
      !identical(as.character(best$coverage_meta$facility_ids),
                 as.character(model$substrate$facility_ids)) ||
      !isTRUE(all.equal(as.numeric(best$coverage_meta$demand), as.numeric(model$substrate$D_active))) ||
      !isTRUE(all.equal(as.numeric(best$coverage_meta$supply), as.numeric(model$substrate$S))) ||
      (!is.null(best$prediction_snapshot) &&
         !identical(best$prediction_snapshot$spec[["kernel", exact = TRUE]],
                    spec[["kernel", exact = TRUE]]))) {
    stop("fit does not belong to this model")
  }
  output <- best$output
  mask <- if (is.null(best$target_mask)) rep(TRUE, length(best$observed)) else best$target_mask
  observed <- as.numeric(best$observed)[mask]
  loss_of <- function(predicted) {
    value <- do.call(best$loss_fn, c(list(predicted = as.numeric(predicted)[mask],
                                          observed = observed), best$loss_args))
    if (is.numeric(value) && length(value) == 1L && is.finite(value)) value else Inf
  }
  solve_at <- .allocation_solver(model, output)
  fitted_outputs <- solve_at(best$theta)
  fitted_loss <- loss_of(fitted_outputs[[output]])
  if (!isTRUE(all.equal(fitted_loss, best$loss, tolerance = 1e-6))) {
    stop("fit does not belong to this model: its loss cannot be reproduced")
  }
  list(output = output, mask = mask, observed = observed, loss_of = loss_of,
       solve_at = solve_at, fitted_outputs = fitted_outputs, fitted_loss = fitted_loss)
}

# Grid-seeded one-dimensional minimization on the log scale, centred on a
# plausible value and widened while the minimum lies at the window edge.
.optimize_widen <- function(f, centre, half = 8, steps = 33, max_widen = 6) {
  at_edge <- TRUE
  for (i in seq_len(max_widen)) {
    grid <- seq(centre - half, centre + half, length.out = steps)
    values <- vapply(grid, f, numeric(1))
    k <- which.min(values)
    at_edge <- k == 1L || k == steps
    if (!at_edge) break
    centre <- grid[k]
    half <- 2 * half
  }
  lo <- grid[max(1L, k - 1L)]
  hi <- grid[min(steps, k + 1L)]
  opt <- stats::optimize(f, c(lo, hi), tol = 1e-10)
  if (opt$objective <= values[k]) list(par = opt$minimum, value = opt$objective, at_edge = at_edge)
  else list(par = grid[k], value = values[k], at_edge = at_edge)
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
  } else if (is.na(tab$loss[tab$comparator == "capacity_only"])) {
    cat("\nThe fitted allocation reproduces the observations better than the allocation",
        "\nwithout distance decay. The capacity-only comparator applies to facility",
        " totals only.\n", sep = "")
  } else {
    cat("\nThe fitted allocation reproduces the counts better than both comparators.\n")
  }
  if (x$flags[["decay_parameter_at_bound"]]) {
    cat("The selected decay parameter lies at a search bound.\n")
  }
  if (x$flags[["comparator_search_at_edge"]]) {
    cat("A comparator search ended at its widest range; its loss may be overstated.\n")
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
