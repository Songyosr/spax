# Default starting values and bounds for allocation fits (SPAX-058, T3) ---------
#
# fit_allocation() calls .allocation_default_search() when `starts` is NULL (and
# for the bounds that are missing when `starts` are given). The rule is "probe +
# scan": probe the decay kernel for the range of sigma over which it still
# decays over the travel times of the data, seed the outside weight v0 from the
# observed outside share, scan the loss over a grid with the fitter's own code
# path, and start the fit from the best grid points. Everything here reuses the
# engine (the kernel, the attractiveness, the solver, the target binding and the
# loss); nothing re-implements a kernel.

.default_search_rule <- "probe+scan v1"
.default_count_outputs <- c("utilization", "flow", "utilization_by_group")
.default_probe_grid <- 10^seq(-8, 8, by = 1 / 8)
.default_grid_points <- 15L
.default_v0_multipliers <- c(0.1, 1, 10)
.default_beta_bounds <- c(0.05, 5)
# The default lower bound of v0 is not below the seed for this outside share: a v0 with
# a smaller outside share cannot be told from the Huff model (v0 = 0) and the fit would
# drift there without ever touching a bound.
.default_v0_floor_share <- 1e-6
# factr = 1e3 stops at a relative loss change of about 2e-13: tighter (10) ends starts that
# reached the optimum with line-search failures, looser (the default 1e7) moves a weakly
# identified v0 (Cornwall gaussian + beta totals: 0.0078 against 0.0109).
.default_search_control <- list(maxit = 300, factr = 1e3, pgtol = 1e-8)
.default_solver_args <- c("lambda", "tol", "max_iter", "norm", "check")

.default_search_message <- list(
  loss = paste0("choose the loss when starts are not given: loss = \"poisson\" ",
                "for counts, or \"weighted_sse\""),
  no_travel = "no finite travel times among the active origins",
  outside_zero = paste0("the observed counts reach the total demand, so the outside ",
                        "weight would go to zero; use no outside option (outside = 0, ",
                        "or fit_v0 = FALSE in clm_allocation()) or check the targets"),
  outside_small = paste0("the observed counts are within 0.1% of the total demand; ",
                         "the outside weight is barely identified"),
  no_v0 = "no positive outside weight in the default search; give starts",
  no_finite = paste0("no parameter value in the default search range gives a finite ",
                     "loss; give starts"))

# Whether `lower` or `upper` gives a value for every parameter the way the
# fitter already reads it: named with every parameter name, or unnamed with one
# value per parameter. Anything else (NULL, a partial named vector) leaves
# entries to the default search.
.default_bounds_cover <- function(x, expected) {
  if (is.null(x) || !is.numeric(x)) return(FALSE)
  nms <- names(x)
  if (is.null(nms) || any(nms == "")) return(length(x) == length(expected))
  all(expected %in% nms)
}

# The given bounds, named by parameter, checked for names and sign. An unnamed
# vector with one value per parameter is named in contract order, as the fitter
# does. Starting from NULL gives no entries.
.default_given_bounds <- function(x, expected, arg) {
  if (is.null(x)) return(stats::setNames(numeric(), character()))
  if (is.numeric(x) && length(x) == length(expected) &&
      (is.null(names(x)) || any(names(x) == ""))) {
    names(x) <- expected
  }
  if (!is.numeric(x) || !length(x) || is.null(names(x)) || anyNA(names(x)) ||
      any(!nzchar(names(x))) || anyDuplicated(names(x)) || anyNA(x) ||
      any(x <= 0) || !all(names(x) %in% expected)) {
    stop("`", arg, "` must be a named positive numeric vector with names among ",
         "the model parameters (", paste(expected, collapse = ", "), ")",
         call. = FALSE)
  }
  x
}

.default_format <- function(x) format(x, digits = 4)

.default_crossing_error <- function(name, lower, upper, lower_source, upper_source) {
  stop("lower must be less than upper for ", name, ": ", .default_format(lower),
       " (", lower_source, ") and ", .default_format(upper), " (", upper_source, ")",
       call. = FALSE)
}

# Whether the kernel is defined at zero travel (the power kernel and a declared
# kernel with zero_distance = FALSE are not).
.default_zero_ok <- function(family, kernel) {
  !(identical(family, "power") || (!is.null(kernel) && !isTRUE(kernel$zero_distance)))
}

# The parts of the model the search needs, per demand group (or one unit for an
# ungrouped model): the kernel as the engine evaluates it, the travel matrix the
# engine uses (zero-demand rows of a group cleaned as in the grouped map), the
# demand and the names of the parameters that apply to it.
.default_search_units <- function(problem) {
  spec <- problem$metadata$spec
  substrate <- problem$substrate
  groups <- spec[["groups", exact = TRUE]]
  build <- function(id, family, kernel, sub, demand, sigma_name, v0_name) {
    list(id = id, family = family, kernel = kernel,
         method = if (is.null(kernel)) family else kernel, name = family,
         zero_ok = .default_zero_ok(family, kernel),
         lower = if (is.null(kernel)) 0 else kernel$lower,
         upper = if (is.null(kernel)) Inf else kernel$upper,
         fun = if (!is.null(kernel)) .decay_kernel_function(kernel),
         plan = if (!is.null(kernel)) .decay_kernel_plan(sub$distance_active),
         D = as.numeric(demand), sigma_name = sigma_name, v0_name = v0_name, sub = sub)
  }
  if (is.null(groups)) {
    kernel <- spec[["kernel", exact = TRUE]]
    return(list(build(NA_character_, spec$family, kernel, substrate,
                      substrate$D_active, "sigma", "v0")))
  }
  by_group <- spec[["by_group", exact = TRUE]]
  lapply(seq_along(groups), function(i) {
    g <- groups[[i]]
    f <- spec$families[[g]]
    kernel <- if (inherits(f, "spax_decay_kernel")) f
    family <- if (is.null(kernel)) f else kernel$name
    demand <- substrate$D_groups[, i]
    distance <- .group_clean_distance(substrate$distance_groups[[i]], demand > 0,
                                      .default_zero_ok(family, kernel))
    attr(distance, "blanked_rows") <- NULL
    build(g, family, kernel,
          list(distance_active = distance, input_policy = substrate$input_policy),
          demand,
          if ("sigma" %in% by_group) paste0("sigma.", g) else "sigma",
          if ("v0" %in% by_group) paste0("v0.", g) else "v0")
  })
}

.default_kernel_weights <- function(unit, sigma) {
  .allocation_kernel(unit$sub, unit$family, sigma, kernel = unit$kernel,
                     fun = unit$fun, plan = unit$plan)
}

# Domain check before any scan: a kernel undefined at zero travel with a zero
# among the travel times is refused with the engine's own message.
.default_domain_check <- function(units) {
  for (u in units) {
    if (u$zero_ok || !any(u$sub$distance_active == 0, na.rm = TRUE)) next
    trial <- min(max(1, u$lower), u$upper)
    tryCatch(.default_kernel_weights(u, trial), error = function(e) {
      stop(conditionMessage(e), call. = FALSE)
    })
  }
  invisible(TRUE)
}

# Smallest positive finite travel, largest nearest travel and largest finite
# travel, over the origins with positive demand and a finite travel time.
.default_unit_travel <- function(unit) {
  d <- unit$sub$distance_active[unit$D > 0, , drop = FALSE]
  d[!is.finite(d)] <- Inf
  nearest <- if (nrow(d)) apply(d, 1L, min) else numeric()
  has <- is.finite(nearest)
  if (!any(has)) return(NULL)
  finite <- d[has, , drop = FALSE]
  finite <- finite[is.finite(finite)]
  positive <- finite[finite > 0]
  list(d_lo = if (length(positive)) min(positive) else NA_real_,
       far = max(nearest[has]), top = max(finite))
}

# The summaries of several units (a shared sigma over groups) combine as the
# union of their origins.
.default_travel_summary <- function(units) {
  parts <- Filter(Negate(is.null), lapply(units, .default_unit_travel))
  if (!length(parts)) stop(.default_search_message$no_travel, call. = FALSE)
  d_lo <- suppressWarnings(min(vapply(parts, `[[`, numeric(1), "d_lo"), na.rm = TRUE))
  if (!is.finite(d_lo)) d_lo <- NA_real_
  far <- max(vapply(parts, `[[`, numeric(1), "far"))
  list(d_lo = d_lo, far = if (is.na(d_lo)) far else max(far, d_lo),
       top = max(vapply(parts, `[[`, numeric(1), "top")))
}

# The range of sigma over which the kernel decays over the travel times, found
# by evaluating the engine's kernel at the nearest, a far and the largest travel
# time on a fixed probe grid (kept inside the kernel's bounds and the given
# sigma bounds). `steep_first` says whether the range runs from the steepest to
# the flattest kernel in ascending order of sigma.
.default_probe_range <- function(unit, travel, given_lower, given_upper) {
  fail <- function() {
    stop("no sigma value makes the ", unit$name, " kernel decay over these travel ",
         "times; give lower and upper for sigma", call. = FALSE)
  }
  if (is.na(travel$d_lo)) fail()
  at <- c(travel$d_lo, travel$far, travel$top)
  weights <- function(sigma, distance = at) {
    w <- tryCatch(suppressWarnings(as.numeric(calc_decay(distance, method = unit$method,
      sigma = sigma, snap = TRUE))), error = function(e) rep(NA_real_, length(distance)))
    if (length(w) != length(distance)) rep(NA_real_, length(distance)) else w
  }
  lo <- max(unit$lower, given_lower)
  hi <- min(unit$upper, given_upper)
  probes <- .default_probe_grid[.default_probe_grid > 0 & .default_probe_grid >= lo &
                                  .default_probe_grid <= hi]
  ok <- vapply(probes, function(s) {
    w <- weights(s)
    # The largest travel may underflow to zero weight (the kernel is then steep
    # enough); the nearest and the far travel must keep a positive weight.
    all(is.finite(w)) && w[1L] > 0 && w[2L] > 0 && w[3L] >= 0 &&
      log(w[2L]) - log(w[1L]) >= -200 && w[3L] / w[1L] <= 0.99
  }, logical(1))
  if (!any(ok)) fail()
  range <- range(probes[ok])
  far_ratio <- function(s) {
    w <- weights(s, at[1:2])
    w[2L] / w[1L]
  }
  list(range = range, steep_first = isTRUE(far_ratio(range[1L]) <= far_ratio(range[2L])))
}

.default_weighted_median <- function(x, w) {
  o <- order(x)
  x <- x[o]
  w <- w[o]
  x[which(cumsum(w) >= 0.5 * sum(w))[1L]]
}

#' Default starting values and bounds for an allocation fit
#'
#' The default search of [fit_allocation()] (SPAX-058): a rule that takes the
#' starting values and the bounds of the fitted parameters from the data. It is
#' called after the loss is resolved and the target is bound once.
#'
#' 1. The domain of every kernel is checked against the travel times first.
#' 2. For each free sigma, the engine's kernel is probed at the smallest positive
#'    travel, a far travel and the largest travel on `10^seq(-8, 8, by = 1/8)`;
#'    a value qualifies when the weights at the nearest and the far travel are
#'    positive (the largest travel may underflow to zero), the nearest facility of
#'    every origin keeps weight (`log w(far) - log w(d_lo) >= -200`) and the kernel
#'    still decays over the range (`w(top) / w(d_lo) <= 0.99`).
#' 3. Each free sigma gets a grid of 15 log-spaced values over its range, from
#'    the steepest to the flattest kernel. Group-specific sigmas walk together.
#' 4. A fitted outside weight v0 is seeded from the observed outside share on the
#'    target mask, `s / (1 - s)` times the demand-weighted median of the total
#'    opportunity of the origins, times 0.1, 1 and 10. A fully observed target
#'    with `s <= 0` is an error, and with `s < 0.001` a warning.
#' 5. The loss is evaluated at every grid point by the fitter's own code path.
#' 6. The starts are the best point of up to three sigma values: local minima
#'    along the grid first, ordered by loss.
#' 7. Default bounds: the sigma range, `[min / 1000, max * 1000]` of the best v0
#'    values (the lower one not below the seed for an outside share of 1e-6, at
#'    the grid point with the lowest loss), `[0.05, 5]` for beta; given entries
#'    replace the default ones.
#'
#' With no free sigma (every sigma held through `fixed`) there is no probe and no
#' sigma grid: one point at the held values, whose starts are the three v0 seeds
#' (or beta = 0.5, 1, 2 when only beta is free).
#'
#' @param problem A corrected CLM `ae_problem`.
#' @param target The bound target (`.bind_problem_target()`).
#' @param output The count output being fitted.
#' @param loss,loss_args The resolved loss function and its arguments.
#' @param lower,upper `NULL`, or named bounds for some of the parameters.
#' @param solver_args Solver controls given to the fit: `lambda`, `tol`,
#'   `max_iter`, `norm`, `check`.
#' @param starts The starts given by the caller, or `NULL`. Given starts widen
#'   the default bounds; the scan runs only to find default bounds of v0.
#' @return A list with the `starts` matrix (`NULL` when `starts` were given),
#'   the merged `lower` and `upper` and the `record` kept in the fit's
#'   `default_search` field (`NULL` when no scan ran).
#' @keywords internal
#' @md
.allocation_default_search <- function(problem, target, output, loss, loss_args,
                                       lower = NULL, upper = NULL,
                                       solver_args = list(), starts = NULL) {
  spec <- problem$metadata$spec
  substrate <- problem$substrate
  theta_names <- problem$theta$names
  contract_lower <- stats::setNames(problem$theta$lower, theta_names)
  contract_upper <- stats::setNames(problem$theta$upper, theta_names)
  lower_given <- .default_given_bounds(lower, theta_names, "lower")
  upper_given <- .default_given_bounds(upper, theta_names, "upper")
  for (nm in intersect(names(lower_given), names(upper_given))) {
    if (lower_given[[nm]] >= upper_given[[nm]]) {
      .default_crossing_error(nm, lower_given[[nm]], upper_given[[nm]], "given", "given")
    }
  }
  sigma_names <- theta_names[theta_names == "sigma" | startsWith(theta_names, "sigma.")]
  v0_names <- theta_names[theta_names == "v0" | startsWith(theta_names, "v0.")]
  fit_beta <- "beta" %in% theta_names
  # The scan is needed for the starts, and for a default bound of v0.
  scan <- is.null(starts) || any(!v0_names %in% names(lower_given)) ||
    any(!v0_names %in% names(upper_given))

  if (!length(target$values) || !(sum(target$values) > 0)) {
    stop("the observed counts are all zero or unobserved, so there is nothing to ",
         "calibrate against", call. = FALSE)
  }
  units <- .default_search_units(problem)
  .default_domain_check(units)
  fixed <- spec[["fixed", exact = TRUE]]
  if (is.null(fixed)) fixed <- stats::setNames(numeric(), character())

  # Sigma ranges ---------------------------------------------------------------
  ranges <- list()
  for (nm in sigma_names) {
    covered <- Filter(function(u) identical(u$sigma_name, nm), units)
    travel <- .default_travel_summary(covered)
    probe <- .default_probe_range(covered[[1L]], travel,
      if (nm %in% names(lower_given)) lower_given[[nm]] else 0,
      if (nm %in% names(upper_given)) upper_given[[nm]] else Inf)
    ranges[[nm]] <- probe
  }

  # Beta, provisionally merged (the final check follows the widening by starts).
  beta_lower <- if ("beta" %in% names(lower_given)) lower_given[["beta"]] else .default_beta_bounds[1L]
  beta_upper <- if ("beta" %in% names(upper_given)) upper_given[["beta"]] else .default_beta_bounds[2L]
  beta_start <- if (fit_beta && beta_lower < beta_upper) {
    min(max(1, beta_lower), beta_upper)
  } else 1

  default_lower <- default_upper <- stats::setNames(rep(NA_real_, length(theta_names)),
                                                    theta_names)
  for (nm in sigma_names) {
    default_lower[[nm]] <- ranges[[nm]]$range[1L]
    default_upper[[nm]] <- ranges[[nm]]$range[2L]
  }
  if (fit_beta) {
    default_lower[["beta"]] <- .default_beta_bounds[1L]
    default_upper[["beta"]] <- .default_beta_bounds[2L]
  }

  record <- NULL
  chosen <- NULL
  if (scan) {
    scanned <- .default_scan(problem, target, output, loss, loss_args, units, ranges,
      sigma_names, v0_names, fit_beta, beta_start, c(beta_lower, beta_upper), fixed,
      lower_given, upper_given, solver_args)
    record <- scanned$record
    chosen <- scanned$starts
    for (nm in v0_names) {
      values <- scanned$best[[nm]]
      values <- values[is.finite(values) & values > 0]
      if (!length(values)) stop(.default_search_message$no_v0, call. = FALSE)
      default_lower[[nm]] <- max(min(values) / 1000, scanned$v0_floor[[nm]])
      default_upper[[nm]] <- max(values) * 1000
    }
  }

  # Merge: given entries replace the default ones; given starts widen the
  # default entries; defaults are clamped into the contract.
  if (!is.null(starts) && is.matrix(starts) && is.numeric(starts) &&
      !is.null(colnames(starts))) {
    for (nm in intersect(colnames(starts), theta_names)) {
      values <- starts[, nm]
      values <- values[is.finite(values)]
      if (!length(values)) next
      default_lower[[nm]] <- min(default_lower[[nm]], min(values))
      default_upper[[nm]] <- max(default_upper[[nm]], max(values))
    }
  }
  default_lower <- pmax(default_lower, contract_lower)
  default_upper <- pmin(default_upper, contract_upper)
  merged_lower <- merged_upper <- stats::setNames(numeric(length(theta_names)), theta_names)
  for (nm in theta_names) {
    lower_source <- if (nm %in% names(lower_given)) "given" else "default"
    upper_source <- if (nm %in% names(upper_given)) "given" else "default"
    merged_lower[[nm]] <- if (lower_source == "given") lower_given[[nm]] else default_lower[[nm]]
    merged_upper[[nm]] <- if (upper_source == "given") upper_given[[nm]] else default_upper[[nm]]
    if (merged_lower[[nm]] >= merged_upper[[nm]]) {
      .default_crossing_error(nm, merged_lower[[nm]], merged_upper[[nm]], lower_source,
                              upper_source)
    }
  }

  if (!is.null(chosen)) {
    chosen <- pmin(pmax(chosen, rep(merged_lower, each = nrow(chosen))),
                   rep(merged_upper, each = nrow(chosen)))
    chosen <- chosen[!duplicated(chosen), , drop = FALSE]
    rownames(chosen) <- paste0("scan", seq_len(nrow(chosen)))
    record$starts <- chosen
    record$bounds <- list(lower = merged_lower, upper = merged_upper)
  }
  list(starts = if (is.null(starts)) chosen, lower = merged_lower, upper = merged_upper,
       record = if (is.null(starts)) record)
}

# The grid scan: seeds, losses and the starts chosen from them.
.default_scan <- function(problem, target, output, loss, loss_args, units, ranges,
                          sigma_names, v0_names, fit_beta, beta_start, beta_bounds, fixed,
                          lower_given, upper_given, solver_args) {
  spec <- problem$metadata$spec
  substrate <- problem$substrate
  theta_names <- problem$theta$names
  groups <- spec[["groups", exact = TRUE]]
  solver_args <- solver_args[names(solver_args) %in% .default_solver_args]
  if (is.null(solver_args$check)) solver_args$check <- FALSE

  # Grids: 15 values per free sigma over its range, steep to flat, walked together.
  n_sigma <- if (length(sigma_names)) .default_grid_points else 1L
  grids <- lapply(ranges, function(r) {
    values <- exp(seq(log(r$range[1L]), log(r$range[2L]), length.out = n_sigma))
    values <- pmin(pmax(values, r$range[1L]), r$range[2L])
    if (r$steep_first) values else rev(values)
  })
  # Beta is 1 at every grid point while fitted (clamped into its bounds).
  beta_seed <- if (fit_beta) beta_start else if ("beta" %in% names(fixed)) {
    fixed[["beta"]]
  } else spec$beta
  attractiveness <- .allocation_supply(substrate, spec$kappa, beta_seed)
  n_facility <- length(attractiveness)
  # With no free sigma there is no probe and no sigma grid: one point at the held
  # values. The points are then the three v0 seeds, or, with only beta free, beta
  # at 0.5, 1 and 2 (clamped into its bounds).
  no_sigma <- !length(sigma_names)
  combos <- if (length(v0_names)) {
    lapply(.default_v0_multipliers, function(m) list(m = m, beta = beta_start))
  } else if (no_sigma && fit_beta) {
    beta_values <- c(0.5, 1, 2)
    if (beta_bounds[1L] < beta_bounds[2L]) {
      beta_values <- pmin(pmax(beta_values, beta_bounds[1L]), beta_bounds[2L])
    }
    lapply(beta_values, function(b) list(m = 1, beta = b))
  } else {
    list(list(m = 1, beta = beta_start))
  }
  fully_observed <- all(target$mask)
  observed <- as.numeric(target$observed)
  n_rows <- if (is.null(target$dim)) target$length else target$dim[1L]
  mask <- matrix(target$mask, nrow = n_rows)
  guarded <- FALSE
  first_error <- NULL
  remember <- function(e) {
    if (is.null(first_error)) first_error <<- conditionMessage(e)
    NULL
  }

  solve_args <- c(list(problem = problem, x0 = NULL, keep_history = FALSE, warn = FALSE,
                       diagnostics = FALSE, requested_outputs = output), solver_args)
  loss_at <- function(theta) {
    tryCatch({
      fit <- suppressWarnings(do.call(.solve_problem, c(solve_args, list(theta = theta))))
      if (!isTRUE(fit$converged)) return(Inf)
      full <- .problem_fit_output(fit, output, required = FALSE)
      if (is.null(full) || !.fit_target_shape_ok(full, target)) return(Inf)
      value <- suppressWarnings(do.call(loss, c(list(predicted = as.numeric(full)[target$mask],
                                                     observed = target$values), loss_args)))
      if (is.numeric(value) && length(value) == 1L && is.finite(value)) as.numeric(value) else Inf
    }, error = function(e) {
      remember(e)
      Inf
    })
  }

  sigma_of <- function(u, point) {
    if (u$sigma_name %in% names(point)) point[[u$sigma_name]] else fixed[[u$sigma_name]]
  }

  # The opportunity W_i = sum_j a_j K_ij of each origin and the modelled counts
  # without an outside option, per group, at one sigma point (NULL if the kernel
  # fails there).
  states_at <- function(point) {
    tryCatch(lapply(units, function(u) {
      K <- .default_kernel_weights(u, sigma_of(u, point))
      O <- K * rep(attractiveness, each = nrow(K))
      W <- rowSums(O)
      list(W = W, D = u$D, F0 = O * ifelse(W > 0, u$D / W, 0))
    }), error = remember)
  }

  # The seed of one v0 parameter: s / (1 - s) times the demand-weighted median
  # opportunity, with s the observed outside share on the target mask (guarded for
  # a fully observed target, clamped otherwise).
  seed_one <- function(states, selected, flat_mask, modelled, label, owner) {
    observed_sum <- sum(observed[flat_mask])
    modelled_sum <- sum(modelled[flat_mask])
    if (!(observed_sum > 0)) {
      stop("the observed counts", if (nzchar(label)) paste0(" of ", label), " are all zero ",
           "or unobserved, so there is nothing to calibrate against", call. = FALSE)
    }
    w <- unlist(lapply(selected, function(i) states[[i]]$W[states[[i]]$W > 0 & states[[i]]$D > 0]))
    d <- unlist(lapply(selected, function(i) states[[i]]$D[states[[i]]$W > 0 & states[[i]]$D > 0]))
    if (!(modelled_sum > 0) || !length(w)) {
      stop("no origin reaches a facility with positive attractiveness; give starts",
           call. = FALSE)
    }
    s <- 1 - observed_sum / modelled_sum
    if (!fully_observed) {
      s <- min(max(s, 1e-6), 1 - 1e-6)
    } else if (!guarded) {
      if (s <= 0) stop(.default_search_message$outside_zero, call. = FALSE)
      if (s < 0.001) {
        warning(.default_search_message$outside_small,
                if (nzchar(owner)) paste0(" (", owner, ")"), call. = FALSE)
      }
    }
    median <- unname(.default_weighted_median(w, d))
    c(seed = s / (1 - s) * median, median = median)
  }

  seeds_at <- function(states) {
    pooled <- Reduce(`+`, lapply(states, `[[`, "F0"))
    live <- Reduce(`|`, lapply(states, function(s) s$W > 0 & s$D > 0))
    modelled <- switch(output,
      flow = pooled,
      utilization = colSums(pooled),
      utilization_by_group = vapply(states, function(s) colSums(s$F0), numeric(n_facility)))
    shared_mask <- mask
    if (identical(output, "flow")) shared_mask[!live, ] <- FALSE
    out <- med <- stats::setNames(numeric(length(v0_names)), v0_names)
    for (nm in v0_names) {
      one <- if (!is.null(groups) && nm != "v0") {
        i <- match(substring(nm, 4L), groups)
        group_mask <- matrix(FALSE, nrow(mask), ncol(mask))
        group_mask[, i] <- mask[, i]
        seed_one(states, i, as.vector(group_mask), as.numeric(modelled),
                 paste0("group '", groups[[i]], "'"), nm)
      } else {
        seed_one(states, seq_along(units), as.vector(shared_mask), as.numeric(modelled),
                 "", "")
      }
      out[[nm]] <- one[["seed"]]
      med[[nm]] <- one[["median"]]
    }
    guarded <<- TRUE
    # The demand-weighted median opportunity goes with the seeds: it prices the
    # lowest outside weight worth searching (the seed for an outside share 1e-6).
    attr(out, "median") <- med
    out
  }

  rows <- list()
  medians <- list()
  for (k in seq_len(n_sigma)) {
    point <- vapply(grids, function(g) g[[k]], numeric(1))
    states <- states_at(point)
    seeds <- if (length(v0_names) && !is.null(states)) seeds_at(states)
    if (!is.null(seeds)) medians[[k]] <- attr(seeds, "median")
    for (combo in combos) {
      v0 <- if (length(v0_names)) {
        if (is.null(seeds)) stats::setNames(rep(NA_real_, length(v0_names)), v0_names) else {
          clamp_lo <- ifelse(v0_names %in% names(lower_given), lower_given[v0_names], 0)
          clamp_hi <- ifelse(v0_names %in% names(upper_given), upper_given[v0_names], Inf)
          pmin(pmax(combo$m * seeds, clamp_lo), clamp_hi)
        }
      }
      theta <- c(point, v0, if (fit_beta) c(beta = combo$beta))
      value <- if (is.null(states) || anyNA(theta)) Inf else loss_at(theta[theta_names])
      rows[[length(rows) + 1L]] <- c(point = k, theta[theta_names], loss = value)
    }
  }
  grid <- as.data.frame(do.call(rbind, rows))
  grid$point <- as.integer(grid$point)
  if (!any(is.finite(grid$loss))) {
    stop(if (is.null(first_error)) .default_search_message$no_finite else first_error,
         call. = FALSE)
  }
  finite <- grid[is.finite(grid$loss), , drop = FALSE]
  if (no_sigma) {
    # One grid point: every finite seed is a start, in order.
    best <- finite[which.min(finite$loss), , drop = FALSE]
    chosen <- as.matrix(finite[, theta_names, drop = FALSE])
  } else {
    best <- do.call(rbind, lapply(split(finite, finite$point), function(x) {
      x[which.min(x$loss), , drop = FALSE]
    }))
    best <- best[order(best$point), , drop = FALSE]
    L <- best$loss
    n <- length(L)
    local_min <- which(vapply(seq_len(n), function(i) {
      L[i] <= min(L[max(1L, i - 1L):min(n, i + 1L)])
    }, logical(1)))
    ranked <- order(L)
    pick <- c(local_min[order(L[local_min])], ranked[!ranked %in% local_min])
    pick <- pick[seq_len(min(3L, length(pick)))]
    chosen <- as.matrix(best[pick, theta_names, drop = FALSE])
  }
  rownames(chosen) <- NULL

  # A single free sigma has a range c(lower, upper); several, a list of those
  # (named by parameter); none, c(NA, NA).
  pair <- function(r) c(lower = r$range[1L], upper = r$range[2L])
  sigma_range <- if (no_sigma) {
    c(NA_real_, NA_real_)
  } else if (length(ranges) == 1L) {
    pair(ranges[[1L]])
  } else {
    lapply(ranges, pair)
  }
  # The lowest outside weight worth searching: the seed for an outside share of 1e-6
  # at the grid point with the lowest loss. Below it the outside option takes less
  # than a millionth of the demand and v0 cannot be told from zero.
  v0_floor <- NULL
  if (length(v0_names)) {
    top <- finite$point[which.min(finite$loss)]
    if (top <= length(medians) && !is.null(medians[[top]])) {
      v0_floor <- .default_v0_floor_share / (1 - .default_v0_floor_share) * medians[[top]]
    }
  }
  list(starts = chosen, best = best, v0_floor = v0_floor,
       record = list(rule = .default_search_rule, sigma_range = sigma_range,
                     n_points = nrow(grid), n_finite = nrow(finite), grid = grid,
                     starts = NULL, bounds = NULL, controls = NULL))
}
