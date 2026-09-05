# Private post-fit coverage -------------------------------------------------

#' Decompose a fitted static CLM on its retained demand support
#'
#' Reuses saved allocation and utilization; no solve, raster extraction, or
#' geometry reconstruction. Requires fits with `coverage_meta` (older saved
#' fits must be regenerated). Supply is used in its original units, without
#' the attractiveness scale `kappa`. No supply or allocation matrix is retained
#' in the resulting object: subsequent summaries use the compact cell table.
#'
#' `norm` optionally defines both capped readings, `E_post = min(A/norm, 1)`
#' and `E_fac = P %*% min(r/norm, 1)`. Units are supplied by the caller.
#' `observed` is an optional named facility vector; names are matched exactly,
#' with NA permitted for unknown counts. Zero observed load has undefined
#' plug-in ratio (NA), distinct from the model's zero-contribution convention.
#'
#' `contact_cut` defaults to regional demand-weighted contact; `adequacy_cut`
#' defaults to `norm`. Without an adequacy cut, quadrants are unclassified.
#' Contact floors mask reported `abar` and quadrant, never `A`, `rho`, or
#' `abar_raw`. `contact_need_floor` applies to aggregates, not individual cells.
#' Cell/zone values exactly at a floor or classification cut are retained/high.
#' This first private layer accepts only the corrected static CLM allocation.
#' @keywords internal
.ae_coverage <- function(fit, norm = NULL, units = NULL, observed = NULL,
                         rho_floor = 0, contact_need_floor = 0,
                         contact_cut = NULL, adequacy_cut = norm) {
  .chck_class(fit, "ae_problem_nfxp_fit", "fit")
  if (!identical(fit$allocation_form, "clm")) {
    stop("coverage currently requires a static CLM fit")
  }
  meta <- fit$coverage_meta
  if (is.null(meta)) stop("fit has no coverage metadata; regenerate the fit")
  if (!is.null(norm)) .chck_positive_scalar(norm, "norm")
  if (!is.null(adequacy_cut)) .chck_positive_scalar(adequacy_cut, "adequacy_cut")
  .chck_nonnegative_scalar(rho_floor, "rho_floor")
  .chck_nonnegative_scalar(contact_need_floor, "contact_need_floor")
  if (rho_floor > 1) stop("rho_floor must be <= 1")
  if (!is.null(units) && (!is.character(units) || length(units) != 1L || is.na(units))) {
    stop("units must be one nonmissing string")
  }
  P <- fit$outputs$allocation
  U <- as.numeric(fit$outputs$utilization)
  D <- meta$demand
  S <- as.numeric(meta$supply)
  ids <- meta$facility_ids
  cells <- fit$surface_meta$demand_kept_index
  if (!is.matrix(P) || !is.numeric(P) ||
      !identical(dim(P), c(length(D), length(S))) ||
      length(U) != length(S) || length(ids) != length(S) ||
      anyNA(ids) || anyDuplicated(ids) ||
      length(cells) != length(D) || anyNA(cells) || anyDuplicated(cells)) {
    stop("coverage inputs have incompatible axes or missing support metadata")
  }
  if (!length(D) || any(!is.finite(D) | D <= 0) ||
      any(!is.finite(S) | S < 0) || any(!is.finite(U) | U < 0) ||
      any(!is.finite(P) | P < 0)) stop("invalid coverage numeric inputs")
  rho <- rowSums(P)
  if (any(rho > 1 + 1e-10)) stop("allocation must be a subprobability matrix")
  implied <- .contract(D, P, over = "rows")
  if (any(abs(implied - U) > 1e-8 * pmax(1, abs(implied), abs(U)))) {
    stop("stored utilization is inconsistent with allocation and demand")
  }
  r <- numeric(length(S))
  supported <- U > 0
  r[supported] <- S[supported] / U[supported]
  A <- .contract(r, P, over = "cols")
  if (any(!is.finite(r)) || any(!is.finite(A))) stop("nonfinite coverage ratio or intensity")
  if (is.null(contact_cut)) contact_cut <- sum(D * rho) / sum(D)
  .chck_nonnegative_scalar(contact_cut, "contact_cut")
  if (contact_cut > 1 + 1e-10) stop("contact_cut must be <= 1")
  cell <- data.frame(cell = cells, demand = D, rho = rho, A = A)
  if (!is.null(norm)) {
    cell$E_post <- pmin(A / norm, 1)
    cell$E_fac <- .contract(pmin(r / norm, 1), P, over = "cols")
  }
  settings <- list(norm = norm, units = units, rho_floor = rho_floor,
                   contact_need_floor = contact_need_floor,
                   contact_cut = contact_cut, adequacy_cut = adequacy_cut)
  cell <- .coverage_reporting(cell, settings, aggregate = FALSE)
  facilities <- data.frame(facility = ids, supply = S, utilization = U,
                           r = r, supported = supported, stringsAsFactors = FALSE)
  if (!is.null(observed)) {
    if (!is.numeric(observed) || !is.null(dim(observed)) ||
        is.null(names(observed)) || anyNA(names(observed)) ||
        anyDuplicated(names(observed)) || !setequal(names(observed), ids) ||
        any(!is.na(observed) & (!is.finite(observed) | observed < 0))) {
      stop("observed must be a nonnegative named vector matching facility IDs; NA is allowed")
    }
    Y <- as.numeric(observed[match(ids, names(observed))])
    plugin <- rep(NA_real_, length(S))
    valid <- !is.na(Y) & Y > 0
    plugin[valid] <- S[valid] / Y[valid]
    facilities$observed <- Y
    facilities$r_plugin <- plugin
    facilities$r_gap <- r - plugin
  }
  total <- sum(D * A)
  supply <- sum(S[supported])
  relative <- if (supply > 0) (total - supply) / supply else total - supply
  ans <- structure(list(
    model = fit$model, family = fit$family, theta = fit$theta,
    allocation_form = fit$allocation_form, settings = settings,
    cells = cell, facilities = facilities,
    conservation = list(allocated = total, supported_supply = supply,
                        relative_difference = relative, passed = abs(relative) <= 1e-8,
                        excluded_facilities = sum(!supported)),
    surface_meta = fit$surface_meta, support = "fitted positive-demand cells"
  ), class = "ae_coverage")
  ans$regional <- .coverage_aggregate(ans)
  ans
}

#' Decompose a static CLM problem at specified parameters
#'
#' Solves once if `state` is absent; otherwise evaluates outputs at that state.
#' This entry point also supports older saved problems without refitting targets.
#' @keywords internal
.problem_coverage <- function(problem, theta, state = NULL, ...) {
  .chck_class(problem, "ae_problem", "problem")
  if (!identical(problem$metadata$spec$allocation, "clm")) {
    stop("coverage currently requires a static CLM problem")
  }
  outputs <- if (is.null(state)) .solve_problem(problem, theta)$outputs else
    .problem_outputs_at(problem, theta, state)
  fit <- structure(list(model = problem$model, family = problem$metadata$spec$family,
                        allocation_form = "clm", theta = theta, outputs = outputs,
                        coverage_meta = list(demand = problem$substrate$D_active,
                                             supply = problem$substrate$S,
                                             facility_ids = problem$substrate$facility_ids),
                        surface_meta = .problem_surface_meta(problem)),
                   class = "ae_problem_nfxp_fit")
  .ae_coverage(fit, ...)
}

.coverage_reporting <- function(tab, settings, aggregate) {
  tab$contact_need <- tab$demand * tab$rho
  tab$abar_raw <- rep(NA_real_, nrow(tab))
  contact <- tab$rho > 0
  tab$abar_raw[contact] <- tab$A[contact] / tab$rho[contact]
  tab$masked <- !contact | tab$rho < settings$rho_floor
  if (aggregate) tab$masked <- tab$masked | tab$contact_need < settings$contact_need_floor
  tab$abar <- tab$abar_raw
  tab$abar[tab$masked] <- NA_real_
  tab$quadrant <- rep("unclassified", nrow(tab))
  if (!is.null(settings$adequacy_cut)) {
    hi_rho <- tab$rho >= settings$contact_cut
    hi_a <- tab$abar_raw >= settings$adequacy_cut
    keep <- !tab$masked
    tab$quadrant[keep] <- ifelse(hi_rho[keep],
      ifelse(hi_a[keep], "adequate", "capacity-limited"),
      ifelse(hi_a[keep], "contact-limited", "both-low"))
  }
  tab
}

#' Aggregate compact coverage with contact-weighted conditional intensity
#'
#' `zones` is NULL for the region, or a data frame with unique `cell` keys and
#' nonmissing `zone` labels covering every retained cell. It may include cells
#' outside the fitted support, which contribute no records. Input order is
#' irrelevant. Unknown zone membership is an error, never silently dropped.
#' Floors are applied after aggregating raw values. All original demand stays
#' in the denominator, including low-contact/unreachable cells.
#' @keywords internal
.coverage_aggregate <- function(x, zones = NULL) {
  .chck_class(x, "ae_coverage", "x")
  d <- x$cells
  group <- rep("region", nrow(d))
  if (!is.null(zones)) {
    if (!is.data.frame(zones) || !all(c("cell", "zone") %in% names(zones)) ||
        anyNA(zones$cell) || anyDuplicated(zones$cell) || anyNA(zones$zone)) {
      stop("zones must contain unique cell keys and nonmissing zone labels")
    }
    pos <- match(d$cell, zones$cell)
    if (anyNA(pos)) stop("zones must cover every retained cell")
    group <- as.character(zones$zone[pos])
  }
  metrics <- intersect(c("rho", "A", "E_post", "E_fac"), names(d))
  totals <- rowsum(cbind(demand = d$demand,
                         as.matrix(d[metrics]) * d$demand), group, reorder = FALSE)
  out <- data.frame(zone = rownames(totals), demand = totals[, "demand"],
                    totals[, metrics, drop = FALSE] / totals[, "demand"],
                    row.names = NULL, check.names = FALSE)
  .coverage_reporting(out, x$settings, aggregate = TRUE)
}

#' Rewrap a coverage value on fitted demand support only
#'
#' Omitted zero-demand and missing-demand cells stay NA. This does not evaluate
#' prediction support or impute access outside the stored fit support.
#' Quadrant codes: 1 both-low, 2 capacity-limited, 3 contact-limited, 4 adequate;
#' unclassified cells are NA. Other output names are numeric cell-table columns.
#' @keywords internal
.coverage_surface <- function(x, output = "A") {
  .chck_class(x, "ae_coverage", "x")
  values <- x$cells
  values$quadrant <- match(values$quadrant,
                           c("both-low", "capacity-limited", "contact-limited", "adequate"))
  .rewrap_problem_surface(as.list(values), output, x$surface_meta$template,
                          x$surface_meta$demand_kept_index)
}

#' Format a private coverage object
#' @method format ae_coverage
#' @keywords internal
#' @export
format.ae_coverage <- function(x, digits = 4, ...) {
  h <- x$regional
  paste0("<ae_coverage> ", x$model, "/", x$family, " (",
         paste(names(x$theta), signif(x$theta, digits), sep = "=", collapse = ", "),
         "): A=", .ae_format_number(h$A, digits),
         if (!is.null(x$settings$units)) paste0(" ", x$settings$units),
         "; rho=", .ae_format_number(h$rho, digits),
         "; abar=", .ae_format_number(h$abar, digits))
}

#' Print a private coverage object
#' @method print ae_coverage
#' @keywords internal
#' @export
print.ae_coverage <- function(x, digits = 4, ...) {
  cat(format(x, digits = digits), "\n")
  if (!is.null(x$settings$norm)) {
    cat("E_post:", .ae_format_number(x$regional$E_post, digits),
        " E_fac:", .ae_format_number(x$regional$E_fac, digits), "\n")
  }
  cat("conservation:", if (x$conservation$passed) "PASS" else "FAIL",
      " relative difference:", .ae_format_number(x$conservation$relative_difference, digits), "\n")
  cat("facilities:", nrow(x$facilities), " excluded (zero load):",
      x$conservation$excluded_facilities, "\n")
  cat("support:", x$support, "\n")
  invisible(x)
}

#' Summarize a private coverage object
#' @method summary ae_coverage
#' @keywords internal
#' @exportS3Method base::summary
summary.ae_coverage <- function(object, ...) {
  numeric_summary <- function(v) {
    v <- v[is.finite(v)]
    if (length(v)) .ae_numeric_summary(v) else
      data.frame(min = NA_real_, q25 = NA_real_, median = NA_real_,
                 mean = NA_real_, q75 = NA_real_, max = NA_real_)
  }
  shares <- .coverage_aggregate(object, data.frame(cell = object$cells$cell,
                                                  zone = object$cells$quadrant))
  shares <- data.frame(quadrant = shares$zone, demand = shares$demand,
                        share = shares$demand / sum(shares$demand))
  structure(list(description = format(object), regional = object$regional,
                 conservation = object$conservation,
                 ratio = numeric_summary(object$facilities$r[object$facilities$supported]),
                 plugin_gap = if ("r_gap" %in% names(object$facilities))
                   numeric_summary(object$facilities$r_gap) else NULL,
                 quadrant_shares = shares), class = "summary.ae_coverage")
}

#' Print a private coverage summary
#' @method print summary.ae_coverage
#' @keywords internal
#' @export
print.summary.ae_coverage <- function(x, ...) {
  cat(x$description, "\n")
  print(x$regional)
  cat("Facility ratio (positive model load):\n")
  print(x$ratio)
  if (!is.null(x$plugin_gap)) {
    cat("Model minus observed plug-in ratio:\n")
    print(x$plugin_gap)
  }
  cat("Quadrant shares of all retained demand:\n")
  print(x$quadrant_shares)
  invisible(x)
}

#' Plot coverage surfaces on the retained fit support
#'
#' Coverage plots E_post when a norm is set, otherwise A. Adequacy plots the
#' reported (floored) abar. `all` draws the four named panels.
#' @method plot ae_coverage
#' @keywords internal
#' @exportS3Method graphics::plot
plot.ae_coverage <- function(x, type = c("coverage", "contact", "adequacy", "quadrant", "all"), ...) {
  type <- match.arg(type)
  if (type == "all") {
    old <- graphics::par(mfrow = c(2, 2))
    on.exit(graphics::par(old), add = TRUE)
    for (panel in c("coverage", "contact", "adequacy", "quadrant")) {
      plot(x, type = panel, ...)
    }
  } else {
    output <- switch(type, coverage = if (is.null(x$settings$norm)) "A" else "E_post",
                     contact = "rho", adequacy = "abar", quadrant = "quadrant")
    args <- list(...)
    if (is.null(args$main)) args$main <- switch(type,
      coverage = if (is.null(x$settings$norm)) "Intensity (A)" else "Coverage (E_post)",
      contact = "Contact (rho)", adequacy = "Intensity given contact (abar)",
      quadrant = "Contact and capacity classification")
    surface <- .coverage_surface(x, output)
    if (all(is.na(x$cells[[output]])) ||
        (type == "quadrant" && all(x$cells$quadrant == "unclassified"))) {
      graphics::plot.new()
      graphics::title(main = paste(type, "(no reportable cells)"))
    } else if (type == "quadrant") {
      levels(surface) <- data.frame(value = 1:4, quadrant =
        c("both-low", "capacity-limited", "contact-limited", "adequate"))
      if (is.null(args$legend)) args$legend <- "bottomleft"
      do.call(terra::plot, c(list(x = surface), args))
    } else do.call(terra::plot, c(list(x = surface), args))
  }
  invisible(x)
}
