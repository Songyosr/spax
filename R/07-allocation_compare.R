#' Compare fitted allocations across model and input specifications
#'
#' Summarizes several fitted allocations side by side: regional contact,
#' attributed capacity and its per-contact value, the spread of contact across
#' origins, agreement between predicted and observed counts, and the fitted
#' loss. Each fit's coverage is computed from its saved outputs; nothing is
#' refitted.
#'
#' Loss differences from the reference are reported only when the comparison is
#' like for like: the same observed targets, loss function and arguments
#' (Poisson fits: the same floor, a fit that stored none used 1e-9, or floors
#' that change neither loss), demand, supply, origin and facility IDs and
#' prepared travel times. Travel times are checked only when `models` are
#' supplied, and each model must reproduce its own fit; otherwise loss
#' differences are left missing. Fits of models with demand groups
#' ([prepare_allocation()]) are comparable only with each other, with the same
#' group IDs and, once `models` are supplied, the same demand and the same
#' travel times for every group; a shared-parameter fit and a `by_group` fit on
#' the same group-labelled target answer whether the group decays differ. The
#' table then shows the free parameters, such as `sigma.<group>`, and coverage
#' comes from the pooled outputs. Contact and capacity values remain comparable
#' across specifications, but they are conditional on each specification's
#' inputs.
#'
#' With `zones`, each fit is also summarized by zone using contact-weighted
#' aggregation, and the zones whose reporting class differs from the reference
#' are listed. Classes require an adequacy cut in `coverage_args`; each fit uses
#' its own regional contact as the contact cut unless `contact_cut` is given.
#'
#' @param fits A named list of `ae_multistart_fit` results from
#'   [fit_allocation()], each with a selected fit.
#' @param models Optional named list of the corresponding models from
#'   [clm_allocation()], with the same names, used to verify that travel times
#'   are identical before reporting loss differences.
#' @param reference Name or position of the reference fit.
#' @param zones Optional data frame with `origin_id` and `zone` columns
#'   covering every fitted origin.
#' @param coverage_args Named reporting controls passed to the coverage
#'   calculation for every fit, such as `norm`, `rho_floor`,
#'   `contact_need_floor`, `contact_cut` and `adequacy_cut`.
#' @return An `ae_allocation_comparison` with a `table` (one row per fit), a
#'   long `zones` table when zones are supplied, and the reference name. Use
#'   `print()` or `as.data.frame(type = "fits")` / `as.data.frame(type = "zones")`.
#' @family experimental allocation workflow
#' @export
#' @md
compare_allocations <- function(fits, models = NULL, reference = 1L, zones = NULL,
                                coverage_args = list()) {
  if (!is.list(fits) || inherits(fits, "ae_multistart_fit") || length(fits) < 2L ||
      is.null(names(fits)) || anyNA(names(fits)) || any(!nzchar(names(fits))) ||
      anyDuplicated(names(fits))) {
    stop("fits must be a named list of at least two allocation fits with unique names")
  }
  for (nm in names(fits)) .chck_class(fits[[nm]], "ae_multistart_fit", paste0("fits$", nm))
  if (!is.null(models)) {
    if (!is.list(models) || !setequal(names(models), names(fits))) {
      stop("models must be a list with the same names as fits")
    }
    for (nm in names(models)) .check_allocation_model(models[[nm]])
  }
  if (is.character(reference)) {
    if (length(reference) != 1L || !reference %in% names(fits)) stop("reference must name a fit")
  } else {
    if (!is.numeric(reference) || length(reference) != 1L || reference < 1 ||
        reference > length(fits) || reference != round(reference)) {
      stop("reference must name a fit or give its position")
    }
    reference <- names(fits)[reference]
  }
  if (!is.list(coverage_args) || (length(coverage_args) &&
      (is.null(names(coverage_args)) || any(!nzchar(names(coverage_args))) ||
       anyDuplicated(names(coverage_args)) ||
       any(!names(coverage_args) %in% setdiff(names(formals(.ae_coverage)), "fit"))))) {
    stop("coverage_args must contain unique named coverage controls")
  }
  best <- lapply(fits, .selected_allocation_fit)
  if (!is.null(models)) {
    # Each model must reproduce its own fit before its travel times are trusted.
    for (nm in names(fits)) .allocation_fit_context(models[[nm]], best[[nm]])
  }
  coverage <- lapply(best, function(b) do.call(.ae_coverage, c(list(fit = b), coverage_args)))

  masked_values <- function(b, what) {
    mask <- if (is.null(b$target_mask)) rep(TRUE, length(b$observed)) else b$target_mask
    as.numeric(b[[what]])[mask]
  }
  ref <- best[[reference]]
  # Fits of models with demand groups are compared only with each other: the
  # same group IDs and, once models are supplied, the same demand and travel by
  # group (the fits keep only the total demand). Without any grouped fit the
  # checks and the reasons are exactly those of an ungrouped comparison.
  any_grouped <- any(vapply(best, function(b) !is.null(b[["groups", exact = TRUE]]), logical(1)))
  same <- function(nm) {
    b <- best[[nm]]
    s <- same_ungrouped(nm, b)
    if (!any_grouped) return(s)
    # NULL for both ungrouped fits, a grouped and an ungrouped fit never match
    groups_match <- identical(b[["groups", exact = TRUE]], ref[["groups", exact = TRUE]])
    sub <- if (!is.null(models)) models[[nm]]$substrate
    ref_sub <- if (!is.null(models)) models[[reference]]$substrate
    c(s[c("observed", "ids", "loss")], groups = groups_match,
      s["demand"],
      demand_groups = if (is.null(models)) NA else groups_match &&
        identical(sub$D_groups, ref_sub$D_groups),
      s["supply"],
      travel = if (is.null(models)) NA else groups_match &&
        # per-group matrices; distance_active is NULL when travel is given by group
        if (is.null(b[["groups", exact = TRUE]])) {
          identical(sub$distance_active, ref_sub$distance_active)
        } else {
          identical(sub$distance_groups, ref_sub$distance_groups)
        })
  }
  same_ungrouped <- function(nm, b) {
    c(observed = identical(masked_values(b, "observed"), masked_values(ref, "observed")) &&
        identical(b$target_mask, ref$target_mask) &&
        # canonical IDs and axes only; the caller's input order may differ
        identical(b$target_alignment[c("output", "axes", "ids")],
                  ref$target_alignment[c("output", "axes", "ids")]),
      ids = identical(as.character(b$coverage_meta$facility_ids),
                      as.character(ref$coverage_meta$facility_ids)) &&
        identical(coverage[[nm]]$cells$origin_id, coverage[[reference]]$cells$origin_id),
      loss = .same_function(b$loss_fn, ref$loss_fn) && .same_loss_args(b, ref) &&
        identical(b$output, ref$output),
      demand = identical(as.numeric(b$coverage_meta$demand), as.numeric(ref$coverage_meta$demand)),
      supply = identical(as.numeric(b$coverage_meta$supply), as.numeric(ref$coverage_meta$supply)),
      travel = if (is.null(models)) NA else
        identical(models[[nm]]$substrate$distance_active, models[[reference]]$substrate$distance_active))
  }
  rows <- lapply(names(fits), function(nm) {
    b <- best[[nm]]
    reg <- coverage[[nm]]$regional
    cells <- coverage[[nm]]$cells
    s <- same(nm)
    checked <- intersect(c("observed", "ids", "loss", "groups", "demand", "supply"), names(s))
    comparable <- all(s[checked]) && isTRUE(s[["travel"]]) &&
      (!"demand_groups" %in% names(s) || isTRUE(s[["demand_groups"]]))
    pred <- masked_values(b, "predicted")
    obs <- masked_values(b, "observed")
    data.frame(
      specification = nm, family = b$family,
      parameters = paste(sprintf("%s=%.4g", names(b$theta), b$theta), collapse = ", "),
      contact = reg$rho[1], intensity = reg$A[1], capacity_per_contact = reg$abar_raw[1],
      cv_contact = stats::sd(cells$rho) / mean(cells$rho),
      count_correlation = if (length(obs) > 1L && stats::sd(pred) > 0) stats::cor(pred, obs) else NA_real_,
      loss = b$loss,
      loss_difference = if (comparable) b$loss - ref$loss else NA_real_,
      comparable = comparable,
      not_comparable = if (comparable) "" else paste(c(names(s)[!is.na(s) & !s],
        if (is.na(s[["travel"]])) {
          paste0(if (any_grouped) "demand by group and ", "travel not checked (supply models)")
        }), collapse = ", "),
      stringsAsFactors = FALSE)
  })
  table <- do.call(rbind, rows)
  zone_table <- NULL
  if (!is.null(zones)) {
    zone_table <- do.call(rbind, lapply(names(fits), function(nm) {
      z <- .coverage_aggregate(coverage[[nm]], zones)
      data.frame(specification = nm, z[, c("zone", "demand", "rho", "A", "abar", "quadrant")],
                 row.names = NULL, stringsAsFactors = FALSE)
    }))
    ref_z <- zone_table[zone_table$specification == reference, c("zone", "quadrant")]
    table$zones_changed <- vapply(names(fits), function(nm) {
      z <- zone_table[zone_table$specification == nm, c("zone", "quadrant")]
      q <- ref_z$quadrant[match(z$zone, ref_z$zone)]
      ch <- z$quadrant != q
      if (any(ch)) paste(sprintf("%s (%s)", z$zone[ch], z$quadrant[ch]), collapse = "; ") else "none"
    }, character(1))
  }
  structure(list(table = table, zones = zone_table, reference = reference),
            class = "ae_allocation_comparison")
}

#' @rdname compare_allocations
#' @param x An `ae_allocation_comparison`.
#' @param digits Significant digits for printed values.
#' @param ... Unused.
#' @export
#' @md
print.ae_allocation_comparison <- function(x, digits = 4, ...) {
  cat("<ae_allocation_comparison> reference: ", x$reference, "\n", sep = "")
  cols <- intersect(c("specification", "contact", "capacity_per_contact", "cv_contact",
                      "count_correlation", "loss_difference", "zones_changed"), names(x$table))
  print(format(x$table[, cols], digits = digits), row.names = FALSE)
  if (any(!x$table$comparable)) {
    cat("Loss differences are shown only for like-for-like fits; see $table$not_comparable.\n")
  }
  invisible(x)
}

#' @rdname compare_allocations
#' @param row.names,optional Standard `as.data.frame()` arguments; unused.
#' @param type `"fits"` for one row per fit or `"zones"` for zone summaries.
#' @exportS3Method base::as.data.frame
#' @md
as.data.frame.ae_allocation_comparison <- function(x, row.names = NULL, optional = FALSE,
                                                   type = c("fits", "zones"), ...) {
  type <- match.arg(type)
  out <- if (type == "fits") x$table else x$zones
  if (is.null(out)) stop("no zone summaries; supply zones to compare_allocations()")
  out
}
