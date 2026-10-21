# Private requested-output protocol -----------------------------------------

.check_output_names <- function(x, name) {
  if (!is.null(x) && (!is.character(x) || !length(x) || anyNA(x) ||
                     any(!nzchar(trimws(x))) || anyDuplicated(x))) {
    stop("`", name, "` must be NULL or unique nonempty output names")
  }
  invisible(TRUE)
}

.validate_output_capabilities <- function(step, output_axes = NULL) {
  requested <- step[["outputs_requested", exact = TRUE]]
  independent <- step[["state_independent_outputs", exact = TRUE]]
  if (!is.null(requested) && !is.function(requested)) {
    stop("`outputs_requested` must be a function when supplied")
  }
  .check_output_names(independent, "state_independent_outputs")
  if (!is.null(output_axes) &&
      any(!independent %in% names(output_axes))) {
    stop("`state_independent_outputs` must name declared output axes")
  }
  invisible(TRUE)
}

# NULL preserves the legacy callback exactly. Requested-output providers must
# return the usual contracted outputs too; the request adds optional outputs.
# Providers without this capability keep their existing outputs(x) behavior.
.problem_step_outputs <- function(step, x, requested_outputs = NULL,
                                   required = TRUE) {
  .check_output_names(requested_outputs, "requested_outputs")
  requested <- step[["outputs_requested", exact = TRUE]]
  outputs <- if (!is.null(requested_outputs) && is.function(requested)) {
    requested(x, requested_outputs)
  } else {
    step[["outputs", exact = TRUE]](x)
  }
  missing <- setdiff(requested_outputs, names(outputs))
  if (required && length(missing)) {
    stop("problem outputs do not include requested output `",
         paste(missing, collapse = "`, `"), "`")
  }
  outputs
}

# Lean model evaluations (SPAX-061) ------------------------------------------
# Inside the optimizer and the default scan a bind keeps only what the fit
# compares. `keep` is derived from the fit's own output by an allow-list, never
# from `requested_outputs`; NULL is today's full state.
.lean_outputs <- c("allocation", "huff_share", "opportunity", "flow", "flow_outside")

.lean_keep <- function(problem, output) {
  spec <- problem$metadata$spec
  if (!identical(spec[["allocation", exact = TRUE]], "clm") ||
      !"keep" %in% names(formals(problem$bind))) {
    return(NULL)
  }
  switch(output,
    utilization = , utilization_by_group = , utilization_outside = ,
    utilization_outside_by_group = character(),
    flow = "flow",
    flow_outside = "flow_outside",
    NULL)
}

# Reading an output a lean step did not keep is a programming error that must
# reach the caller: the handlers on the optimizer path re-raise this class.
.lean_missing <- function(output, kept) {
  stop(errorCondition(
    paste0("output `", paste(output, collapse = "`, `"),
           "` was not kept by this lean model evaluation (kept: ",
           if (length(kept)) paste(kept, collapse = ", ") else "none",
           "); evaluate without `keep`"),
    class = "spax_lean_missing", call = NULL))
}

.lean_check <- function(requested, kept) {
  missing <- setdiff(intersect(requested, .lean_outputs), kept)
  if (length(missing)) .lean_missing(missing, kept)
  invisible(TRUE)
}

# Expected origin-by-facility counts for corrected CLM only. Demand supplies
# the denominator and count unit; this helper does not infer an event or period.
.clm_flow <- function(allocation, demand) {
  if (!is.matrix(allocation) || nrow(allocation) != length(demand)) {
    stop("CLM allocation rows must match active demand")
  }
  allocation * as.numeric(demand)
}

# Outside counts as model outputs (SPAX-059) -------------------------------------
# The corrected CLM with an outside option declares three more count outputs: the
# inside count output (`flow`, `utilization`, `utilization_by_group`) extended by
# the modelled outside count. The outside entry is named "(outside)" on its axis,
# which is why no facility may carry that ID when one of them is requested. They
# are ordinary expected counts (Poisson fits use the ordinary loss); the names map
# each one to the inside output it extends.
.outside_label <- "(outside)"
.outside_outputs <- c(flow_outside = "flow", utilization_outside = "utilization",
                      utilization_outside_by_group = "utilization_by_group")

# `flow` with the modelled outside count of each origin (`D_i * outside_share_i`)
# as one more column.
.clm_flow_outside <- function(flow, outside) {
  if (!is.matrix(flow) || nrow(flow) != length(outside)) {
    stop("CLM flow rows must match the outside counts")
  }
  out <- cbind(flow, as.numeric(outside))
  columns <- colnames(flow)
  if (!is.null(columns)) {
    dimnames(out) <- list(rownames(flow), c(columns, .outside_label))
  } else if (!is.null(rownames(flow))) {
    dimnames(out) <- list(rownames(flow), NULL)
  }
  out
}

# `utilization` with the modelled outside count of all origins as one more element.
.clm_utilization_outside <- function(utilization, outside_total) {
  out <- c(as.numeric(utilization), as.numeric(outside_total))
  if (!is.null(names(utilization))) names(out) <- c(names(utilization), .outside_label)
  out
}

# `utilization_by_group` (facilities by groups) with the modelled outside count
# of each group as one more row. A group's outside share is `NA` where it has no
# demand and no usable travel; there it carries no count.
.clm_group_outside <- function(utilization_by_group, D_groups, outside_share_by_group) {
  share <- outside_share_by_group
  share[is.na(share)] <- 0
  outside <- colSums(D_groups * share)
  out <- rbind(utilization_by_group, as.numeric(outside))
  rows <- rownames(utilization_by_group)
  if (!is.null(rows)) {
    dimnames(out) <- list(c(rows, .outside_label), colnames(utilization_by_group))
  } else {
    dimnames(out) <- list(NULL, colnames(utilization_by_group))
  }
  out
}

# Whether a built-in static CLM specification has an outside option: a fitted
# outside weight, or a positive held one (by group too).
.spec_has_outside_option <- function(spec) {
  isTRUE(spec$fit_v0) || isTRUE(spec$v0 > 0) ||
    any(spec[["v0_groups", exact = TRUE]] > 0)
}

# Refuse an outside-count target the model cannot carry. Models without an
# outside option have no outside count to fit (the Huff model, and any CLM whose
# outside weight is held at zero): the check is the earliest common place of every
# fitting path (target binding). A model built by an earlier version of spax does
# not declare the outputs: it must be rebuilt.
.check_outside_target <- function(problem, output) {
  if (!is.character(output) || length(output) != 1L ||
      !output %in% names(.outside_outputs)) return(invisible(TRUE))
  spec <- problem$metadata$spec
  if (identical(spec$model, "huff") && !.spec_has_outside_option(spec)) {
    stop("outside counts need a model with an outside option", call. = FALSE)
  }
  if (!output %in% names(spec$output_axes)) {
    why <- if (!identical(spec$allocation, "clm")) {
      ": it needs the corrected CLM allocation"
    } else if (output == "utilization_outside_by_group" &&
               is.null(spec[["groups", exact = TRUE]])) {
      ": it needs a model with demand groups"
    } else if (identical(spec$model, "huff")) {
      ": rebuild the model with this version of spax"
    }
    stop("output '", output, "' is not declared by this model", why, call. = FALSE)
  }
  invisible(TRUE)
}
