# Private observation identity binding --------------------------------------

#' Resolve a provider's declared output axes and their canonical IDs
#'
#' Raster origin IDs are retained cell numbers within the prepared template.
#' Older prepared raster problems use their explicit `demand_kept_index` as
#' these cell keys when `origin_ids` is absent; this is not a length-based guess.
#' Custom providers can declare `output_axes` in their spec and supply
#' `origin_ids` / `facility_ids` (and `group_ids` for demand groups) in the
#' substrate. No axis is inferred from an output name or a coincident vector
#' length.
#' @keywords internal
#' @md
.problem_output_identity <- function(problem, output) {
  if (!is.character(output) || length(output) != 1L || is.na(output) || !nzchar(output)) {
    stop("`output` must be a length-one character value")
  }
  declared <- problem$metadata$spec$output_axes
  if (is.null(declared) || !output %in% names(declared)) {
    return(list(output = output, axes = NULL, ids = list()))
  }
  if (!is.character(declared) || is.null(names(declared)) ||
      anyNA(names(declared)) || anyDuplicated(names(declared))) {
    stop("provider output_axes must have unique output names")
  }
  axis <- declared[[output]]
  if (is.na(axis) || !axis %in% c("origin", "facility", "origin_facility",
                                  "facility_group", "origin_group")) {
    stop("unsupported declared output axis for `", output, "`")
  }
  # The group axis (demand groups) takes its IDs from `substrate$group_ids`.
  axes <- switch(axis,
    origin_facility = c("origin", "facility"),
    facility_group = c("facility", "group"),
    origin_group = c("origin", "group"),
    axis)
  ids <- lapply(axes, function(a) {
    value <- problem$substrate[[paste0(a, "_ids")]]
    if (a == "origin" && is.null(value) &&
        !is.null(problem$substrate$demand_kept_index)) {
      value <- as.character(problem$substrate$demand_kept_index)
    }
    if (is.null(value)) return(NULL)
    if (!is.atomic(value) || !is.null(dim(value)) || anyNA(value)) {
      stop("declared ", a, " IDs must be a nonmissing vector")
    }
    value <- as.character(value)
    if (any(!nzchar(trimws(value))) || anyDuplicated(value)) {
      stop("declared ", a, " IDs must be unique and nonblank")
    }
    value
  })
  names(ids) <- axes
  list(output = output, axes = axes, ids = ids)
}

#' Bind vector or matrix observations once to a declared output identity
#'
#' Each named axis must contain the complete canonical ID set; unnamed axes
#' stay positional. Missing observations use NA values rather than omitted IDs.
#' Matrix orientation and dimensions are never guessed. A private bound object
#' can be passed by multistart/profile wrappers without repeating ID matches;
#' its identity must still agree with the receiving problem and output.
#'
#' Custom loss callbacks receive canonical observation order. Observation-indexed
#' auxiliary values in `loss_args` must already use that order; arbitrary loss
#' arguments are neither interpreted nor silently rearranged here.
#' Unannotated custom providers retain positional numeric fitting for unnamed
#' targets, but their fitted vectors are no longer relabelled as facilities
#' merely because lengths agree. Declare axes and IDs for identity-safe names.
#' @keywords internal
#' @md
.bind_problem_target <- function(problem, observed, output = "utilization") {
  identity <- .problem_output_identity(problem, output)
  if (inherits(observed, "ae_bound_target")) {
    if (!identical(observed$identity, identity)) {
      stop("bound target identity does not match the problem output")
    }
    .check_group_targets(problem, observed)
    return(observed)
  }
  target <- .fit_target_meta(observed, "observed")
  dims <- if (is.null(dim(observed))) length(observed) else dim(observed)
  labels <- if (is.null(dim(observed))) list(names(observed)) else dimnames(observed)
  if (is.null(labels)) labels <- rep(list(NULL), length(dims))
  if (length(identity$axes) && length(identity$axes) != length(dims)) {
    stop("observed shape does not match the declared output axes")
  }
  indices <- lapply(dims, seq_len)
  named <- !vapply(labels, is.null, logical(1))
  for (k in seq_along(dims)) {
    ids <- if (length(identity$axes)) identity$ids[[k]] else NULL
    axis <- if (length(identity$axes)) identity$axes[k] else paste0("axis ", k)
    if (!is.null(ids) && length(ids) != dims[k]) {
      stop("observed ", axis, " dimension does not match the declared ID count")
    }
    if (!named[k]) {
      # Group-labelled targets are matched by IDs on both axes, never by position.
      if ("group" %in% identity$axes) {
        stop("observed ", axis, " axis must be named (group-labelled targets are ",
             "matched by IDs)")
      }
      next
    }
    labels_k <- labels[[k]]
    if (anyNA(labels_k) || any(!nzchar(trimws(labels_k))) || anyDuplicated(labels_k)) {
      stop("observed ", axis, " IDs must be nonmissing, unique and nonblank")
    }
    if (is.null(ids)) {
      stop("named observed ", axis, " requires declared output axes and IDs")
    }
    if (!setequal(labels_k, ids)) {
      stop("observed ", axis, " IDs must exactly match the declared IDs (no missing or extra IDs)")
    }
    indices[[k]] <- match(ids, labels_k)
  }
  reordered <- any(vapply(seq_along(indices), function(k) {
    !identical(indices[[k]], seq_len(dims[k]))
  }, logical(1)))
  canonical <- observed
  if (reordered) {
    canonical <- if (is.null(dim(observed))) observed[indices[[1]]] else
      observed[indices[[1]], indices[[2]], drop = FALSE]
    target <- .fit_target_meta(canonical, "observed")
  }
  target$observed <- canonical
  target$identity <- identity
  target$alignment <- list(
    output = output, axes = identity$axes, ids = identity$ids,
    named = named, input_ids = labels,
    canonical_to_input = indices,
    input_to_canonical = lapply(indices, function(x) match(seq_along(x), x))
  )
  class(target) <- "ae_bound_target"
  .check_group_targets(problem, target)
  target
}

#' Refuse group-specific parameters that the target cannot identify
#'
#' A model with demand groups fitted with free group-specific parameters
#' (`sigma.<g>`, `v0.<g>`) needs group-labelled targets: `utilization_by_group`
#' with observed counts for every group that has such a parameter, and positive
#' demand in that group. Aggregated targets leave the group parameters
#' multimodal and weakly identified (SPAX-057, D2). Fixed group parameters and
#' shared parameters are allowed with any target; ungrouped problems return at
#' once. The check runs inside `.bind_problem_target()`, in both its branches,
#' so every fitting path (multistart, profile, nfxp, ladder, bootstrap and
#' inference refits) refuses once, before any start. Group-specific free
#' parameters are read from the names of the theta contract (a profile
#' subproblem rebuilds the contract from its names alone).
#' @param problem An `ae_problem`.
#' @param target A bound target (`ae_bound_target`).
#' @keywords internal
#' @md
.check_group_targets <- function(problem, target) {
  spec <- problem$metadata$spec
  groups <- spec[["groups", exact = TRUE]]
  if (is.null(groups)) return(invisible(TRUE))
  by <- spec[["by_group", exact = TRUE]]
  candidates <- c(if ("sigma" %in% by) paste0("sigma.", groups),
                  if ("v0" %in% by) paste0("v0.", groups))
  specific <- intersect(problem$theta$names, candidates)
  if (!length(specific)) return(invisible(TRUE))
  # The group of every free group-specific parameter (IDs contain no ".").
  owners <- unique(sub("^[^.]*\\.", "", specific))
  fail <- function(names, extra = NULL) {
    stop("group-specific parameters (", paste(names, collapse = ", "), ") ",
         "need group-labelled targets: fit them to counts recorded by group ",
         "(obs_totals() by group, or output = \"utilization_by_group\" in ",
         "fit_allocation()), or hold them", extra, call. = FALSE)
  }
  identity <- target$identity
  if (!identical(identity$output, "utilization_by_group") ||
      !setequal(identity$ids$group, groups)) {
    fail(specific)
  }
  observed <- target$observed
  unobserved <- owners[vapply(owners, function(g) {
    all(is.na(observed[, match(g, identity$ids$group)]))
  }, logical(1))]
  if (length(unobserved)) {
    fail(specific, paste0(" (group(s) with no observed cell: ",
                              paste(unobserved, collapse = ", "), ")"))
  }
  D <- problem$substrate$D_groups
  no_demand <- owners[vapply(owners, function(g) {
    is.null(D) || !any(D[, g] > 0)
  }, logical(1))]
  if (length(no_demand)) {
    fail(specific, paste0(" (group(s) with no positive active demand: ",
                              paste(no_demand, collapse = ", "), ")"))
  }
  invisible(TRUE)
}

#' Apply target-axis names without changing raw model outputs
#' @keywords internal
#' @md
.name_bound_target <- function(value, target) {
  ids <- target$identity$ids
  if (!length(ids)) return(value)
  if (is.null(dim(value))) {
    if (!is.null(ids[[1]])) names(value) <- ids[[1]]
    return(value)
  }
  # Preserve unnamed matrix axes; name axes that the provider or caller named.
  dn <- dimnames(value)
  observed_dn <- dimnames(target$observed)
  if (is.null(dn)) dn <- list(NULL, NULL)
  if (is.null(observed_dn)) observed_dn <- list(NULL, NULL)
  for (k in seq_along(ids)) {
    if (!is.null(ids[[k]]) && (!is.null(dn[[k]]) || !is.null(observed_dn[[k]]))) {
      dn[[k]] <- ids[[k]]
    }
  }
  if (any(!vapply(dn, is.null, logical(1)))) dimnames(value) <- dn
  value
}

#' Read the canonical observation identity retained by a fitted object
#' @keywords internal
#' @md
.fit_observation_identity <- function(fit) {
  a <- fit$target_alignment
  if (is.null(a) || !length(a$ids) || all(vapply(a$ids, is.null, logical(1)))) {
    return(NULL)
  }
  list(axes = a$axes, ids = a$ids)
}
