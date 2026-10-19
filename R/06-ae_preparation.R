# Private checked allocation inputs ----------------------------------------

#' Prepare named numeric allocation inputs for existing AE providers
#'
#' Demand is a numeric vector, supply a nonnegative finite vector, and distance
#' an origin-by-facility numeric matrix. Demand may also be a named list of
#' named numeric vectors (demand groups, SPAX-057), and distance a named list of
#' matrices by group; see `.prepare_allocation_groups()`. Canonical IDs must be
#' complete, unique and nonblank. Named axes match independently; unnamed axes are positional.
#' An unnamed `included` mask follows the supplied demand order and is reordered
#' with demand. A named mask instead matches canonical origin IDs independently.
#'
#' Positive included demand enters load support; zero, unknown and explicitly
#' excluded origins retain distinct registry statuses. Only active travel rows
#' are validated and retained. Positive infinity is an absent edge; missing
#' travel errors unless `legacy_zero` is explicitly selected. Negative travel
#' is invalid. Model-specific finite-distance domains are checked at binding.
#'
#' Optional `spatial` is a list with a one-layer raster `template` and a complete
#' `cell_index` mapping, optionally named by origin ID. The mapping is distinct
#' from registry order. Only plain geometry is saved, so serialization does not
#' depend on a live raster pointer or source file. Metadata declares demand and
#' supply units/periods and travel units; unspecified fields remain NA and no
#' unit conversion is performed. The result is private `ae_prepared_inputs`.
#' @keywords internal
#' @md
.prepare_allocation <- function(demand, supply, distance,
                                origin_ids = NULL,
                                facility_ids = NULL,
                                included = NULL,
                                missing_demand = c("error", "exclude"),
                                missing_travel = c("error", "legacy_zero"),
                                spatial = NULL, metadata = list()) {
  missing_demand <- match.arg(missing_demand)
  missing_travel <- match.arg(missing_travel)
  if (is.data.frame(demand)) {
    stop("give demand groups as a named list of numeric vectors (one per group), ",
         "not a data frame")
  }
  grouped <- is.list(demand)
  # Defaults: origin IDs are the names of the demand vector (of the first group
  # for grouped demand), facility IDs the travel column names, and every origin
  # is included.
  if (is.null(origin_ids)) {
    origin_ids <- if (grouped) {
      if (length(demand)) names(demand[[1L]]) else NULL
    } else names(demand)
  }
  if (is.null(facility_ids)) {
    facility_ids <- if (is.list(distance) && length(distance) &&
                        is.matrix(distance[[1L]])) colnames(distance[[1L]]) else
                          colnames(distance)
  }
  origins <- .allocation_ids(origin_ids, "origin_ids")
  facilities <- .allocation_ids(facility_ids, "facility_ids")
  if (is.null(included)) included <- rep(TRUE, length(origins))
  if (grouped) {
    return(.prepare_allocation_groups(demand, supply, distance, origins, facilities,
      included, missing_demand, missing_travel, spatial, metadata))
  }
  if (!is.numeric(demand) || !is.null(dim(demand)) || length(demand) != length(origins)) {
    stop("demand must be a numeric vector matching origin_ids")
  }
  if (!is.numeric(supply) || !is.null(dim(supply)) || length(supply) != length(facilities)) {
    stop("supply must be a numeric vector matching facility_ids")
  }
  if (!is.matrix(distance) || !is.numeric(distance) ||
      !identical(dim(distance), c(length(origins), length(facilities)))) {
    stop("distance must be an origin-by-facility numeric matrix")
  }
  order_d <- .allocation_order(names(demand), origins, "demand")
  if (!is.logical(included) || !is.null(dim(included)) ||
      length(included) != length(origins) || anyNA(included)) {
    stop("included must be a nonmissing logical vector matching demand")
  }
  order_i <- if (is.null(names(included))) order_d else
    .allocation_order(names(included), origins, "included")
  included <- unname(included[order_i])
  demand <- unname(demand[order_d])
  if (any(!is.na(demand) & (!is.finite(demand) | demand < 0))) {
    stop("demand must be finite and nonnegative, or missing")
  }
  if (missing_demand == "error" && any(included & is.na(demand))) {
    stop("included missing demand requires missing_demand = 'exclude'")
  }
  rows <- .allocation_order(rownames(distance), origins, "distance origin")
  cols <- .allocation_order(colnames(distance), facilities, "distance facility")
  keep <- which(included & is.finite(demand) & demand > 0)
  active <- distance[rows[keep], cols, drop = FALSE]
  # Numeric matrices have canonical row labels; raster extraction keeps its
  # historical dimnames, preserving raw output names along that legacy route.
  dimnames(active) <- list(origins[keep], facilities)
  supply <- supply[.allocation_order(names(supply), facilities, "supply")]
  S <- matrix(unname(supply), ncol = 1L)
  .finalize_allocation(demand, included, origins, S, facilities, "supply",
    active, keep, list(source = "numeric", missing_demand = missing_demand,
                      missing_travel = missing_travel, kernel_domain = "strict"),
    spatial, metadata)
}

# Grouped numeric preparation (SPAX-057). `demand` is a named list by group of
# named numeric vectors with one name set (the origin IDs, any order); `distance`
# is one matrix shared by the groups or a named list of matrices by group, each
# aligned by origin and facility IDs as the ungrouped matrix is. An origin is
# active when it is included, every group demand is known (finite, nonnegative)
# and the total is positive; with missing_demand = "exclude" an origin with any
# group demand missing is unknown as a whole. A group with zero demand at an
# active origin carries no flow, so its travel is not checked there. Unnamed
# `included` follows the order of the first group. The ungrouped fields keep their
# meaning (D_active is the total demand; distance_active the shared matrix, NULL
# for per-group travel); the group fields are appended.
.prepare_allocation_groups <- function(demand, supply, distance, origins, facilities,
                                       included, missing_demand, missing_travel,
                                       spatial, metadata) {
  groups <- names(demand)
  .check_group_ids(groups, "demand group names")
  n <- length(origins)
  if (!is.numeric(supply) || !is.null(dim(supply)) || length(supply) != length(facilities)) {
    stop("supply must be a numeric vector matching facility_ids")
  }
  Dm <- matrix(NA_real_, n, length(groups), dimnames = list(origins, groups))
  for (g in groups) {
    d <- demand[[g]]
    if (!is.numeric(d) || !is.null(dim(d)) || is.null(names(d))) {
      stop("demand group '", g, "' must be a named numeric vector of origin demand")
    }
    order_g <- .allocation_order(names(d), origins, paste0("demand group '", g, "'"))
    if (g == groups[1L]) order_d <- order_g
    Dm[, g] <- unname(d[order_g])
  }
  if (!is.logical(included) || !is.null(dim(included)) ||
      length(included) != n || anyNA(included)) {
    stop("included must be a nonmissing logical vector matching demand")
  }
  order_i <- if (is.null(names(included))) order_d else
    .allocation_order(names(included), origins, "included")
  included <- unname(included[order_i])
  if (any(!is.na(Dm) & (!is.finite(Dm) | Dm < 0))) {
    stop("demand must be finite and nonnegative, or missing")
  }
  unknown_group <- is.na(Dm)
  if (missing_demand == "error" && any(included & rowSums(unknown_group) > 0)) {
    bad <- groups[colSums(unknown_group & included) > 0]
    stop("included missing demand requires missing_demand = 'exclude' (missing in group: ",
         paste(bad, collapse = ", "), ")")
  }
  # rowSums() is NA when any group demand is missing: the origin is unknown.
  total <- unname(rowSums(Dm))
  keep <- which(included & is.finite(total) & total > 0)
  per_group <- .fca_per_group(distance, groups, "distance", element = is.matrix)
  shared <- is.null(per_group)
  travel <- if (shared) stats::setNames(rep(list(distance), length(groups)), groups) else
    per_group
  active_g <- vector("list", length(groups))
  names(active_g) <- groups
  for (g in groups) {
    M <- travel[[g]]
    label <- if (shared) "distance" else paste0("distance[['", g, "']]")
    if (!is.matrix(M) || !is.numeric(M) || !identical(dim(M), c(n, length(facilities)))) {
      stop(if (shared) "distance must be an origin-by-facility numeric matrix" else
        paste0("distance for group '", g, "' must be an origin-by-facility numeric matrix"))
    }
    if (shared && g != groups[1L]) {
      active_g[[g]] <- active_g[[1L]]
      next
    }
    rows <- .allocation_order(rownames(M), origins, paste(label, "origin"))
    cols <- .allocation_order(colnames(M), facilities, paste(label, "facility"))
    a <- M[rows[keep], cols, drop = FALSE]
    dimnames(a) <- list(origins[keep], facilities)
    active_g[[g]] <- a
  }
  supply <- supply[.allocation_order(names(supply), facilities, "supply")]
  S <- matrix(unname(supply), ncol = 1L)
  policy <- list(source = "numeric", missing_demand = missing_demand,
                 missing_travel = missing_travel, kernel_domain = "strict")
  .finalize_allocation_groups(Dm, total, included, origins, S, facilities, active_g,
    shared, keep, policy, spatial, metadata)
}

.allocation_ids <- function(ids, label) {
  if (is.null(ids) || !is.atomic(ids) || !is.null(dim(ids)) || !length(ids) || anyNA(ids)) {
    stop(label, " must be a complete nonmissing ID vector")
  }
  ids <- as.character(ids)
  if (any(!nzchar(trimws(ids))) || anyDuplicated(ids)) {
    stop(label, " must contain unique nonblank IDs")
  }
  ids
}

.allocation_order <- function(labels, ids, label) {
  if (is.null(labels)) return(seq_along(ids))
  # Canonical IDs have already been validated by the preparation entry point.
  if (identical(labels, ids)) return(seq_along(ids))
  labels <- .allocation_ids(labels, paste(label, "IDs"))
  if (!setequal(labels, ids)) stop(label, " IDs must exactly match canonical IDs")
  match(ids, labels)
}

.allocation_metadata <- function(metadata) {
  fields <- c("demand_units", "demand_period", "supply_units", "supply_period", "travel_units")
  if (!is.list(metadata) || (length(metadata) &&
      (is.null(names(metadata)) || anyNA(names(metadata)) ||
       anyDuplicated(names(metadata)) || any(!names(metadata) %in% fields)))) {
    stop("metadata must be a named list of demand/supply units and periods or travel_units")
  }
  out <- stats::setNames(rep(list(NA_character_), length(fields)), fields)
  for (name in names(metadata)) {
    value <- metadata[[name]]
    if (!is.character(value) || length(value) != 1L ||
        (!is.na(value) && !nzchar(trimws(value)))) {
      stop("metadata entries must be one nonblank string or NA")
    }
    out[[name]] <- value
  }
  out
}

.allocation_spatial <- function(spatial, origins) {
  if (is.null(spatial)) return(list(template = NULL, cell_index = NULL))
  if (!is.list(spatial) || is.null(spatial$template) || is.null(spatial$cell_index)) {
    stop("spatial must provide template and cell_index")
  }
  template <- spatial$template
  if (!inherits(template, "SpatRaster") || terra::nlyr(template) != 1L) {
    stop("spatial template must be a one-layer SpatRaster")
  }
  cells <- spatial$cell_index
  identity <- is.integer(cells) && is.null(names(cells)) &&
    is.null(dim(cells)) && identical(cells, seq_along(origins)) &&
    length(cells) <= terra::ncell(template)
  if (!identity && (!is.numeric(cells) || !is.null(dim(cells)) || length(cells) != length(origins) ||
      any(!is.finite(cells)) || any(cells != floor(cells)) ||
      any(cells < 1 | cells > terra::ncell(template)) || anyDuplicated(cells))) {
    stop("spatial cell_index must contain unique valid template cells for every origin")
  }
  if (!is.null(names(cells))) {
    cells <- unname(cells[.allocation_order(names(cells), origins, "spatial cell_index")])
  }
  geometry <- structure(list(nrow = terra::nrow(template), ncol = terra::ncol(template),
                             extent = as.vector(terra::ext(template)), crs = terra::crs(template)),
                        class = "ae_spatial_template")
  list(template = geometry, cell_index = cells)
}

.allocation_template <- function(template) {
  if (!inherits(template, "ae_spatial_template")) return(template)
  e <- template$extent
  terra::rast(nrows = template$nrow, ncols = template$ncol,
              xmin = e[1], xmax = e[2], ymin = e[3], ymax = e[4], crs = template$crs)
}

# Common checked finalizer: IDs were checked by the numeric entry point or
# generated from raster cell positions; raster facility IDs are checked there.
# Do not rescan generated all-cell strings or repeat user-ID validation here.
.finalize_allocation <- function(demand, included, origins, S, facilities, supply_cols,
                                 active, keep, policy, spatial = NULL, metadata = list()) {
  .allocation_check_supply(S, facilities)
  expected <- which(included & is.finite(demand) & demand > 0)
  if (!identical(keep, expected) || !is.matrix(active) || !is.numeric(active) ||
      !identical(dim(active), c(length(keep), length(facilities)))) {
    stop("active inputs do not match the declared demand support")
  }
  travel <- .allocation_travel_state(active)
  if (policy$kernel_domain == "strict") {
    if (any(travel$invalid)) stop("travel must be nonnegative or positive infinity")
    if (policy$missing_travel == "error" && any(travel$unknown)) {
      stop("active travel is missing; declare missing_travel = 'legacy_zero' to omit unknown edges")
    }
  }
  status <- .allocation_origin_status(demand, included, keep)
  geometry <- .allocation_spatial(spatial, origins)
  structure(list(D_active = demand[keep], distance_active = active, S = S,
                 facility_ids = facilities, origin_ids = origins[keep],
                 supply_cols = supply_cols, demand_kept_index = keep,
                 template = geometry$template, cell_index = geometry$cell_index,
                 all_origin_ids = origins, origin_status = status,
                 active_unknown_travel = travel$unknown,
                 active_travel_status = travel$status,
                 input_policy = policy, input_metadata = .allocation_metadata(metadata)),
            class = "ae_prepared_inputs")
}

.allocation_check_supply <- function(S, facilities) {
  if (!is.matrix(S) || !is.numeric(S) || nrow(S) != length(facilities) ||
      any(!is.finite(S) | S < 0)) stop("supply must be finite and nonnegative")
  invisible(TRUE)
}

# Registry status of every origin; `demand` is NA for unknown demand.
.allocation_origin_status <- function(demand, included, keep) {
  status <- rep("excluded", length(demand))
  status[included & is.na(demand)] <- "unknown"
  status[included & !is.na(demand) & (!is.finite(demand) | demand < 0)] <- "invalid_omitted"
  status[included & is.finite(demand) & demand == 0] <- "zero"
  status[keep] <- "active"
  status
}

# Per-row travel flags and status of the active matrix. `relevant` (grouped
# preparation) restricts the unknown and invalid flags to the rows where the
# group has positive demand; NULL keeps every row.
.allocation_travel_state <- function(active, relevant = NULL) {
  unknown_travel <- invalid_travel <- rep(FALSE, nrow(active))
  names(unknown_travel) <- names(invalid_travel) <- rownames(active)
  if (anyNA(active)) unknown_travel <- rowSums(is.na(active)) > 0
  # The scalar Inf also covers empty/all-missing input without a min warning.
  if (min(active, Inf, na.rm = TRUE) < 0) {
    invalid_travel <- rowSums(active < 0, na.rm = TRUE) > 0
  }
  if (!is.null(relevant)) {
    unknown_travel <- unknown_travel & relevant
    invalid_travel <- invalid_travel & relevant
  }
  travel_status <- rep("known", nrow(active))
  # A row can be all absent only if each successive column is +Inf. Narrow
  # the candidate indices without constructing an origin-by-facility mask.
  absent <- which(active[, 1L] == Inf)
  for (j in seq_len(ncol(active))[-1L]) {
    if (!length(absent)) break
    absent <- absent[which(active[absent, j] == Inf)]
  }
  travel_status[absent] <- "all_absent"
  travel_status[unknown_travel] <- "unknown"
  travel_status[invalid_travel] <- "invalid_legacy"
  list(unknown = unknown_travel, invalid = invalid_travel, status = travel_status)
}

# Grouped finalizer (numeric route only). `Dm` is the full origin-by-group
# demand matrix in canonical order, `total` its row sums (NA for unknown
# origins), `active_g` the named list of n_act x J travel matrices by group
# (the same matrix repeated when travel is `shared`).
.finalize_allocation_groups <- function(Dm, total, included, origins, S, facilities,
                                        active_g, shared, keep, policy,
                                        spatial = NULL, metadata = list()) {
  .allocation_check_supply(S, facilities)
  groups <- colnames(Dm)
  Dg <- Dm[keep, , drop = FALSE]
  rownames(Dg) <- origins[keep]
  state <- stats::setNames(vector("list", length(groups)), groups)
  for (g in groups) {
    state[[g]] <- .allocation_travel_state(active_g[[g]], relevant = Dg[, g] > 0)
    if (any(state[[g]]$invalid)) {
      stop("travel must be nonnegative or positive infinity (group '", g, "')")
    }
    if (policy$missing_travel == "error" && any(state[[g]]$unknown)) {
      stop("active travel is missing; declare missing_travel = 'legacy_zero' to omit ",
           "unknown edges (group '", g, "')")
    }
  }
  pick <- function(field) lapply(state, `[[`, field)
  unknown_g <- pick("unknown")
  status_g <- pick("status")
  union_or <- function(x) Reduce(`|`, x)
  unknown <- union_or(unknown_g)
  invalid <- union_or(pick("invalid"))
  # All absent: every group with positive demand at the row has no edge.
  all_absent <- Reduce(`&`, Map(function(st, g) st == "all_absent" | !(Dg[, g] > 0),
                                status_g, groups))
  travel_status <- rep("known", length(keep))
  travel_status[all_absent] <- "all_absent"
  travel_status[unknown] <- "unknown"
  travel_status[invalid] <- "invalid_legacy"
  geometry <- .allocation_spatial(spatial, origins)
  structure(list(D_active = unname(total[keep]),
                 distance_active = if (shared) active_g[[1L]] else NULL, S = S,
                 facility_ids = facilities, origin_ids = origins[keep],
                 supply_cols = "supply", demand_kept_index = keep,
                 template = geometry$template, cell_index = geometry$cell_index,
                 all_origin_ids = origins,
                 origin_status = .allocation_origin_status(total, included, keep),
                 active_unknown_travel = unknown,
                 active_travel_status = travel_status,
                 input_policy = policy, input_metadata = .allocation_metadata(metadata),
                 group_ids = groups, D_groups = Dg, distance_groups = active_g,
                 active_travel_status_groups = status_g,
                 active_unknown_travel_groups = unknown_g),
            class = "ae_prepared_inputs")
}

.resolve_allocation <- function(demand, supply = NULL, distance = NULL,
                                id_col = NULL, supply_cols = NULL) {
  if (inherits(demand, "ae_prepared_inputs")) {
    if (!is.null(supply) || !is.null(distance) || !is.null(id_col) || !is.null(supply_cols)) {
      stop("prepared inputs cannot be combined with raw supply, distance or supply selectors")
    }
    return(demand)
  }
  .interaction_substrate(demand, supply, distance, id_col = id_col, supply_cols = supply_cols)
}

# Decay weights for the allocation workflow. `family` is a built-in kernel name,
# the internal no-decay family "none", or the name of the declared `kernel` (a
# custom `spax_decay_kernel`, with `fun` its rebuilt function when the caller
# already has it). Built-ins are evaluated as before; "none" is 1 on every
# finite distance; a custom kernel is evaluated on the finite distances only.
# Non-finite distances keep a non-finite value for the caller's zeroing (an
# infinite distance is already 0 for a custom kernel).
.decay_weights <- function(family, kernel, distance, sigma, fun = NULL) {
  if (!is.null(kernel)) {
    return(.eval_decay_kernel(kernel, distance, sigma, fun))
  }
  if (identical(family, "none")) {
    weights <- distance
    storage.mode(weights) <- "double"
    weights[is.finite(distance)] <- 1
    return(weights)
  }
  calc_decay(distance, method = family, sigma = sigma, snap = TRUE)
}

# Which entries of a prepared travel matrix are finite, which does not change
# with the parameters. A model with a declared kernel records it once, so that the
# kernel runs on the finite entries only, without scanning the whole matrix at
# every parameter value. The plan keeps only what is needed: when every entry is
# finite there is nothing to select and no index is stored (the kernel then runs
# on the matrix itself); otherwise it keeps the index of the finite entries, not a
# copy of their values. `any_zero` is whether a finite entry is zero.
.decay_kernel_plan <- function(distance) {
  any_zero <- any(distance == 0, na.rm = TRUE)
  finite <- which(is.finite(distance))
  if (length(finite) && length(finite) == length(distance)) {
    return(list(all_finite = TRUE, any_zero = any_zero))
  }
  list(all_finite = FALSE, index = finite, any_zero = any_zero)
}

# Domain checks and weights of the decay kernel on the active travel matrix. A
# declared custom `kernel` also requires `sigma` inside its bounds and, when
# declared undefined at distance zero, refuses finite zero travel on strict
# inputs, as for the power kernel. With a `plan` from .decay_kernel_plan(), a
# declared kernel is evaluated on the finite entries only; the weights are the
# same as without it.
.allocation_kernel <- function(substrate, family, sigma, kernel = NULL, fun = NULL,
                               plan = NULL) {
  distance <- substrate$distance_active
  strict <- identical(substrate$input_policy$kernel_domain, "strict")
  if (strict && family == "power" && any(distance == 0, na.rm = TRUE)) {
    stop("power decay requires strictly positive finite travel distances")
  }
  if (strict && family == "gaussian" && sigma <= 0) {
    stop("gaussian decay requires positive sigma")
  }
  if (!is.null(kernel)) {
    if (sigma < kernel$lower || sigma > kernel$upper) {
      stop(kernel$name, " decay requires sigma from ", format(kernel$lower), " to ",
           format(kernel$upper))
    }
    if (sigma <= 0) {
      stop(kernel$name, " decay requires positive sigma")
    }
    if (!is.null(plan)) {
      return(.allocation_kernel_planned(kernel, fun, plan, distance, sigma, strict))
    }
    if (strict && !kernel$zero_distance && any(distance == 0, na.rm = TRUE)) {
      stop(kernel$name, " decay requires strictly positive finite travel distances")
    }
  }
  K <- .decay_weights(family, kernel, distance, sigma, fun)
  if (!strict) {
    K[!is.finite(K)] <- 0
  } else {
    finite <- is.finite(distance)
    if (any(!is.finite(K[finite]) | K[finite] < 0)) {
      stop("decay kernel is invalid on finite travel distances")
    }
    # Only explicitly absent/unknown edges are zeroed on checked numeric input.
    K[!finite] <- 0
  }
  K
}

# A declared kernel on the finite travel entries described by `plan`: on the
# matrix itself when every entry is finite, otherwise on the finite entries only,
# taken from `distance` at each call. Absent and unknown edges get 0, as on the
# unplanned path, and the weights are the same.
.allocation_kernel_planned <- function(kernel, fun, plan, distance, sigma, strict) {
  if (strict && !kernel$zero_distance && plan$any_zero) {
    stop(kernel$name, " decay requires strictly positive finite travel distances")
  }
  all_finite <- isTRUE(plan$all_finite)
  if (!all_finite && !length(plan$index)) {
    return(matrix(0, nrow(distance), ncol(distance), dimnames = dimnames(distance)))
  }
  if (is.null(fun)) fun <- .decay_kernel_function(kernel)
  input <- if (all_finite) distance else distance[plan$index]
  w <- do.call(fun, c(list(input, sigma), kernel$args))
  if (!is.numeric(w) || length(w) != length(input)) {
    stop("the kernel must return one numeric value for each distance")
  }
  if (strict) {
    if (any(!is.finite(w) | w < 0)) {
      stop("decay kernel is invalid on finite travel distances")
    }
  } else {
    w[!is.finite(w)] <- 0
  }
  if (all_finite) {
    attributes(w) <- NULL
    storage.mode(w) <- "double"
    dim(w) <- dim(distance)
    dimnames(w) <- dimnames(distance)
    return(w)
  }
  K <- matrix(0, nrow(distance), ncol(distance), dimnames = dimnames(distance))
  K[plan$index] <- w
  K
}

.allocation_supply <- function(substrate, kappa, beta = NULL) {
  S <- as.vector(substrate$S[, 1])
  scaled <- kappa * S
  strict <- identical(substrate$input_policy$kernel_domain, "strict")
  if (strict && any(!is.finite(scaled))) stop("scaled supply is nonfinite on checked numeric input")
  if (is.null(beta)) return(scaled)
  if (!strict) return(.huff_attractiveness(S, kappa, beta))
  .chck_positive_scalar(beta, "beta")
  a <- scaled^beta
  if (any(!is.finite(a))) stop("attractiveness is nonfinite on checked numeric input")
  a
}

.allocation_cell_index <- function(meta) {
  if (!is.null(meta$cell_index)) return(meta$cell_index)
  # Old saved raster fits had only a raster and its retained cell indices.
  if (inherits(meta$template, "SpatRaster")) return(meta$demand_kept_index)
  NULL
}
