# Private checked allocation inputs ----------------------------------------

#' Prepare named numeric allocation inputs for existing AE providers
#'
#' Demand is a numeric vector, supply a nonnegative finite vector, and distance
#' an origin-by-facility numeric matrix. Canonical IDs must be complete, unique
#' and nonblank. Named axes match independently; unnamed axes are positional.
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
.prepare_allocation <- function(demand, supply, distance,
                                origin_ids = names(demand),
                                facility_ids = colnames(distance),
                                included = rep(TRUE, length(demand)),
                                missing_demand = c("error", "exclude"),
                                missing_travel = c("error", "legacy_zero"),
                                spatial = NULL, metadata = list()) {
  missing_demand <- match.arg(missing_demand)
  missing_travel <- match.arg(missing_travel)
  origins <- .allocation_ids(origin_ids, "origin_ids")
  facilities <- .allocation_ids(facility_ids, "facility_ids")
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
  if (!is.matrix(S) || !is.numeric(S) || nrow(S) != length(facilities) ||
      any(!is.finite(S) | S < 0)) stop("supply must be finite and nonnegative")
  expected <- which(included & is.finite(demand) & demand > 0)
  if (!identical(keep, expected) || !is.matrix(active) || !is.numeric(active) ||
      !identical(dim(active), c(length(keep), length(facilities)))) {
    stop("active inputs do not match the declared demand support")
  }
  unknown_travel <- invalid_travel <- rep(FALSE, nrow(active))
  names(unknown_travel) <- names(invalid_travel) <- rownames(active)
  if (anyNA(active)) unknown_travel <- rowSums(is.na(active)) > 0
  # The scalar Inf also covers empty/all-missing input without a min warning.
  if (min(active, Inf, na.rm = TRUE) < 0) {
    invalid_travel <- rowSums(active < 0, na.rm = TRUE) > 0
  }
  if (policy$kernel_domain == "strict") {
    if (any(invalid_travel)) stop("travel must be nonnegative or positive infinity")
    if (policy$missing_travel == "error" && any(unknown_travel)) {
      stop("active travel is missing; declare missing_travel = 'legacy_zero' to omit unknown edges")
    }
  }
  status <- rep("excluded", length(origins))
  status[included & is.na(demand)] <- "unknown"
  status[included & !is.na(demand) & (!is.finite(demand) | demand < 0)] <- "invalid_omitted"
  status[included & is.finite(demand) & demand == 0] <- "zero"
  status[keep] <- "active"
  travel_status <- rep("known", length(keep))
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
  geometry <- .allocation_spatial(spatial, origins)
  structure(list(D_active = demand[keep], distance_active = active, S = S,
                 facility_ids = facilities, origin_ids = origins[keep],
                 supply_cols = supply_cols, demand_kept_index = keep,
                 template = geometry$template, cell_index = geometry$cell_index,
                 all_origin_ids = origins, origin_status = status,
                 active_unknown_travel = unknown_travel,
                 active_travel_status = travel_status,
                 input_policy = policy, input_metadata = .allocation_metadata(metadata)),
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

.allocation_kernel <- function(substrate, family, sigma) {
  distance <- substrate$distance_active
  strict <- identical(substrate$input_policy$kernel_domain, "strict")
  if (strict && family == "power" && any(distance == 0, na.rm = TRUE)) {
    stop("power decay requires strictly positive finite travel distances")
  }
  if (strict && family == "gaussian" && sigma <= 0) {
    stop("gaussian decay requires positive sigma")
  }
  K <- calc_decay(distance, method = family, sigma = sigma, snap = TRUE)
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
