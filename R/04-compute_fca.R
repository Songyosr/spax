# SPAX-004: typed FCA engine over spax_field -------------------------------

#' Return the non-I axis and IDs for a kernel field
#' @keywords internal
.fca_kernel_axis_ids <- function(kernel) {
  axis <- .ae_nonI_axis(kernel)
  ids <- as.character(.field_layer_index(kernel)[[axis]])
  list(axis = axis, ids = ids)
}

#' Validate demand/access kernels share one typed facility axis
#' @keywords internal
.fca_check_kernel_pair <- function(demand_kernel, access_kernel) {
  demand_axis <- .fca_kernel_axis_ids(demand_kernel)
  access_axis <- .fca_kernel_axis_ids(access_kernel)

  if (!identical(demand_axis$axis, access_axis$axis)) {
    stop("demand and access kernels must use the same non-I axis")
  }
  if (!setequal(demand_axis$ids, access_axis$ids)) {
    stop("demand and access kernels must span the same facility ids")
  }

  invisible(TRUE)
}

#' Coerce supply values to one vector field per measure
#' @keywords internal
.fca_supply_fields <- function(supply, kernel, id_col = NULL,
                               supply_cols = NULL, indicator_names = NULL) {
  kernel_axis <- .fca_kernel_axis_ids(kernel)
  axis <- kernel_axis$axis
  weight_ids <- kernel_axis$ids

  if (inherits(supply, "spax_vector_field")) {
    if (!identical(.field_domain(supply), axis)) {
      stop("`supply` field must use the same axis as the kernels")
    }
    if (!setequal(.field_axis_values(supply, axis), weight_ids)) {
      stop("`supply` field must span the same facility ids as the kernels")
    }
    output_names <- if (is.null(indicator_names)) "supply" else indicator_names
    return(list(fields = list(supply), names = output_names))
  }

  processed <- .help_process_supply(
    supply = supply,
    id_col = id_col,
    supply_cols = supply_cols,
    weight_ids = weight_ids
  )
  values <- as.matrix(processed$values)
  output_names <- if (is.null(indicator_names)) processed$cols else indicator_names

  fields <- lapply(seq_len(ncol(values)), function(k) {
    vals <- values[, k]
    names(vals) <- processed$ids
    .spax_vector_field(vals, domain = axis, role = "supply")
  })
  list(fields = fields, names = output_names)
}

#' Unwrap a compute_fca result field as a public SpatRaster
#' @keywords internal
.fca_result_raster <- function(field) {
  .chck_class(field, "spax_raster_field", "field")
  result <- .field_data(field)
  output_names <- .field_meta(field)$output_names
  if (!is.null(output_names)) {
    names(result) <- output_names
  }
  result
}

#' Internal typed FCA engine
#'
#' Implements normalize -> gather -> ratio -> spread over spax_field inputs.
#' Public wrappers unwrap the returned field to preserve existing return types.
#' @keywords internal
compute_fca <- function(demand, supply, demand_kernel, access_kernel,
                        demand_normalize = "identity",
                        id_col = NULL, supply_cols = NULL,
                        indicator_names = NULL,
                        snap = FALSE) {
  demand <- .as_spax_field(demand, domain = "I", role = "demand", snap = snap)
  demand_kernel <- .as_spax_field(
    demand_kernel,
    domain = c("I", "facility"),
    role = "kernel",
    allow_positional = TRUE,
    snap = snap
  )
  access_kernel <- .as_spax_field(
    access_kernel,
    domain = c("I", "facility"),
    role = "kernel",
    allow_positional = TRUE,
    snap = snap
  )
  .fca_check_kernel_pair(demand_kernel, access_kernel)

  demand_kernel <- .ae_normalize(
    demand_kernel,
    by = .field_cell_axis(demand_kernel),
    method = demand_normalize
  )
  potential_demand <- .ae_gather(demand, demand_kernel)
  supply_fields <- .fca_supply_fields(
    supply = supply,
    kernel = demand_kernel,
    id_col = id_col,
    supply_cols = supply_cols,
    indicator_names = indicator_names
  )

  access_fields <- lapply(supply_fields$fields, function(supply_field) {
    ratios <- .ae_ratio(supply_field, potential_demand, zero = 0)
    .ae_spread(ratios, access_kernel)
  })

  access_rasters <- lapply(access_fields, .field_data)
  result <- do.call(c, access_rasters)
  names(result) <- supply_fields$names

  if (length(access_fields) == 1) {
    return(.spax_raster_field(result, domain = "I",
                                     role = "realization",
                                     meta = list(output_names = supply_fields$names),
                                     snap = snap))
  }

  frame <- data.frame(
    layer = supply_fields$names,
    measure = supply_fields$names,
    stringsAsFactors = FALSE
  )
  .spax_raster_field(
    result,
    domain = c("I", "measure"),
    frame = frame,
    role = "realization",
    meta = list(output_names = supply_fields$names),
    snap = snap
  )
}

# SPAX-055: the block FCA driver ---------------------------------------------

#' Block FCA driver (the future `compute_fca()`)
#'
#' This has the signature that `compute_fca()` takes when the field path is
#' removed (SPAX-055, task T4), which renames it. Until then it runs unless
#' `options(spax.fca_engine = "field")` is set.
#'
#' Checks the grids, binds the supply to the facility IDs, runs the two-pass
#' block engine and returns a SpatRaster with one layer per supply measure on
#' the grid of the first raster of `rasters`. That raster also names the
#' facilities; the wrappers pass the distance raster first.
#'
#' @param demand One-layer SpatRaster of demand.
#' @param supply Vector, matrix or data frame, as for [compute_access()].
#' @param rasters Named list of the other SpatRasters the kernels read; the
#'   first one has one layer per facility.
#' @param kernels `list(demand = function(b, rows), access = function(b, rows))`
#'   as for `.fca_blocks()`.
#' @inheritParams compute_access
#' @param demand_normalize Normalization of the demand kernel.
#' @return SpatRaster of accessibility.
#' @keywords internal
.compute_fca_blocks <- function(demand, supply, rasters, kernels,
                                demand_normalize = "identity",
                                id_col = NULL, supply_cols = NULL,
                                indicator_names = NULL, snap = FALSE) {
  .chck_class(demand, "SpatRaster", "demand")
  if (terra::nlyr(demand) > 1L) {
    stop("multi-layer raster fields must declare at least one non-I axis")
  }
  for (nm in names(rasters)) {
    if (terra::nrow(rasters[[nm]]) != terra::nrow(demand) ||
        terra::ncol(rasters[[nm]]) != terra::ncol(demand)) {
      stop(sprintf("demand and %s must have the same number of rows and columns", nm))
    }
  }
  ids <- .fca_ids(rasters[[1L]], names(rasters)[1L])

  processed <- .help_process_supply(
    supply = supply,
    id_col = id_col,
    supply_cols = supply_cols,
    weight_ids = ids
  )
  values <- as.matrix(processed$values)
  if (nrow(values) != length(ids)) {
    stop("supply must have one row for each facility")
  }
  output_names <- if (is.null(indicator_names)) processed$cols else indicator_names
  if (length(output_names) != ncol(values)) {
    stop("indicator_names must have one name for each supply measure")
  }

  out <- .fca_blocks(
    rasters = c(list(demand = demand), rasters),
    kernels = kernels,
    supply = values,
    normalize = demand_normalize,
    snap = snap
  )
  .fca_rewrap(out, rasters[[1L]], output_names)
}
