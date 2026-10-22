# SPAX-055: the FCA driver on the block engine --------------------------------

#' Internal FCA driver
#'
#' Checks the grids, binds the supply to the facility IDs, runs the two-pass
#' block engine and returns a SpatRaster on the grid of the first raster of
#' `rasters`. That raster also names the facilities; the wrappers pass the
#' distance raster first.
#'
#' Ungrouped, `demand` is one SpatRaster, `kernels` one kernel pair and the
#' result has one layer per supply measure. Grouped, `demand` is a named list of
#' one-layer SpatRasters (the names are the group IDs), `kernels` a list over the
#' groups, named by group ID, of such pairs, and `rasters` holds the rasters the
#' kernels read, every key ending in `.<group>` (see `.fca_decay_spec()`). The
#' result then has the layers `<measure>.<group>` and `<measure>.all` (see
#' `.fca_blocks()` and `.fca_layer_table()`), and a `layers` attribute: the
#' layer table, a data frame with the columns `layer`, `measure` and `group`
#' (`"all"` for the overall layer). The attribute is read by the wrapper right
#' away, which stores it in `parameters$layers`; it does not survive raster
#' arithmetic.
#'
#' @param demand One-layer SpatRaster of demand, or a named list of them (one
#'   per demand group).
#' @param supply Vector, matrix or data frame, as for [compute_access()].
#' @param rasters Named list of the other SpatRasters the kernels read; the
#'   first one has one layer per facility. A key must not name two different
#'   rasters.
#' @param kernels `list(demand = function(b, rows), access = function(b, rows))`
#'   as for `.fca_blocks()`, or a list of them over the groups.
#' @inheritParams compute_access
#' @param demand_normalize Normalization of the demand kernel.
#' @return SpatRaster of accessibility.
#' @keywords internal
compute_fca <- function(demand, supply, rasters, kernels,
                        demand_normalize = "identity",
                        id_col = NULL, supply_cols = NULL,
                        indicator_names = NULL, snap = FALSE) {
  grouped <- is.list(demand) && !inherits(demand, "SpatRaster")
  if (grouped) {
    groups <- names(demand)
    if (!length(demand) || is.null(groups) || anyNA(groups) ||
        any(groups == "") || anyDuplicated(groups)) {
      .stop("demand groups must be a named list with unique, non-empty names")
    }
    for (g in groups) {
      .chck_class(demand[[g]], "SpatRaster", paste0("demand group '", g, "'"))
      if (terra::nlyr(demand[[g]]) > 1L) {
        .stop("each demand group must be a single-layer raster")
      }
    }
    demand_list <- stats::setNames(demand, paste0("demand.", groups))
  } else {
    .chck_class(demand, "SpatRaster", "demand")
    if (terra::nlyr(demand) > 1L) {
      .stop("give demand groups as a named list of single-layer rasters")
    }
    groups <- NULL
    demand_list <- list(demand = demand)
  }
  for (nm in names(rasters)) {
    if (terra::nrow(rasters[[nm]]) != terra::nrow(demand_list[[1L]]) ||
        terra::ncol(rasters[[nm]]) != terra::ncol(demand_list[[1L]])) {
      .stop(sprintf("demand and %s must have the same number of rows and columns", nm))
    }
  }
  for (nm in names(demand_list)[-1L]) {
    if (!terra::compareGeom(demand_list[[1L]], demand_list[[nm]],
                            stopOnError = FALSE)) {
      .stop("all demand groups must have the same geometry")
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
    .stop("supply must have one row for each facility")
  }
  measures <- if (is.null(indicator_names)) processed$cols else indicator_names
  if (length(measures) != ncol(values)) {
    .stop("indicator_names must have one name for each supply measure")
  }

  out <- .fca_blocks(
    rasters = .fca_merge_rasters(c(demand_list, rasters)),
    kernels = kernels,
    supply = values,
    normalize = demand_normalize,
    snap = snap,
    groups = groups
  )
  layers <- .fca_layer_table(measures, groups)
  result <- .fca_rewrap(out, rasters[[1L]], layers$layer)
  if (grouped) {
    attr(result, "layers") <- layers
  }
  result
}
