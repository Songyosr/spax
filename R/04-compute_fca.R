# SPAX-055: the FCA driver on the block engine --------------------------------

#' Internal FCA driver
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
compute_fca <- function(demand, supply, rasters, kernels,
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
