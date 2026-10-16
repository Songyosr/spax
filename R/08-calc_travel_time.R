#' Travel times to facilities over a friction surface
#'
#' Computes least-cost travel time from each facility to every non-missing cell
#' of an analysis grid, by accumulating friction (time per unit distance) along
#' the cheapest path between neighboring cells with [terra::costDist()]. The
#' result is continuous, with no isochrone bands, and can be passed directly to
#' [prepare_allocation()] as its travel matrix.
#'
#' The friction surface is projected to the grid's lattice, extended by
#' `buffer` so that routes can leave the analysis area, and cells outside
#' `mask` are treated as barriers (for example, to keep routes within one
#' country). Global motorized friction surfaces, such as those of the Malaria
#' Atlas Project, give minutes per metre; the travel times are then in minutes.
#' The function downloads nothing.
#'
#' Times are measured between cell centers; the facility's own cell has time
#' zero. Paths that leave the facility cell diagonally are underestimated by at
#' most about a fifth of one cell crossing. Because paths move between
#' neighboring cells in eight directions, travel over uniform friction exceeds
#' the straight-line time by up to about 8%.
#'
#' @param friction A one-layer `SpatRaster` of travel time per unit of map
#'   distance (for example minutes per metre). Missing cells are barriers.
#' @param facilities An `sf` or `SpatVector` of facility points.
#' @param template A one-layer `SpatRaster` in a projected coordinate reference
#'   system with map units of metres. Its non-missing cells are the origins.
#' @param ids Name of the facility ID column. The IDs must be unique and
#'   nonmissing; they name the columns (or layers) of the result.
#' @param mask Optional polygons (`sf` or `SpatVector`); friction outside them
#'   is a barrier.
#' @param buffer Distance in map units by which the working grid extends beyond
#'   the template (default 50 km).
#' @param max_time Travel times above this value become unreachable: `Inf` in
#'   the matrix (an absent edge for [prepare_allocation()]) and `NA` in the
#'   raster.
#' @param type `"matrix"` (default) for an origin-by-facility matrix whose row
#'   names are template cell numbers, or `"raster"` for one layer per facility.
#' @return For `type = "matrix"`, a numeric matrix with a `cell_index`
#'   attribute giving the template cell of each row, so that
#'   `spatial = list(template = template, cell_index = attr(x, "cell_index"))`
#'   can be passed to [prepare_allocation()]. For `type = "raster"`, a
#'   `SpatRaster` on the template grid.
#' @examples
#' library(sf)
#' library(terra)
#'
#' # Cornwall population grid in British National Grid (metres)
#' pop <- project(read_spax_example("cornwall_pop.tif"), "EPSG:27700")
#'
#' # A uniform friction surface of 50 km/h, in minutes per metre
#' friction <- init(pop, 60 / 50000)
#'
#' # Minutes from five practices to every populated cell
#' gp <- cornwall_gp[1:5, ]
#' travel <- calc_travel_time(friction, gp, pop, ids = "practice", buffer = 5000)
#' summary(travel[, 1])
#' @family experimental allocation workflow
#' @export
#' @md
calc_travel_time <- function(friction, facilities, template, ids,
                             mask = NULL, buffer = 50000, max_time = Inf,
                             type = c("matrix", "raster")) {
  type <- match.arg(type)
  if (!inherits(friction, "SpatRaster") || terra::nlyr(friction) != 1L) {
    stop("friction must be a one-layer SpatRaster")
  }
  if (!inherits(template, "SpatRaster") || terra::nlyr(template) != 1L) {
    stop("template must be a one-layer SpatRaster")
  }
  if (terra::is.lonlat(template)) {
    stop("template must use a projected coordinate reference system in metres")
  }
  if (inherits(facilities, "sf")) facilities <- terra::vect(facilities)
  if (!inherits(facilities, "SpatVector") || terra::geomtype(facilities) != "points") {
    stop("facilities must be sf or SpatVector points")
  }
  if (!is.character(ids) || length(ids) != 1L || !ids %in% names(facilities)) {
    stop("ids must name a column of facilities")
  }
  fid <- as.character(facilities[[ids, drop = TRUE]])
  if (anyNA(fid) || any(!nzchar(fid)) || anyDuplicated(fid)) {
    stop("facility IDs must be unique and nonmissing")
  }
  .chck_nonnegative_scalar(buffer, "buffer")
  if (!is.numeric(max_time) || length(max_time) != 1L || is.na(max_time) || max_time <= 0) {
    stop("max_time must be a positive number or Inf")
  }

  steps <- ceiling(buffer / min(terra::res(template)))
  work <- terra::extend(terra::rast(template), steps)
  fr <- terra::project(friction, work, method = "bilinear")
  if (!is.null(mask)) {
    if (inherits(mask, "sf")) mask <- terra::vect(mask)
    if (!inherits(mask, "SpatVector") || terra::geomtype(mask) != "polygons") {
      stop("mask must be sf or SpatVector polygons")
    }
    fr <- terra::mask(fr, terra::project(mask, terra::crs(work)))
  }
  facilities <- terra::project(facilities, terra::crs(work))
  cells <- terra::cellFromXY(work, terra::crds(facilities))
  if (anyNA(cells)) {
    stop("facilities outside the working grid: ", paste(fid[is.na(cells)], collapse = ", "))
  }
  start_friction <- terra::values(fr, mat = FALSE)[cells]
  if (anyNA(start_friction)) {
    stop("facilities on missing friction or outside the mask: ",
         paste(fid[is.na(start_friction)], collapse = ", "))
  }
  # costDist() marks the start by giving it zero friction, which drops the half
  # step out of the facility cell; add it back for straight moves (diagonal
  # first moves remain short by at most (sqrt(2) - 1) / 2 of a cell crossing).
  step <- min(terra::res(work))
  layers <- lapply(seq_along(cells), function(k) {
    target <- fr
    target[cells[k]] <- 0
    d <- terra::costDist(target, target = 0)
    d <- d + 0.5 * step * start_friction[k]
    d[cells[k]] <- 0
    terra::crop(d, template)
  })
  times <- terra::mask(terra::rast(layers), template)
  names(times) <- fid
  if (type == "raster") {
    if (is.finite(max_time)) times <- terra::ifel(times > max_time, NA, times)
    return(times)
  }
  keep <- which(!is.na(terra::values(template, mat = FALSE)))
  out <- terra::values(times, mat = TRUE)[keep, , drop = FALSE]
  out[is.na(out) | out > max_time] <- Inf
  dimnames(out) <- list(as.character(keep), fid)
  attr(out, "cell_index") <- keep
  out
}
