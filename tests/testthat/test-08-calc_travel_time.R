# calc_travel_time(): least-cost travel over a friction surface, checked
# against straight-line times on uniform friction.

# Toy grids in WGS 84 / UTM zone 30N: terra projects inputs to the grid, and a
# system on the WGS 84 datum needs no PROJ datum grid, which could be downloaded.
tt_grid <- function(n = 21, res = 100) {
  g <- terra::rast(nrows = n, ncols = n, xmin = 0, xmax = n * res, ymin = 0,
                   ymax = n * res, crs = "EPSG:32630")
  terra::values(g) <- 1
  g
}
tt_points <- function(xy, ids) {
  v <- terra::vect(xy, crs = "EPSG:32630")
  v$id <- ids
  v
}

test_that("uniform friction gives straight-line times within the raster approximation", {
  g <- tt_grid()
  m <- calc_travel_time(terra::init(g, 0.01), tt_points(cbind(1050, 1050), "c"), g,
                        ids = "id", buffer = 0)
  expect_identical(colnames(m), "c")
  expect_identical(attr(m, "cell_index"), seq_len(terra::ncell(g)))
  xy <- terra::xyFromCell(g, attr(m, "cell_index"))
  d <- sqrt((xy[, 1] - 1050)^2 + (xy[, 2] - 1050)^2) * 0.01
  row <- abs(xy[, 2] - 1050) < 1e-6
  diagonal <- abs(abs(xy[, 1] - 1050) - abs(xy[, 2] - 1050)) < 1e-6
  expect_equal(unname(m[row, 1]), d[row], tolerance = 1e-6)
  expect_true(all(abs(m[diagonal, 1] - d[diagonal]) <= (sqrt(2) - 1) / 2 + 1e-6))
  far <- d > 3
  expect_true(all(abs(m[far, 1] - d[far]) / d[far] < 0.08))
  expect_equal(unname(m[d == 0, 1]), 0)
})

test_that("barriers force detours, max_time removes edges, rasters match", {
  g <- tt_grid()
  fac <- tt_points(cbind(c(550, 1550), c(1050, 1050)), c("west", "east"))
  # A wall at x = 1000-1100 with one gap at the top.
  wall <- terra::vect("POLYGON ((1000 0, 1100 0, 1100 1900, 1000 1900, 1000 0))",
                      crs = "EPSG:32630")
  keep <- terra::erase(terra::as.polygons(terra::ext(g), crs = "EPSG:32630"), wall)
  open <- calc_travel_time(terra::init(g, 0.01), fac, g, ids = "id", buffer = 0)
  walled <- calc_travel_time(terra::init(g, 0.01), fac, g, ids = "id", mask = keep, buffer = 0)
  across <- terra::cellFromXY(g, cbind(1550, 1050))
  expect_gt(walled[as.character(across), "west"], open[as.character(across), "west"] + 10)
  inside_wall <- terra::cellFromXY(g, cbind(1050, 550))
  expect_identical(walled[as.character(inside_wall), "west"], Inf)
  capped <- calc_travel_time(terra::init(g, 0.01), fac, g, ids = "id", buffer = 0, max_time = 5)
  expect_true(all(is.infinite(capped[open > 5])))
  expect_equal(capped[open <= 5], open[open <= 5])
  r <- calc_travel_time(terra::init(g, 0.01), fac, g, ids = "id", buffer = 0,
                        max_time = 5, type = "raster")
  expect_identical(names(r), c("west", "east"))
  v <- terra::values(r)
  expect_equal(v[!is.na(v)], capped[is.finite(capped)])
})

test_that("only non-missing template cells are origins and prepare_allocation accepts the matrix", {
  g <- tt_grid()
  g[1:50] <- NA
  m <- calc_travel_time(terra::init(g, 0.01), tt_points(cbind(1050, 1050), "c"), g,
                        ids = "id", buffer = 0)
  expect_identical(attr(m, "cell_index"), 51:terra::ncell(g))
  demand <- stats::setNames(rep(1, nrow(m)), rownames(m))
  inputs <- prepare_allocation(demand, c(c = 10), m,
    spatial = list(template = g, cell_index = attr(m, "cell_index")))
  expect_s3_class(inputs, "ae_prepared_inputs")
})

test_that("invalid inputs fail clearly", {
  g <- tt_grid()
  fr <- terra::init(g, 0.01)
  fac <- tt_points(cbind(1050, 1050), "c")
  lonlat <- terra::rast(nrows = 5, ncols = 5)
  expect_error(calc_travel_time(fr, fac, lonlat, ids = "id"), "projected")
  expect_error(calc_travel_time(fr, fac, g, ids = "nope"), "ids must name")
  expect_error(calc_travel_time(fr, tt_points(cbind(c(10, 20), c(10, 20)), c("a", "a")), g,
                                ids = "id"), "unique")
  expect_error(calc_travel_time(fr, tt_points(cbind(99999, 99999), "far"), g, ids = "id",
                                buffer = 0), "outside the working grid")
  expect_error(calc_travel_time(c(fr, fr), fac, g, ids = "id"), "one-layer")
  expect_error(calc_travel_time(fr, fac, g, ids = "id", max_time = 0), "max_time")
  zero <- fr
  zero[1] <- 0
  expect_error(calc_travel_time(zero, fac, g, ids = "id", buffer = 0), "positive")
  oblong <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 2000, ymin = 0, ymax = 1000,
                        crs = "EPSG:32630")
  terra::values(oblong) <- 1
  expect_error(calc_travel_time(terra::init(oblong, 0.01), fac, oblong, ids = "id"), "square")
})
