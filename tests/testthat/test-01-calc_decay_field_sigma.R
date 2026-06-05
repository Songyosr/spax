# Variable-catchment FCA (V2SFCA): calc_decay with a SpatRaster sigma.
# Closes the ring-0 coverage hole "decay parameter as a field, not a scalar".

test_that("a constant-valued sigma raster equals the scalar sigma", {
  d <- terra::rast(nrows = 4, ncols = 4)
  terra::values(d) <- as.numeric(1:16)
  sig <- terra::setValues(d, 5)

  expect_equal(
    terra::values(calc_decay(d, "gaussian", sigma = sig)),
    terra::values(calc_decay(d, "gaussian", sigma = 5))
  )
  expect_equal(
    terra::values(calc_decay(d, "exponential", sigma = sig)),
    terra::values(calc_decay(d, "exponential", sigma = 5))
  )
})

test_that("a varying sigma raster yields a spatially varying catchment", {
  d <- terra::rast(nrows = 4, ncols = 4)
  terra::values(d) <- rep(10, 16)                       # same distance everywhere
  sig <- terra::setValues(d, rep(c(2, 20), length.out = 16))

  w <- terra::values(calc_decay(d, "gaussian", sigma = sig))[, 1]
  # at a fixed distance, larger sigma -> larger weight, so weights vary by cell
  expect_gt(length(unique(round(w, 8))), 1)
})

test_that("a single-layer sigma broadcasts across distance layers (per-cell catchment)", {
  d1 <- terra::rast(nrows = 4, ncols = 4); terra::values(d1) <- as.numeric(1:16)
  d2 <- terra::rast(nrows = 4, ncols = 4); terra::values(d2) <- as.numeric(16:1)
  d  <- c(d1, d2); names(d) <- c("f1", "f2")
  sig <- terra::setValues(d1, seq(2, 8, length.out = 16))

  w <- calc_decay(d, "gaussian", sigma = sig)
  expect_equal(terra::nlyr(w), 2L)
  expect_equal(
    unname(terra::values(w[[1]])),
    unname(terra::values(calc_decay(d1, "gaussian", sigma = sig)))
  )
})

test_that("calc_decay rejects a misaligned, non-positive, or geometry-less sigma raster", {
  d <- terra::rast(nrows = 4, ncols = 4); terra::values(d) <- as.numeric(1:16)
  bad <- terra::rast(nrows = 3, ncols = 3); terra::values(bad) <- as.numeric(1:9)
  neg <- terra::setValues(d, -1)

  expect_error(calc_decay(d, "gaussian", sigma = bad))          # misaligned geometry
  expect_error(calc_decay(d, "gaussian", sigma = neg))          # non-positive
  expect_error(calc_decay(1:5, "gaussian", sigma = terra::setValues(d, 3)))  # raster sigma needs raster distance
})
