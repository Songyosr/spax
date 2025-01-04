# Helper function to create test rasters
create_test_raster <- function(values, nrows = 3, ncols = 3) {
  r <- terra::rast(nrows = nrows, ncols = ncols)
  terra::values(r) <- values
  return(r)
}

test_that("transform_pmf produces valid PMF from density values", {
  test_values <- c(10, 20, 30, 40)
  density_rast <- create_test_raster(test_values, 2, 2)

  pmf <- transform_pmf(density_rast)

  expect_s4_class(pmf, "SpatRaster")
  expect_equal(sum(terra::values(pmf)), 1)

  pmf_values <- as.vector(terra::values(pmf))
  expected_values <- test_values / sum(test_values)
  expect_equal(pmf_values, expected_values)
})

test_that("transform_pmf handles zero values correctly", {
  test_values <- c(0, 10, 20, 0)
  density_rast <- create_test_raster(test_values, 2, 2)
  pmf <- transform_pmf(density_rast)
  expect_equal(sum(terra::values(pmf)), 1)
})

test_that("transform_pmf handles NA values correctly", {
  test_values <- c(NA, 10, 20, 30)
  density_rast <- create_test_raster(test_values, 2, 2)
  pmf <- transform_pmf(density_rast)
  expect_equal(sum(terra::values(pmf), na.rm = TRUE), 1)
})

test_that("transform_pmf errors on negative values", {
  test_values <- c(-1, 10, 20, 30)
  density_rast <- create_test_raster(test_values, 2, 2)
  expect_error(transform_pmf(density_rast), "values in x must be >= 0")

  all_neg_rast <- create_test_raster(rep(-1, 4), 2, 2)
  expect_error(transform_pmf(all_neg_rast), "values in x must be >= 0")
})

test_that("transform_pmf errors on invalid inputs", {
  expect_error(transform_pmf(c(1, 2, 3)), "x must be one of the following classes: SpatRaster")

  all_na_rast <- create_test_raster(rep(NA, 4), 2, 2)
  expect_error(transform_pmf(all_na_rast), "Input raster 'x' contains only NA values")

  all_zero_rast <- create_test_raster(rep(0, 4), 2, 2)
  expect_error(transform_pmf(all_zero_rast), "Sum of all values in 'x' is zero - cannot create PMF")
})

test_that("transform_pmf preserves relative proportions", {
  test_values <- c(10, 20, 30, 40)
  density_rast <- create_test_raster(test_values, 2, 2)
  pmf <- transform_pmf(density_rast)

  pmf_values <- as.vector(terra::values(pmf))
  original_ratios <- test_values[-1] / test_values[-length(test_values)]
  pmf_ratios <- pmf_values[-1] / pmf_values[-length(test_values)]
  expect_equal(original_ratios, pmf_ratios)
})

test_that("transform_pmf handles different raster sizes", {
  test_values <- seq(1, 25)
  density_rast <- create_test_raster(test_values, 5, 5)
  pmf <- transform_pmf(density_rast)
  expect_equal(dim(pmf)[1:2], c(5, 5))
  expect_equal(sum(terra::values(pmf)), 1)
})

test_that("transform_pmf maintains spatial properties", {
  test_values <- c(10, 20, 30, 40)
  density_rast <- create_test_raster(test_values, 2, 2)
  terra::ext(density_rast) <- c(0, 100, 0, 100)
  terra::crs(density_rast) <- "EPSG:4326"

  pmf <- transform_pmf(density_rast)

  expect_equal(as.vector(terra::ext(pmf)), as.vector(terra::ext(density_rast)))
  expect_equal(terra::crs(pmf, proj = TRUE), terra::crs(density_rast, proj = TRUE))
  expect_equal(terra::res(pmf), terra::res(density_rast))
})

test_that("transform_pmf snap mode works correctly", {
  test_values <- c(10, 20, 30, 40)
  density_rast <- create_test_raster(test_values, 2, 2)

  # Results should be identical with and without snap
  result_normal <- transform_pmf(density_rast, snap = FALSE)
  result_snap <- transform_pmf(density_rast, snap = TRUE)
  expect_equal(terra::values(result_normal), terra::values(result_snap))
})
