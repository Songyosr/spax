#' @testfile Tests for transform_pmf function
#' @description Test suite for probability mass function computation
library(testthat)
library(terra)

# Helper function to create test rasters
create_test_raster <- function(values, nrows = 3, ncols = 3) {
  r <- rast(nrows = nrows, ncols = ncols)
  values(r) <- values
  return(r)
}

test_that("transform_pmf produces valid PMF from density values", {
  # Create test raster with known values
  test_values <- c(10, 20, 30, 40)
  density_rast <- create_test_raster(test_values, 2, 2)

  # Calculate PMF
  pmf <- transform_pmf(density_rast)

  # Tests
  expect_s4_class(pmf, "SpatRaster")
  expect_equal(sum(values(pmf)), 1)

  # Compare raw values instead of raster objects
  pmf_values <- as.vector(values(pmf))
  expected_values <- test_values/sum(test_values)
  expect_equal(pmf_values, expected_values)
})

test_that("transform_pmf handles zero values correctly", {
  test_values <- c(0, 10, 20, 0)
  density_rast <- create_test_raster(test_values, 2, 2)
  pmf <- transform_pmf(density_rast)
  expect_equal(sum(values(pmf)), 1)
})

test_that("transform_pmf handles NA values correctly", {
  test_values <- c(NA, 10, 20, 30)
  density_rast <- create_test_raster(test_values, 2, 2)
  pmf <- transform_pmf(density_rast)
  expect_equal(sum(values(pmf), na.rm = TRUE), 1)
})

test_that("transform_pmf errors on negative values", {
  # Test raster with negative values
  test_values <- c(-1, 10, 20, 30)
  density_rast <- create_test_raster(test_values, 2, 2)
  expect_error(transform_pmf(density_rast), "Input values cannot be negative")

  # Test raster with all negative values
  all_neg_rast <- create_test_raster(rep(-1, 4), 2, 2)
  expect_error(transform_pmf(all_neg_rast), "Input values cannot be negative")
})

test_that("transform_pmf errors on invalid inputs", {
  # Test invalid input class
  expect_error(transform_pmf(c(1, 2, 3)), "Input must be a SpatRaster object")

  # Test all NA values
  all_na_rast <- create_test_raster(rep(NA, 4), 2, 2)
  expect_error(transform_pmf(all_na_rast), "Input raster contains only NA values")

  # Test all zero values
  all_zero_rast <- create_test_raster(rep(0, 4), 2, 2)
  expect_error(transform_pmf(all_zero_rast), "Total sum is zero")
})

test_that("transform_pmf preserves relative proportions", {
  # Test that relative proportions are maintained
  test_values <- c(10, 20, 30, 40)
  density_rast <- create_test_raster(test_values, 2, 2)
  pmf <- transform_pmf(density_rast)

  # Check ratios between cells are preserved
  pmf_values <- as.vector(values(pmf))
  original_ratios <- test_values[-1] / test_values[-length(test_values)]
  pmf_ratios <- pmf_values[-1] / pmf_values[-length(test_values)]
  expect_equal(original_ratios, pmf_ratios)
})

test_that("transform_pmf handles different raster sizes", {
  # Test with different raster dimensions
  test_values <- seq(1, 25)
  density_rast <- create_test_raster(test_values, 5, 5)
  pmf <- transform_pmf(density_rast)
  expect_equal(dim(pmf)[1:2], c(5, 5))
  expect_equal(sum(values(pmf)), 1)
})

test_that("transform_pmf maintains spatial properties", {
  # Test that spatial properties are preserved
  test_values <- c(10, 20, 30, 40)
  density_rast <- create_test_raster(test_values, 2, 2)
  ext(density_rast) <- c(0, 100, 0, 100)  # Set custom extent
  crs(density_rast) <- "EPSG:4326"        # Set CRS

  pmf <- transform_pmf(density_rast)

  # Compare individual components instead of whole extent object
  expect_equal(as.vector(ext(pmf)), as.vector(ext(density_rast)))
  expect_equal(crs(pmf, proj=TRUE), crs(density_rast, proj=TRUE))
  expect_equal(res(pmf), res(density_rast))
})
