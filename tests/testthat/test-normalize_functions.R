# tests/testthat/test-weight-functions.R

#' Test suite for weight normalization functions
#' Contains tests for numeric vectors, matrices, and SpatRaster objects
#' across different normalization methods

library(testthat)
library(terra)

# Helper function to create test SpatRaster
create_test_raster <- function(nrow = 3, ncol = 3, nlyr = 2) {
  r <- rast(nrows = nrow, ncols = ncol, nlyr = nlyr)
  values(r) <- runif(nrow * ncol * nlyr)
  return(r)
}

# Setup test data
simple_weights <- c(0.5, 1.0, 1.5, 2.0)
zero_weights <- c(0, 0, 0, 0)
na_weights <- c(0.5, NA, 1.5, 2.0)
test_raster <- create_test_raster()

# Tests for normalize() function
test_that("normalize handles different methods correctly", {
  expect_equal(normalize(simple_weights, "identity"), simple_weights)
  expect_equal(normalize(simple_weights, "normalize"), simple_weights/sum(simple_weights))
  expect_equal(normalize(simple_weights, "semi_normalize"), simple_weights/sum(simple_weights))

  # Custom function test
  custom_norm <- function(x) x/2
  expect_equal(normalize(simple_weights, custom_norm), simple_weights/2)
})

test_that("normalize throws appropriate errors", {
  expect_error(normalize(simple_weights, "invalid_method"))
  expect_error(normalize("not_numeric", "normalize"))
})

# Tests for calc_identity
test_that("calc_identity returns input unchanged", {
  expect_equal(calc_identity(simple_weights), simple_weights)
  expect_equal(calc_identity(zero_weights), zero_weights)
  expect_equal(calc_identity(na_weights), na_weights)

  # Test with raster
  rast_result <- calc_identity(test_raster)
  expect_s4_class(rast_result, "SpatRaster")
  expect_equal(values(rast_result), values(test_raster))
})

# Tests for calc_normalize
test_that("calc_normalize produces correct normalized weights", {
  # Test with simple numeric vector
  result <- calc_normalize(simple_weights)
  expect_equal(sum(result), 1)
  expect_equal(result, simple_weights/sum(simple_weights))

  # Test with zeros
  expect_equal(calc_normalize(zero_weights), zero_weights)

  # Test with NAs
  na_result <- calc_normalize(na_weights)
  expect_equal(sum(na_result, na.rm = TRUE), 1)

  # Test with raster
  rast_result <- calc_normalize(test_raster)
  expect_s4_class(rast_result, "SpatRaster")
  # Check each cell sums to 1 across layers
  cell_sums <- sum(rast_result)
  expect_true(all(abs(values(cell_sums)[!is.na(values(cell_sums))] - 1) < 1e-10))
})

# Tests for calc_semi_normalize
test_that("calc_semi_normalize works correctly", {
  # Test case where sum > 1
  high_sum <- c(0.5, 1.0, 1.5)
  result <- calc_semi_normalize(high_sum)
  expect_equal(sum(result), 1)

  # Test case where sum <= 1
  low_sum <- c(0.2, 0.3, 0.4)
  expect_equal(calc_semi_normalize(low_sum), low_sum)

  # Test with raster
  rast_result <- calc_semi_normalize(test_raster)
  expect_s4_class(rast_result, "SpatRaster")
  cell_sums <- sum(rast_result)
  # Check that no cell sum exceeds 1
  expect_true(all(values(cell_sums)[!is.na(values(cell_sums))] <= 1 + 1e-10))
})

# Tests for calc_competing
test_that("calc_competing calculates relative weights correctly", {
  # Test with default reference (max value)
  result <- calc_competing(simple_weights)
  expect_equal(max(result), 1)
  expect_equal(result, simple_weights/max(simple_weights))

  # Test with custom reference value
  ref_value <- 4
  result <- calc_competing(simple_weights, ref_value)
  expect_equal(result, simple_weights/ref_value)

  # Test with zeros
  expect_equal(calc_competing(zero_weights), zero_weights)

  # Test with raster
  rast_result <- calc_competing(test_raster)
  expect_s4_class(rast_result, "SpatRaster")
  # Check maximum value is 1
  expect_true(max(values(rast_result), na.rm = TRUE) <= 1 + 1e-10)
})

# Edge cases and error handling
test_that("functions handle edge cases appropriately", {
  # Test with negative values
  neg_weights <- c(-1, -2, -3)
  expect_error(calc_normalize(neg_weights), NA)  # Should not error

  # Test with single value
  single_weight <- 5
  expect_equal(calc_normalize(single_weight), 1)

  # Test with empty vector
  expect_error(calc_normalize(numeric(0)))

  # Test with all NAs
  all_na <- c(NA, NA, NA)
  expect_true(all(is.na(calc_normalize(all_na))))
})

# Performance tests (optional)
test_that("functions perform efficiently with large datasets", {
  # Skip on CI/CD
  skip_on_ci()

  # Create large raster
  large_raster <- create_test_raster(1000, 1000, 5)

  # Test performance
  expect_lt(
    system.time(calc_normalize(large_raster))[["elapsed"]],
    5  # Should complete in less than 5 seconds
  )
})
#
