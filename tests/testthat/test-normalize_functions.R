# tests/testthat/test-weight-functions.R

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

# Tests for calc_normalize() function
test_that("calc_normalize handles different methods correctly", {
  # Test standard normalization
  expect_equal(
    calc_normalize(simple_weights, "standard"),
    simple_weights/sum(simple_weights)
  )

  # Test semi-normalization
  expect_equal(
    calc_normalize(simple_weights, "semi"),
    simple_weights/sum(simple_weights)  # sum > 1 case
  )

  # Test reference normalization
  expect_equal(
    calc_normalize(simple_weights, "reference"),
    simple_weights/max(simple_weights)
  )

  # Test identity
  expect_equal(calc_normalize(simple_weights, "identity"), simple_weights)

  # Test custom function
  custom_norm <- function(x) x/2
  expect_equal(calc_normalize(simple_weights, custom_norm), simple_weights/2)
})

test_that("calc_normalize throws appropriate errors", {
  expect_error(calc_normalize(simple_weights, "invalid_method"))
  expect_error(calc_normalize("not_numeric", "standard"))
  expect_error(calc_normalize(numeric(0)), "Input weights cannot be empty")
})

# Tests for standard normalization
test_that("standard normalization produces correct weights", {
  # Test with simple numeric vector
  result <- calc_normalize(simple_weights, "standard")
  expect_equal(sum(result), 1)
  expect_equal(result, simple_weights/sum(simple_weights))

  # Test with zeros
  expect_equal(calc_normalize(zero_weights, "standard"), zero_weights)

  # Test with NAs
  na_result <- calc_normalize(na_weights, "standard")
  expect_equal(sum(na_result, na.rm = TRUE), 1)

  # Test with raster
  rast_result <- calc_normalize(test_raster, "standard")
  expect_s4_class(rast_result, "SpatRaster")
  # Check each cell sums to 1 across layers
  cell_sums <- sum(rast_result)
  expect_true(all(abs(values(cell_sums)[!is.na(values(cell_sums))] - 1) < 1e-10))
})

# Tests for semi-normalization
test_that("semi-normalization works correctly", {
  # Test case where sum > 1
  high_sum <- c(0.5, 1.0, 1.5)
  result <- calc_normalize(high_sum, "semi")
  expect_equal(sum(result), 1)

  # Test case where sum <= 1
  low_sum <- c(0.2, 0.3, 0.4)
  expect_equal(calc_normalize(low_sum, "semi"), low_sum)

  # Test with raster
  rast_result <- calc_normalize(test_raster, "semi")
  expect_s4_class(rast_result, "SpatRaster")
  cell_sums <- sum(rast_result)
  # Check that no cell sum exceeds 1
  expect_true(all(values(cell_sums)[!is.na(values(cell_sums))] <= 1 + 1e-10))
})

# Tests for reference normalization
test_that("reference normalization works correctly", {
  # Test with default reference (max value)
  result <- calc_normalize(simple_weights, "reference")
  expect_equal(max(result), 1)
  expect_equal(result, simple_weights/max(simple_weights))

  # Test with custom reference value
  ref_value <- 4
  result <- calc_normalize(simple_weights, "reference", ref_value = ref_value)
  expect_equal(result, simple_weights/ref_value)

  # Test with raster
  rast_result <- calc_normalize(test_raster, "reference")
  expect_s4_class(rast_result, "SpatRaster")
  # Check maximum value is 1
  expect_true(max(values(rast_result), na.rm = TRUE) <= 1 + 1e-10)
})

# Edge cases and error handling
test_that("calc_normalize handles edge cases appropriately", {
  # Test with negative values
  neg_weights <- c(-1, -2, -3)
  expect_error(calc_normalize(neg_weights, "standard"), NA)  # Should not error

  # Test with single value
  single_weight <- 5
  expect_equal(calc_normalize(single_weight, "standard"), 1)

  # Test with all NAs
  all_na <- c(NA, NA, NA)
  expect_true(all(is.na(calc_normalize(all_na, "standard"))))
})

# Performance tests (optional)
test_that("calc_normalize performs efficiently with large datasets", {
  skip_on_ci()

  # Create large raster
  large_raster <- create_test_raster(1000, 1000, 5)

  # Test performance
  expect_lt(
    system.time(calc_normalize(large_raster, "standard"))[["elapsed"]],
    5  # Should complete in less than 5 seconds
  )
})
