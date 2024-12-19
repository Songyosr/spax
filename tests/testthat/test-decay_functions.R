# Import required libraries
library(testthat)
library(terra)  # For SpatRaster tests

# Helper setup function to create test data
create_test_distances <- function() {
  list(
    vector = c(0, 10, 20, 30, 40, 50),
    matrix = matrix(1:25, 5, 5),
    raster = rast(matrix(1:100, 10, 10))
  )
}

# Test main calc_decay function
test_that("calc_decay handles different input types", {
  test_data <- create_test_distances()

  # Test vector input
  expect_type(
    calc_decay(test_data$vector, method = "gaussian"),
    "double"
  )

  # Test matrix input
  expect_true(
    is.matrix(calc_decay(test_data$matrix, method = "gaussian"))
  )

  # Test raster input
  expect_s4_class(
    calc_decay(test_data$raster, method = "gaussian"),
    "SpatRaster"
  )
})

test_that("calc_decay handles custom functions", {
  custom_decay <- function(distance, sigma, scale = 1) {
    scale * exp(-(distance^2) / (2 * sigma^2))
  }
  result <- calc_decay(1:5, method = custom_decay, sigma = 2, scale = 2)
  expect_type(result, "double")
  expect_length(result, 5)
})

# Test individual weight functions
test_that("gaussian weights computation works correctly", {
  distances <- seq(0, 50, by = 10)
  weights <- .gaussian_weights(distances, sigma = 20)

  # Test basic properties
  expect_length(weights, length(distances))
  expect_true(all(weights <= 1))
  expect_true(all(weights >= 0))

  # Test specific known values
  expect_equal(weights[1], 1)  # Weight at distance 0 should be 1
  expect_true(weights[1] > weights[2])  # Decreasing with distance
})

test_that("binary weights computation works correctly", {
  distances <- seq(0, 100, by = 20)
  threshold <- 50
  weights <- .binary_weights(distances, sigma = threshold)

  expect_true(all(weights %in% c(0, 1)))
  expect_true(all(weights[distances <= threshold] == 1))
  expect_true(all(weights[distances > threshold] == 0))
})

test_that("exponential weights computation works correctly", {
  distances <- seq(0, 50, by = 10)
  weights <- .exponential_weights(distances, sigma = 0.1)

  expect_length(weights, length(distances))
  expect_true(all(weights <= 1))
  expect_true(all(weights >= 0))
  expect_true(all(diff(weights) < 0))  # Strictly decreasing
})

test_that("power weights computation works correctly", {
  distances <- seq(1, 50, by = 10)  # Start from 1 to avoid infinity
  weights <- .power_weights(distances, sigma = 2)

  expect_length(weights, length(distances))
  expect_true(all(weights > 0))
  expect_true(all(diff(weights) < 0))  # Strictly decreasing
})

test_that("inverse weights computation works correctly", {
  distances <- seq(0, 50, by = 10)
  weights <- .inverse_weights(distances, c = 1)

  expect_length(weights, length(distances))
  expect_true(all(weights > 0))
  expect_true(all(diff(weights) < 0))  # Strictly decreasing
  expect_equal(weights[1], 1)  # At distance 0 with c=1
})

# Test error handling
test_that("calc_decay handles invalid inputs appropriately", {
  # Test invalid method
  expect_error(
    calc_decay(1:5, method = "invalid_method"),
    "Invalid method specified"
  )

  # Test NA handling
  distances <- c(0, 10, NA, 30)
  weights <- calc_decay(distances, method = "gaussian")
  expect_true(is.na(weights[3]))
})

# Test edge cases
test_that("calc_decay handles edge cases", {
  # Empty input
  expect_length(calc_decay(numeric(0), method = "gaussian"), 0)

  # Single value
  expect_length(calc_decay(0, method = "gaussian"), 1)

  # Very large distances
  large_dist <- 1e6
  weights <- calc_decay(large_dist, method = "gaussian")
  expect_true(weights < 1e-6)  # Should be very close to zero
})
