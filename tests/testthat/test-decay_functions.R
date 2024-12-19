# Test organization follows a hierarchical structure matching the function organization
# tests/testthat/test-decay.R

#' Create test data for decay function testing
#' @keywords internal
create_test_data <- function() {
  list(
    vector = c(0, 10, 20, 30, 40, 50),
    matrix = matrix(1:25, 5, 5),
    raster = rast(matrix(1:100, 10, 10))
  )
}

# Helper validation tests ----------------------------------------------------

test_that(".chck_decay validates inputs correctly", {
  test_data <- create_test_data()

  # Valid inputs should return TRUE
  expect_true(.chck_decay(test_data$vector, "gaussian", 30))
  expect_true(.chck_decay(test_data$raster, "exponential", 0.1))

  # Invalid inputs should error
  expect_error(.chck_decay("not_numeric", "gaussian"))
  expect_error(.chck_decay(test_data$vector, "invalid_method"))
  expect_error(.chck_decay(test_data$vector, "gaussian", sigma = "not_numeric"))

  # Snap mode should skip validation
  expect_true(.chck_decay("invalid", "invalid", snap = TRUE))
})

# Individual decay function tests -------------------------------------------

test_that(".help_decay_gaussian calculates correctly", {
  x <- seq(0, 50, by = 10)
  sigma <- 20
  weights <- .help_decay_gaussian(x, sigma)

  # Test mathematical properties
  expect_equal(weights[1], 1)  # At distance 0
  expect_equal(
    weights,
    exp(-(x^2) / (2 * sigma^2))
  )

  # Test decay properties
  expect_true(all(weights <= 1))
  expect_true(all(weights >= 0))
  expect_true(all(diff(weights) < 0))  # Strictly decreasing

  # Test NA handling
  expect_true(is.na(.help_decay_gaussian(NA, sigma)))
})

test_that(".help_decay_exponential calculates correctly", {
  x <- seq(0, 50, by = 10)
  sigma <- 0.1
  weights <- .help_decay_exponential(x, sigma)

  # Test mathematical properties
  expect_equal(weights[1], 1)  # At distance 0
  expect_equal(
    weights,
    exp(-sigma * x)
  )

  # Test decay properties
  expect_true(all(weights <= 1))
  expect_true(all(weights >= 0))
  expect_true(all(diff(weights) < 0))
})

test_that(".help_decay_power calculates correctly", {
  x <- seq(1, 50, by = 10)  # Start at 1 to avoid infinity
  sigma <- 2
  weights <- .help_decay_power(x, sigma)

  # Test mathematical properties
  expect_equal(
    weights,
    x^(-sigma)
  )

  # Test decay properties
  expect_true(all(weights > 0))
  expect_true(all(diff(weights) < 0))
})

test_that(".help_decay_binary calculates correctly", {
  x <- seq(0, 100, by = 20)
  sigma <- 50
  weights <- .help_decay_binary(x, sigma)

  # Test threshold behavior
  expect_equal(weights, as.numeric(x <= sigma))
  expect_true(all(weights %in% c(0, 1)))

  # Test NA handling
  x_na <- c(x, NA)
  weights_na <- .help_decay_binary(x_na, sigma)
  expect_equal(weights_na[is.na(x_na)], 0)
})

test_that(".help_decay_inverse calculates correctly", {
  x <- seq(0, 50, by = 10)
  c <- 1
  weights <- .help_decay_inverse(x, c)

  # Test mathematical properties
  expect_equal(weights, 1/(x + c))
  expect_equal(weights[1], 1/(0 + c))

  # Test decay properties
  expect_true(all(weights > 0))
  expect_true(all(diff(weights) < 0))
})

# Main function tests -----------------------------------------------------

test_that("calc_decay handles different input types", {
  test_data <- create_test_data()

  # Test each input type with each method
  methods <- c("gaussian", "exponential", "power", "inverse", "binary")

  for (method in methods) {
    # Vector input
    v_result <- calc_decay(test_data$vector, method)
    expect_type(v_result, "double")
    expect_length(v_result, length(test_data$vector))

    # Matrix input
    m_result <- calc_decay(test_data$matrix, method)
    expect_true(is.matrix(m_result))
    expect_equal(dim(m_result), dim(test_data$matrix))

    # Raster input
    r_result <- calc_decay(test_data$raster, method)
    expect_s4_class(r_result, "SpatRaster")
    expect_equal(dim(r_result), dim(test_data$raster))
  }
})

test_that("calc_decay handles custom functions", {
  test_data <- create_test_data()

  # Custom function with proper default
  custom_fn <- function(distance, sigma = 2, scale = 1) {
    scale * exp(-(distance^2) / (2 * sigma^2))
  }

  # Test with explicit parameters
  result <- calc_decay(
    test_data$vector,
    method = custom_fn,
    sigma = 2,
    scale = 2
  )

  expect_equal(
    result,
    2 * exp(-(test_data$vector^2) / (2 * 2^2))
  )

  # Test with default parameters
  expect_type(
    calc_decay(test_data$vector, custom_fn),
    "double"
  )
})

test_that("calc_decay handles custom functions with various signatures", {
  x <- 1:5

  # Custom function with required sigma
  custom_fn1 <- function(distance, sigma) {
    exp(-(distance^2) / (2 * sigma^2))
  }
  expect_error(
    calc_decay(x, method = custom_fn1),
    "argument \"sigma\" is missing, with no default"
  )
  expect_no_error(calc_decay(x, method = custom_fn1, sigma = 2))

  # Custom function with optional sigma
  custom_fn2 <- function(distance, sigma = 1) {
    exp(-(distance^2) / (2 * sigma^2))
  }
  expect_no_error(calc_decay(x, method = custom_fn2))  # Uses default
  expect_no_error(calc_decay(x, method = custom_fn2, sigma = 2))  # Uses provided

  # Custom function without sigma
  custom_fn3 <- function(distance, k = 1) {
    1 / (1 + k * distance)
  }
  expect_no_error(calc_decay(x, method = custom_fn3))  # Ignores sigma
  expect_no_error(calc_decay(x, method = custom_fn3, k = 2))  # Uses k
  expect_no_error(calc_decay(x, method = custom_fn3, sigma = 2, k = 2))  # Ignores sigma, uses k

  # Verify results
  result3a <- calc_decay(x, method = custom_fn3)
  expect_equal(result3a, 1 / (1 + x))

  result3b <- calc_decay(x, method = custom_fn3, k = 2)
  expect_equal(result3b, 1 / (1 + 2 * x))
})

test_that("calc_decay handles snap mode correctly", {
  test_data <- create_test_data()

  # Should work without validation in snap mode
  expect_silent(
    calc_decay(test_data$vector, "gaussian", snap = TRUE)
  )

  # Should still compute correctly
  regular <- calc_decay(test_data$vector, "gaussian", snap = FALSE)
  snapped <- calc_decay(test_data$vector, "gaussian", snap = TRUE)
  expect_equal(regular, snapped)
})

test_that("calc_decay handles edge cases", {
  # Empty input
  expect_length(
    calc_decay(numeric(0), "gaussian"),
    0
  )

  # Single value
  expect_length(
    calc_decay(0, "gaussian"),
    1
  )

  # Very large distances
  expect_true(
    calc_decay(1e6, "gaussian") < 1e-6
  )

  # Zero sigma
  expect_error(
    calc_decay(1:5, "gaussian", sigma = 0)
  )

  # Negative sigma
  expect_error(
    calc_decay(1:5, "gaussian", sigma = -1)
  )
})

test_that("calc_decay preserves raster attributes automatically", {
  skip_if_not_installed("terra")

  # Create test raster with defined CRS and extent
  r <- terra::rast(matrix(1:100, 10, 10))
  e <- terra::ext(0, 10, 0, 10)
  terra::ext(r) <- e
  terra::crs(r) <- "EPSG:4326"

  # Test built-in methods
  methods <- c("gaussian", "exponential", "power", "inverse", "binary")
  for (m in methods) {
    result <- calc_decay(r, method = m)
    # Compare actual extent values instead of pointers
    expect_equal(as.vector(terra::ext(result)), as.vector(e))
    expect_equal(terra::crs(result), terra::crs(r))
    expect_equal(terra::res(result), terra::res(r))
  }

  # Test custom function
  custom_fn <- function(distance, k = 1) {
    1 / (1 + k * distance)
  }
  result <- calc_decay(r, method = custom_fn, k = 2)
  expect_equal(as.vector(terra::ext(result)), as.vector(e))
  expect_equal(terra::crs(result), terra::crs(r))
  expect_equal(terra::res(result), terra::res(r))
})
