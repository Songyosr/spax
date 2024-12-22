# tests/testthat/test-02-sample_pmf.R

# Initialize random seed for all tests in this file
set.seed(42)

# Helper function to create test data with proper seed handling
create_test_pmf <- function(nrows = 5, ncols = 5, normalized = TRUE) {
  set.seed(42)  # Ensure consistent random values
  r <- terra::rast(nrows = nrows, ncols = ncols)
  values <- runif(nrows * ncols)
  if (normalized) {
    values <- values / sum(values)
  }
  terra::values(r) <- values
  return(r)
}

# Test setup function for common test data and seed initialization
setup_test_data <- function() {
  set.seed(42)  # Reset seed for each test
  pmf <- create_test_pmf()
  list(
    pmf = pmf,
    pmf_norm = create_test_pmf(normalized = TRUE),
    pmf_raw = create_test_pmf(normalized = FALSE),
    pmf_single = create_test_pmf(nrows = 1, ncols = 1)
  )
}

# 1. Basic Input Validation Tests --------------------------------------------

test_that("sample_pmf validates input types correctly", {
  pmf <- create_test_pmf()

  # Invalid input type
  expect_error(
    sample_pmf(as.matrix(terra::values(pmf)), n = 10),
    "Input 'x' must be a SpatRaster object"
  )

  # Missing both n and size/prob
  expect_error(
    sample_pmf(pmf, method = "poisson"),
    "Either n or both size and prob must be specified"
  )

  # Invalid probability
  expect_error(
    sample_pmf(pmf, size = 100, prob = 1.5),
    "prob must be between 0 and 1"
  )

  # Invalid evolution proportion
  expect_error(
    sample_pmf(pmf, n = 10, evolve_prop = 2),
    "evolve_prop must be between 0 and 1"
  )

  # Invalid iterations
  expect_error(
    sample_pmf(pmf, n = 10, iterations = 0),
    "iterations must be a positive integer"
  )

  # Invalid method
  expect_error(
    sample_pmf(pmf, n = 10, method = "invalid"),
    "Unknown method string"
  )
})

# 2. PMF Input Validation Tests ---------------------------------------------

test_that("sample_pmf handles different input formats correctly", {
  # Already normalized PMF
  pmf_norm <- create_test_pmf(normalized = TRUE)
  expect_no_error(sample_pmf(pmf_norm, n = 10))

  # Non-normalized density
  pmf_raw <- create_test_pmf(normalized = FALSE)
  expect_no_error(sample_pmf(pmf_raw, n = 10))

  # Input with all zeros
  pmf_zero <- create_test_pmf()
  terra::values(pmf_zero) <- 0
  expect_error(sample_pmf(pmf_zero, n = 10))

  # Input with all NAs
  pmf_na <- create_test_pmf()
  terra::values(pmf_na) <- NA
  expect_error(sample_pmf(pmf_na, n = 10))
})

# 3. Sample Size Generation Tests ------------------------------------------

test_that("sample_pmf generates correct number of samples", {
  pmf <- create_test_pmf()

  # Fixed size
  n <- 50
  result_fixed <- sample_pmf(pmf, n = n)
  expect_equal(sum(terra::values(result_fixed), na.rm = TRUE), n)

  # Poisson sampling
  size <- 1000
  prob <- 0.05
  result_pois <- sample_pmf(pmf, size = size, prob = prob, method = "poisson")
  # Check mean is roughly size * prob
  expect_true(abs(sum(terra::values(result_pois), na.rm = TRUE) - size * prob) < size * prob * 0.5)

  # Binomial sampling
  result_binom <- sample_pmf(pmf, size = size, prob = prob, method = "binomial")
  expect_true(sum(terra::values(result_binom), na.rm = TRUE) <= size)

  # Custom sampling function
  custom_fn <- function() rpois(1, lambda = 30)
  result_custom <- sample_pmf(pmf, method = custom_fn)
  expect_true(is.numeric(sum(terra::values(result_custom), na.rm = TRUE)))
})

# 4. Evolution Tests ------------------------------------------------------

test_that("sample_pmf evolution works correctly", {
  pmf <- create_test_pmf()
  n <- 100
  iterations <- 5

  # Independent sampling (evolve_prop = 1)
  result_indep <- sample_pmf(pmf, n = n, iterations = iterations, evolve_prop = 1)
  expect_equal(terra::nlyr(result_indep), iterations)
  expect_equal(sum(terra::values(result_indep), na.rm = TRUE), n * iterations)

  # No evolution (evolve_prop = 0)
  result_no_evol <- sample_pmf(pmf, n = n, iterations = iterations, evolve_prop = 0)
  expect_equal(terra::nlyr(result_no_evol), 1)
  expect_equal(sum(terra::values(result_no_evol), na.rm = TRUE), n)

  # Partial evolution
  evolve_prop <- 0.5
  result_partial <- sample_pmf(pmf, n = n, iterations = iterations, evolve_prop = evolve_prop)
  expect_equal(terra::nlyr(result_partial), iterations)

  # Check that subsequent layers have some correlation
  for(i in 2:iterations) {
    layer1 <- terra::values(result_partial[[i-1]])
    layer2 <- terra::values(result_partial[[i]])
    correlation <- cor(layer1, layer2, use = "complete.obs")
    expect_true(correlation > 0)  # Should have positive correlation due to sample retention
  }
})

# 5. Spatial Distribution Tests -------------------------------------------

test_that("sample_pmf maintains spatial distribution properties", {
  # Create PMF with clear spatial gradient
  r <- terra::rast(nrows = 10, ncols = 10)
  terra::values(r) <- seq(0.1, 1, length.out = 100)
  pmf <- transform_pmf(r)

  # Generate large number of samples
  n <- 10000
  result <- sample_pmf(pmf, n = n)

  # Compare empirical vs expected proportions
  empirical_props <- terra::values(result) / n
  expected_props <- terra::values(pmf)

  # Correlation between empirical and expected should be high
  correlation <- cor(empirical_props, expected_props, use = "complete.obs")
  expect_true(correlation > 0.7)  # Arbitrary threshold, adjust based on needs
})

# 6. Reproducibility Tests -----------------------------------------------

test_that("sample_pmf provides reproducible results with seeds", {
  pmf <- create_test_pmf()
  n <- 100
  seed <- 42

  # Same seed should give same results
  result1 <- sample_pmf(pmf, n = n, seed = seed)
  result2 <- sample_pmf(pmf, n = n, seed = seed)
  expect_equal(terra::values(result1), terra::values(result2))

  # Different seeds should give different results
  result3 <- sample_pmf(pmf, n = n, seed = seed + 1)
  expect_false(identical(terra::values(result1), terra::values(result3)))
})

# 7. Edge Cases Tests ---------------------------------------------------

test_that("sample_pmf handles edge cases appropriately", {
  # Single cell raster
  pmf_single <- create_test_pmf(nrows = 1, ncols = 1)
  expect_no_error(sample_pmf(pmf_single, n = 10))

  # Very small sample size
  pmf <- create_test_pmf()
  result_small <- sample_pmf(pmf, n = 1)
  expect_equal(sum(terra::values(result_small), na.rm = TRUE), 1)

  # Zero samples
  result_zero <- sample_pmf(pmf, n = 0)
  expect_true(all(is.na(terra::values(result_zero))))

  # Extreme evolution proportions
  expect_no_error(sample_pmf(pmf, n = 10, evolve_prop = 0.001))
  expect_no_error(sample_pmf(pmf, n = 10, evolve_prop = 0.999))
})
