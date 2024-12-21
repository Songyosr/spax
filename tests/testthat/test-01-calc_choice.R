# tests/testthat/test-calc_choice.R

# Helper function to create test data - using terra only
create_test_weights <- function() {
  r <- terra::rast(nrows=3, ncols=3, nlyr=2)
  terra::values(r) <- c(0.5, 0.3, 0.2, 0.4, 0.6, 0.8, 0.1, 0.7, 0.9,  # Layer 1
                        0.2, 0.4, 0.6, 0.3, 0.1, 0.5, 0.8, 0.2, 0.4)   # Layer 2
  names(r) <- c("facility1", "facility2")
  return(r)
}

test_that("calc_choice handles basic normalization correctly", {
  weights <- create_test_weights()
  result <- calc_choice(weights)

  # Test output structure
  expect_s4_class(result, "SpatRaster")
  expect_equal(terra::nlyr(result), 2)

  # Test probabilities sum to 1
  sum_layer <-  terra::app(result, sum)  # Changed from sum to app
  expect_true(all(abs(terra::values(sum_layer) - 1) < 1e-10))

  # Test names are preserved
  expect_equal(names(result), c("facility1", "facility2"))
})

test_that("calc_choice handles attractiveness correctly", {
  weights <- create_test_weights()
  attractiveness <- c(100, 50)  # First facility twice as attractive

  result <- calc_choice(weights, attractiveness = attractiveness)

  # Test effect of attractiveness
  # First facility should have higher probabilities than base case
  base_result <- calc_choice(weights)
  expect_true(all(terra::values(result[[1]]) > terra::values(base_result[[1]])))
})

test_that("calc_choice handles alpha parameter correctly", {
  weights <- create_test_weights()
  attractiveness <- c(2, 1)

  # Compare different alpha values
  result1 <- calc_choice(weights, attractiveness = attractiveness, alpha = 1)
  result2 <- calc_choice(weights, attractiveness = attractiveness, alpha = 2)

  # Higher alpha should amplify attractiveness differences
  expect_true(all(terra::values(result2[[1]]) > terra::values(result1[[1]])))
})

test_that("calc_choice handles outside option (a0) correctly", {
  weights <- create_test_weights()
  result <- calc_choice(weights, a0 = 0.5)

  # Sum of probabilities should be less than 1 due to outside option
  sum_layer <- terra::app(result, sum)  # Changed from sum to app
  expect_true(all(terra::values(sum_layer) < 1))
})

test_that("calc_choice validates inputs correctly", {
  weights <- create_test_weights()

  # Test invalid weights
  expect_error(calc_choice("not_a_raster"))

  # Test invalid attractiveness
  expect_error(calc_choice(weights, attractiveness = c(1))) # Wrong length
  expect_error(calc_choice(weights, attractiveness = c(-1, 1))) # Negative values

  # Test invalid alpha
  expect_error(calc_choice(weights, attractiveness = c(1,1), alpha = "invalid"))
  expect_error(calc_choice(weights, attractiveness = c(1,1), alpha = c(1,2)))

  # Test invalid a0
  expect_error(calc_choice(weights, a0 = -1))
  expect_error(calc_choice(weights, a0 = "invalid"))
})

test_that("calc_choice handles real-world data correctly", {
  skip_if_not_installed("terra")

  # Convert hos_iscr directly using terra
  weights <- terra::rast(hos_iscr)
  doctor_attr <- hc12_hos$s_doc

  result <- calc_choice(weights, attractiveness = doctor_attr)

  # Basic structure tests
  expect_s4_class(result, "SpatRaster")
  expect_equal(terra::nlyr(result), length(doctor_attr))

  # Probability sum test
  sum_layer <- terra::app(result, sum)
  non_na_vals <- terra::values(sum_layer)[!is.na(terra::values(sum_layer))]
  expect_true(all(abs(non_na_vals - 1) < 1e-10))
})

test_that("calc_choice snap mode works correctly", {
  weights <- create_test_weights()
  attractiveness <- c(100, 50)

  # Results should be identical with and without snap
  result_normal <- calc_choice(weights, attractiveness = attractiveness, snap = FALSE)
  result_snap <- calc_choice(weights, attractiveness = attractiveness, snap = TRUE)

  expect_equal(terra::values(result_normal), terra::values(result_snap))

  # Performance test
  # normal_time <- system.time(replicate(100, calc_choice(weights, snap = FALSE)))
  # snap_time <- system.time(replicate(100, calc_choice(weights, snap = TRUE)))
  #
  # expect_lt(snap_time["elapsed"], normal_time["elapsed"])
})

test_that("calc_choice handles edge cases gracefully", {
  # Single layer case
  single_layer <- terra::rast(matrix(runif(9), 3, 3))
  expect_no_error(calc_choice(single_layer))

  # All zero weights
  zero_weights <- create_test_weights() * 0
  expect_no_error(calc_choice(zero_weights))

  # NA values
  na_weights <- create_test_weights()
  terra::values(na_weights)[1] <- NA
  expect_no_error(calc_choice(na_weights))

  # Large number of layers
  many_layers <- terra::rast(replicate(10, terra::rast(matrix(runif(9), 3, 3))))
  expect_no_error(calc_choice(many_layers))
})
