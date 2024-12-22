# tests/testthat/test-spread_weighted.R

#' Create test data for spread_weighted testing
#' @keywords internal
create_test_data <- function() {
  # Create deterministic test data for reliable testing
  w1 <- matrix(c(1,2,3,
                 4,5,6,
                 7,8,9), 3, 3, byrow = TRUE)
  w2 <- matrix(c(9,8,7,
                 6,5,4,
                 3,2,1), 3, 3, byrow = TRUE)

  list(
    weights = c(rast(w1), rast(w2)),
    vector_values = c(10, 20),
    matrix_values = matrix(c(10, 20, 30, 40), 2, 2,
                           dimnames = list(NULL, c("val1", "val2"))),
    df_values = data.frame(
      id = 1:2,
      val1 = c(10, 20),
      val2 = c(30, 40)
    ),
    # Add expected results for validation
    expected_sums = list(
      vector = sum(w1 * 10 + w2 * 20),
      matrix_val1 = sum(w1 * 10 + w2 * 20),
      matrix_val2 = sum(w1 * 30 + w2 * 40)
    )
  )
}
# Test input validation -------------------------------------------------------

test_that("spread_weighted validates inputs correctly", {
  td <- create_test_data()

  # Valid inputs should work
  expect_no_error(
    spread_weighted(td$vector_values, td$weights)
  )

  # Invalid weights
  expect_error(
    spread_weighted(td$vector_values, "not_a_raster"),
    "weights must be a SpatRaster object"
  )

  # Mismatched lengths
  expect_error(
    spread_weighted(c(1,2,3), td$weights),
    "Length of values must match number of weight layers"
  )

  # Missing value_cols for data.frame
  expect_error(
    spread_weighted(td$df_values, td$weights),
    "value_cols must be specified"
  )

  # Invalid value_cols
  expect_error(
    spread_weighted(td$df_values, td$weights, value_cols = "nonexistent"),
    "Not all value_cols found in data.frame"
  )
})

# Test core functionality ---------------------------------------------------

# Core Functionality Tests ------------------------------------------------

test_that("spread_weighted correctly processes vector input", {
  td <- create_test_data()
  result <- spread_weighted(td$vector_values, td$weights)

  expect_s4_class(result, "SpatRaster")
  expect_equal(dim(result), c(3, 3, 1))

  # Check actual computation
  actual_sum <- global(result, "sum", na.rm = TRUE)$sum
  expect_equal(actual_sum, td$expected_sums$vector)
})

test_that("spread_weighted correctly processes matrix input", {
  td <- create_test_data()
  result <- spread_weighted(td$matrix_values, td$weights)

  expect_s4_class(result, "SpatRaster")
  expect_equal(dim(result), c(3, 3, 2))
  expect_equal(names(result), c("val1", "val2"))

  # Check computations for both columns
  val1_sum <- global(result[[1]], "sum", na.rm = TRUE)$sum
  val2_sum <- global(result[[2]], "sum", na.rm = TRUE)$sum

  expect_equal(val1_sum, td$expected_sums$matrix_val1)
  expect_equal(val2_sum, td$expected_sums$matrix_val2)
})

# Feature-specific Tests -------------------------------------------------

test_that("spread_weighted handles name_prefix correctly", {
  td <- create_test_data()
  result <- spread_weighted(td$matrix_values, td$weights,
                            name_prefix = "test_")

  expect_equal(names(result), c("test_val1", "test_val2"))
})

test_that("spread_weighted full_output provides correct components", {
  td <- create_test_data()
  result <- spread_weighted(td$vector_values, td$weights,
                            full_output = TRUE)

  expect_named(result, c("total_distribution", "unit_distribution"))
  expect_s4_class(result$total_distribution, "SpatRaster")
  expect_s4_class(result$unit_distribution, "SpatRaster")

  # Check unit distributions sum to total
  unit_sum <- global(sum(result$unit_distribution), "sum", na.rm = TRUE)$sum
  total_sum <- global(result$total_distribution, "sum", na.rm = TRUE)$sum
  expect_equal(unit_sum, total_sum)
})

# Edge Cases Tests ------------------------------------------------------

test_that("spread_weighted handles edge cases correctly", {
  td <- create_test_data()

  # Test zero values with value checking
  zero_result <- spread_weighted(c(0, 0), td$weights)
  expect_equal(global(zero_result, "sum", na.rm = TRUE)$sum, 0)

  # Test NA handling in weights
  weights_with_na <- td$weights
  values(weights_with_na)[1] <- NA
  na_result <- spread_weighted(td$vector_values, weights_with_na)
  # Should sum correctly ignoring NA values
  expect_false(any(is.na(values(na_result))))
  expect_gt(global(na_result, "sum", na.rm = TRUE)$sum, 0)

  # Test NA handling in values
  na_val_result <- spread_weighted(c(NA, 1), td$weights)
  # Should still sum correctly using only non-NA value
  expect_false(any(is.na(values(na_val_result))))
  # Only second layer with value 1 should contribute
  expect_equal(global(na_val_result, "sum", na.rm = TRUE)$sum,
               sum(values(td$weights[[2]]), na.rm = TRUE))
})

# Package Data Tests ---------------------------------------------------

test_that("spread_weighted works correctly with package data", {
  # Create test weights with explicitly verifiable layers
  n_facilities <- 2  # Number of rows in our test data
  weights_m1 <- matrix(1:100, 10, 10)
  weights_m2 <- matrix(100:1, 10, 10)
  test_weights <- c(rast(weights_m1), rast(weights_m2))

  # Verify number of layers before proceeding
  expect_equal(nlyr(test_weights), n_facilities)

  # Apply decay
  test_weights <- calc_decay(test_weights, method = "gaussian", sigma = 30)

  # Create test data matching number of layers
  test_df <- data.frame(
    s_doc = c(10, 20),    # 2 rows matching 2 layers
    s_nurse = c(30, 40)
  )

  # Test multiple columns with value verification
  result <- spread_weighted(test_df, test_weights,
                            value_cols = c("s_doc", "s_nurse"))

  expect_s4_class(result, "SpatRaster")
  expect_equal(names(result), c("s_doc", "s_nurse"))
  expect_equal(nlyr(result), 2)

  # Verify both layers have expected characteristics
  expect_true(all(global(result, "max", na.rm = TRUE)$max > 0))
})
