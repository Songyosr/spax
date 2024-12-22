# tests/testthat/test-gather_functions.R

# Helper function to create test data
create_test_data <- function(multi = FALSE) {
  # Create a small test raster for values
  if (!multi) {
    values <- terra::rast(nrows=5, ncols=5, xmin=0, xmax=10, ymin=0, ymax=10)
    terra::values(values) <- 1:25
  } else {
    v1 <- terra::rast(nrows=5, ncols=5, xmin=0, xmax=10, ymin=0, ymax=10)
    v2 <- terra::rast(nrows=5, ncols=5, xmin=0, xmax=10, ymin=0, ymax=10)
    terra::values(v1) <- 1:25
    terra::values(v2) <- (1:25) * 2
    values <- c(v1, v2)
    names(values) <- c("sim1", "sim2")
  }

  # Create a multi-layer weight raster
  weights <- terra::rast(nrows=5, ncols=5, nlyr=3, xmin=0, xmax=10, ymin=0, ymax=10)
  terra::values(weights) <- runif(75)  # 25 cells * 3 layers
  names(weights) <- c("unit1", "unit2", "unit3")

  list(
    values = values,
    weights = weights
  )
}

# 1. Core Functionality Tests ---------------------------------------------

test_that("gather_weighted performs basic aggregation correctly", {
  td <- create_test_data(multi = FALSE)
  result <- gather_weighted(td$values, td$weights)  # default simplify=FALSE

  # Check data.frame output by default
  expect_s3_class(result, "data.frame")
  expect_named(result, c("unit_id", "weighted_sum"))
  expect_equal(nrow(result), terra::nlyr(td$weights))

  # Verify sums are numeric and non-negative
  expect_type(result$weighted_sum, "double")
  expect_true(all(result$weighted_sum >= 0))

  # Check simplified output
  result_simple <- gather_weighted(td$values, td$weights, simplify = TRUE)
  expect_type(result_simple, "double")
  expect_named(result_simple, names(td$weights))
})

test_that("gather_weighted handles multi-layer correctly", {
  td <- create_test_data(multi = TRUE)
  result <- gather_weighted(td$values, td$weights)  # default simplify=FALSE

  # Check data.frame output by default
  expect_s3_class(result, "data.frame")
  expect_named(result, c("unit_id", "sim1_weighted_sum", "sim2_weighted_sum"))
  expect_equal(nrow(result), terra::nlyr(td$weights))

  # Check matrix output when simplified
  result_simple <- gather_weighted(td$values, td$weights, simplify = TRUE)
  expect_true(is.matrix(result_simple))
  expect_equal(rownames(result_simple), names(td$weights))
  expect_equal(colnames(result_simple), names(td$values))
})

test_that("gather_weighted preserves layer names correctly", {
  # Create test data with specific names
  values <- terra::rast(nrows = 3, ncols = 3)
  terra::values(values) <- 1:9

  weights <- terra::rast(nrows = 3, ncols = 3, nlyr = 2)
  terra::values(weights) <- rep(c(0.5, 0.5), each = 9)
  names(weights) <- c("zone_A", "zone_B")

  # Test single layer
  result <- gather_weighted(values, weights)
  expect_equal(result$unit_id, c("zone_A", "zone_B"))

  result_simple <- gather_weighted(values, weights, simplify = TRUE)
  expect_named(result_simple, c("zone_A", "zone_B"))

  # Test multi layer
  values_multi <- c(values, values * 2)
  names(values_multi) <- c("sim1", "sim2")

  result_multi <- gather_weighted(values_multi, weights)
  expect_equal(result_multi$unit_id, c("zone_A", "zone_B"))
  expect_named(result_multi, c("unit_id", "sim1_weighted_sum", "sim2_weighted_sum"))

  result_multi_simple <- gather_weighted(values_multi, weights, simplify = TRUE)
  expect_equal(rownames(result_multi_simple), c("zone_A", "zone_B"))
  expect_equal(colnames(result_multi_simple), c("sim1", "sim2"))
})

# 2. Input Validation Tests ----------------------------------------------

test_that("gather_weighted validates input types", {
  test_data <- create_test_data()

  # Non-SpatRaster values
  expect_error(
    gather_weighted(as.matrix(1:25), test_data$weights),
    "values must be a SpatRaster object"
  )

  # Non-SpatRaster weights
  expect_error(
    gather_weighted(test_data$values, as.matrix(runif(75))),
    "weights must be a SpatRaster object"
  )
})

test_that("gather_weighted validates raster compatibility", {
  # Create base raster
  values <- terra::rast(nrows=5, ncols=5, xmin=0, xmax=10, ymin=0, ymax=10)
  terra::values(values) <- 1:25

  # Create raster with different resolution but same extent
  weights_diff_res <- terra::rast(nrows=10, ncols=10, nlyr=2,
                                  xmin=0, xmax=10, ymin=0, ymax=10)
  terra::values(weights_diff_res) <- runif(200)

  expect_error(
    gather_weighted(values, weights_diff_res),
    "values and weights must have the same resolution"
  )

  # Create raster with different extent but same resolution
  weights_diff_ext <- terra::rast(nrows=5, ncols=5, nlyr=2,
                                  xmin=10, xmax=20, ymin=10, ymax=20)
  terra::values(weights_diff_ext) <- runif(50)

  expect_error(
    gather_weighted(values, weights_diff_ext),
    "values and weights must have the same extent"
  )
})

# 3. Edge Cases Tests --------------------------------------------------

test_that("gather_weighted handles edge cases correctly", {
  # Zero values
  values_zero <- terra::rast(nrows = 3, ncols = 3)
  terra::values(values_zero) <- 0

  weights <- terra::rast(nrows = 3, ncols = 3, nlyr = 2)
  terra::values(weights) <- runif(18)
  names(weights) <- c("unit1", "unit2")

  result_zero <- gather_weighted(values_zero, weights)
  expect_true(all(result_zero$weighted_sum == 0))

  # Single-layer weights
  weights_single <- terra::rast(nrows = 3, ncols = 3)
  terra::values(weights_single) <- runif(9)
  names(weights_single) <- "single_unit"

  result_single <- gather_weighted(values_zero, weights_single)
  expect_equal(nrow(result_single), 1)
  expect_equal(result_single$unit_id, "single_unit")

  # Extreme values
  values_extreme <- terra::rast(nrows = 3, ncols = 3)
  terra::values(values_extreme) <- 1e10

  result_extreme <- gather_weighted(values_extreme, weights)
  expect_true(all(is.finite(result_extreme$weighted_sum)))
})

# 4. NA Handling Tests ------------------------------------------------

test_that("gather_weighted handles NA values correctly", {
  # Create test data with NAs
  values <- terra::rast(nrows = 3, ncols = 3)
  terra::values(values) <- c(1:4, NA, 6:9)

  weights <- terra::rast(nrows = 3, ncols = 3, nlyr = 2)
  terra::values(weights) <- c(rep(0.5, 8), NA, rep(0.5, 8), NA)
  names(weights) <- c("unit1", "unit2")

  # Test with na.rm = TRUE
  result_rm <- gather_weighted(values, weights, na.rm = TRUE)
  expect_false(any(is.na(result_rm$weighted_sum)))

  # Test with na.rm = FALSE
  result_keep <- gather_weighted(values, weights, na.rm = FALSE)
  expect_true(any(is.na(result_keep$weighted_sum)))
})

# 5. Snap Mode Tests -------------------------------------------------

test_that("gather_weighted snap mode works correctly", {
  td <- create_test_data(multi = TRUE)

  # Test single layer snap
  result_snap <- gather_weighted(td$values[[1]], td$weights, snap = TRUE)
  expect_type(result_snap, "double")
  expect_length(result_snap, terra::nlyr(td$weights))

  # Test multi layer snap
  result_multi_snap <- gather_weighted(td$values, td$weights, snap = TRUE)
  expect_true(is.matrix(result_multi_snap))
  expect_equal(nrow(result_multi_snap), terra::nlyr(td$weights))
  expect_equal(ncol(result_multi_snap), terra::nlyr(td$values))
})

# 6. Performance Tests -----------------------------------------------

test_that("gather_weighted preserves computational accuracy", {
  td <- create_test_data(multi = TRUE)

  # Test that multi-layer results match individual computations
  multi_result <- gather_weighted(td$values, td$weights, simplify = TRUE)
  single_results <- lapply(1:nlyr(td$values), function(i) {
    gather_weighted(td$values[[i]], td$weights, simplify = TRUE)
  })

  for (i in 1:nlyr(td$values)) {
    expect_equal(multi_result[,i], single_results[[i]])
  }
})
