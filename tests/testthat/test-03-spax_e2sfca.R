# tests/testthat/test-03-e2sfca.R

# Helper function to create test data
create_test_data <- function() {
  # Create simple test rasters
  demand <- terra::rast(nrows = 3, ncols = 3)
  terra::values(demand) <- c(10, 20, 30, 40, 50, 60, 70, 80, 90)

  # Create distance raster for 2 facilities
  dist1 <- terra::rast(nrows = 3, ncols = 3)
  dist2 <- terra::rast(nrows = 3, ncols = 3)
  terra::values(dist1) <- c(1, 2, 3, 2, 3, 4, 3, 4, 5)
  terra::values(dist2) <- c(5, 4, 3, 4, 3, 2, 3, 2, 1)
  distance <- c(dist1, dist2)
  names(distance) <- c("facility1", "facility2")

  # Sample supply data in different formats
  supply_df <- data.frame(
    id = c("facility1", "facility2"),
    doctors = c(10, 15),
    nurses = c(20, 25)
  )

  supply_matrix <- matrix(
    c(10, 15, 20, 25),
    nrow = 2,
    dimnames = list(c("facility1", "facility2"), c("doctors", "nurses"))
  )

  supply_vector <- c(10, 15)
  names(supply_vector) <- c("facility1", "facility2")

  list(
    demand = demand,
    distance = distance,
    supply_df = supply_df,
    supply_matrix = supply_matrix,
    supply_vector = supply_vector
  )
}

# 1. Test Helper Functions -----------------------------------------------------

test_that(".help_process_supply handles different input types correctly", {
  td <- create_test_data()
  weight_ids <- c("facility1", "facility2")

  # Test data.frame input
  df_result <- .help_process_supply(
    td$supply_df,
    id_col = "id",
    supply_cols = c("doctors", "nurses"),
    weight_ids = weight_ids
  )
  expect_equal(df_result$ids, weight_ids)
  expect_equal(df_result$cols, c("doctors", "nurses"))
  expect_equal(dim(df_result$values), c(2, 2))

  # Test matrix input
  mat_result <- .help_process_supply(
    td$supply_matrix,
    weight_ids = weight_ids
  )
  expect_equal(mat_result$ids, weight_ids)
  expect_equal(mat_result$cols, c("doctors", "nurses"))

  # Test vector input
  vec_result <- .help_process_supply(
    td$supply_vector,
    weight_ids = weight_ids
  )
  expect_equal(vec_result$ids, weight_ids)
  expect_equal(vec_result$cols, "supply")
})

test_that(".compute_access_core calculates correctly", {
  td <- create_test_data()

  # Create test weights
  weights <- calc_decay(td$distance, method = "gaussian", sigma = 2)
  demand_weights <- calc_normalize(weights, method = "standard")
  access_weights <- weights

  result <- .compute_access_core(
    demand = td$demand,
    supply_values = td$supply_matrix,
    demand_weights = demand_weights,
    access_weights = access_weights,
    indicator_names = c("doc_access", "nurse_access")
  )

  # Test structure
  expect_s4_class(result, "SpatRaster")
  expect_equal(names(result), c("doc_access", "nurse_access"))
  expect_equal(dim(result)[1:2], c(3, 3))
})

# 2. Test Validation Functions ------------------------------------------------

test_that(".chck_compute_access validates inputs correctly", {
  td <- create_test_data()

  weights <- calc_decay(td$distance, method = "gaussian", sigma = 2)
  demand_weights <- calc_normalize(weights, method = "standard")
  access_weights <- weights

  # Valid inputs should not error
  expect_no_error(
    .chck_compute_access(
      td$demand, td$supply_df, demand_weights, access_weights,
      id_col = "id", supply_cols = c("doctors", "nurses")
    )
  )

  # Test invalid inputs
  expect_error(
    .chck_compute_access(
      "not_a_raster",
      td$supply_df,
      demand_weights,
      access_weights,
      id_col = "id", # Added this
      supply_cols = c("doctors", "nurses") # Added this
    ),
    "demand must be one of the following classes: SpatRaster"
  )

  # Test mismatched dimensions
  wrong_weights <- terra::rast(td$distance[[1]]) # single layer
  expect_error(
    .chck_compute_access(
      td$demand,
      td$supply_df,
      wrong_weights,
      access_weights,
      id_col = "id", # Added this
      supply_cols = c("doctors", "nurses") # Added this
    ),
    "Length of demand_weights layers \\(1\\) must match length of facilities \\(2\\)"
  )
})

test_that(".chck_e2sfca validates inputs correctly", {
  td <- create_test_data()

  # Valid inputs should not error
  expect_no_error(
    .chck_e2sfca(
      td$demand, td$supply_df, td$distance,
      decay_params = list(method = "gaussian", sigma = 2),
      demand_normalize = "standard",
      id_col = "id", supply_cols = c("doctors", "nurses")
    )
  )

  # Test invalid decay_params
  expect_error(
    .chck_e2sfca(
      td$demand, td$supply_df, td$distance,
      decay_params = "not_a_list",
      demand_normalize = "standard",
      supply_cols = c("doctors"),
      id_col = "id"
    ),
    "decay_params must be one of the following classes: list"
  )

  # Test invalid demand_normalize
  expect_error(
    .chck_e2sfca(
      td$demand, td$supply_df, td$distance,
      decay_params = list(method = "gaussian", sigma = 2),
      demand_normalize = "invalid",
      supply_cols = c("doctors"),
      id_col = "id"
    ),
    "demand_normalize must be one of:"
  )
})

# 3. Test Main Functions ----------------------------------------------------

test_that("spax_e2sfca returns valid spax object", {
  td <- create_test_data()

  # Basic usage with single supply column first
  result <- spax_e2sfca(
    demand = td$demand,
    supply = td$supply_df,
    distance = td$distance,
    decay_params = list(method = "gaussian", sigma = 2),
    demand_normalize = "standard",
    id_col = "id",
    supply_cols = c("doctors")
  )

  # Test class and structure
  expect_s3_class(result, "spax")
  expect_named(result, c("accessibility", "type", "parameters", "facilities",
                         "iterations", "variations", "call"))

  # Test core components
  expect_s4_class(result$accessibility, "SpatRaster")
  expect_equal(result$type, "E2SFCA")
  expect_equal(names(result$accessibility), c("doctors"))

  # Test facilities data
  expect_s3_class(result$facilities, "data.frame")
  expect_equal(nrow(result$facilities), 2)
  expect_true(all(c("id", "doctors") %in% names(result$facilities)))

  # Test parameters
  expect_type(result$parameters, "list")
  expect_equal(result$parameters$decay_params$method, "gaussian")
  expect_equal(result$parameters$decay_params$sigma, 2)
  expect_equal(result$parameters$demand_normalize, "standard")

  # Now test with multiple supply columns
  result_multi <- spax_e2sfca(
    demand = td$demand,
    supply = td$supply_df,
    distance = td$distance,
    decay_params = list(method = "gaussian", sigma = 2),
    demand_normalize = "standard",
    id_col = "id",
    supply_cols = c("doctors", "nurses")
  )

  expect_s3_class(result_multi, "spax")
  expect_equal(names(result_multi$accessibility), c("doctors", "nurses"))
  expect_true(all(c("id", "doctors", "nurses") %in% names(result_multi$facilities)))

})

test_that("spax_e2sfca handles different supply formats", {
  td <- create_test_data()

  # Test with data.frame
  df_result <- spax_e2sfca(
    demand = td$demand,
    supply = td$supply_df,
    distance = td$distance,
    decay_params = list(method = "gaussian", sigma = 2),
    demand_normalize = "standard",
    id_col = "id",
    supply_cols = c("doctors", "nurses")
  )
  expect_s3_class(df_result, "spax")
  expect_equal(names(df_result$accessibility), c("doctors", "nurses"))

  # Test with matrix
  mat_result <- spax_e2sfca(
    demand = td$demand,
    supply = td$supply_matrix,
    distance = td$distance,
    decay_params = list(method = "gaussian", sigma = 2)
  )
  expect_s3_class(mat_result, "spax")
  expect_equal(dim(mat_result$accessibility)[3], ncol(td$supply_matrix))

  # Test with vector
  vec_result <- spax_e2sfca(
    demand = td$demand,
    supply = td$supply_vector,
    distance = td$distance,
    decay_params = list(method = "gaussian", sigma = 2)
  )
  expect_s3_class(vec_result, "spax")
  expect_equal(dim(vec_result$accessibility)[3], 1)
})

test_that("spax_e2sfca handles different normalizations", {
  td <- create_test_data()

  # Test different normalization methods
  normalizations <- c("identity", "standard", "semi")
  results <- lapply(normalizations, function(norm) {
    spax_e2sfca(
      demand = td$demand,
      supply = td$supply_df,
      distance = td$distance,
      decay_params = list(method = "gaussian", sigma = 2),
      demand_normalize = norm,
      id_col = "id",
      supply_cols = "doctors"
    )
  })

  # Check class consistency
  expect_true(all(sapply(results, inherits, "spax")))

  # Results should be different for each normalization method
  access_values <- lapply(results, function(x) terra::values(x$accessibility))
  expect_false(identical(access_values[[1]], access_values[[2]]))
  expect_false(identical(access_values[[2]], access_values[[3]]))

  # Check parameters are correctly stored
  expect_equal(sapply(results, function(x) x$parameters$demand_normalize),
               normalizations)
})

test_that("spax_e2sfca preserves snap mode functionality", {
  td <- create_test_data()

  # Compare results with and without snap
  normal_result <- spax_e2sfca(
    demand = td$demand,
    supply = td$supply_df,
    distance = td$distance,
    decay_params = list(method = "gaussian", sigma = 2),
    demand_normalize = "standard",
    id_col = "id",
    supply_cols = c("doctors"),
    snap = FALSE
  )

  snap_result <- spax_e2sfca(
    demand = td$demand,
    supply = td$supply_df,
    distance = td$distance,
    decay_params = list(method = "gaussian", sigma = 2),
    demand_normalize = "standard",
    id_col = "id",
    supply_cols = c("doctors"),
    snap = TRUE
  )

  # Both should be spax objects with identical accessibility values
  expect_s3_class(normal_result, "spax")
  expect_s3_class(snap_result, "spax")
  expect_equal(
    terra::values(normal_result$accessibility),
    terra::values(snap_result$accessibility)
  )
})

