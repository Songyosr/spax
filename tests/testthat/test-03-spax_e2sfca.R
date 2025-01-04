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

test_that("compute_access handles different supply formats", {
  td <- create_test_data()

  weights <- calc_decay(td$distance, method = "gaussian", sigma = 2)
  demand_weights <- calc_normalize(weights, method = "standard")
  access_weights <- weights

  # Test with data.frame
  df_result <- compute_access(
    td$demand, td$supply_df, demand_weights, access_weights,
    id_col = "id", supply_cols = c("doctors", "nurses")
  )
  expect_s4_class(df_result, "SpatRaster")
  expect_equal(names(df_result), c("A_doctors", "A_nurses"))

  # Test with matrix
  mat_distance <- td$distance # Should already have correct number of layers
  mat_result <- compute_access(
    td$demand,
    td$supply_matrix,
    demand_weights,
    access_weights
  )
  expect_s4_class(mat_result, "SpatRaster")
  expect_equal(dim(mat_result)[3], ncol(td$supply_matrix))


  # Test with vector
  vec_weights <- weights # weights should have same number of layers as vector length
  vec_result <- compute_access(
    td$demand,
    td$supply_vector,
    demand_weights,
    access_weights
  )
  expect_s4_class(vec_result, "SpatRaster")
})


test_that("spax_e2sfca implements E2SFCA correctly", {
  td <- create_test_data()

  # Basic usage
  result <- spax_e2sfca(
    demand = td$demand,
    supply = td$supply_df,
    distance = td$distance,
    decay_params = list(method = "gaussian", sigma = 2),
    demand_normalize = "standard",
    id_col = "id",
    supply_cols = c("doctors", "nurses")
  )

  expect_s4_class(result, "SpatRaster")
  expect_equal(names(result), c("A_doctors", "A_nurses"))

  # Test snap mode
  snap_result <- spax_e2sfca(
    demand = td$demand,
    supply = td$supply_df,
    distance = td$distance,
    decay_params = list(method = "gaussian", sigma = 2),
    demand_normalize = "standard",
    id_col = "id",
    supply_cols = c("doctors", "nurses"),
    snap = TRUE
  )

  # Results should be the same with and without snap
  expect_equal(terra::values(result), terra::values(snap_result))
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

  # Results should be different for each normalization method
  values <- lapply(results, terra::values)
  expect_false(identical(values[[1]], values[[2]]))
  expect_false(identical(values[[2]], values[[3]]))
})
