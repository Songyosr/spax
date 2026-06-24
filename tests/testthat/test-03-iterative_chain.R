# tests/testthat/test-03-iterative_chain.R

create_ifca_test_data <- function() {
  demand <- terra::rast(nrows = 3, ncols = 3)
  terra::values(demand) <- c(10, 20, 30, 40, 50, 60, 70, 80, 90)

  d1 <- terra::rast(nrows = 3, ncols = 3)
  d2 <- terra::rast(nrows = 3, ncols = 3)
  terra::values(d1) <- c(1, 2, 3, 2, 3, 4, 3, 4, 5)
  terra::values(d2) <- c(5, 4, 3, 4, 3, 2, 3, 2, 1)
  distance <- c(d1, d2)
  names(distance) <- c("f1", "f2")

  list(
    demand = demand,
    distance = distance,
    supply = c(10, 15)
  )
}

test_that(".help_prep_facilities and .help_add_0facilities handle zero supply", {
  td <- create_ifca_test_data()
  supply <- c(10, 0)

  processed <- .help_prep_facilities(supply, td$distance)
  expect_equal(processed$supply, 10)
  expect_equal(length(processed$zero_map), 2)
  expect_equal(terra::nlyr(processed$distances), 1)

  restored <- .help_add_0facilities(results = 99, zero_map = processed$zero_map, fill = 0)
  expect_equal(restored, c(99, 0))
})

test_that(".chck_spax_ifca validates key constraints", {
  td <- create_ifca_test_data()

  expect_no_error(
    .chck_spax_ifca(
      distance_raster = td$distance,
      demand = td$demand,
      supply = td$supply,
      decay_params = list(method = "gaussian", sigma = 2),
      lambda = 0.5,
      max_iter = 10,
      tolerance = 1e-4,
      window_size = 3
    )
  )

  expect_error(
    .chck_spax_ifca(
      distance_raster = td$distance,
      demand = td$demand,
      supply = td$supply,
      decay_params = list(method = "gaussian", sigma = 2),
      lambda = 1.2,
      max_iter = 10,
      tolerance = 1e-4,
      window_size = 3
    ),
    "lambda must be between"
  )
})

test_that("spax_ifca snap path and full path return expected structures", {
  td <- create_ifca_test_data()

  fast <- spax_ifca(
    distance_raster = td$distance,
    demand = td$demand,
    supply = td$supply,
    decay_params = list(method = "gaussian", sigma = 2),
    max_iter = 8,
    window_size = 3,
    snap = TRUE
  )
  expect_type(fast, "double")
  expect_equal(length(fast), 2)

  full <- spax_ifca(
    distance_raster = td$distance,
    demand = td$demand,
    supply = td$supply,
    decay_params = list(method = "gaussian", sigma = 2),
    max_iter = 8,
    window_size = 3,
    snap = FALSE
  )
  expect_s3_class(full, "spax")
  expect_equal(full$type, "iFCA")
  expect_s4_class(full$accessibility, "SpatRaster")
  expect_true(all(c("id", "utilization", "ratio", "attractiveness") %in% names(full$facilities)))
})
