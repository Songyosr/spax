# SPAX-004: typed compute_fca engine

.mk_compute_fca_data <- function() {
  demand <- terra::rast(nrows = 3, ncols = 3)
  terra::values(demand) <- c(10, 20, 30, 40, 50, 60, 70, 80, 90)

  dist1 <- terra::rast(nrows = 3, ncols = 3)
  dist2 <- terra::rast(nrows = 3, ncols = 3)
  terra::values(dist1) <- c(1, 2, 3, 2, 3, 4, 3, 4, 5)
  terra::values(dist2) <- c(5, 4, 3, 4, 3, 2, 3, 2, 1)
  distance <- c(dist1, dist2)
  names(distance) <- c("facility1", "facility2")

  supply_matrix <- matrix(
    c(10, 15, 20, 25),
    nrow = 2,
    dimnames = list(c("facility1", "facility2"), c("doctors", "nurses"))
  )
  supply_vector <- c(facility1 = 10, facility2 = 15)

  list(
    demand = demand,
    distance = distance,
    supply_matrix = supply_matrix,
    supply_vector = supply_vector
  )
}

.raw_fca_expected <- function(demand, supply, demand_kernel, access_kernel,
                              demand_normalize = "identity",
                              indicator_names = NULL) {
  demand_weights <- calc_normalize(demand_kernel, method = demand_normalize)
  processed <- .help_process_supply(supply, weight_ids = names(demand_kernel))
  demand_by_site <- gather_demand(demand, demand_weights)
  ratios <- sweep(processed$values, 1, demand_by_site$potential_demand, "/")
  result <- spread_weighted(ratios, access_kernel)
  names(result) <- indicator_names %||% processed$cols
  result
}

.kernel_field <- function(kernel) {
  frame <- data.frame(
    layer = names(kernel),
    facility = names(kernel),
    stringsAsFactors = FALSE
  )
  .create_spax_raster_field(
    kernel,
    domain = c("I", "facility"),
    frame = frame,
    role = "kernel"
  )
}

test_that("compute_fca single-measure output equals the raw FCA core", {
  td <- .mk_compute_fca_data()
  weights <- calc_decay(td$distance, method = "gaussian", sigma = 2)

  result <- compute_fca(
    demand = td$demand,
    supply = td$supply_vector,
    demand_kernel = weights,
    access_kernel = weights,
    demand_normalize = "standard"
  )
  expected <- .raw_fca_expected(
    demand = td$demand,
    supply = td$supply_vector,
    demand_kernel = weights,
    access_kernel = weights,
    demand_normalize = "standard"
  )

  expect_s3_class(result, "spax_raster_field")
  expect_equal(.field_domain(result), "I")
  expect_equal(terra::values(.fca_result_raster(result)), terra::values(expected))
})

test_that("compute_fca multi-measure output preserves names and values", {
  td <- .mk_compute_fca_data()
  weights <- calc_decay(td$distance, method = "gaussian", sigma = 2)

  result <- compute_fca(
    demand = td$demand,
    supply = td$supply_matrix,
    demand_kernel = weights,
    access_kernel = weights,
    demand_normalize = "standard"
  )
  expected <- .raw_fca_expected(
    demand = td$demand,
    supply = td$supply_matrix,
    demand_kernel = weights,
    access_kernel = weights,
    demand_normalize = "standard"
  )

  expect_s3_class(result, "spax_raster_field")
  expect_equal(names(.fca_result_raster(result)), c("doctors", "nurses"))
  expect_equal(terra::values(.fca_result_raster(result)), terra::values(expected))
})

test_that("compute_fca raw and field doors are equivalent", {
  td <- .mk_compute_fca_data()
  weights <- calc_decay(td$distance, method = "gaussian", sigma = 2)
  demand_field <- .create_spax_raster_field(td$demand, domain = "I", role = "demand")
  kernel_field <- .kernel_field(weights)

  raw_result <- compute_fca(
    demand = td$demand,
    supply = td$supply_matrix,
    demand_kernel = weights,
    access_kernel = weights,
    demand_normalize = "semi"
  )
  field_result <- compute_fca(
    demand = demand_field,
    supply = td$supply_matrix,
    demand_kernel = kernel_field,
    access_kernel = kernel_field,
    demand_normalize = "semi"
  )

  expect_equal(terra::values(.fca_result_raster(raw_result)),
               terra::values(.fca_result_raster(field_result)))
})

test_that("compute_fca demand normalization methods match raw behavior", {
  td <- .mk_compute_fca_data()
  weights <- calc_decay(td$distance, method = "gaussian", sigma = 2)

  for (method in c("identity", "standard", "semi")) {
    result <- compute_fca(
      demand = td$demand,
      supply = td$supply_matrix,
      demand_kernel = weights,
      access_kernel = weights,
      demand_normalize = method
    )
    expected <- .raw_fca_expected(
      demand = td$demand,
      supply = td$supply_matrix,
      demand_kernel = weights,
      access_kernel = weights,
      demand_normalize = method
    )

    expect_equal(terra::values(.fca_result_raster(result)), terra::values(expected))
  }
})

test_that("compute_fca rejects mismatched demand/access kernel facility ids", {
  td <- .mk_compute_fca_data()
  weights <- calc_decay(td$distance, method = "gaussian", sigma = 2)
  access_weights <- weights
  names(access_weights) <- c("facility1", "facility3")

  expect_error(
    compute_fca(
      demand = td$demand,
      supply = td$supply_vector,
      demand_kernel = weights,
      access_kernel = access_weights
    ),
    "same facility ids"
  )
})
