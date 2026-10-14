# gather_weighted()/gather_demand(): weighted sums of a raster per unit.

gather_inputs <- function() {
  values <- toy_rast(as.numeric(1:25), 5, 5)
  multi <- c(values, values * 2)
  names(multi) <- c("sim1", "sim2")
  weights <- toy_rast(seq(0.01, 0.75, by = 0.01), 5, 5, nlyr = 3,
                      names = c("unit1", "unit2", "unit3"))
  list(values = values, multi = multi, weights = weights,
       expected = crossprod(terra::values(weights), terra::values(multi)))
}

test_that("gather_weighted sums values times weights for each unit", {
  td <- gather_inputs()
  units <- c("unit1", "unit2", "unit3")
  sums <- unname(td$expected)

  expect_equal(gather_weighted(td$values, td$weights, simplify = TRUE),
               td$expected[, "sim1"])
  expect_equal(gather_weighted(td$values, td$weights),
               data.frame(unit_id = units, weighted_sum = sums[, 1]))
  expect_equal(gather_weighted(td$multi, td$weights, simplify = TRUE),
               td$expected)
  expect_equal(gather_weighted(td$multi, td$weights),
               data.frame(unit_id = units, sim1_weighted_sum = sums[, 1],
                          sim2_weighted_sum = sums[, 2]))

  # snap skips validation and formatting
  expect_equal(gather_weighted(td$values, td$weights, snap = TRUE), sums[, 1])
  expect_equal(gather_weighted(td$multi, td$weights, snap = TRUE), sums)

  # gather_demand() is the same sum under its own column names.
  expect_equal(gather_demand(td$values, td$weights),
               data.frame(location_id = units, potential_demand = sums[, 1]))
})

test_that("gather_weighted handles missing values, zeros and one unit", {
  values <- toy_rast(c(1:4, NA, 6:9))  # weighted cells 1-8 sum to 31
  W <- matrix(0.5, 9, 2)
  W[9, ] <- NA
  weights <- toy_rast(W, nlyr = 2, names = c("unit1", "unit2"))

  expect_equal(gather_weighted(values, weights, simplify = TRUE),
               c(unit1 = 15.5, unit2 = 15.5))
  expect_equal(gather_weighted(values, weights, na.rm = FALSE, simplify = TRUE),
               c(unit1 = NA_real_, unit2 = NA_real_))
  expect_equal(gather_weighted(values * 0, weights)$weighted_sum, c(0, 0))
  expect_equal(gather_weighted(values, weights[[1]]),
               data.frame(unit_id = "unit1", weighted_sum = 15.5))
})

test_that("gather functions reject invalid inputs", {
  td <- gather_inputs()
  shifted <- terra::rast(nrows = 5, ncols = 5, nlyrs = 3,
                         xmin = 10, xmax = 20, ymin = 10, ymax = 20)
  terra::values(shifted) <- 0.5

  expect_error(gather_weighted(as.matrix(1:25), td$weights),
               "values must be one of")
  expect_error(gather_weighted(td$values, as.matrix(1:75)),
               "weights must be one of")
  expect_error(gather_weighted(td$values, toy_rast(0.5, 10, 10, nlyr = 2)),
               "same geometry")
  expect_error(gather_weighted(td$values, shifted), "same geometry")
  expect_error(gather_weighted(td$values, td$weights, na.rm = "yes"),
               "na.rm must be one of")
  expect_error(gather_demand(td$values, td$weights * 2),
               "probability values between 0 and 1")
  expect_error(gather_demand(td$values, td$weights - 0.5),
               "probability values between 0 and 1")
})
