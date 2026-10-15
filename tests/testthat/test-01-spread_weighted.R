# spread_weighted()/spread_access(): spread unit values over weight layers.

spread_inputs <- function() {
  W <- cbind(as.numeric(1:9), as.numeric(9:1))
  list(W = W, weights = toy_rast(W, nlyr = 2),
       values = matrix(c(10, 20, 30, 40), 2, 2,
                       dimnames = list(NULL, c("val1", "val2"))))
}

# Per cell: sum_j v_j * w_ij, skipping missing products.
spread_expected <- function(W, v) rowSums(sweep(W, 2, v, "*"), na.rm = TRUE)

test_that("spread_weighted gives sum_j v_j * w_ij in every cell", {
  td <- spread_inputs()
  expected <- cbind(spread_expected(td$W, td$values[, 1]),
                    spread_expected(td$W, td$values[, 2]))

  vec <- spread_weighted(td$values[, 1], td$weights)
  expect_true(terra::compareGeom(vec, td$weights))
  expect_equal(unname(terra::values(vec))[, 1], expected[, 1])

  mat <- spread_weighted(td$values, td$weights)
  expect_equal(names(mat), c("val1", "val2"))
  expect_equal(unname(terra::values(mat)), expected)

  df <- data.frame(id = 1:2, td$values)
  from_df <- spread_weighted(df, td$weights, value_cols = c("val2", "val1"),
                             name_prefix = "s_")
  expect_equal(names(from_df), c("s_val2", "s_val1"))
  expect_equal(unname(terra::values(from_df)), expected[, 2:1])

  expect_equal(names(spread_weighted(unname(td$values), td$weights)),
               c("value1", "value2"))
  expect_equal(terra::values(spread_weighted(td$values, td$weights, snap = TRUE)),
               terra::values(mat))
})

test_that("spread_weighted skips missing weights and values", {
  td <- spread_inputs()
  W <- td$W
  W[1, 1] <- NA
  weights <- toy_rast(W, nlyr = 2)

  expect_equal(unname(terra::values(spread_weighted(c(10, 20), weights)))[, 1],
               spread_expected(W, c(10, 20)))
  expect_equal(unname(terra::values(spread_weighted(c(NA, 1), td$weights)))[, 1],
               td$W[, 2])
  expect_equal(unname(terra::values(spread_weighted(c(0, 0), td$weights)))[, 1],
               rep(0, 9))
})

test_that("full_output returns the per-unit layers behind each total", {
  td <- spread_inputs()

  vec <- spread_weighted(c(10, 20), td$weights, full_output = TRUE)
  expect_named(vec, c("total_distribution", "unit_distribution"))
  expect_equal(unname(terra::values(vec$unit_distribution)),
               sweep(td$W, 2, c(10, 20), "*"))
  expect_equal(unname(terra::values(vec$total_distribution))[, 1],
               spread_expected(td$W, c(10, 20)))

  mat <- spread_weighted(td$values, td$weights, full_output = TRUE)
  expect_equal(names(mat$total_distribution), c("val1", "val2"))
  expect_named(mat$unit_distribution, c("val1", "val2"))
  expect_equal(unname(terra::values(mat$unit_distribution$val2)),
               sweep(td$W, 2, td$values[, 2], "*"))
})

test_that("spread_weighted rejects invalid inputs", {
  td <- spread_inputs()
  df <- data.frame(id = 1:2, td$values)
  points <- sf::st_as_sf(data.frame(x = 1:2, y = 1:2, v = c(10, 20)),
                         coords = c("x", "y"))

  expect_error(spread_weighted(c(10, 20), "not_a_raster"), "weights must be one of")
  expect_error(spread_weighted(points, td$weights), "is an sf object")
  expect_error(spread_weighted(list(10, 20), td$weights), "values must be one of")
  expect_error(spread_weighted(c(1, 2, 3), td$weights),
               "must match length of weight layers")
  expect_error(spread_weighted(rbind(td$values, 1), td$weights), "rows in values")
  expect_error(spread_weighted(df, td$weights), "value_cols must be specified")
  expect_error(spread_weighted(df, td$weights, value_cols = "nonexistent"),
               "not found in values")
  # snap skips validation, but the core still refuses a length mismatch
  expect_error(spread_weighted(c(1, 2, 3), td$weights, snap = TRUE),
               "must match number of weight layers")
})

test_that("spread_access spreads ratios like spread_weighted with prefixed names", {
  td <- spread_inputs()
  plain <- spread_weighted(td$values, td$weights)

  acc <- spread_access(td$values, td$weights, plot_prefix = "acc_")
  expect_equal(names(acc), c("acc_val1", "acc_val2"))
  expect_equal(unname(terra::values(acc)), unname(terra::values(plain)))

  full <- spread_access(data.frame(td$values), td$weights, ratio_cols = "val2",
                        full_output = TRUE, plot_prefix = "acc_")
  expect_named(full, c("access_scores", "site_specific"))
  expect_equal(names(full$access_scores), "acc_val2")
  expect_equal(unname(terra::values(full$access_scores)),
               unname(terra::values(plain[[2]])))
  expect_named(full$site_specific, "val2")

  expect_equal(terra::values(spread_access(c(10, 20), td$weights)),
               terra::values(spread_weighted(c(10, 20), td$weights)))
})
