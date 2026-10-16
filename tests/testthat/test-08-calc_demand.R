# calc_demand(): group counts x rates x scale, optionally applied to a total.

demand_groups <- function() {
  g <- terra::rast(nrows = 2, ncols = 2, nlyrs = 3, xmin = 0, xmax = 2, ymin = 0, ymax = 2)
  terra::values(g) <- cbind(young = c(10, 0, 5, NA), adult = c(20, 0, 15, NA),
                            old = c(5, 0, 10, NA))
  names(g) <- c("young", "adult", "old")
  g
}
demand_rates <- c(young = 0, adult = 0.002, old = 0.005)

test_that("group demand is the rate-weighted sum times the scale", {
  g <- demand_groups()
  d <- calc_demand(g, demand_rates, scale = 0.8)
  expect_s4_class(d, "SpatRaster")
  v <- terra::values(d, mat = FALSE)
  expect_equal(v[1], (20 * 0.002 + 5 * 0.005) * 0.8)
  expect_equal(v[2], 0)
  expect_true(is.na(v[4]))
  expect_equal(attr(d, "implied_rate"), (35 * 0.002 + 15 * 0.005) / 65)
  expect_equal(attr(d, "fallback_cells"), 0L)
  # Rates are matched by name, not position.
  expect_equal(terra::values(calc_demand(g, rev(demand_rates), scale = 0.8)), terra::values(d))
})

test_that("a total takes each cell's group composition, with a fallback rate", {
  g <- demand_groups()
  total <- terra::rast(g, nlyrs = 1)
  terra::values(total) <- c(40, 12, 30, NA)
  d <- calc_demand(g, demand_rates, total = total)
  v <- terra::values(d, mat = FALSE)
  expect_equal(v[1], 40 * (20 * 0.002 + 5 * 0.005) / 35)
  # Cell 2 has a total but no group population: overall rate.
  expect_equal(v[2], 12 * attr(d, "implied_rate"))
  expect_equal(attr(d, "fallback_cells"), 1L)
  expect_true(is.na(v[4]))
  expect_error(calc_demand(g, demand_rates, total = total, fallback = "error"),
               "1 cells have a total but no group population")
})

test_that("matrix input works like a raster", {
  m <- terra::values(demand_groups(), mat = TRUE)
  d <- calc_demand(m, demand_rates, total = c(40, 12, 30, NA))
  expect_type(d, "double")
  expect_equal(d[1], 40 * (20 * 0.002 + 5 * 0.005) / 35)
})

test_that("invalid inputs fail clearly", {
  g <- demand_groups()
  expect_error(calc_demand(g, demand_rates[1:2]), "no rate for: old")
  expect_error(calc_demand(g, c(demand_rates, extra = 1)), "no group for: extra")
  expect_error(calc_demand(g, c(young = -1, adult = 0, old = 0)), "nonnegative")
  expect_error(calc_demand(g, demand_rates, scale = 0), "scale")
  expect_error(calc_demand(list(1), demand_rates), "SpatRaster or a numeric matrix")
  wrong <- terra::rast(nrows = 3, ncols = 3)
  expect_error(calc_demand(g, demand_rates, total = wrong), "same grid")
  expect_error(calc_demand(terra::values(g, mat = TRUE), demand_rates, total = 1:2),
               "one value per row")
})
