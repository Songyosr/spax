# calc_normalize(): normalization methods on vectors and rasters.

test_that("calc_normalize applies each method to numeric vectors", {
  x <- c(0.5, 1.0, 1.5, 2.0)
  low <- c(0.2, 0.3, 0.4)
  halve <- function(x, factor = 2) x / factor
  cases <- list(
    list(x, "identity", NULL, 0, x),
    list(x, "standard", NULL, 0, x / 5),
    list(x, "standard", NULL, 1, x / 6),
    list(x, "semi", NULL, 0, x / 5),
    list(low, "semi", NULL, 0, low),
    list(low, "semi", NULL, 0.5, low / 1.4),
    list(x, "reference", NULL, 0, x / 2),
    list(x, "reference", 4, 0, x / 4),
    list(c(0.5, NA, 1.5, 2), "standard", NULL, 0, c(0.5, NA, 1.5, 2) / 4),
    list(c(0, 0, 0), "standard", NULL, 0, c(0, 0, 0)),
    list(c(NA, NA), "standard", NULL, 0, c(NA, NA)),
    list(5, "standard", NULL, 0, 1)
  )
  for (i in seq_along(cases)) {
    case <- cases[[i]]
    expect_equal(calc_normalize(case[[1]], case[[2]], ref_value = case[[3]],
                                a0 = case[[4]]),
                 case[[5]], info = paste("vector case", i))
  }
  expect_equal(calc_normalize(x, halve), x / 2)
  expect_equal(calc_normalize(x, halve, factor = 4), x / 4)

  expect_error(calc_normalize(c(1, -2, 3), "standard"), "non-negative")
  expect_error(calc_normalize(toy_rast(c(1, -2, 3, 4), 2, 2), "identity"),
               "non-negative")
  # snap skips validation, and a custom method receives x as is
  expect_equal(calc_normalize(c(1, -2, 3), "identity", snap = TRUE), c(1, -2, 3))
  expect_equal(calc_normalize(c(-1, 0, 1), function(x) x + 1), c(0, 1, 2))
})

test_that("calc_normalize normalizes each raster cell across layers", {
  # Cell sums: 0.3, 0.5, 0.4, 1.2, 1.2, 2.5, 0, 0.4 (one layer NA), 3.
  V <- cbind(c(0.1, 0.2, 0.3, 0.6, 0.8, 1.0, 0, 0.4, 2),
             c(0.2, 0.3, 0.1, 0.6, 0.4, 1.5, 0, NA, 1))
  r <- toy_rast(V, nlyr = 2, names = c("a", "b"))
  rs <- rowSums(V, na.rm = TRUE)
  halve <- function(x) x / 2
  cases <- list(
    list("identity", 0, V),
    list("standard", 0, V * ifelse(rs > 0, 1 / rs, 0)),
    list("standard", 0.5, V / (rs + 0.5)),
    list("semi", 0, V / ifelse(rs > 1, rs, 1)),
    list(halve, 0, V / 2)
  )
  for (i in seq_along(cases)) {
    case <- cases[[i]]
    result <- calc_normalize(r, case[[1]], a0 = case[[2]])
    expect_true(terra::compareGeom(result, r) &&
                  identical(names(result), c("a", "b")))
    expect_equal(unname(terra::values(result)), case[[3]],
                 info = paste("raster case", i))
  }

  # Reference: by default each cell's maximum across layers (missing when it
  # is not positive); a given reference value applies to every cell.
  cell_max <- pmax(V[, 1], V[, 2], na.rm = TRUE)
  expect_equal(unname(terra::values(calc_normalize(r, "reference"))),
               V * ifelse(cell_max > 0, 1 / cell_max, NA))
  expect_equal(unname(terra::values(calc_normalize(r, "reference", ref_value = 4))),
               V / 4)
  expect_true(all(is.na(terra::values(calc_normalize(r, "reference",
                                                     ref_value = 0)))))
})

test_that("calc_normalize validates its inputs unless snapped", {
  x <- c(0.5, 1.0, 1.5, 2.0)

  expect_error(calc_normalize(numeric(0)), "Input weights cannot be empty")
  expect_error(calc_normalize(x, "invalid_method"), "Invalid normalization method")
  expect_error(calc_normalize("not_numeric", "standard"))
  expect_error(calc_normalize(x, "reference", ref_value = "a"),
               "ref_value must be numeric")
  expect_error(calc_normalize(x, "reference", ref_value = c(1, 2)),
               "ref_value must have length 1")
  expect_error(calc_normalize(x, "reference", ref_value = NA_real_),
               "ref_value must not be missing")
  expect_error(calc_normalize(x, a0 = -1), "a0 must be >= 0")
  expect_error(calc_normalize(x, a0 = "a"), "a0 must be numeric")
  expect_error(calc_normalize(x, a0 = c(1, 2)), "a0 must have length 1")

  # snap skips validation and computes the same result
  expect_silent(calc_normalize(x, "invalid_method", snap = TRUE))
  expect_equal(calc_normalize(x, "standard", snap = TRUE), calc_normalize(x))
  expect_equal(calc_normalize(x, function(x, factor) x / factor, snap = TRUE,
                              factor = 4),
               x / 4)
})
