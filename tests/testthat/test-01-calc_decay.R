# calc_decay(): built-in kernels, custom functions, validation and
# variable-catchment (raster) sigma.

# The five built-in kernels with an explicit sigma (inverse takes none).
decay_kernels <- function() {
  list(
    gaussian = list(sigma = 20, f = function(d, s) exp(-d^2 / (2 * s^2))),
    exponential = list(sigma = 0.1, f = function(d, s) exp(-s * d)),
    power = list(sigma = 2, f = function(d, s) d^(-s)),
    inverse = list(sigma = NULL, f = function(d, s) 1 / (d + 1)),
    binary = list(sigma = 25, f = function(d, s) 1 * (d <= s))
  )
}

test_that("built-in kernels follow their formulas on vectors, matrices and rasters", {
  kernels <- decay_kernels()
  d_mat <- matrix(seq(1, 49, by = 2), 5, 5)
  d_rast <- toy_rast(seq(5, 90, by = 5), nlyr = 2, names = c("a", "b"))

  for (m in names(kernels)) {
    k <- kernels[[m]]
    d_vec <- c(0, 1, 10, 20, 25, 30, 50, NA)
    expect_equal(
      list(calc_decay(d_vec, m, sigma = k$sigma),
           calc_decay(d_mat, m, sigma = k$sigma),
           terra::values(calc_decay(d_rast, m, sigma = k$sigma))),
      list(k$f(d_vec, k$sigma), k$f(d_mat, k$sigma),
           k$f(terra::values(d_rast), k$sigma)),
      info = m
    )
  }
  at_zero <- c("gaussian", "exponential", "inverse", "binary")
  expect_equal(
    vapply(at_zero, function(m) calc_decay(0, m, sigma = kernels[[m]]$sigma),
           numeric(1)),
    setNames(rep(1, 4), at_zero)
  )
  expect_equal(calc_decay(0, "power", sigma = 2), Inf)
  expect_length(calc_decay(numeric(0), "gaussian", sigma = 1), 0)
})

test_that("calc_decay uses the documented sigma when none is given", {
  d <- c(0, 1, 10, 97.5, 98)
  defaults <- c(gaussian = 11.64884, exponential = 0.1, power = 2, binary = 97.5)
  for (m in names(defaults)) {
    expect_equal(calc_decay(d, m), calc_decay(d, m, sigma = defaults[[m]]), info = m)
  }
  # inverse ignores sigma and takes its offset through `...`
  expect_equal(calc_decay(d, "inverse", sigma = 5), 1 / (d + 1))
  expect_equal(calc_decay(d, "inverse", c = 2), 1 / (d + 2))
})

test_that("calc_decay keeps raster geometry and layer names", {
  kernels <- decay_kernels()
  r <- toy_rast(seq(5, 90, by = 5), nlyr = 2, names = c("a", "b"))
  custom <- function(distance, k = 1) 1 / (1 + k * distance)

  for (m in names(kernels)) {
    w <- calc_decay(r, m, sigma = kernels[[m]]$sigma)
    expect_true(terra::compareGeom(w, r) && identical(names(w), names(r)),
                info = m)
  }
  w <- calc_decay(r, custom, k = 2)
  expect_true(terra::compareGeom(w, r))
  expect_equal(terra::values(w), 1 / (1 + 2 * terra::values(r)))
})

test_that("custom functions get sigma and extras only when they declare them", {
  x <- c(1, 2, 3, 4, 5)
  needs_sigma <- function(distance, sigma) exp(-distance^2 / (2 * sigma^2))
  scaled <- function(distance, sigma = 2, scale = 1) {
    scale * exp(-distance^2 / (2 * sigma^2))
  }
  no_sigma <- function(distance, k = 1) 1 / (1 + k * distance)

  # sigma is not invented for a function that requires it
  expect_error(calc_decay(x, method = needs_sigma))
  expect_equal(calc_decay(x, needs_sigma, sigma = 2), exp(-x^2 / 8))
  expect_equal(calc_decay(x, scaled), exp(-x^2 / 8))
  expect_equal(calc_decay(x, scaled, sigma = 1, scale = 3), 3 * exp(-x^2 / 2))
  expect_equal(calc_decay(x, no_sigma), 1 / (1 + x))
  expect_equal(calc_decay(x, no_sigma, sigma = 2, k = 2, unused = 9),
               1 / (1 + 2 * x))
})

test_that("calc_decay validates distance, method and sigma unless snapped", {
  d <- c(1, 2, 3)

  expect_error(calc_decay("a", "gaussian", sigma = 1), "distance must be one of")
  expect_error(calc_decay(matrix("a", 2, 2), "gaussian", sigma = 1),
               "distance must be one of")
  expect_error(calc_decay(matrix(TRUE, 2, 2), "gaussian", sigma = 1),
               "distance must be one of")
  # Integer distances are numeric.
  expect_equal(calc_decay(1:3, "gaussian", sigma = 2),
               calc_decay(c(1, 2, 3), "gaussian", sigma = 2))
  expect_equal(calc_decay(matrix(1:4, 2), "power", sigma = 2),
               calc_decay(matrix(c(1, 2, 3, 4), 2), "power", sigma = 2))
  expect_error(calc_decay(d, "triangular", sigma = 1), "Invalid method")
  expect_error(calc_decay(d, "gaussian", sigma = "a"), "sigma must be numeric")
  expect_error(calc_decay(d, "gaussian", sigma = c(1, 2)), "sigma must have length 1")
  expect_error(calc_decay(d, "gaussian", sigma = 0), "sigma must be >")
  expect_error(calc_decay(d, "gaussian", sigma = -1), "sigma must be >")

  # snap skips validation but computes the same weights
  expect_equal(calc_decay(d, "gaussian", sigma = 2, snap = TRUE),
               calc_decay(d, "gaussian", sigma = 2))
  expect_no_error(calc_decay(d, "exponential", sigma = -1, snap = TRUE))
  expect_error(calc_decay(d, "triangular", snap = TRUE), "Invalid method")
  # Classed numerics such as units objects are refused.
  skip_if_not_installed("units")
  expect_error(calc_decay(units::set_units(c(1, 2), "km"), "gaussian", sigma = 1),
               "distance must be one of")
})

test_that("a raster sigma gives each cell its own catchment", {
  d1 <- toy_rast(as.numeric(1:16), 4, 4)
  d <- c(d1, toy_rast(as.numeric(16:1), 4, 4))
  sig <- toy_rast(seq(2, 8, length.out = 16), 4, 4)
  s <- terra::values(sig)[, 1]
  gauss <- function(dist, sigma) exp(-dist^2 / (2 * sigma^2))

  # One sigma layer is shared by every distance layer.
  expect_equal(unname(terra::values(calc_decay(d, "gaussian", sigma = sig))),
               gauss(unname(terra::values(d)), s))
  # One sigma layer per distance layer.
  expect_equal(
    unname(terra::values(calc_decay(d, "gaussian", sigma = c(sig, sig * 2)))),
    gauss(unname(terra::values(d)), cbind(s, 2 * s, deparse.level = 0))
  )
  expect_equal(unname(terra::values(calc_decay(d1, "exponential", sigma = sig))),
               unname(exp(-s * terra::values(d1))))
  # A constant raster equals the scalar.
  expect_equal(terra::values(calc_decay(d, "gaussian", sigma = toy_rast(5, 4, 4))),
               terra::values(calc_decay(d, "gaussian", sigma = 5)))
})

test_that("calc_decay rejects misaligned, non-positive or mismatched sigma rasters", {
  d <- toy_rast(as.numeric(1:16), 4, 4, nlyr = 2)

  expect_error(calc_decay(d, "gaussian", sigma = toy_rast(1, 3, 3)),
               "same geometry")
  expect_error(calc_decay(d, "gaussian", sigma = toy_rast(-1, 4, 4)),
               "must be positive")
  expect_error(calc_decay(d, "gaussian", sigma = toy_rast(1, 4, 4, nlyr = 3)),
               "1 layer or one layer per")
  expect_error(calc_decay(c(1, 2), "gaussian", sigma = toy_rast(1, 4, 4)),
               "requires a SpatRaster `distance`")
})
