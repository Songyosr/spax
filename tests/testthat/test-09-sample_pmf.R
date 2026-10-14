# sample_pmf(): count realizations drawn from a PMF raster.

# A fixed 5 x 5 PMF (or the raw weights behind it); no random draws.
sample_test_pmf <- function(normalized = TRUE, nrows = 5, ncols = 5) {
  w <- seq_len(nrows * ncols) %% 7 + 1
  toy_rast(if (normalized) w / sum(w) else w, nrows, ncols)
}

layer_sums <- function(x) unname(colSums(terra::values(x), na.rm = TRUE))

test_that("sample_pmf validates its inputs", {
  pmf <- sample_test_pmf()

  expect_error(sample_pmf(as.matrix(terra::values(pmf)), n = 10),
               "must be a SpatRaster")
  expect_error(sample_pmf(pmf, method = "poisson"),
               "Either n or both size and prob")
  expect_error(sample_pmf(pmf, size = 100, prob = 1.5), "prob must be between 0 and 1")
  expect_error(sample_pmf(pmf, n = 10, evolve_prop = 2),
               "evolve_prop must be between 0 and 1")
  expect_error(sample_pmf(pmf, n = 10, iterations = 0),
               "iterations must be a positive integer")
  expect_error(sample_pmf(pmf, n = 10, method = "invalid"), "Unknown method string")
  expect_error(sample_pmf(toy_rast(0, 5, 5), n = 10), "Sum of all values")
  expect_error(sample_pmf(toy_rast(NA_real_, 5, 5), n = 10), "only NA values")
  for (n in list(-1, 2.5, NA_real_)) {
    expect_error(sample_pmf(pmf, n = n), "n must be a non-negative integer")
  }

  bad_method <- function(value) function() value
  expect_error(sample_pmf(pmf, method = bad_method("a")), "must return numeric")
  expect_error(sample_pmf(pmf, method = bad_method(c(1, 2))), "must return 1 values")
  expect_error(sample_pmf(pmf, method = bad_method(-1)), "non-negative integer")
  expect_error(sample_pmf(pmf, method = bad_method(1.5)), "non-negative integer")
})

test_that("each realization holds the drawn number of samples", {
  pmf <- sample_test_pmf()
  seed <- 42

  fixed <- sample_pmf(pmf, n = 50, iterations = 3, seed = seed)
  expect_equal(terra::nlyr(fixed), 3)
  expect_equal(layer_sums(fixed), c(50, 50, 50))

  # Built-in methods draw one sample size per iteration from their law.
  draws <- list(
    poisson = withr::with_seed(seed, rpois(4, 1000 * 0.05)),
    binomial = withr::with_seed(seed, rbinom(4, 1000, 0.05)),
    nbinom = withr::with_seed(seed, rnbinom(4, 1000, 0.05))
  )
  for (method in names(draws)) {
    sampled <- sample_pmf(pmf, size = 1000, prob = 0.05, method = method,
                          iterations = 4, seed = seed)
    expect_equal(layer_sums(sampled), draws[[method]], info = method)
  }

  # A custom method sets the sample size directly; with raw weights the total
  # is reported as the size parameter.
  expect_message(
    custom <- sample_pmf(sample_test_pmf(normalized = FALSE),
                         method = function(k) k, k = 30, seed = seed),
    "Using total population"
  )
  expect_equal(layer_sums(custom), 30)
})

test_that("zero cells are NA unless replace_0 = FALSE", {
  pmf <- sample_test_pmf()

  sparse <- terra::values(sample_pmf(pmf, n = 3, seed = 1))[, 1]
  kept <- terra::values(sample_pmf(pmf, n = 3, replace_0 = FALSE, seed = 1))[, 1]
  expect_true(all(is.na(sparse) | sparse > 0))
  expect_equal(kept, ifelse(is.na(sparse), 0, sparse))

  expect_true(all(is.na(terra::values(sample_pmf(pmf, n = 0, seed = 1)))))
  expect_true(all(terra::values(sample_pmf(pmf, n = 0, replace_0 = FALSE,
                                           seed = 1)) == 0))
  expect_equal(layer_sums(sample_pmf(sample_test_pmf(nrows = 1, ncols = 1),
                                     n = 10, seed = 1)), 10)
})

test_that("evolve_prop controls how samples carry over between iterations", {
  pmf <- sample_test_pmf()

  expect_warning(
    frozen <- sample_pmf(pmf, n = 100, iterations = 5, evolve_prop = 0, seed = 1),
    "evolve_prop = 0"
  )
  expect_equal(terra::nlyr(frozen), 1)
  expect_equal(layer_sums(frozen), 100)

  for (prop in c(0.001, 0.5, 0.999)) {
    evolving <- sample_pmf(pmf, n = 100, iterations = 5, evolve_prop = prop,
                           seed = 1)
    expect_equal(layer_sums(evolving), rep(100, 5), info = prop)
  }

  # Retained samples make consecutive realizations positively correlated.
  partial <- terra::values(sample_pmf(pmf, n = 100, iterations = 5,
                                      evolve_prop = 0.5, replace_0 = FALSE,
                                      seed = 42))
  expect_true(all(diag(cor(partial[, -5], partial[, -1])) > 0))
})

test_that("samples follow the PMF and seeds reproduce them", {
  pmf <- transform_pmf(toy_rast(seq(0.1, 1, length.out = 100), 10, 10))
  many <- sample_pmf(pmf, n = 10000, replace_0 = FALSE, seed = 42)
  expect_gt(cor(terra::values(many)[, 1] / 10000, terra::values(pmf)[, 1]), 0.7)

  small <- sample_test_pmf()
  first <- sample_pmf(small, n = 100, seed = 42)
  expect_equal(terra::values(sample_pmf(small, n = 100, seed = 42)),
               terra::values(first))
  expect_false(identical(terra::values(sample_pmf(small, n = 100, seed = 43)),
                         terra::values(first)))

  # A given seed leaves the caller's random stream untouched.
  local_test_seed(1)
  before <- .Random.seed
  sample_pmf(small, n = 10, seed = 42)
  expect_identical(.Random.seed, before)

  # In snap mode the input is not converted, only flagged.
  expect_message(sample_pmf(sample_test_pmf(normalized = FALSE), n = 5,
                            snap = TRUE, seed = 1),
                 "must be PMF in snap mode")
})
