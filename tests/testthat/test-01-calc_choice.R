# calc_choice(): Huff-style choice probabilities from spatial weights.

# Cell 1 of facility1 is missing and cell 9 has no weight at all.
choice_weights <- function() {
  W <- matrix(c(0.5, 0.3, 0.2, 0.4, 0.6, 0.8, 0.1, 0.7, 0.9,
                0.2, 0.4, 0.6, 0.3, 0.1, 0.5, 0.8, 0.2, 0.4), ncol = 2)
  W[1, 1] <- NA
  W[9, ] <- 0
  W
}

test_that("calc_choice gives W * A^alpha / (sum of W * A^alpha + a0) per cell", {
  W <- choice_weights()
  weights <- toy_rast(W, nlyr = 2, names = c("facility1", "facility2"))
  cases <- list(
    list(attractiveness = NULL, alpha = 1, a0 = 0),
    list(attractiveness = c(100, 50), alpha = 1, a0 = 0),
    list(attractiveness = c(2, 1), alpha = 2, a0 = 0.5)
  )

  for (case in cases) {
    score <- if (is.null(case$attractiveness)) W else
      sweep(W, 2, case$attractiveness^case$alpha, "*")
    denom <- rowSums(score, na.rm = TRUE) + case$a0
    expected <- score * ifelse(denom > 0, 1 / denom, 0)
    for (snap in c(FALSE, TRUE)) {
      result <- do.call(calc_choice, c(list(weights), case, snap = snap))
      expect_equal(unname(terra::values(result)), expected,
                   info = paste(deparse(case), "snap =", snap))
    }
  }

  result <- calc_choice(weights, attractiveness = c(2, 1))
  expect_true(terra::compareGeom(result, weights))
  expect_equal(names(result), c("facility1", "facility2"))
})

test_that("calc_choice validates weights, attractiveness, alpha and a0", {
  weights <- toy_rast(c(0.5, 0.2), nlyr = 2)

  expect_error(calc_choice("not_a_raster"), "weights must be one of")
  expect_error(calc_choice(weights * -1), "weights must be non-negative")
  expect_error(calc_choice(weights, attractiveness = "a"),
               "attractiveness must be numeric")
  expect_error(calc_choice(weights, attractiveness = 1),
               "attractiveness must have length 2")
  expect_error(calc_choice(weights, attractiveness = c(-1, 1)),
               "attractiveness must be >= 0")
  expect_error(calc_choice(weights, alpha = "a"), "alpha must be numeric")
  expect_error(calc_choice(weights, alpha = c(1, 2)), "alpha must have length 1")
  expect_error(calc_choice(weights, a0 = "a"), "a0 must be numeric")
  expect_error(calc_choice(weights, a0 = c(1, 2)), "a0 must have length 1")
  expect_error(calc_choice(weights, a0 = -1), "a0 must be >= 0")
})
