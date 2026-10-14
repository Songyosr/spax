# Public FCA wrappers (spax_2sfca, spax_e2sfca, spax_m2sfca, compute_access)
# checked against the base-R fca_oracle(), plus the spax S3 methods.

# Toy inputs as plain matrices for the oracle.
fca_toy_matrices <- function(td = fca_toy()) {
  list(D = terra::values(td$demand)[, 1],
       d = unname(terra::values(td$distance)),
       S = unname(td$supply_matrix))
}

access_values <- function(result) unname(terra::values(result$accessibility))

test_that("spax_2sfca, spax_e2sfca and spax_m2sfca match the FCA oracle", {
  td <- fca_toy()
  m <- fca_toy_matrices(td)
  run <- function(fun, ...) {
    fun(td$demand, td$supply_df, td$distance, id_col = "id",
        supply_cols = c("doctors", "nurses"), ...)
  }

  # 2SFCA: a binary catchment within the threshold, no normalization.
  binary <- 1 * (m$d <= 3)
  two <- run(spax_2sfca, threshold = 3)
  expect_equal(two$type, "2SFCA")
  expect_equal(two$parameters$decay_params, list(method = "binary", sigma = 3))
  expect_equal(access_values(two), fca_oracle(m$D, binary, binary, m$S))

  # sigma = 2.4 puts the cell sums of W on both sides of 1, so the three
  # demand normalizations all differ.
  W <- exp(-m$d^2 / (2 * 2.4^2))
  decay <- list(method = "gaussian", sigma = 2.4)
  for (norm in c("identity", "standard", "semi")) {
    e2 <- run(spax_e2sfca, decay_params = decay, demand_normalize = norm)
    expect_equal(access_values(e2), fca_oracle(m$D, W, W, m$S, norm), info = norm)
    expect_equal(e2$parameters, list(decay_params = decay, demand_normalize = norm))
  }
  expect_equal(e2$type, "E2SFCA")
  # The configuration of the former E2SFCA golden snapshot.
  W2 <- exp(-m$d^2 / 8)
  expect_equal(access_values(run(spax_e2sfca, demand_normalize = "standard",
                                 decay_params = list(method = "gaussian", sigma = 2))),
               fca_oracle(m$D, W2, W2, m$S, "standard"))

  # M2SFCA squares the kernel in the access step only.
  m2 <- run(spax_m2sfca, decay_params = decay, demand_normalize = "standard")
  expect_equal(m2$type, "M2SFCA")
  expect_equal(access_values(m2), fca_oracle(m$D, W, W^2, m$S, "standard"))
  m2_vector <- spax_m2sfca(td$demand, td$supply_vector, td$distance,
                           decay_params = decay)
  expect_equal(access_values(m2_vector), fca_oracle(m$D, W, W^2, m$S[, 1]))
})

test_that("compute_access applies separate demand and access weights", {
  td <- fca_toy()
  m <- fca_toy_matrices(td)
  Wd <- exp(-m$d^2 / 8)
  Wa <- 1 * (m$d <= 3)

  result <- compute_access(
    demand = td$demand,
    supply = td$supply_matrix,
    demand_weights = terra::setValues(td$distance, Wd),
    access_weights = terra::setValues(td$distance, Wa),
    indicator_names = c("doc_access", "nurse_access")
  )
  expect_s4_class(result, "SpatRaster")
  expect_true(terra::compareGeom(result, td$demand))
  expect_equal(names(result), c("doc_access", "nurse_access"))
  expect_equal(unname(terra::values(result)), fca_oracle(m$D, Wd, Wa, m$S))
})

test_that("supply is matched to facilities by ID in every format", {
  td <- fca_toy()
  m <- fca_toy_matrices(td)
  decay <- list(method = "gaussian", sigma = 2)
  W <- exp(-m$d^2 / 8)
  expected <- fca_oracle(m$D, W, W, m$S)
  e2 <- function(supply, ...) {
    spax_e2sfca(td$demand, supply, td$distance, decay_params = decay, ...)
  }

  # Data frame rows and vector names out of layer order bind by ID.
  reversed <- td$supply_df[2:1, ]
  from_df <- e2(reversed, id_col = "id", supply_cols = c("doctors", "nurses"),
                indicator_names = c("doc_access", "nurse_access"))
  expect_equal(access_values(from_df), expected)
  expect_equal(names(from_df$accessibility), c("doc_access", "nurse_access"))
  expect_equal(from_df$facilities, data.frame(id = reversed$id,
                                              reversed[c("doctors", "nurses")]))

  from_matrix <- e2(td$supply_matrix)
  expect_equal(access_values(from_matrix), expected)
  expect_equal(names(from_matrix$accessibility), c("doctors", "nurses"))
  expect_equal(from_matrix$facilities$id, c("facility1", "facility2"))

  from_vector <- e2(rev(td$supply_vector))
  expect_equal(access_values(from_vector), expected[, 1, drop = FALSE])
  expect_equal(as.list(from_vector$facilities),
               list(id = c("facility2", "facility1"), supply = c(15, 10)))

  # snap skips validation without changing the result
  snapped <- e2(td$supply_vector, snap = TRUE)
  expect_equal(access_values(snapped), expected[, 1, drop = FALSE])
})

test_that("FCA wrappers reject invalid supply, decay and geometry", {
  td <- fca_toy()
  decay <- list(method = "gaussian", sigma = 2)
  e2 <- function(supply = td$supply_df, ..., distance = td$distance) {
    spax_e2sfca(td$demand, supply, distance, ...)
  }
  points <- sf::st_as_sf(td$supply_df, coords = c("doctors", "nurses"),
                         remove = FALSE)
  duplicated_ids <- td$supply_df
  duplicated_ids$id <- "facility1"

  expect_error(e2(points, id_col = "id", supply_cols = "doctors"),
               "is an sf object")
  expect_error(e2(supply_cols = "doctors"), "id_col must be specified")
  expect_error(e2(id_col = "id"), "supply_cols must be specified")
  expect_error(e2(id_col = "id", supply_cols = "beds"), "not found in supply")
  expect_error(e2(duplicated_ids, id_col = "id", supply_cols = "doctors"),
               "must be unique")
  expect_error(e2(td$supply_vector, decay_params = "gaussian"),
               "decay_params must be one of")
  expect_error(e2(td$supply_vector, decay_params = list(sigma = 2)),
               "decay_params must include 'method'")
  expect_error(e2(td$supply_vector, decay_params = decay, demand_normalize = "max"),
               "demand_normalize must be one of")
  expect_error(e2(td$supply_vector, distance = toy_rast(1, 4, 4, nlyr = 2)),
               "same geometry")
  expect_error(e2(c(facility1 = 10), decay_params = decay),
               "must match length of facilities")
  for (threshold in list(-1, "a", c(1, 2), Inf)) {
    expect_error(spax_2sfca(td$demand, td$supply_vector, td$distance,
                            threshold = threshold),
                 "threshold must be a single positive number")
  }

  weights <- calc_decay(td$distance, "gaussian", sigma = 2)
  expect_error(compute_access("not_a_raster", td$supply_vector, weights, weights),
               "demand must be one of")
  expect_error(compute_access(td$demand, td$supply_vector, weights[[1]], weights),
               "must match length of facilities")
})

test_that("spax objects print, summarise and plot their accessibility", {
  td <- fca_toy()
  td$distance[9] <- NA # cell 9 reaches no facility
  result <- spax_e2sfca(td$demand, td$supply_df, td$distance,
                        decay_params = list(method = "gaussian", sigma = 2),
                        id_col = "id", supply_cols = c("doctors", "nurses"))
  expect_s3_class(result, "spax")
  expect_true(all(c("accessibility", "type", "parameters", "facilities", "call")
                  %in% names(result)))

  expect_output(expect_invisible(print(result)), "E2SFCA(.|\n)*doctors")

  s <- summary(result)
  expect_s3_class(s, "summary.spax")
  v <- terra::values(result$accessibility[["nurses"]])[, 1]
  nurses <- s$accessibility$nurses
  expect_equal(unlist(nurses[c("min", "max", "mean", "sd")]),
               c(min = min(v, na.rm = TRUE), max = max(v, na.rm = TRUE),
                 mean = mean(v, na.rm = TRUE), sd = sd(v, na.rm = TRUE)))
  expect_equal(nurses$quantiles,
               stats::quantile(v, c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))
  expect_equal(nurses$coverage$proportion, 8 / 9)
  expect_equal(s$facilities$count, 2)
  expect_equal(s$facilities$supply$doctors$total, 25)
  expect_output(expect_invisible(print(s)), "E2SFCA(.|\n)*doctors(.|\n)*nurses")
  expect_output(print(s), "Median")
  # Custom probabilities are labelled by percentage.
  expect_output(print(summary(result, quantiles = c(0.1, 0.9))), "10%:(.|\n)*90%:")

  withr::local_pdf(NULL)
  expect_no_error(plot(result))
  expect_no_error(plot(result, y = "nurses", main = "Nurses"))
})
