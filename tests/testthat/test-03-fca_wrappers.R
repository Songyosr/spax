# Public FCA wrappers (spax_2sfca, spax_e2sfca, spax_m2sfca, spax_3sfca,
# spax_huff2sfca, compute_access) checked against the base-R fca_oracle() and
# hand-computed selection weights, plus the spax S3 methods.

# Toy inputs as plain matrices for the oracle.
fca_toy_matrices <- function(td = fca_toy()) {
  list(D = terra::values(td$demand)[, 1],
       d = unname(terra::values(td$distance)),
       S = unname(td$supply_matrix))
}

access_values <- function(result) unname(terra::values(result$accessibility))

# Selection weights by hand: the weights `Tw` of each origin (times a^alpha for
# Huff-2SFCA) divided by their sum over the facilities; 0 where they sum to 0 and
# NA where a weight is missing.
selection_by_hand <- function(Tw, a = NULL, alpha = 1) {
  score <- if (is.null(a)) Tw else sweep(Tw, 2, a^alpha, "*")
  rs <- rowSums(score, na.rm = TRUE)
  score * ifelse(rs > 0, 1 / rs, 0)
}

# 3SFCA and Huff-2SFCA by hand: decay weights `W` and selection weights `G` give
# the kernel K = W * G of both steps, then the oracle's ratios and accessibility.
selection_fca_by_hand <- function(D, W, G, S) {
  K <- W * G
  fca_oracle(D, K, K, S)
}

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
  for (threshold in list(-1, "a", c(1, 2), Inf, NA_real_, NULL)) {
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
  # Coverage counts cells with a value; covered means accessibility above zero.
  expect_equal(nurses$coverage$cells_total, sum(!is.na(v)))
  expect_equal(nurses$coverage$cells_covered, sum(v > 0, na.rm = TRUE))
  expect_equal(nurses$coverage$proportion, sum(v > 0, na.rm = TRUE) / sum(!is.na(v)))
  expect_equal(s$facilities$count, 2)
  expect_equal(s$facilities$supply$doctors$total, 25)
  expect_output(expect_invisible(print(s)), "E2SFCA(.|\n)*doctors(.|\n)*nurses")
  expect_output(print(s), "Median")
  # Custom probabilities are labelled by percentage.
  expect_output(print(summary(result, quantiles = c(0.1, 0.9))), "10%:(.|\n)*90%:")
  # Labels follow stats::quantile().
  expect_equal(names(summary(result, quantiles = 1 / 3)$accessibility$nurses$quantiles),
               names(stats::quantile(0, 1 / 3)))
  # The first argument is named as in the generic, summary(object, ...).
  expect_identical(summary(object = result), s)
  expect_identical(summary(object = result, quantiles = 0.5),
                   summary(result, quantiles = 0.5))

  withr::local_pdf(NULL)
  expect_no_error(plot(result))
  expect_no_error(plot(result, y = "nurses", main = "Nurses"))
})

# 3SFCA and Huff-2SFCA --------------------------------------------------------

test_that("spax_3sfca and spax_huff2sfca have the documented signatures", {
  expect_identical(
    names(formals(spax_3sfca)),
    c("demand", "supply", "distance", "decay_params", "selection_params",
      "id_col", "supply_cols", "indicator_names", "snap")
  )
  expect_identical(
    names(formals(spax_huff2sfca)),
    c("demand", "supply", "distance", "decay_params", "selection_params",
      "attractiveness", "alpha", "id_col", "supply_cols", "indicator_names", "snap")
  )
  for (fun in list(spax_3sfca, spax_huff2sfca)) {
    expect_identical(formals(fun)$decay_params,
                     quote(list(method = "gaussian", sigma = 30)))
    expect_identical(formals(fun)$selection_params, quote(decay_params))
    expect_false(formals(fun)$snap)
  }
  expect_null(formals(spax_huff2sfca)$attractiveness)
  expect_identical(formals(spax_huff2sfca)$alpha, 1)
})

test_that("spax_3sfca matches the hand calculation in every supply format", {
  td <- fca_toy()
  m <- fca_toy_matrices(td)
  decay <- list(method = "gaussian", sigma = 2.4)
  W <- exp(-m$d^2 / (2 * 2.4^2))
  G <- selection_by_hand(W)
  expected <- selection_fca_by_hand(m$D, W, G, m$S)
  expect_equal(rowSums(G), rep(1, 9)) # the selection weights of a cell sum to 1

  run <- function(supply, ...) {
    spax_3sfca(td$demand, supply, td$distance, decay_params = decay, ...)
  }
  # Data frame rows in any order bind by ID.
  from_df <- run(td$supply_df[2:1, ], id_col = "id", supply_cols = c("doctors", "nurses"))
  expect_s3_class(from_df, "spax")
  expect_equal(from_df$type, "3SFCA")
  expect_equal(access_values(from_df), expected)
  expect_equal(names(from_df$accessibility), c("doctors", "nurses"))
  expect_true(terra::compareGeom(from_df$accessibility, td$demand))

  from_matrix <- run(td$supply_matrix)
  expect_equal(access_values(from_matrix), expected)
  expect_equal(names(from_matrix$accessibility), c("doctors", "nurses"))

  # Named vectors bind by name; unnamed ones follow the layer order.
  expect_equal(access_values(run(td$supply_vector)), expected[, 1, drop = FALSE])
  expect_equal(access_values(run(rev(td$supply_vector))), expected[, 1, drop = FALSE])
  expect_equal(access_values(run(unname(td$supply_vector))), expected[, 1, drop = FALSE])

  # Custom output names, and the defaults of the signature (Gaussian, sigma 30).
  named <- run(td$supply_matrix, indicator_names = c("doc_access", "nurse_access"))
  expect_equal(names(named$accessibility), c("doc_access", "nurse_access"))
  W30 <- exp(-m$d^2 / (2 * 30^2))
  expect_equal(
    access_values(spax_3sfca(td$demand, td$supply_matrix, td$distance)),
    selection_fca_by_hand(m$D, W30, selection_by_hand(W30), m$S)
  )

  # The result is neither E2SFCA nor E2SFCA with standardized demand weights.
  e2 <- function(norm) {
    access_values(spax_e2sfca(td$demand, td$supply_matrix, td$distance,
                              decay_params = decay, demand_normalize = norm))
  }
  expect_false(isTRUE(all.equal(access_values(from_matrix), e2("identity"))))
  expect_false(isTRUE(all.equal(access_values(from_matrix), e2("standard"))))
})

test_that("spax_huff2sfca matches the hand calculation in every supply format", {
  td <- fca_toy()
  m <- fca_toy_matrices(td)
  decay <- list(method = "gaussian", sigma = 2.4)
  W <- exp(-m$d^2 / (2 * 2.4^2))
  a <- c(10, 15) # the supply of the one-measure inputs below, in layer order

  # Attractiveness defaults to the supply measure; alpha defaults to 1.
  P <- selection_by_hand(W, a, 1)
  expected <- selection_fca_by_hand(m$D, W, P, m$S[, 1])
  expect_false(isTRUE(all.equal(P, selection_by_hand(W)))) # it matters here
  run <- function(supply, ...) {
    spax_huff2sfca(td$demand, supply, td$distance, decay_params = decay, ...)
  }
  from_vector <- run(td$supply_vector)
  expect_s3_class(from_vector, "spax")
  expect_equal(from_vector$type, "Huff-2SFCA")
  expect_equal(access_values(from_vector), expected)
  expect_equal(access_values(run(rev(td$supply_vector))), expected)
  from_df <- run(td$supply_df[2:1, ], id_col = "id", supply_cols = "doctors")
  expect_equal(access_values(from_df), expected)
  expect_equal(names(from_df$accessibility), "doctors")
  from_matrix <- run(td$supply_matrix[, "doctors", drop = FALSE])
  expect_equal(access_values(from_matrix), expected)
  expect_equal(names(from_matrix$accessibility), "doctors")

  # alpha scales the attractiveness exponent.
  for (alpha in c(0.5, 1.5, 3)) {
    P_alpha <- selection_by_hand(W, a, alpha)
    expect_equal(access_values(run(td$supply_vector, alpha = alpha)),
                 selection_fca_by_hand(m$D, W, P_alpha, m$S[, 1]),
                 info = paste("alpha =", alpha))
  }

  # Several supply measures share the attractiveness that is given.
  several <- run(td$supply_df, id_col = "id", supply_cols = c("doctors", "nurses"),
                 attractiveness = c(5, 20), alpha = 2)
  expect_equal(access_values(several),
               selection_fca_by_hand(m$D, W, selection_by_hand(W, c(5, 20), 2), m$S))
  expect_equal(names(several$accessibility), c("doctors", "nurses"))
  named <- run(td$supply_matrix, attractiveness = c(5, 20), alpha = 2,
               indicator_names = c("doc_access", "nurse_access"))
  expect_equal(access_values(named), access_values(several))
  expect_equal(names(named$accessibility), c("doc_access", "nurse_access"))

  # The defaults of the signature: Gaussian, sigma 30, alpha 1.
  W30 <- exp(-m$d^2 / (2 * 30^2))
  expect_equal(
    access_values(spax_huff2sfca(td$demand, td$supply_vector, td$distance)),
    selection_fca_by_hand(m$D, W30, selection_by_hand(W30, a, 1), m$S[, 1])
  )
})

test_that("selection_params can differ from decay_params", {
  td <- fca_toy()
  m <- fca_toy_matrices(td)
  decay <- list(method = "gaussian", sigma = 2.4)
  W <- exp(-m$d^2 / (2 * 2.4^2))
  run <- function(fun, selection, ...) {
    fun(td$demand, td$supply_matrix, td$distance, decay_params = decay,
        selection_params = selection, ...)
  }
  same <- run(spax_3sfca, decay)
  expect_equal(access_values(same), access_values(spax_3sfca(
    td$demand, td$supply_matrix, td$distance, decay_params = decay)))

  # Select among the facilities within 3 minutes; the decay weights stay Gaussian.
  selection <- list(method = "binary", sigma = 3)
  Tw <- 1 * (m$d <= 3)
  r3 <- run(spax_3sfca, selection)
  expect_equal(access_values(r3),
               selection_fca_by_hand(m$D, W, selection_by_hand(Tw), m$S))
  expect_false(isTRUE(all.equal(access_values(r3), access_values(same))))
  expect_identical(r3$parameters$decay_params, decay)
  expect_identical(r3$parameters$selection_params, selection)

  # Huff-2SFCA takes the weights T from selection_params as well.
  a <- c(10, 15)
  rh <- run(spax_huff2sfca, selection, attractiveness = a, alpha = 1.5)
  expect_equal(access_values(rh),
               selection_fca_by_hand(m$D, W, selection_by_hand(Tw, a, 1.5), m$S))
  expect_identical(rh$parameters$selection_params, selection)

  # The cells that no facility selects (3 minutes from both) have no gathered
  # demand and an index of 0, not missing.
  tight <- list(method = "binary", sigma = 2)
  Tt <- 1 * (m$d <= 2)
  rt <- run(spax_3sfca, tight)
  expect_equal(access_values(rt),
               selection_fca_by_hand(m$D, W, selection_by_hand(Tt), m$S))
  expect_equal(access_values(rt)[c(3, 5, 7), ], matrix(0, 3, 2))
  expect_false(anyNA(access_values(rt)))

  # An identical list takes the same path as the default.
  expect_identical(access_values(spax_3sfca(
    td$demand, td$supply_matrix, td$distance, decay_params = decay,
    selection_params = list(method = "gaussian", sigma = 2.4))),
    access_values(same))
})

test_that("alpha = 0 makes Huff-2SFCA equal 3SFCA, and zero attractiveness is never selected", {
  td <- fca_toy()
  m <- fca_toy_matrices(td)
  decay <- list(method = "gaussian", sigma = 2.4)
  W <- exp(-m$d^2 / (2 * 2.4^2))
  s3 <- spax_3sfca(td$demand, td$supply_matrix, td$distance, decay_params = decay)
  huff <- function(a, alpha) {
    spax_huff2sfca(td$demand, td$supply_matrix, td$distance, decay_params = decay,
                   attractiveness = a, alpha = alpha)
  }
  # Any attractiveness has no effect at alpha = 0, zeros included (0^0 = 1).
  for (a in list(c(10, 15), c(0, 15), c(0, 0), c(1e6, 1))) {
    expect_equal(access_values(huff(a, 0)), access_values(s3), info = deparse(a))
  }
  expect_equal(access_values(spax_huff2sfca(
    td$demand, td$supply_vector, td$distance, decay_params = decay, alpha = 0)),
    access_values(s3)[, 1, drop = FALSE])
  expect_equal(huff(c(10, 15), 0)$parameters$alpha, 0)

  # With a positive alpha a facility of zero attractiveness is never selected, so
  # it gathers no demand and has a ratio of zero.
  P <- selection_by_hand(W, c(0, 15), 1)
  expect_equal(P[, 1], rep(0, 9))
  expect_equal(access_values(huff(c(0, 15), 1)),
               selection_fca_by_hand(m$D, W, P, m$S))
  # Equal attractiveness gives the same shares as 3SFCA at any alpha.
  expect_equal(access_values(huff(c(7, 7), 2.5)), access_values(s3))
})

test_that("both variants return the supply of the facilities that gather demand", {
  td <- fca_toy()
  m <- fca_toy_matrices(td)
  sigma <- 2.4
  decay <- list(method = "gaussian", sigma = sigma)
  W <- exp(-m$d^2 / (2 * sigma^2))
  returned <- function(result, D = m$D) {
    colSums(D * access_values(result), na.rm = TRUE)
  }
  # The demand each facility gathers, and the supply of those that gather some.
  expected <- function(K, D = m$D, S = m$S) {
    colSums(S[colSums(D * K, na.rm = TRUE) > 0, , drop = FALSE])
  }
  run3 <- function(..., distance = td$distance, demand = td$demand) {
    spax_3sfca(demand, td$supply_matrix, distance, ...)
  }
  runh <- function(..., distance = td$distance, demand = td$demand) {
    spax_huff2sfca(demand, td$supply_matrix, distance, ...)
  }

  # Every facility gathers demand: the total supply returns.
  K <- W * selection_by_hand(W)
  r3 <- run3(decay_params = decay)
  expect_equal(returned(r3), expected(K))
  expect_equal(returned(r3), colSums(m$S))
  Kh <- W * selection_by_hand(W, c(5, 20), 1.5)
  rh <- runh(decay_params = decay, attractiveness = c(5, 20), alpha = 1.5)
  expect_equal(returned(rh), colSums(m$S))
  expect_equal(returned(rh), expected(Kh))

  # Another selection, and a facility without attractiveness that gathers nothing.
  K2 <- W * selection_by_hand(1 * (m$d <= 3))
  r2 <- run3(decay_params = decay, selection_params = list(method = "binary", sigma = 3))
  expect_equal(returned(r2), expected(K2))
  K0 <- W * selection_by_hand(W, c(0, 15), 1)
  r0 <- runh(decay_params = decay, attractiveness = c(0, 15))
  expect_equal(colSums(m$D * K0)[1], 0)
  expect_equal(returned(r0), expected(K0))
  expect_equal(returned(r0), m$S[2, ])
  expect_false(isTRUE(all.equal(returned(r0), colSums(m$S))))

  # No facility selected anywhere: nothing is gathered and every index is 0.
  none <- run3(decay_params = decay, selection_params = list(method = "binary", sigma = 0.5))
  expect_equal(access_values(none), matrix(0, 9, 2))
  expect_equal(returned(none), c(0, 0))

  # Cells without a travel time and without demand drop out of both sides.
  d <- unname(terra::values(td$distance))
  d[1, 1] <- NA # cell 1 does not reach facility1
  d[9, ] <- NA # cell 9 reaches no facility
  D <- m$D
  D[5] <- NA
  dist_na <- terra::setValues(td$distance, d)
  demand_na <- terra::setValues(td$demand, D)
  Wn <- exp(-d^2 / (2 * sigma^2))
  Kn <- Wn * selection_by_hand(Wn)
  rn <- run3(decay_params = decay, distance = dist_na, demand = demand_na)
  expect_true(all(is.na(access_values(rn)[9, ])))
  expect_equal(access_values(rn), fca_oracle(D, Kn, Kn, m$S))
  expect_equal(returned(rn, D), expected(Kn, D))
  expect_equal(returned(rn, D), colSums(m$S))
})

test_that("attractiveness is aligned to the distance layers by name or by order", {
  td <- fca_toy()
  m <- fca_toy_matrices(td)
  decay <- list(method = "gaussian", sigma = 2.4)
  W <- exp(-m$d^2 / (2 * 2.4^2))
  expected <- selection_fca_by_hand(m$D, W, selection_by_hand(W, c(10, 15), 2), m$S)
  run <- function(a) {
    spax_huff2sfca(td$demand, td$supply_matrix, td$distance, decay_params = decay,
                   attractiveness = a, alpha = 2)
  }

  # Named in layer order, named in another order, and unnamed in layer order.
  layer_order <- c(facility1 = 10, facility2 = 15)
  for (a in list(layer_order, c(facility2 = 15, facility1 = 10), c(10, 15))) {
    result <- run(a)
    expect_equal(access_values(result), expected, info = deparse(a))
    expect_identical(result$parameters$attractiveness, layer_order)
    expect_identical(result$parameters$alpha, 2)
  }
  # Integers are stored as numbers; unnamed values follow the layer order only.
  expect_identical(run(c(10L, 15L))$parameters$attractiveness, layer_order)
  expect_false(isTRUE(all.equal(access_values(run(c(15, 10))), expected)))

  # Without attractiveness the supply measure is used, named by facility ID
  # whatever the supply format and row order.
  one <- function(supply, ...) {
    spax_huff2sfca(td$demand, supply, td$distance, decay_params = decay, ...)
  }
  expect_identical(one(td$supply_vector)$parameters$attractiveness, layer_order)
  expect_identical(one(rev(td$supply_vector))$parameters$attractiveness, layer_order)
  expect_identical(
    one(td$supply_df[2:1, ], id_col = "id", supply_cols = "doctors")$parameters$attractiveness,
    layer_order
  )
  expect_identical(
    one(td$supply_matrix[, "doctors", drop = FALSE])$parameters$attractiveness,
    layer_order
  )
  expect_identical(
    one(td$supply_df, id_col = "id", supply_cols = "nurses")$parameters$attractiveness,
    c(facility1 = 20, facility2 = 25)
  )
})

test_that("several supply measures need an attractiveness", {
  td <- fca_toy()
  expect_error(
    spax_huff2sfca(td$demand, td$supply_df, td$distance, id_col = "id",
                   supply_cols = c("doctors", "nurses")),
    "give attractiveness when supply has several measures"
  )
  expect_error(
    spax_huff2sfca(td$demand, td$supply_matrix, td$distance),
    "give attractiveness when supply has several measures"
  )
  # alpha = 0 does not remove the need: the error is about the missing input.
  expect_error(
    spax_huff2sfca(td$demand, td$supply_matrix, td$distance, alpha = 0),
    "give attractiveness when supply has several measures"
  )
  # The same inputs work once an attractiveness is given, one layer per measure.
  result <- spax_huff2sfca(td$demand, td$supply_matrix, td$distance,
                           attractiveness = c(1, 2))
  expect_equal(names(result$accessibility), c("doctors", "nurses"))
})

test_that("a declared kernel is a decay or selection method of both variants", {
  td <- fca_toy()
  m <- fca_toy_matrices(td)
  plateau <- decay_kernel(function(distance, sigma, plateau = 2) {
    ifelse(distance <= plateau, 1, exp(-(distance - plateau)^2 / (2 * sigma^2)))
  }, name = "plateau", parameter = "scale", args = list(plateau = 2))
  decay <- list(method = plateau, sigma = 2)
  W <- ifelse(m$d <= 2, 1, exp(-(m$d - 2)^2 / (2 * 2^2)))

  r3 <- spax_3sfca(td$demand, td$supply_matrix, td$distance, decay_params = decay)
  expect_equal(access_values(r3),
               selection_fca_by_hand(m$D, W, selection_by_hand(W), m$S))
  expect_identical(r3$parameters$decay_params, decay)
  expect_identical(r3$parameters$selection_params, decay)
  rh <- spax_huff2sfca(td$demand, td$supply_vector, td$distance, decay_params = decay,
                       alpha = 2)
  expect_equal(access_values(rh),
               selection_fca_by_hand(m$D, W, selection_by_hand(W, c(10, 15), 2), m$S[, 1]))

  # As the selection method only, beside Gaussian decay weights.
  Wg <- exp(-m$d^2 / (2 * 2.4^2))
  r_sel <- spax_3sfca(td$demand, td$supply_matrix, td$distance,
                      decay_params = list(method = "gaussian", sigma = 2.4),
                      selection_params = decay)
  expect_equal(access_values(r_sel),
               selection_fca_by_hand(m$D, Wg, selection_by_hand(W), m$S))
  expect_identical(r_sel$parameters$selection_params, decay)

  # A built-in declaration gives exactly the result of its method string.
  for (fun in list(spax_3sfca, spax_huff2sfca)) {
    declared <- fun(td$demand, td$supply_vector, td$distance,
                    decay_params = list(method = decay_kernel("gaussian"), sigma = 2))
    string <- fun(td$demand, td$supply_vector, td$distance,
                  decay_params = list(method = "gaussian", sigma = 2))
    expect_identical(access_values(declared), access_values(string))
  }
})

test_that("a missing travel time is an unreachable facility", {
  td <- fca_toy()
  m <- fca_toy_matrices(td)
  d <- m$d
  d[1, 1] <- NA # cell 1 does not reach facility1
  d[9, ] <- NA # cell 9 reaches no facility
  dist_na <- terra::setValues(td$distance, d)
  W <- exp(-d^2 / (2 * 2.4^2))
  decay <- list(method = "gaussian", sigma = 2.4)

  r3 <- spax_3sfca(td$demand, td$supply_matrix, dist_na, decay_params = decay)
  expect_equal(access_values(r3),
               selection_fca_by_hand(m$D, W, selection_by_hand(W), m$S))
  expect_true(all(is.na(access_values(r3)[9, ])))
  expect_false(anyNA(access_values(r3)[-9, ]))

  a <- c(10, 15)
  rh <- spax_huff2sfca(td$demand, td$supply_vector, dist_na, decay_params = decay,
                       alpha = 1.5)
  expect_equal(access_values(rh),
               selection_fca_by_hand(m$D, W, selection_by_hand(W, a, 1.5), m$S[, 1]))
  expect_true(is.na(access_values(rh)[9, 1]))
})

test_that("snap skips validation without changing the result", {
  td <- fca_toy()
  decay <- list(method = "gaussian", sigma = 2.4)
  tight <- list(method = "binary", sigma = 3)
  for (supply in list(td$supply_vector, td$supply_matrix[, "doctors", drop = FALSE])) {
    expect_equal(
      access_values(spax_3sfca(td$demand, supply, td$distance, decay_params = decay,
                               selection_params = tight, snap = TRUE)),
      access_values(spax_3sfca(td$demand, supply, td$distance, decay_params = decay,
                               selection_params = tight))
    )
    expect_equal(
      access_values(spax_huff2sfca(td$demand, supply, td$distance, decay_params = decay,
                                   selection_params = tight, alpha = 1.5, snap = TRUE)),
      access_values(spax_huff2sfca(td$demand, supply, td$distance, decay_params = decay,
                                   selection_params = tight, alpha = 1.5))
    )
  }
  # Named attractiveness is still matched by name, and a data frame by ID.
  by_name <- spax_huff2sfca(td$demand, td$supply_df, td$distance, decay_params = decay,
                            attractiveness = c(facility2 = 15, facility1 = 10),
                            id_col = "id", supply_cols = c("doctors", "nurses"),
                            snap = TRUE)
  expect_equal(
    access_values(by_name),
    access_values(spax_huff2sfca(td$demand, td$supply_matrix, td$distance,
                                 decay_params = decay, attractiveness = c(10, 15)))
  )
  expect_identical(by_name$parameters$attractiveness, c(facility1 = 10, facility2 = 15))
  # A mismatch of names is an alignment error, not skipped validation.
  expect_error(
    spax_huff2sfca(td$demand, td$supply_vector, td$distance, decay_params = decay,
                   attractiveness = c(a = 1, b = 2), snap = TRUE),
    "names of attractiveness must be the distance layer names"
  )
})

test_that("spax_3sfca and spax_huff2sfca reject invalid inputs", {
  td <- fca_toy()
  decay <- list(method = "gaussian", sigma = 2)
  for (fun in list(spax_3sfca, spax_huff2sfca)) {
    run <- function(supply = td$supply_vector, ..., distance = td$distance) {
      fun(td$demand, supply, distance, ...)
    }
    points <- sf::st_as_sf(td$supply_df, coords = c("doctors", "nurses"), remove = FALSE)
    duplicated_ids <- td$supply_df
    duplicated_ids$id <- "facility1"

    expect_error(run(points, id_col = "id", supply_cols = "doctors"), "is an sf object")
    expect_error(run(td$supply_df, supply_cols = "doctors"), "id_col must be specified")
    expect_error(run(td$supply_df, id_col = "id"), "supply_cols must be specified")
    expect_error(run(td$supply_df, id_col = "id", supply_cols = "beds"),
                 "not found in supply")
    expect_error(run(duplicated_ids, id_col = "id", supply_cols = "doctors"),
                 "must be unique")
    expect_error(run(decay_params = "gaussian"), "decay_params must be one of")
    expect_error(run(decay_params = list(sigma = 2)), "decay_params must include 'method'")
    expect_error(run(distance = toy_rast(1, 4, 4, nlyr = 2)), "same geometry")
    expect_error(run(c(facility1 = 10), decay_params = decay),
                 "must match length of facilities")
    expect_error(run(td$supply_df, id_col = "other", supply_cols = "doctors"),
                 "not found in supply")
    # There is no demand_normalize argument.
    expect_error(run(demand_normalize = "standard"), "unused argument")

    # selection_params has the form of decay_params.
    expect_error(run(selection_params = "gaussian"),
                 "selection_params must be one of the following classes: list")
    expect_error(run(selection_params = 3),
                 "selection_params must be one of the following classes: list")
    expect_error(run(selection_params = list(sigma = 2)),
                 "selection_params must include 'method'")
    expect_error(run(selection_params = list()), "selection_params must include 'method'")
  }
})

test_that("spax_huff2sfca rejects an invalid attractiveness or alpha", {
  td <- fca_toy()
  run <- function(...) {
    spax_huff2sfca(td$demand, td$supply_matrix, td$distance, ...)
  }
  expect_error(run(attractiveness = c("a", "b")), "attractiveness must be numeric")
  expect_error(run(attractiveness = list(1, 2)), "attractiveness must be numeric")
  expect_error(run(attractiveness = matrix(c(1, 2), ncol = 1)),
               "attractiveness must be a numeric vector")
  expect_error(run(attractiveness = 1),
               "Length of attractiveness (1) must match length of distance layers (2)",
               fixed = TRUE)
  expect_error(run(attractiveness = c(1, 2, 3)),
               "Length of attractiveness (3) must match length of distance layers (2)",
               fixed = TRUE)
  for (bad in list(c(-1, 5), c(NA, 5), c(1, NaN), c(Inf, 5), c(1, -Inf))) {
    expect_error(run(attractiveness = bad),
                 "attractiveness must contain only finite, nonnegative values",
                 info = deparse(bad))
  }
  expect_error(run(attractiveness = c(facility1 = 1, other = 2)),
               "names of attractiveness must be the distance layer names")
  expect_error(run(attractiveness = c(facility1 = 1, facility1 = 2)),
               "names of attractiveness must be the distance layer names")
  # Names that cover only some of the layers, or an empty name, do not align.
  expect_error(run(attractiveness = stats::setNames(c(1, 2), c("facility1", ""))),
               "names of attractiveness must be the distance layer names")

  # The supply measure that stands in for attractiveness must be valid as well.
  for (bad in list(c(facility1 = -1, facility2 = 15), c(facility1 = NA, facility2 = 15),
                   c(facility1 = Inf, facility2 = 15))) {
    expect_error(spax_huff2sfca(td$demand, bad, td$distance),
                 "the supply measure used as attractiveness must contain only finite, nonnegative values")
  }
  # A supply value of zero is a valid attractiveness.
  expect_no_error(spax_huff2sfca(td$demand, c(facility1 = 0, facility2 = 15), td$distance))
  expect_no_error(run(attractiveness = c(0, 0)))

  # alpha is a finite nonnegative scalar.
  for (bad in list(-1, NA_real_, Inf, -Inf, NaN)) {
    expect_error(run(attractiveness = c(1, 2), alpha = bad),
                 "alpha must be a nonnegative scalar", info = deparse(bad))
  }
  expect_error(run(attractiveness = c(1, 2), alpha = "a"), "alpha must be numeric")
  expect_error(run(attractiveness = c(1, 2), alpha = TRUE), "alpha must be numeric")
  expect_error(run(attractiveness = c(1, 2), alpha = c(1, 2)), "alpha must have length 1")
  expect_error(run(attractiveness = c(1, 2), alpha = numeric()), "alpha must have length 1")
})

test_that("3SFCA and Huff-2SFCA results keep named parameters and the facility table", {
  td <- fca_toy()
  decay <- list(method = "gaussian", sigma = 2)
  tight <- list(method = "binary", sigma = 3)
  r3 <- spax_3sfca(td$demand, td$supply_df, td$distance, decay_params = decay,
                   id_col = "id", supply_cols = c("doctors", "nurses"))
  expect_named(r3$parameters, c("decay_params", "selection_params"))
  expect_identical(r3$parameters$decay_params, decay)
  expect_identical(r3$parameters$selection_params, decay) # the default
  expect_null(r3$parameters$demand_normalize)
  expect_true(all(c("accessibility", "type", "parameters", "facilities", "call")
                  %in% names(r3)))
  expect_equal(r3$call[[1]], quote(spax_3sfca))
  expect_identical(
    spax_3sfca(td$demand, td$supply_df, td$distance, decay_params = decay,
               selection_params = tight, id_col = "id",
               supply_cols = "doctors")$parameters$selection_params,
    tight
  )

  rh <- spax_huff2sfca(td$demand, td$supply_df, td$distance, decay_params = decay,
                       selection_params = tight, alpha = 2, id_col = "id",
                       supply_cols = "doctors")
  expect_named(rh$parameters,
               c("decay_params", "selection_params", "attractiveness", "alpha"))
  expect_identical(rh$parameters$decay_params, decay)
  expect_identical(rh$parameters$selection_params, tight)
  expect_identical(rh$parameters$attractiveness, c(facility1 = 10, facility2 = 15))
  expect_identical(rh$parameters$alpha, 2)
  expect_equal(rh$call[[1]], quote(spax_huff2sfca))
  # The default alpha is stored too.
  expect_identical(
    spax_huff2sfca(td$demand, td$supply_vector, td$distance)$parameters$alpha, 1
  )

  # The facility table is the one of the other wrappers, for every supply format.
  reversed <- td$supply_df[2:1, ]
  wrappers <- list(
    function(...) spax_3sfca(td$demand, ..., td$distance),
    function(...) spax_huff2sfca(td$demand, ..., td$distance, attractiveness = c(1, 2)),
    function(...) spax_e2sfca(td$demand, ..., td$distance),
    function(...) spax_m2sfca(td$demand, ..., td$distance)
  )
  for (wrapper in wrappers) {
    from_df <- wrapper(reversed, id_col = "id", supply_cols = c("doctors", "nurses"))
    expect_equal(from_df$facilities,
                 data.frame(id = c("facility2", "facility1"),
                            doctors = c(15, 10), nurses = c(25, 20)),
                 ignore_attr = "row.names")
    expect_equal(wrapper(td$supply_matrix)$facilities,
                 data.frame(id = c("facility1", "facility2"),
                            doctors = c(10, 15), nurses = c(20, 25)),
                 ignore_attr = "row.names")
    unnamed <- unname(td$supply_matrix)
    expect_equal(wrapper(unnamed)$facilities,
                 data.frame(id = c("facility_1", "facility_2"),
                            V1 = c(10, 15), V2 = c(20, 25)),
                 ignore_attr = "row.names")
    expect_equal(as.list(wrapper(rev(td$supply_vector))$facilities),
                 list(id = c("facility2", "facility1"), supply = c(15, 10)))
    expect_equal(as.list(wrapper(unname(td$supply_vector))$facilities),
                 list(id = c("facility_1", "facility_2"), supply = c(10, 15)))
  }
})

test_that("3SFCA and Huff-2SFCA results print, summarise and plot", {
  td <- fca_toy()
  decay <- list(method = "gaussian", sigma = 2)
  r3 <- spax_3sfca(td$demand, td$supply_df, td$distance, decay_params = decay,
                   id_col = "id", supply_cols = c("doctors", "nurses"))
  rh <- spax_huff2sfca(td$demand, td$supply_df, td$distance, decay_params = decay,
                       attractiveness = c(10, 15), id_col = "id",
                       supply_cols = c("doctors", "nurses"))
  withr::local_pdf(NULL)
  for (result in list(r3, rh)) {
    expect_s3_class(result, "spax")
    label <- result$type
    expect_output(expect_invisible(print(result)),
                  paste0("^<spax access> ", label, ", 2 layers on 3 x 3 cells(.|\n)*",
                         "Decay function: gaussian \\(sigma = 2\\)"))
    expect_output(print(result), "Measures: 2(.|\n)*doctors(.|\n)*nurses")
    s <- summary(result)
    expect_s3_class(s, "summary.spax")
    expect_equal(s$type, label)
    expect_equal(s$facilities$count, 2)
    expect_equal(s$facilities$supply$doctors$total, 25)
    expect_output(print(s), paste0("^<spax access summary> ", label, "\n"))
    expect_no_error(plot(result))
    expect_no_error(plot(result, y = "nurses", main = "Nurses"))
  }
  expect_match(r3$type, "^3SFCA$")
  expect_match(rh$type, "^Huff-2SFCA$")
  # A declared kernel prints by its name.
  k <- decay_kernel(function(distance, sigma) exp(-distance / sigma),
                    name = "plain_exponential", parameter = "scale")
  declared <- spax_3sfca(td$demand, td$supply_vector, td$distance,
                         decay_params = list(method = k, sigma = 2))
  expect_output(print(declared), "Decay function: plain_exponential (sigma = 2)",
                fixed = TRUE)
})
