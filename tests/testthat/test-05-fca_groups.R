# Demand groups in the block engine and the internal driver (SPAX-056, T1):
# per-group kernels with suffixed raster keys, several groups competing for the
# same supply, and the overall layer (decision D1: a group with demand in a cell
# its travel cannot reach counts with access 0). The first tests call the driver
# directly with per-group kernels; later tests use the grouped wrappers.

# A Gaussian kernel (decay `sigma`) on the travel raster stored under `key`.
group_gauss <- function(key, sigma) {
  k <- function(b, rows) exp(-b[[key]][rows, , drop = FALSE]^2 / (2 * sigma^2))
  list(demand = k, access = k)
}

# Column positions of measure m (of M) in a grouped result with G groups.
group_cols <- function(m, G) (m - 1) * (G + 1) + seq_len(G + 1)

# Two groups on the 3 x 4 grid with three facilities and two supply measures.
# Group a travels as in fca_block_fixture() (demand missing in cell 7, zero in
# cell 5; cell 9 reaches no facility, cell 3 not facility f1). Group b has its
# own travel (a slower mode) that cannot reach cell 4, and demand that is zero
# in cells 3 and 12 (where the demand of a is also zero in 12, so the total
# demand is 0) and positive in cells 4, 5 and 9.
group_fixture <- function() {
  f <- fca_block_fixture(3)
  Da <- f$D
  Da[12] <- 0
  Db <- c(5, 15, 0, 25, 35, 45, 55, 65, 75, 85, 95, 0)
  dist_a <- f$dist
  dist_b <- f$dist * 1.5
  dist_b[4, ] <- NA
  rast_of <- function(d) {
    terra::setValues(f$distance, d)
  }
  c(f, list(
    Da = Da, Db = Db, dist_a = dist_a, dist_b = dist_b,
    demand_a = toy_rast(Da, nrows = 3, ncols = 4),
    demand_b = toy_rast(Db, nrows = 3, ncols = 4),
    travel_a = rast_of(dist_a), travel_b = rast_of(dist_b),
    sigma = c(a = 2.5, b = 4)
  ))
}

# Run the grouped driver with the Gaussian kernels of group_fixture().
run_groups <- function(f, ..., supply = f$S) {
  compute_fca(
    demand = list(a = f$demand_a, b = f$demand_b),
    supply = supply,
    rasters = list(distance.a = f$travel_a, distance.b = f$travel_b),
    kernels = list(a = group_gauss("distance.a", f$sigma[["a"]]),
                   b = group_gauss("distance.b", f$sigma[["b"]])),
    ...
  )
}

gauss_w <- function(dist, sigma) exp(-dist^2 / (2 * sigma^2))

# The weights of a decay specification on the whole travel raster.
decay_weights <- function(distance, params) {
  unname(terra::values(
    do.call(calc_decay, c(list(distance = distance), params, list(snap = TRUE))),
    mat = TRUE
  ))
}

expect_group_result <- function(result, expected, G, tolerance = 1e-12) {
  got <- unname(terra::values(result, mat = TRUE))
  M <- ncol(expected$all)
  for (m in seq_len(M)) {
    want <- cbind(do.call(cbind, lapply(expected$groups, function(a) a[, m])),
                  expected$all[, m])
    expect_identical(is.na(got[, group_cols(m, G)]), is.na(want),
                     info = paste("measure", m))
    expect_equal(got[, group_cols(m, G)], want, tolerance = tolerance,
                 info = paste("measure", m))
  }
}

test_that("the overall layer follows the rule of decision D1 (hand-computed)", {
  # One facility, five cells in a row, kernel values given directly.
  mk <- function(v) {
    r <- terra::rast(nrows = 1, ncols = 5)
    terra::values(r) <- v
    r
  }
  wa <- mk(c(1, 0.5, NA, 0.5, 0.5))   # a cannot reach the facility from cell 3
  wb <- mk(c(0.5, NA, 1, 0.5, 0.5))
  names(wa) <- "f"
  names(wb) <- "f"
  Da <- c(10, 20, 0, NA, 0)
  Db <- c(20, 30, 40, 10, 0)
  kernel <- function(key) {
    k <- function(b, rows) b[[key]][rows, , drop = FALSE]
    list(demand = k, access = k)
  }
  out <- compute_fca(
    demand = list(a = mk(Da), b = mk(Db)), supply = c(f = 12),
    rasters = list(w.a = wa, w.b = wb),
    kernels = list(a = kernel("w.a"), b = kernel("w.b"))
  )
  # U = (10 * 1 + 20 * 0.5) + (20 * 0.5 + 40 * 1 + 10 * 0.5) = 75 (the missing
  # weight of b in cell 2 counts as 0): R = 12 / 75 = 0.16.
  expect_identical(names(out), c("supply.a", "supply.b", "supply.all"))
  v <- unname(terra::values(out, mat = TRUE))
  R <- 0.16
  expect_equal(v[, 1], R * c(1, 0.5, NA, 0.5, 0.5))
  expect_equal(v[, 2], R * c(0.5, NA, 1, 0.5, 0.5))
  expect_equal(v[, 3], c(
    (10 * 0.16 + 20 * 0.08) / 30,  # both groups reach the cell
    (20 * 0.08 + 30 * 0) / 50,     # b cannot reach it: counts with access 0
    0.16,                          # a has no demand and no access: skipped, not 0 * NA
    NA,                            # the demand of a is missing
    NA                             # no demand at all
  ))
  # The layer table.
  expect_identical(attr(out, "layers"), data.frame(
    layer = c("supply.a", "supply.b", "supply.all"),
    measure = "supply", group = c("a", "b", "all")
  ))
})

test_that("two groups with different sigma and their own travel match the oracle", {
  f <- group_fixture()
  Wa <- gauss_w(f$dist_a, f$sigma[["a"]])
  Wb <- gauss_w(f$dist_b, f$sigma[["b"]])
  for (norm in c("identity", "standard", "semi")) {
    got <- run_groups(f, demand_normalize = norm)
    expect_s4_class(got, "SpatRaster")
    expect_identical(names(got), c("a.a", "a.b", "a.all", "b.a", "b.b", "b.all"))
    expected <- fca_oracle(list(f$Da, f$Db), list(Wa, Wb), list(Wa, Wb), f$S, norm)
    expect_group_result(got, expected, G = 2)
  }
})

test_that("unreachable, zero and missing demand follow the documented rules", {
  f <- group_fixture()
  Wa <- gauss_w(f$dist_a, f$sigma[["a"]])
  Wb <- gauss_w(f$dist_b, f$sigma[["b"]])
  out <- unname(terra::values(run_groups(f), mat = TRUE))
  # Measure "a" (columns 1-3: group a, group b, overall).
  A <- out[, 1:3]
  # Cell 4: b has demand but cannot reach it; the overall layer counts b as 0.
  expect_true(is.na(A[4, 2]))
  expect_equal(A[4, 3], 40 * A[4, 1] / (40 + 25))
  # Cell 9: nobody reaches it: the group layers are NA, the overall layer is 0
  # because both groups have demand there.
  expect_true(all(is.na(A[9, 1:2])))
  expect_equal(A[9, 3], 0)
  # Cell 5: a has zero demand, so the overall layer is the access of b.
  expect_equal(A[5, 3], A[5, 2])
  # Cell 3: b has zero demand, so the overall layer is the access of a.
  expect_equal(A[3, 3], A[3, 1])
  # Cell 7: the demand of a is missing: overall NA, group layers defined.
  expect_true(is.na(A[7, 3]))
  expect_false(anyNA(A[7, 1:2]))
  # Cell 12: no demand in either group: overall NA, group layers defined.
  expect_true(is.na(A[12, 3]))
  expect_false(anyNA(A[12, 1:2]))

  # A second group with missing demand in another cell.
  Db_na <- f$Db
  Db_na[10] <- NA
  f2 <- f
  f2$demand_b <- toy_rast(Db_na, nrows = 3, ncols = 4)
  got <- run_groups(f2)
  expected <- fca_oracle(list(f$Da, Db_na), list(Wa, Wb), list(Wa, Wb), f$S)
  expect_group_result(got, expected, G = 2)
  expect_true(all(is.na(unname(terra::values(got, mat = TRUE))[10, c(3, 6)])))
  expect_false(anyNA(unname(terra::values(got, mat = TRUE))[10, c(1, 2, 4, 5)]))

  # Under snap, negative or infinite demand is not active and not counted.
  Db_bad <- f$Db
  Db_bad[2] <- -4
  Db_bad[6] <- Inf
  f3 <- f
  f3$demand_b <- toy_rast(Db_bad, nrows = 3, ncols = 4)
  expect_error(run_groups(f3), "demand must be finite and nonnegative")
  Db_zero <- Db_bad
  Db_zero[c(2, 6)] <- 0
  f4 <- f
  f4$demand_b <- toy_rast(Db_zero, nrows = 3, ncols = 4)
  expect_equal(unname(terra::values(run_groups(f3, snap = TRUE))),
               unname(terra::values(run_groups(f4))))
})

test_that("groups conserve supply: E2SFCA (identity) and a 3SFCA kernel", {
  f <- group_fixture()
  D <- list(f$Da, f$Db)
  supply_of_gatherers <- function(P) colSums(f$S[P > 0, , drop = FALSE])
  conserved <- function(result, G) {
    v <- unname(terra::values(result, mat = TRUE))
    vapply(1:2, function(m) {
      sum(vapply(seq_len(G), function(g) {
        a <- v[, (m - 1) * (G + 1) + g]
        ok <- is.finite(D[[g]]) & D[[g]] > 0 & !is.na(a)
        sum(D[[g]][ok] * a[ok])
      }, numeric(1)))
    }, numeric(1))
  }

  # E2SFCA with identity normalization.
  Wa <- gauss_w(f$dist_a, f$sigma[["a"]])
  Wb <- gauss_w(f$dist_b, f$sigma[["b"]])
  P <- colSums(f$Da * Wa, na.rm = TRUE) + colSums(f$Db * Wb, na.rm = TRUE)
  expect_equal(conserved(run_groups(f), 2), unname(supply_of_gatherers(P)))

  # 3SFCA kernels from the builders, with the group suffix on every key.
  sa <- .fca_group_kernels("3sfca", f$travel_a,
                           list(method = "gaussian", sigma = 2.5),
                           list(method = "gaussian", sigma = 2.5), suffix = ".a")
  sb <- .fca_group_kernels("3sfca", f$travel_b,
                           list(method = "gaussian", sigma = 4),
                           list(method = "exponential", sigma = 0.3), suffix = ".b")
  expect_identical(names(sa$rasters), "distance.a")
  expect_identical(names(sb$rasters), "distance.b")
  got <- compute_fca(
    demand = list(a = f$demand_a, b = f$demand_b), supply = f$S,
    rasters = c(sa$rasters, sb$rasters),
    kernels = list(a = sa$kernels, b = sb$kernels)
  )
  Ta <- Wa * (Wa / rowSums(Wa, na.rm = TRUE))
  Tb <- Wb * (function(T) T / rowSums(T, na.rm = TRUE))(exp(-0.3 * f$dist_b))
  expected <- fca_oracle(list(f$Da, f$Db), list(Ta, Tb), list(Ta, Tb), f$S)
  expect_group_result(got, expected, G = 2)
  P <- colSums(f$Da * Ta, na.rm = TRUE) + colSums(f$Db * Tb, na.rm = TRUE)
  expect_equal(conserved(got, 2), unname(supply_of_gatherers(P)))
})

test_that("builders give every group its own keys, with raster sigma and plain functions", {
  f <- group_fixture()
  sig <- terra::setValues(terra::rast(f$demand_a), seq(1.5, 4, length.out = 12))
  plain <- function(distance, sigma = 3) terra::ifel(distance < sigma, 1, 0)
  sa <- .fca_decay_spec(f$travel_a, list(method = "gaussian", sigma = sig),
                        "decay", FALSE, suffix = ".a")
  sb <- .fca_decay_spec(f$travel_b, list(method = plain, sigma = 4),
                        "decay", FALSE, suffix = ".b")
  expect_identical(names(sa$rasters), c("distance.a", "sigma_decay.a"))
  expect_identical(names(sb$rasters), c("distance.b", "weights_decay.b"))
  # The same call without a suffix keeps the keys of the ungrouped engine.
  expect_identical(names(.fca_decay_spec(f$travel_a, list(method = "gaussian",
                                                          sigma = sig),
                                         "decay", FALSE)$rasters),
                   c("distance", "sigma_decay"))
  acc <- .fca_access_kernels(f$travel_a, f$travel_b, suffix = ".g")
  expect_identical(names(acc$rasters), c("demand_weights.g", "access_weights.g"))

  got <- compute_fca(
    demand = list(a = f$demand_a, b = f$demand_b), supply = f$S,
    rasters = c(sa$rasters, sb$rasters),
    kernels = list(a = list(demand = sa$weights, access = sa$weights),
                   b = list(demand = sb$weights, access = sb$weights))
  )
  Wa <- decay_weights(f$travel_a, list(method = "gaussian", sigma = sig))
  Wb <- decay_weights(f$travel_b, list(method = plain, sigma = 4))
  expect_group_result(
    got, fca_oracle(list(f$Da, f$Db), list(Wa, Wb), list(Wa, Wb), f$S), G = 2
  )
})

test_that("identical groups give the ungrouped result for the summed demand", {
  f <- fca_block_fixture(3)
  k <- group_gauss("distance", 2.5)
  D <- f$D
  Da <- 0.3 * D
  Db <- 0.7 * D
  one <- compute_fca(f$demand, f$S, list(distance = f$distance), k)
  ungrouped <- unname(terra::values(one, mat = TRUE))
  # The same kernel under the keys of each group (one travel object).
  two <- compute_fca(
    demand = list(a = toy_rast(Da, nrows = 3, ncols = 4),
                  b = toy_rast(Db, nrows = 3, ncols = 4)),
    supply = f$S,
    rasters = list(distance.a = f$distance, distance.b = f$distance),
    kernels = list(a = group_gauss("distance.a", 2.5),
                   b = group_gauss("distance.b", 2.5))
  )
  v <- unname(terra::values(two, mat = TRUE))
  for (m in 1:2) {
    cols <- group_cols(m, 2)
    # Every group layer is the ungrouped result.
    expect_equal(v[, cols[1]], ungrouped[, m], tolerance = 1e-12)
    expect_equal(v[, cols[2]], ungrouped[, m], tolerance = 1e-12)
    # So is the overall layer wherever it is defined and the cell is reachable;
    # it differs only by the documented rules: 0 (not NA) in a cell with demand
    # that nothing reaches (9), NA where the demand is missing (7) or the total
    # demand is 0 (5).
    ok <- setdiff(seq_along(D), c(5, 7, 9))
    expect_equal(v[ok, cols[3]], ungrouped[ok, m], tolerance = 1e-12)
    expect_equal(v[9, cols[3]], 0)
    expect_true(all(is.na(v[c(5, 7), cols[3]])))
    expect_true(is.na(ungrouped[9, m]) && !anyNA(ungrouped[c(5, 7), m]))
  }
  # A one-element group list equals the ungrouped result; its overall layer
  # follows the same rules.
  solo <- compute_fca(
    list(g = f$demand), f$S, list(distance.g = f$distance),
    list(g = group_gauss("distance.g", 2.5))
  )
  expect_identical(names(solo), c("a.g", "a.all", "b.g", "b.all"))
  sv <- unname(terra::values(solo, mat = TRUE))
  expect_equal(sv[, c(1, 3)], ungrouped, tolerance = 1e-12)
})

test_that("results do not depend on the block size", {
  f <- group_fixture()
  default <- unname(terra::values(run_groups(f)))
  for (block_values in list(1, 10, 25)) {
    withr::local_options(list(spax.fca_block_values = block_values))
    expect_equal(unname(terra::values(run_groups(f))), default, tolerance = 1e-12,
                 info = format(block_values))
    expect_identical(is.na(unname(terra::values(run_groups(f)))), is.na(default))
  }
  # Also with one travel object shared by both groups.
  shared <- function() {
    compute_fca(
      list(a = f$demand_a, b = f$demand_b), f$S,
      list(distance.a = f$travel_a, distance.b = f$travel_a),
      list(a = group_gauss("distance.a", 2.5), b = group_gauss("distance.b", 4))
    )
  }
  base <- unname(terra::values(shared()))
  withr::local_options(list(spax.fca_block_values = 1))
  expect_equal(unname(terra::values(shared())), base, tolerance = 1e-12)
})

test_that("groups that share one travel object read it once", {
  f <- group_fixture()
  path <- withr::local_tempfile(fileext = ".tif")
  terra::writeRaster(f$travel_a, path, datatype = "FLT8S")
  travel <- terra::rast(path)
  rasters <- list(distance.a = travel, distance.b = travel)
  blocks <- .raster_blocks(c(list(demand.a = f$demand_a, demand.b = f$demand_b),
                             rasters))
  expect_identical(blocks$same[["distance.b"]], "distance.a")

  withr::local_options(list(spax.fca_block_values = 1))
  run <- function(...) {
    compute_fca(
      list(a = f$demand_a, b = f$demand_b), f$S, rasters = list(...),
      kernels = list(a = group_gauss("distance.a", 2.5),
                     b = group_gauss("distance.b", 4))
    )
  }
  expect_no_warning(got <- run(distance.a = travel, distance.b = travel))
  # Equal values in separate objects (opened twice) give the same result.
  other <- terra::rast(path)
  expect_no_warning(copy <- run(distance.a = travel, distance.b = other))
  expect_equal(terra::values(got), terra::values(copy), tolerance = 1e-12)
  expect_false(.same_raster(travel, other))
})

test_that("a raster key may not name two different rasters", {
  f <- group_fixture()
  # The same object twice under one key is one raster.
  merged <- .fca_merge_rasters(list(distance = f$travel_a, w = f$travel_b,
                                    distance = f$travel_a))
  expect_identical(names(merged), c("distance", "w"))
  expect_error(
    .fca_merge_rasters(list(distance = f$travel_a, distance = f$travel_b)),
    "names two different rasters"
  )
  k <- group_gauss("distance", 2.5)
  expect_error(
    compute_fca(f$demand_a, f$S,
                list(distance = f$travel_a, distance = f$travel_b), k),
    "names two different rasters"
  )
  # A group key that collides with a kernel raster is caught the same way.
  expect_error(
    compute_fca(list(a = f$demand_a), f$S, list(demand.a = f$travel_b),
                list(a = k)),
    "names two different rasters"
  )
})

test_that("grouped layer names, the layer table and the checks of the driver", {
  f <- group_fixture()
  got <- run_groups(f, indicator_names = c("doc", "nurse"))
  expect_identical(names(got), c("doc.a", "doc.b", "doc.all",
                                 "nurse.a", "nurse.b", "nurse.all"))
  expect_identical(attr(got, "layers"), data.frame(
    layer = names(got),
    measure = rep(c("doc", "nurse"), each = 3),
    group = rep(c("a", "b", "all"), 2)
  ))
  expect_true(terra::compareGeom(got, f$travel_a, stopOnError = FALSE))
  expect_identical(.fca_layer_table(c("x", "y")),
                   data.frame(layer = c("x", "y"), measure = c("x", "y"),
                              group = NA_character_))
  # The group order is the order of the demand list, not of the kernels.
  swapped <- compute_fca(
    list(b = f$demand_b, a = f$demand_a), f$S,
    list(distance.b = f$travel_b, distance.a = f$travel_a),
    list(a = group_gauss("distance.a", 2.5), b = group_gauss("distance.b", 4))
  )
  expect_identical(names(swapped)[1:3], c("a.b", "a.a", "a.all"))
  expect_equal(unname(terra::values(swapped)[, c(2, 1, 3)]),
               unname(terra::values(got)[, 1:3]), tolerance = 1e-12)
  # Ungrouped results keep their names and have no layer table.
  one <- compute_fca(f$demand_a, f$S, list(distance = f$travel_a),
                     group_gauss("distance", 2.5))
  expect_identical(names(one), c("a", "b"))
  expect_null(attr(one, "layers"))

  k <- list(a = group_gauss("distance.a", 2.5))
  rasters <- list(distance.a = f$travel_a)
  expect_error(compute_fca(list(f$demand_a), f$S, rasters, k),
               "named list with unique, non-empty names")
  expect_error(compute_fca(list(a = f$demand_a, a = f$demand_b), f$S, rasters, k),
               "named list with unique, non-empty names")
  expect_error(compute_fca(list(a = f$demand_a, b = f$demand_b), f$S, rasters, k),
               "one element per group")
  expect_error(compute_fca(list(a = c(f$demand_a, f$demand_a)), f$S, rasters, k),
               "single-layer")
  expect_error(compute_fca(list(a = 1), f$S, rasters, k), "SpatRaster")
  wide <- toy_rast(1:20, nrows = 4, ncols = 5)
  expect_error(compute_fca(list(a = f$demand_a, b = wide), f$S, rasters,
                           c(k, list(b = k$a))),
               "same geometry")
})

# The public wrappers with demand groups (SPAX-056, T2) --------------------------

# Selection weights by hand: the weights `Tw` of each cell (times a^alpha) divided
# by their sum over the facilities; 0 where they sum to 0, NA where missing.
group_choice <- function(Tw, a = NULL, alpha = 1) {
  score <- if (is.null(a)) Tw else sweep(Tw, 2, a^alpha, "*")
  rs <- rowSums(score, na.rm = TRUE)
  score * ifelse(rs > 0, 1 / rs, 0)
}

wrap_demand <- function(f) list(a = f$demand_a, b = f$demand_b)
wrap_travel <- function(f) list(a = f$travel_a, b = f$travel_b)
wrap_names <- c("m1", "m2")
wrap_layers <- c("m1.a", "m1.b", "m1.all", "m2.a", "m2.b", "m2.all")
wrap_decay <- list(a = list(method = "gaussian", sigma = 2.5),
                   b = list(method = "gaussian", sigma = 4))

# A copy of group b's travel raster with other facility names, order or grid.
renamed_stack <- function(x, names) {
  names(x) <- names
  x
}

test_that("every wrapper with demand groups matches the grouped oracle", {
  f <- group_fixture()
  D <- list(f$Da, f$Db)
  Wg <- list(gauss_w(f$dist_a, 2.5), gauss_w(f$dist_b, 4))
  layers <- data.frame(layer = wrap_layers, measure = rep(wrap_names, each = 3),
                       group = rep(c("a", "b", "all"), 2))
  check_result <- function(result, expected, type) {
    expect_s3_class(result, "spax")
    expect_identical(result$type, type)
    expect_group_result(result$accessibility, expected, G = 2)
    expect_identical(names(result$accessibility), wrap_layers)
    expect_identical(result$parameters$groups, c("a", "b"))
    expect_identical(result$parameters$layers, layers)
    expect_null(attr(result$accessibility, "layers"))
    expect_identical(result$facilities$id, f$ids)
  }

  for (norm in c("identity", "standard", "semi")) {
    e2 <- spax_e2sfca(wrap_demand(f), f$S, wrap_travel(f), wrap_decay,
                      demand_normalize = norm, indicator_names = wrap_names)
    check_result(e2, fca_oracle(D, Wg, Wg, f$S, norm), "E2SFCA")
    expect_identical(e2$parameters$decay_params, wrap_decay)
    expect_identical(e2$parameters$demand_normalize, norm)
  }

  m2 <- spax_m2sfca(wrap_demand(f), f$S, wrap_travel(f), wrap_decay,
                    demand_normalize = "standard", indicator_names = wrap_names)
  check_result(m2, fca_oracle(D, Wg, lapply(Wg, function(w) w^2), f$S, "standard"),
               "M2SFCA")

  # 2SFCA: one threshold per group, in any order, or one shared number.
  Bg <- list(1 * (f$dist_a <= 3), 1 * (f$dist_b <= 5))
  two <- spax_2sfca(wrap_demand(f), f$S, wrap_travel(f), threshold = c(b = 5, a = 3))
  expect_group_result(two$accessibility, fca_oracle(D, Bg, Bg, f$S), G = 2)
  expect_identical(two$type, "2SFCA")
  expect_identical(two$parameters$groups, c("a", "b"))
  expect_identical(two$parameters$decay_params,
                   list(a = list(method = "binary", sigma = 3),
                        b = list(method = "binary", sigma = 5)))
  Bs <- list(1 * (f$dist_a <= 4), 1 * (f$dist_b <= 4))
  shared <- spax_2sfca(wrap_demand(f), f$S, wrap_travel(f), threshold = 4)
  expect_group_result(shared$accessibility, fca_oracle(D, Bs, Bs, f$S), G = 2)
  expect_identical(shared$parameters$decay_params, list(method = "binary", sigma = 4))
  # Under snap too.
  snapped <- spax_2sfca(wrap_demand(f), f$S, wrap_travel(f),
                        threshold = c(a = 3, b = 5), snap = TRUE)
  expect_group_result(snapped$accessibility, fca_oracle(D, Bg, Bg, f$S), G = 2)

  # 3SFCA: the selection weights per group, and the shortcut of a selection
  # identical to the decay applies per group.
  selection <- list(a = wrap_decay$a, b = list(method = "exponential", sigma = 0.3))
  Ta <- Wg[[1]]
  Tb <- exp(-0.3 * f$dist_b)
  K3 <- list(Wg[[1]] * group_choice(Ta), Wg[[2]] * group_choice(Tb))
  r3 <- spax_3sfca(wrap_demand(f), f$S, wrap_travel(f), wrap_decay, selection,
                   indicator_names = wrap_names)
  check_result(r3, fca_oracle(D, K3, K3, f$S), "3SFCA")
  expect_identical(r3$parameters$selection_params, selection)
  # The default selection is the decay of each group.
  Kd <- lapply(Wg, function(w) w * group_choice(w))
  r3d <- spax_3sfca(wrap_demand(f), f$S, wrap_travel(f), wrap_decay,
                    indicator_names = wrap_names)
  check_result(r3d, fca_oracle(D, Kd, Kd, f$S), "3SFCA")
  expect_identical(r3d$parameters$selection_params, wrap_decay)

  # Huff-2SFCA: attractiveness and alpha shared, selection per group.
  attract <- c(f1 = 1, f2 = 2, f3 = 4)
  Kh <- list(Wg[[1]] * group_choice(Ta, attract, 1.5),
             Wg[[2]] * group_choice(Tb, attract, 1.5))
  rh <- spax_huff2sfca(wrap_demand(f), f$S, wrap_travel(f), wrap_decay, selection,
                       attractiveness = attract, alpha = 1.5,
                       indicator_names = wrap_names)
  check_result(rh, fca_oracle(D, Kh, Kh, f$S), "Huff-2SFCA")
  expect_identical(rh$parameters$attractiveness, attract)
  expect_identical(rh$parameters$alpha, 1.5)
  # Attractiveness named by the facilities of the first group's stack.
  rh_named <- spax_huff2sfca(wrap_demand(f), f$S, wrap_travel(f), wrap_decay,
                             selection, attractiveness = rev(attract), alpha = 1.5,
                             indicator_names = wrap_names)
  expect_equal(rh_named$parameters$attractiveness, attract)
})

test_that("compute_access takes weights per group", {
  f <- group_fixture()
  D <- list(f$Da, f$Db)
  Wd <- list(gauss_w(f$dist_a, 2.5), gauss_w(f$dist_b, 4))
  Wa <- list(1 * (f$dist_a <= 3), Wd[[2]]^2)
  dw <- lapply(Wd, function(w) terra::setValues(f$distance, w))
  aw <- lapply(Wa, function(w) terra::setValues(f$distance, w))
  names(dw) <- names(aw) <- c("a", "b")
  got <- compute_access(wrap_demand(f), f$S, dw, aw, indicator_names = wrap_names)
  expect_s4_class(got, "SpatRaster")
  expect_identical(names(got), wrap_layers)
  expect_null(attr(got, "layers"))
  expect_group_result(got, fca_oracle(D, Wd, Wa, f$S), G = 2)

  # Group order of the list does not matter; shared weights serve every group.
  rev_got <- compute_access(wrap_demand(f), f$S, dw[c("b", "a")], aw[c("b", "a")],
                            indicator_names = wrap_names)
  expect_equal(unname(terra::values(rev_got)), unname(terra::values(got)))
  mixed <- compute_access(wrap_demand(f), f$S, dw$a, aw, indicator_names = wrap_names)
  expect_group_result(mixed, fca_oracle(D, list(Wd[[1]], Wd[[1]]), Wa, f$S), G = 2)
  # The access weights follow the facility IDs of the demand weights (the stacks
  # of the groups still agree with each other).
  perm <- lapply(aw, function(x) x[[c("f3", "f1", "f2")]])
  expect_equal(unname(terra::values(
    compute_access(wrap_demand(f), f$S, dw, perm, indicator_names = wrap_names)
  )), unname(terra::values(got)))
  # Under snap the same.
  expect_equal(unname(terra::values(
    compute_access(wrap_demand(f), f$S, dw, aw, indicator_names = wrap_names,
                   snap = TRUE)
  )), unname(terra::values(got)))
})

test_that("a one-element group list equals the ungrouped result", {
  f <- group_fixture()
  decay <- list(method = "gaussian", sigma = 2.5)
  attract <- c(f1 = 1, f2 = 2, f3 = 4)
  run <- function(fun, demand, ...) {
    fun(demand, f$S, f$travel_a, ..., indicator_names = wrap_names)
  }
  values <- function(r) unname(terra::values(r$accessibility, mat = TRUE))
  cases <- list(
    e2sfca = function(d) run(spax_e2sfca, d, decay, "standard"),
    m2sfca = function(d) run(spax_m2sfca, d, decay),
    `3sfca` = function(d) run(spax_3sfca, d, decay),
    huff = function(d) run(spax_huff2sfca, d, decay, attractiveness = attract)
  )
  for (nm in names(cases)) {
    one <- cases[[nm]](list(g = f$demand_a))
    ungrouped <- cases[[nm]](f$demand_a)
    expect_identical(names(one$accessibility), c("m1.g", "m1.all", "m2.g", "m2.all"),
                     info = nm)
    expect_identical(one$parameters$groups, "g")
    expect_equal(values(one)[, c(1, 3)], values(ungrouped), tolerance = 1e-12,
                 info = nm)
    expect_null(ungrouped$parameters$groups)
    expect_null(ungrouped$parameters$layers)
    expect_identical(names(ungrouped$accessibility), wrap_names)
  }
  two <- spax_2sfca(list(g = f$demand_a), f$S, f$travel_a, 3)
  base <- spax_2sfca(f$demand_a, f$S, f$travel_a, 3)
  expect_equal(values(two)[, c(1, 3)], values(base), tolerance = 1e-12)
  W <- terra::setValues(f$distance, gauss_w(f$dist_a, 2.5))
  ca <- compute_access(list(g = f$demand_a), f$S, W, W, indicator_names = wrap_names)
  expect_equal(unname(terra::values(ca))[, c(1, 3)],
               unname(terra::values(compute_access(f$demand_a, f$S, W, W,
                                                   indicator_names = wrap_names))),
               tolerance = 1e-12)
})

test_that("each group may use a declared kernel, a plain function or a raster sigma", {
  f <- group_fixture()
  D <- list(f$Da, f$Db)
  pexp <- decay_kernel(function(distance, sigma) exp(-distance / sigma),
                       name = "plain_exponential", parameter = "scale")
  plain <- function(distance, sigma = 3) terra::ifel(distance < sigma, 1, 0)
  sig <- terra::setValues(terra::rast(f$demand_a), seq(1.5, 4, length.out = 12))

  # A declared kernel for a, a plain function for b.
  d1 <- list(a = list(method = pexp, sigma = 2), b = list(method = plain, sigma = 4))
  W1 <- list(exp(-f$dist_a / 2), decay_weights(f$travel_b, d1$b))
  got <- spax_e2sfca(wrap_demand(f), f$S, wrap_travel(f), d1,
                     indicator_names = wrap_names)
  expect_group_result(got$accessibility, fca_oracle(D, W1, W1, f$S), G = 2)

  # A raster sigma for a, a built-in for b; also in M2SFCA and 3SFCA.
  d2 <- list(a = list(method = "gaussian", sigma = sig), b = wrap_decay$b)
  W2 <- list(decay_weights(f$travel_a, d2$a), gauss_w(f$dist_b, 4))
  got <- spax_e2sfca(wrap_demand(f), f$S, wrap_travel(f), d2,
                     indicator_names = wrap_names)
  expect_group_result(got$accessibility, fca_oracle(D, W2, W2, f$S), G = 2)
  got <- spax_m2sfca(wrap_demand(f), f$S, wrap_travel(f), d2,
                     indicator_names = wrap_names)
  expect_group_result(got$accessibility,
                      fca_oracle(D, W2, lapply(W2, function(w) w^2), f$S), G = 2)
  K <- lapply(W2, function(w) w * group_choice(w))
  got <- spax_3sfca(wrap_demand(f), f$S, wrap_travel(f), d2,
                    indicator_names = wrap_names)
  expect_group_result(got$accessibility, fca_oracle(D, K, K, f$S), G = 2)
  # Under snap too.
  got <- spax_e2sfca(wrap_demand(f), f$S, wrap_travel(f), d1,
                     indicator_names = wrap_names, snap = TRUE)
  expect_group_result(got$accessibility, fca_oracle(D, W1, W1, f$S), G = 2)
})

test_that("shared and per-group arguments mix, each independently", {
  f <- group_fixture()
  D <- list(f$Da, f$Db)
  # Shared travel (one object) with per-group decay.
  W <- lapply(wrap_decay, function(p) gauss_w(f$dist_a, p$sigma))
  got <- spax_e2sfca(wrap_demand(f), f$S, f$travel_a, wrap_decay,
                     indicator_names = wrap_names)
  expect_group_result(got$accessibility, fca_oracle(D, W, W, f$S), G = 2)
  plan <- .fca_group_plan(c("a", "b"), f$travel_a, wrap_decay)
  expect_true(.same_raster(plan$distance$a, plan$distance$b))
  # Per-group travel with a shared decay.
  W <- list(gauss_w(f$dist_a, 3), gauss_w(f$dist_b, 3))
  got <- spax_e2sfca(wrap_demand(f), f$S, wrap_travel(f),
                     list(method = "gaussian", sigma = 3),
                     indicator_names = wrap_names)
  expect_group_result(got$accessibility, fca_oracle(D, W, W, f$S), G = 2)
  expect_identical(got$parameters$decay_params, list(method = "gaussian", sigma = 3))
  # Shared selection with per-group decay and travel (Huff), the shortcut off.
  attract <- c(f1 = 1, f2 = 2, f3 = 4)
  Wg <- list(gauss_w(f$dist_a, 2.5), gauss_w(f$dist_b, 4))
  K <- list(Wg[[1]] * group_choice(exp(-0.3 * f$dist_a), attract),
            Wg[[2]] * group_choice(exp(-0.3 * f$dist_b), attract))
  got <- spax_huff2sfca(wrap_demand(f), f$S, wrap_travel(f), wrap_decay,
                        list(method = "exponential", sigma = 0.3),
                        attractiveness = attract, indicator_names = wrap_names)
  expect_group_result(got$accessibility, fca_oracle(D, K, K, f$S), G = 2)
  # The shared travel object is read once: a file-backed raster, one-row blocks.
  path <- withr::local_tempfile(fileext = ".tif")
  terra::writeRaster(f$travel_a, path, datatype = "FLT8S")
  travel <- terra::rast(path)
  withr::local_options(list(spax.fca_block_values = 1))
  expect_no_warning(
    file_backed <- spax_e2sfca(wrap_demand(f), f$S, travel, wrap_decay,
                               indicator_names = wrap_names)
  )
  expect_equal(unname(terra::values(file_backed$accessibility)),
               unname(terra::values(
                 spax_e2sfca(wrap_demand(f), f$S, f$travel_a, wrap_decay,
                             indicator_names = wrap_names)$accessibility
               )), tolerance = 1e-12)
})

test_that("results do not depend on the block size, with per-group stacks", {
  f <- group_fixture()
  attract <- c(f1 = 1, f2 = 2, f3 = 4)
  dw <- lapply(wrap_travel(f), function(x) terra::setValues(x, exp(-terra::values(x)^2 / 8)))
  runs <- list(
    e2 = function() spax_e2sfca(wrap_demand(f), f$S, wrap_travel(f), wrap_decay,
                                demand_normalize = "standard")$accessibility,
    two = function() spax_2sfca(wrap_demand(f), f$S, wrap_travel(f),
                                c(a = 3, b = 5))$accessibility,
    huff = function() spax_huff2sfca(wrap_demand(f), f$S, wrap_travel(f), wrap_decay,
                                     attractiveness = attract)$accessibility,
    access = function() compute_access(wrap_demand(f), f$S, dw, dw)
  )
  for (nm in names(runs)) {
    default <- unname(terra::values(runs[[nm]]()))
    for (block_values in list(1, 10, 25)) {
      withr::local_options(list(spax.fca_block_values = block_values))
      got <- unname(terra::values(runs[[nm]]()))
      expect_equal(got, default, tolerance = 1e-12,
                   info = paste(nm, format(block_values)))
      expect_identical(is.na(got), is.na(default), info = paste(nm, block_values))
    }
  }
})

test_that("a multi-layer demand raster is refused, with and without snap", {
  f <- group_fixture()
  multi <- c(f$demand_a, f$demand_b)
  msg <- "give demand groups as a named list of single-layer rasters"
  W <- terra::setValues(f$distance, gauss_w(f$dist_a, 2.5))
  for (snap in c(FALSE, TRUE)) {
    expect_error(spax_e2sfca(multi, f$S, f$travel_a, snap = snap), msg)
    expect_error(spax_m2sfca(multi, f$S, f$travel_a, snap = snap), msg)
    expect_error(spax_2sfca(multi, f$S, f$travel_a, 3, snap = snap), msg)
    expect_error(spax_3sfca(multi, f$S, f$travel_a, snap = snap), msg)
    expect_error(spax_huff2sfca(multi, f$S, f$travel_a, attractiveness = 1:3,
                                snap = snap), msg)
    expect_error(compute_access(multi, f$S, W, W, snap = snap), msg)
    # Nor as an element of a group list.
    expect_error(spax_e2sfca(list(a = multi), f$S, f$travel_a, snap = snap), msg)
  }
})

test_that("bad group names and demand groups are refused, with and without snap", {
  f <- group_fixture()
  a <- f$demand_a
  b <- f$demand_b
  W <- terra::setValues(f$distance, gauss_w(f$dist_a, 2.5))
  msg <- "demand group names must be unique, non-empty"
  bad <- list(
    duplicate = c(list(a = a), list(a = b)),
    empty = stats::setNames(list(a, b), c("a", "")),
    missing = stats::setNames(list(a, b), c("a", NA)),
    unnamed = list(a, b),
    all = list(a = a, all = b),
    dot = list(a = a, `b.c` = b),
    none = list()
  )
  for (snap in c(FALSE, TRUE)) {
    for (nm in names(bad)) {
      expect_error(spax_e2sfca(bad[[nm]], f$S, f$travel_a, snap = snap), msg,
                   info = nm)
      expect_error(spax_3sfca(bad[[nm]], f$S, f$travel_a, snap = snap), msg,
                   info = nm)
      expect_error(spax_2sfca(bad[[nm]], f$S, f$travel_a, 3, snap = snap), msg,
                   info = nm)
      expect_error(compute_access(bad[[nm]], f$S, W, W, snap = snap), msg,
                   info = nm)
    }
    # Elements that are not rasters, or with another geometry.
    expect_error(spax_e2sfca(list(a = a, b = 1), f$S, f$travel_a, snap = snap),
                 "demand group 'b' must be one of the following classes")
    other <- toy_rast(1:20, nrows = 4, ncols = 5)
    expect_error(spax_e2sfca(list(a = a, b = other), f$S, f$travel_a, snap = snap),
                 "same geometry")
  }
  # Valid names: dashes, spaces, digits and case are allowed.
  ok <- spax_e2sfca(list(`under 16` = a, `65-plus` = b, All = a), f$S, f$travel_a,
                    indicator_names = wrap_names)
  expect_identical(ok$parameters$groups, c("under 16", "65-plus", "All"))
})

test_that("per-group arguments need one element per group", {
  f <- group_fixture()
  msg <- "give one element per group: a, b"
  W <- terra::setValues(f$distance, gauss_w(f$dist_a, 2.5))
  forms <- function(x) list(missing = x["a"], extra = c(x, list(c = x[[1]])),
                            swapped = c(x["a"], list(c = x[[2]])))
  for (snap in c(FALSE, TRUE)) {
    for (nm in names(forms(wrap_decay))) {
      expect_error(spax_e2sfca(wrap_demand(f), f$S, f$travel_a,
                               forms(wrap_decay)[[nm]], snap = snap),
                   msg, info = nm)
      expect_error(spax_m2sfca(wrap_demand(f), f$S, f$travel_a,
                               forms(wrap_decay)[[nm]], snap = snap),
                   msg, info = nm)
      expect_error(spax_3sfca(wrap_demand(f), f$S, f$travel_a, wrap_decay,
                              forms(wrap_decay)[[nm]], snap = snap),
                   msg, info = nm)
      expect_error(spax_huff2sfca(wrap_demand(f), f$S, f$travel_a, wrap_decay,
                                  forms(wrap_decay)[[nm]], attractiveness = 1:3,
                                  snap = snap), msg, info = nm)
      expect_error(spax_e2sfca(wrap_demand(f), f$S, forms(wrap_travel(f))[[nm]],
                               wrap_decay, snap = snap), msg, info = nm)
      expect_error(compute_access(wrap_demand(f), f$S,
                                  forms(wrap_travel(f))[[nm]], W, snap = snap),
                   msg, info = nm)
      expect_error(compute_access(wrap_demand(f), f$S, W,
                                  forms(wrap_travel(f))[[nm]], snap = snap),
                   msg, info = nm)
    }
    expect_error(spax_2sfca(wrap_demand(f), f$S, f$travel_a, c(a = 3), snap = snap),
                 msg)
    expect_error(spax_2sfca(wrap_demand(f), f$S, f$travel_a, c(a = 3, c = 4),
                            snap = snap), msg)
    expect_error(spax_2sfca(wrap_demand(f), f$S, f$travel_a, c(a = 3, b = 4, c = 5),
                            snap = snap), msg)
  }
  # A list that shares no name with the groups is a shared argument and is
  # validated as before; so is an unnamed threshold vector.
  expect_error(spax_e2sfca(wrap_demand(f), f$S, f$travel_a, list(sigma = 2)),
               "decay_params must include 'method'")
  expect_error(spax_3sfca(wrap_demand(f), f$S, f$travel_a, wrap_decay,
                          list(sigma = 2)),
               "selection_params must include 'method'")
  expect_error(spax_2sfca(wrap_demand(f), f$S, f$travel_a, c(3, 4)),
               "threshold must be a single positive number")
  # The elements of a per-group argument are validated as before.
  expect_error(spax_e2sfca(wrap_demand(f), f$S, f$travel_a,
                           list(a = wrap_decay$a, b = list(sigma = 2))),
               "decay_params must include 'method'")
  expect_error(spax_2sfca(wrap_demand(f), f$S, f$travel_a, c(a = 3, b = -1)),
               "threshold must be a single positive number")
  expect_error(spax_2sfca(wrap_demand(f), f$S, f$travel_a, c(a = 3, b = NA_real_)),
               "threshold must be a single positive number")
})

test_that("travel stacks of the groups must name the same facilities", {
  f <- group_fixture()
  msg <- "travel layers of every group must name the same facilities in the same order"
  other_grid <- toy_rast(seq_len(60), nrows = 5, ncols = 4, nlyr = 3, names = f$ids)
  bad_b <- list(
    names = renamed_stack(f$travel_b, c("f1", "f2", "x")),
    order = f$travel_b[[c("f3", "f2", "f1")]],
    geometry = other_grid
  )
  W <- terra::setValues(f$distance, gauss_w(f$dist_a, 2.5))
  for (snap in c(FALSE, TRUE)) {
    for (nm in names(bad_b)) {
      tr <- list(a = f$travel_a, b = bad_b[[nm]])
      expect_error(spax_e2sfca(wrap_demand(f), f$S, tr, wrap_decay, snap = snap),
                   msg, info = nm)
      expect_error(spax_3sfca(wrap_demand(f), f$S, tr, wrap_decay, snap = snap),
                   msg, info = nm)
      expect_error(spax_huff2sfca(wrap_demand(f), f$S, tr, wrap_decay,
                                  attractiveness = 1:3, snap = snap), msg, info = nm)
      expect_error(spax_2sfca(wrap_demand(f), f$S, tr, 3, snap = snap), msg,
                   info = nm)
      weights <- list(a = W, b = terra::setValues(f$travel_b, 1))
      weights$b <- if (nm == "geometry") other_grid else {
        if (nm == "names") renamed_stack(weights$b, c("f1", "f2", "x")) else
          weights$b[[c("f3", "f2", "f1")]]
      }
      wmsg <- sub("^travel", "weight", msg)
      expect_error(compute_access(wrap_demand(f), f$S, weights, W, snap = snap),
                   wmsg, info = nm)
      expect_error(compute_access(wrap_demand(f), f$S, W, weights, snap = snap),
                   wmsg, info = nm)
    }
  }
  # Reordering as the message suggests works.
  fixed <- list(a = f$travel_a, b = bad_b$order[[names(f$travel_a)]])
  got <- spax_e2sfca(wrap_demand(f), f$S, fixed, wrap_decay,
                     indicator_names = wrap_names)
  expect_identical(names(got$accessibility), wrap_layers)
})

test_that("negative demand in a group is refused unless snap", {
  f <- group_fixture()
  Db <- f$Db
  Db[2] <- -1
  d <- list(a = f$demand_a, b = toy_rast(Db, nrows = 3, ncols = 4))
  expect_error(spax_e2sfca(d, f$S, wrap_travel(f), wrap_decay),
               "demand must be finite and nonnegative")
  expect_no_error(spax_e2sfca(d, f$S, wrap_travel(f), wrap_decay, snap = TRUE))
})

test_that("grouped spax objects print and summarise their groups", {
  f <- group_fixture()
  per <- spax_e2sfca(wrap_demand(f), f$S, wrap_travel(f), wrap_decay,
                     demand_normalize = "standard", indicator_names = wrap_names)
  expect_output(print(per), "Measures: 2", fixed = TRUE)
  expect_output(print(per), "Demand groups: a, b", fixed = TRUE)
  expect_output(print(per), "Decay function (group a): gaussian (sigma = 2.5)",
                fixed = TRUE)
  expect_output(print(per), "Decay function (group b): gaussian (sigma = 4)",
                fixed = TRUE)
  expect_output(print(per), "Demand normalization: standard", fixed = TRUE)
  out <- paste(capture.output(print(per)), collapse = "\n")
  expect_match(out, "- m1\n- m2", fixed = TRUE)
  expect_false(grepl("- m1.a", out, fixed = TRUE))

  # A shared decay is one line.
  shared <- spax_e2sfca(wrap_demand(f), f$S, f$travel_a,
                        list(method = "gaussian", sigma = 3),
                        indicator_names = wrap_names)
  out <- paste(capture.output(print(shared)), collapse = "\n")
  expect_match(out, "Demand groups: a, b", fixed = TRUE)
  expect_match(out, "- Decay function: gaussian (sigma = 3)", fixed = TRUE)
  expect_false(grepl("(group a)", out, fixed = TRUE))

  # 2SFCA with thresholds per group.
  two <- spax_2sfca(wrap_demand(f), f$S, wrap_travel(f), c(a = 3, b = 5))
  expect_output(print(two), "Decay function (group b): binary (sigma = 5)",
                fixed = TRUE)

  s <- summary(per)
  expect_s3_class(s, "summary.spax")
  expect_identical(names(s$accessibility), wrap_layers)
  expect_identical(s$parameters$groups, c("a", "b"))
  expect_output(print(s), "Demand groups: a, b", fixed = TRUE)
  expect_output(print(s), "Decay function (group a): gaussian (sigma = 2.5)",
                fixed = TRUE)
  expect_output(print(s), "m1.all:", fixed = TRUE)
  withr::local_pdf(NULL)
  expect_no_error(plot(per, y = "m1.all"))

  # Ungrouped printing has no group lines.
  ungrouped <- spax_e2sfca(f$demand_a, f$S, f$travel_a, wrap_decay$a,
                           indicator_names = wrap_names)
  out <- paste(capture.output(print(ungrouped)), collapse = "\n")
  expect_false(grepl("Demand groups", out, fixed = TRUE))
  expect_match(out, "Measures: 2\n- m1\n- m2", fixed = TRUE)
})

test_that("grouped spax objects survive save_spax and read_spax", {
  skip_if_no_proj()
  f <- group_fixture()
  result <- spax_e2sfca(wrap_demand(f), f$S, wrap_travel(f), wrap_decay,
                        indicator_names = wrap_names)
  tmp <- withr::local_tempdir()
  file <- file.path(tmp, "grouped")
  save_spax(result, file)
  loaded <- read_spax(file)
  expect_identical(names(loaded$accessibility), wrap_layers)
  expect_identical(loaded$parameters$groups, c("a", "b"))
  expect_identical(loaded$parameters$layers, result$parameters$layers)
  expect_identical(loaded$parameters$decay_params, wrap_decay)
  expect_equal(terra::values(loaded$accessibility),
               terra::values(result$accessibility), tolerance = 1e-6)
  expect_output(print(loaded), "Decay function (group a)", fixed = TRUE)
})

test_that("groups named like elements of a shared decay list keep the shared decay", {
  f <- fca_toy()
  dp <- list(method = "gaussian", sigma = 2)
  ref <- spax_e2sfca(list(x = f$demand * 0.4, y = f$demand * 0.6), f$supply_vector,
                     f$distance, decay_params = dp)
  for (groups in list(c("sigma", "young"), c("method", "sigma"))) {
    demand <- stats::setNames(list(f$demand * 0.4, f$demand * 0.6), groups)
    out <- expect_no_error(spax_e2sfca(demand, f$supply_vector, f$distance,
                                       decay_params = dp))
    expect_equal(unname(terra::values(out$accessibility)),
                 unname(terra::values(ref$accessibility)), tolerance = 1e-12)
    out_snap <- expect_no_error(spax_e2sfca(demand, f$supply_vector, f$distance,
                                            decay_params = dp, snap = TRUE))
    expect_equal(unname(terra::values(out_snap$accessibility)),
                 unname(terra::values(ref$accessibility)), tolerance = 1e-12)
  }
})
