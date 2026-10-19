# Demand groups in the block engine and the internal driver (SPAX-056, T1):
# per-group kernels with suffixed raster keys, several groups competing for the
# same supply, and the overall layer (decision D1: a group with demand in a cell
# its travel cannot reach counts with access 0). The driver is called directly
# with per-group kernels; the wrappers are not grouped yet.

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
  wa <- mk(c(1, 0.5, 0.25, 0.5, 0.5))
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
  expect_equal(v[, 1], R * c(1, 0.5, 0.25, 0.5, 0.5))
  expect_equal(v[, 2], R * c(0.5, NA, 1, 0.5, 0.5))
  expect_equal(v[, 3], c(
    (10 * 0.16 + 20 * 0.08) / 30,  # both groups reach the cell
    (20 * 0.08 + 30 * 0) / 50,     # b cannot reach it: counts with access 0
    0.16,                          # a has no demand: skipped, not 0 * something
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
