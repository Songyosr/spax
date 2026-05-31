# SPAX-002: atom layer (.ae_*) over spax_field

mk_fields <- function() {
  set.seed(42)
  kern <- c(
    terra::rast(nrows = 4, ncols = 4, vals = runif(16)),
    terra::rast(nrows = 4, ncols = 4, vals = runif(16))
  )
  names(kern) <- c("fac_a", "fac_b")
  kernel <- .create_spax_raster_field(kern, domain = c("I", "facility"))

  dem <- terra::rast(nrows = 4, ncols = 4, vals = runif(16) * 100)
  demand <- .create_spax_raster_field(dem, domain = "I")

  ratios <- .create_spax_vector_field(c(fac_a = 1.5, fac_b = 2.5), domain = "facility")

  list(kernel = kernel, demand = demand, ratios = ratios)
}

# Atoms ----------------------------------------------------------------------

test_that(".ae_aggregate over I collapses to a vector field (DEC-006)", {
  f <- mk_fields()
  u <- .ae_aggregate(f$kernel, over = "I")

  expect_s3_class(u, "spax_vector_field")
  expect_equal(.field_backend(u), "vector")
  expect_equal(.field_domain(u), "facility")
  expect_equal(names(.field_data(u)), c("fac_a", "fac_b"))
  expect_equal(
    unname(.field_data(u)),
    unname(terra::global(.field_data(f$kernel), "sum", na.rm = TRUE)[[1]])
  )
})

test_that(".ae_aggregate over a non-I axis collapses layers to a raster field", {
  f <- mk_fields()
  a <- .ae_aggregate(f$kernel, over = "facility")

  expect_s3_class(a, "spax_raster_field")
  expect_equal(.field_domain(a), "I")
  expect_equal(terra::nlyr(.field_data(a)), 1)
  expect_equal(
    unname(terra::values(.field_data(a))),
    unname(terra::values(terra::app(.field_data(f$kernel), fun = sum, na.rm = TRUE)))
  )
})

test_that(".ae_transform applies a unary scalar map", {
  f <- mk_fields()
  t <- .ae_transform(f$kernel, function(x) x * 2)
  expect_equal(
    terra::values(.field_data(t)),
    terra::values(.field_data(f$kernel) * 2)
  )
})

test_that(".ae_combine combines two fields elementwise", {
  f <- mk_fields()
  c2 <- .ae_combine(f$kernel, f$kernel, op = `+`)
  expect_equal(
    terra::values(.field_data(c2)),
    terra::values(.field_data(f$kernel) * 2)
  )
})

test_that(".ae_lift broadcasts a vector field onto edges (constant per layer)", {
  f <- mk_fields()
  lifted <- .ae_lift(f$ratios, to = c("I", "facility"), template = f$kernel)

  expect_s3_class(lifted, "spax_raster_field")
  expect_equal(terra::nlyr(.field_data(lifted)), 2)
  v <- terra::values(.field_data(lifted))
  expect_true(all(v[, 1] == 1.5))
  expect_true(all(v[, 2] == 2.5))
})

test_that(".ae_lift replicates a domain-I raster across template layers", {
  f <- mk_fields()
  lifted <- .ae_lift(f$demand, to = c("I", "facility"), template = f$kernel)
  expect_equal(terra::nlyr(.field_data(lifted)), 2)
  expect_equal(
    terra::values(.field_data(lifted))[, 1],
    terra::values(.field_data(f$demand))[, 1]
  )
  expect_equal(
    terra::values(.field_data(lifted))[, 2],
    terra::values(.field_data(f$demand))[, 1]
  )
})

# Verbs + equivalence vs raw kernels -----------------------------------------

test_that(".ae_gather equals gather_weighted (fused verb)", {
  f <- mk_fields()
  g <- .ae_gather(f$demand, f$kernel)

  expect_s3_class(g, "spax_vector_field")
  expect_equal(names(.field_data(g)), c("fac_a", "fac_b"))
  raw <- gather_weighted(.field_data(f$demand), .field_data(f$kernel), simplify = TRUE)
  expect_equal(unname(.field_data(g)), unname(raw))
})

test_that(".ae_spread equals spread_weighted (recomposed verb)", {
  f <- mk_fields()
  s <- .ae_spread(f$ratios, f$kernel)

  expect_s3_class(s, "spax_raster_field")
  expect_equal(.field_domain(s), "I")

  vals_ord <- .field_data(f$ratios)[as.character(.field_layer_index(f$kernel)$facility)]
  raw <- spread_weighted(unname(vals_ord), .field_data(f$kernel))
  expect_equal(unname(terra::values(.field_data(s))), unname(terra::values(raw)))
})

test_that(".ae_spread matches spread_weighted at moderate size", {
  set.seed(7)
  big <- c(
    terra::rast(nrows = 40, ncols = 40, vals = runif(1600)),
    terra::rast(nrows = 40, ncols = 40, vals = runif(1600)),
    terra::rast(nrows = 40, ncols = 40, vals = runif(1600))
  )
  names(big) <- c("a", "b", "c")
  bigK <- .create_spax_raster_field(big, domain = c("I", "facility"))
  R <- .create_spax_vector_field(c(a = 1, b = 2, c = 3), domain = "facility")

  s <- .ae_spread(R, bigK)
  vals_ord <- .field_data(R)[as.character(.field_layer_index(bigK)$facility)]
  raw <- spread_weighted(unname(vals_ord), .field_data(bigK))
  expect_equal(unname(terra::values(.field_data(s))), unname(terra::values(raw)))
})

test_that(".ae_ratio divides zero-safely", {
  num <- .create_spax_vector_field(c(fac_a = 10, fac_b = 20), domain = "facility")
  den <- .create_spax_vector_field(c(fac_a = 2, fac_b = 0), domain = "facility")
  r <- .ae_ratio(num, den)
  expect_equal(unname(.field_data(r)), c(5, 0))
})

test_that(".ae_combine rejects vector fields with extra axis ids", {
  a <- .create_spax_vector_field(c(fac_a = 1, fac_b = 2), domain = "facility")
  b <- .create_spax_vector_field(c(fac_a = 10, fac_b = 20, fac_c = 30),
                                 domain = "facility")

  expect_error(.ae_combine(a, b, op = `+`), "different axis ids")
})

test_that(".ae_update applies a damped mix", {
  st <- .create_spax_vector_field(c(fac_a = 1, fac_b = 1), domain = "facility")
  tg <- .create_spax_vector_field(c(fac_a = 3, fac_b = 5), domain = "facility")
  up <- .ae_update(st, tg, lambda = 0.5)
  expect_equal(unname(.field_data(up)), c(2, 3))

  expect_error(.ae_update(st, tg, lambda = 0), "lambda")
})

test_that(".ae_normalize matches calc_normalize", {
  f <- mk_fields()
  n <- .ae_normalize(f$kernel, method = "standard")
  expect_equal(
    terra::values(.field_data(n)),
    terra::values(calc_normalize(.field_data(f$kernel), method = "standard"))
  )
})

test_that("E2SFCA inner step composes from atoms (gather -> ratio -> spread)", {
  # supply S_j, demand D_i, kernel K_ij -> ratios R_j = S_j / U_j -> access A_i.
  f <- mk_fields()
  supply <- .create_spax_vector_field(c(fac_a = 10, fac_b = 5), domain = "facility")

  U <- .ae_gather(f$demand, f$kernel)        # potential demand per facility
  R <- .ae_ratio(supply, U)                   # supply-to-demand ratio
  A <- .ae_spread(R, f$kernel)                # accessibility surface

  expect_s3_class(A, "spax_raster_field")
  expect_equal(.field_domain(A), "I")
  # equals the raw two-step on the same inputs
  raw_U <- gather_weighted(.field_data(f$demand), .field_data(f$kernel), simplify = TRUE)
  raw_R <- c(10, 5) / unname(raw_U)
  raw_A <- spread_weighted(raw_R, .field_data(f$kernel))
  expect_equal(unname(terra::values(.field_data(A))), unname(terra::values(raw_A)))
})

# DEC-005 alignment + product-axis guards (SPAX-002 review) -------------------

.mk_axis_field <- function(ids, vals, axis = "facility") {
  r <- do.call(c, lapply(vals, function(v) terra::rast(nrows = 2, ncols = 2, vals = v)))
  names(r) <- paste0("ly", seq_along(vals))
  frame <- data.frame(layer = names(r), stringsAsFactors = FALSE)
  frame[[axis]] <- ids
  .create_spax_raster_field(r, domain = c("I", axis), frame = frame)
}

.mk_prod_field <- function(tuples, vals) {
  r <- do.call(c, lapply(vals, function(v) terra::rast(nrows = 2, ncols = 2, vals = v)))
  names(r) <- paste0("ly", seq_along(vals))
  frame <- data.frame(
    layer = names(r), J = tuples$J, mode = tuples$mode, stringsAsFactors = FALSE
  )
  .create_spax_raster_field(r, domain = c("I", "J", "mode"), frame = frame)
}

test_that(".ae_combine aligns single-axis fields by id, not layer order", {
  a <- .mk_axis_field(c("fac_a", "fac_b"), c(1, 2))
  b <- .mk_axis_field(c("fac_b", "fac_a"), c(2, 1)) # same id->value, reversed
  cmb <- .ae_combine(a, b, op = `+`)
  got <- vapply(1:2, function(k) terra::values(.field_data(cmb))[1, k], numeric(1))
  expect_equal(unname(got), c(2, 4)) # fac_a 1+1, fac_b 2+2 (layer-order would give 3,3)
})

test_that(".ae_combine aligns product-axis (I, J, mode) fields by tuple", {
  tup <- data.frame(J = c("j1", "j1", "j2", "j2"), mode = c("car", "bus", "car", "bus"))
  a <- .mk_prod_field(tup, c(1, 2, 3, 4))
  b <- .mk_prod_field(tup[4:1, ], c(4, 3, 2, 1)) # reversed tuple order, same map
  cmb <- .ae_combine(a, b, op = `+`)
  got <- vapply(1:4, function(k) terra::values(.field_data(cmb))[1, k], numeric(1))
  expect_equal(unname(got), c(2, 4, 6, 8)) # tuple-aligned (layer-order would give 5,5,5,5)
})

test_that(".ae_gather rejects a source that is not single-layer domain I", {
  f <- mk_fields()
  expect_error(.ae_gather(f$kernel, f$kernel), "domain I")
})

test_that(".ae_aggregate rejects grouped product-axis aggregation (SPAX-005)", {
  a <- .mk_prod_field(
    data.frame(J = c("j1", "j1", "j2", "j2"), mode = c("car", "bus", "car", "bus")),
    c(1, 2, 3, 4)
  )
  expect_error(.ae_aggregate(a, over = "mode"), "SPAX-005")
})
