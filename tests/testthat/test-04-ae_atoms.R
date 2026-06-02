# SPAX-002: atom layer (.ae_*) over spax_field

mk_fields <- function() {
  set.seed(42)
  kern <- c(
    terra::rast(nrows = 4, ncols = 4, vals = runif(16)),
    terra::rast(nrows = 4, ncols = 4, vals = runif(16))
  )
  names(kern) <- c("fac_a", "fac_b")
  kernel <- .spax_raster_field(kern, domain = c("I", "facility"))

  dem <- terra::rast(nrows = 4, ncols = 4, vals = runif(16) * 100)
  demand <- .spax_raster_field(dem, domain = "I")

  ratios <- .spax_vector_field(c(fac_a = 1.5, fac_b = 2.5), domain = "facility")

  list(kernel = kernel, demand = demand, ratios = ratios)
}

.mk_prod_field <- function(tuples, vals) {
  r <- do.call(c, lapply(vals, function(v) terra::rast(nrows = 2, ncols = 2, vals = v)))
  names(r) <- paste0("ly", seq_along(vals))
  frame <- data.frame(
    layer = names(r), J = tuples$J, mode = tuples$mode, stringsAsFactors = FALSE
  )
  .spax_raster_field(r, domain = c("I", "J", "mode"), frame = frame)
}

# Atoms ----------------------------------------------------------------------

test_that(".ae_aggregate over I collapses to a vector field (DEC-006)", {
  f <- mk_fields()
  u <- .ae_aggregate(f$kernel, by = "facility")

  expect_s3_class(u, "spax_vector_field")
  expect_equal(.field_backend(u), "vector")
  expect_equal(.field_domain(u), "facility")
  expect_equal(names(.field_data(u)), c("V1", "V2"))
  expect_equal(.field_axis_values(u, "facility"), c("fac_a", "fac_b"))
  expect_equal(
    unname(.field_data(u)),
    unname(terra::global(.field_data(f$kernel), "sum", na.rm = TRUE)[[1]])
  )
})

test_that(".ae_aggregate over a non-I axis collapses layers to a raster field", {
  f <- mk_fields()
  a <- .ae_aggregate(f$kernel, by = "I")

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
  lifted <- .ae_lift(f$ratios, template = f$kernel)

  expect_s3_class(lifted, "spax_raster_field")
  expect_equal(terra::nlyr(.field_data(lifted)), 2)
  v <- terra::values(.field_data(lifted))
  expect_true(all(v[, 1] == 1.5))
  expect_true(all(v[, 2] == 2.5))
})

test_that(".ae_lift replicates a domain-I raster across template layers", {
  f <- mk_fields()
  lifted <- .ae_lift(f$demand, template = f$kernel)
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
  expect_equal(names(.field_data(g)), c("V1", "V2"))
  expect_equal(.field_axis_values(g, "facility"), c("fac_a", "fac_b"))
  raw <- gather_weighted(.field_data(f$demand), .field_data(f$kernel), simplify = TRUE)
  expect_equal(unname(.field_data(g)), unname(raw))
})

test_that(".ae_spread equals spread_weighted (recomposed verb)", {
  f <- mk_fields()
  s <- .ae_spread(f$ratios, f$kernel)

  expect_s3_class(s, "spax_raster_field")
  expect_equal(.field_domain(s), "I")

  vals_ord <- .ae_align_to_layers(f$ratios, f$kernel)
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
  bigK <- .spax_raster_field(big, domain = c("I", "facility"))
  R <- .spax_vector_field(c(a = 1, b = 2, c = 3), domain = "facility")

  s <- .ae_spread(R, bigK)
  vals_ord <- .ae_align_to_layers(R, bigK)
  raw <- spread_weighted(unname(vals_ord), .field_data(bigK))
  expect_equal(unname(terra::values(.field_data(s))), unname(terra::values(raw)))
})

test_that(".ae_ratio divides zero-safely", {
  num <- .spax_vector_field(c(fac_a = 10, fac_b = 20), domain = "facility")
  den <- .spax_vector_field(c(fac_a = 2, fac_b = 0), domain = "facility")
  r <- .ae_ratio(num, den)
  expect_equal(unname(.field_data(r)), c(5, 0))
})

test_that(".ae_combine rejects vector fields with extra axis ids", {
  a <- .spax_vector_field(c(fac_a = 1, fac_b = 2), domain = "facility")
  b <- .spax_vector_field(c(fac_a = 10, fac_b = 20, fac_c = 30),
                                 domain = "facility")

  expect_error(.ae_combine(a, b, op = `+`), "different axis tuples")
})

test_that("Ops.spax_field aligns vector fields by names", {
  a <- .spax_vector_field(c(fac_a = 1, fac_b = 2), domain = "facility")
  b <- .spax_vector_field(c(fac_b = 20, fac_a = 10), domain = "facility")

  res <- a + b

  expect_s3_class(res, "spax_vector_field")
  expect_equal(names(.field_data(res)), c("V1", "V2"))
  expect_equal(.field_axis_values(res, "facility"), c("fac_a", "fac_b"))
  expect_equal(unname(.field_data(res)), c(11, 22))
})

test_that("Ops.spax_field rejects vector fields with extra or missing ids", {
  a <- .spax_vector_field(c(fac_a = 1, fac_b = 2), domain = "facility")
  b <- .spax_vector_field(c(fac_a = 10, fac_b = 20, fac_c = 30),
                                 domain = "facility")

  expect_error(a + b, "different axis tuples")
  expect_error(b + a, "different axis tuples")
})

test_that("Ops.spax_field supports scalar arithmetic in operand order", {
  a <- .spax_vector_field(c(fac_a = 2, fac_b = 4), domain = "facility",
                                 role = "state", meta = list(source = "test"))

  expect_equal(unname(.field_data(a + 1)), c(3, 5))
  expect_equal(unname(.field_data(1 + a)), c(3, 5))
  expect_equal(unname(.field_data(a - 1)), c(1, 3))
  expect_equal(unname(.field_data(1 - a)), c(-1, -3))
  expect_equal(unname(.field_data(a / 2)), c(1, 2))
  expect_equal(unname(.field_data(2 / a)), c(1, 0.5))
  expect_equal(unname(.field_data(a ^ 2)), c(4, 16))
  expect_equal(unname(.field_data(2 ^ a)), c(4, 16))
  expect_equal(.field_role(a + 1), "state")
  expect_equal(.field_meta(a + 1)$source, "test")
})

test_that("Math.spax_field applies pointwise transforms to vector fields", {
  a <- .spax_vector_field(c(fac_a = 1, fac_b = 4), domain = "facility",
                                 role = "state", meta = list(source = "test"))

  logged <- log(a)
  rooted <- sqrt(a)

  expect_s3_class(logged, "spax_vector_field")
  expect_equal(unname(.field_data(logged)), log(c(1, 4)))
  expect_equal(unname(.field_data(rooted)), c(1, 2))
  expect_equal(.field_role(logged), "state")
  expect_equal(.field_meta(logged)$source, "test")
})

test_that(".ae_update applies a damped mix", {
  st <- .spax_vector_field(c(fac_a = 1, fac_b = 1), domain = "facility")
  tg <- .spax_vector_field(c(fac_a = 3, fac_b = 5), domain = "facility")
  up <- .ae_update(st, tg, lambda = 0.5)
  expect_equal(unname(.field_data(up)), c(2, 3))

  expect_error(.ae_update(st, tg, lambda = 0), "lambda")
})

test_that(".ae_normalize matches calc_normalize", {
  f <- mk_fields()
  n <- .ae_normalize(f$kernel, by = "I", method = "standard")
  expect_equal(
    terra::values(.field_data(n)),
    terra::values(calc_normalize(.field_data(f$kernel), method = "standard"))
  )
})

test_that(".ae_normalize infers raster cell axis when by is omitted", {
  f <- mk_fields()
  expect_warning(
    inferred <- .ae_normalize(f$kernel, method = "standard"),
    "`by` omitted"
  )
  explicit <- .ae_normalize(f$kernel, by = "I", method = "standard")
  expect_equal(terra::values(.field_data(inferred)),
               terra::values(.field_data(explicit)))
})

test_that(".ae_normalize groups product-axis raster fields by mode", {
  field <- .mk_prod_field(
    data.frame(J = c("j1", "j1", "j2", "j2"),
               mode = c("car", "bus", "car", "bus")),
    c(1, 2, 3, 4)
  )
  normalized <- .ae_normalize(field, by = c("I", "mode"), method = "standard")
  values <- terra::values(.field_data(normalized))

  expect_s3_class(normalized, "spax_raster_field")
  expect_equal(.field_domain(normalized), c("I", "J", "mode"))
  expect_equal(.field_role(normalized), "map")
  expect_equal(unname(values[1, ]), c(0.25, 1 / 3, 0.75, 2 / 3))
  expect_equal(values[, 1] + values[, 3], rep(1, nrow(values)))
  expect_equal(values[, 2] + values[, 4], rep(1, nrow(values)))
})

test_that(".ae_normalize groups product-axis raster fields by facility", {
  field <- .mk_prod_field(
    data.frame(J = c("j1", "j1", "j2", "j2"),
               mode = c("car", "bus", "car", "bus")),
    c(1, 2, 3, 4)
  )
  normalized <- .ae_normalize(field, by = c("I", "J"), method = "standard")
  values <- terra::values(.field_data(normalized))

  expect_equal(unname(values[1, ]), c(1 / 3, 2 / 3, 3 / 7, 4 / 7))
  expect_equal(values[, 1] + values[, 2], rep(1, nrow(values)))
  expect_equal(values[, 3] + values[, 4], rep(1, nrow(values)))
})

test_that(".ae_normalize can normalize raster fields after collapsing cells", {
  field <- .mk_prod_field(
    data.frame(J = c("j1", "j1", "j2", "j2"),
               mode = c("car", "bus", "car", "bus")),
    c(1, 2, 3, 4)
  )
  normalized <- .ae_normalize(field, by = "J", method = "standard")
  values <- terra::values(.field_data(normalized))

  expect_equal(unname(values[1, ]), c(1 / 12, 2 / 12, 3 / 28, 4 / 28))
  expect_equal(sum(values[, 1]) + sum(values[, 2]), 1)
  expect_equal(sum(values[, 3]) + sum(values[, 4]), 1)
})

test_that(".ae_normalize supports vector fields by retained axes", {
  frame <- data.frame(J = c("j1", "j1", "j2", "j2"),
                      mode = c("car", "bus", "car", "bus"))
  field <- .spax_vector_field(c(1, 2, 3, 4), domain = c("J", "mode"),
                              frame = frame)

  by_mode <- .ae_normalize(field, by = "mode", method = "standard")
  expect_s3_class(by_mode, "spax_vector_field")
  expect_equal(.field_role(by_mode), "map")
  expect_equal(unname(.field_data(by_mode)), c(0.25, 1 / 3, 0.75, 2 / 3))

  global <- .ae_normalize(field, method = "standard")
  expect_equal(sum(.field_data(global)), 1)
  expect_equal(unname(.field_data(global)), c(0.1, 0.2, 0.3, 0.4))
})

test_that(".ae_normalize handles outside option and semi normalization by group", {
  frame <- data.frame(J = c("j1", "j1", "j2", "j2"),
                      mode = c("car", "bus", "car", "bus"))
  field <- .spax_vector_field(c(1, 2, 3, 4), domain = c("J", "mode"),
                              frame = frame)

  outside <- .ae_normalize(field, by = "mode", method = "standard", a0 = 1)
  expect_equal(unname(.field_data(outside)), c(0.2, 2 / 7, 0.6, 4 / 7))

  semi_field <- .spax_vector_field(
    c(0.2, 0.8, 0.4, 0.8),
    domain = c("J", "mode"),
    frame = frame
  )
  semi <- .ae_normalize(semi_field, by = "mode", method = "semi")
  expect_equal(unname(.field_data(semi)), c(0.2, 0.5, 0.4, 0.5))
})

test_that("E2SFCA inner step composes from atoms (gather -> ratio -> spread)", {
  # supply S_j, demand D_i, kernel K_ij -> ratios R_j = S_j / U_j -> access A_i.
  f <- mk_fields()
  supply <- .spax_vector_field(c(fac_a = 10, fac_b = 5), domain = "facility")

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
  .spax_raster_field(r, domain = c("I", axis), frame = frame)
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

test_that("Ops.spax_field aligns raster fields by index tuple", {
  a <- .mk_axis_field(c("fac_a", "fac_b"), c(1, 2))
  b <- .mk_axis_field(c("fac_b", "fac_a"), c(2, 1))

  res <- a + b
  got <- vapply(1:2, function(k) terra::values(.field_data(res))[1, k], numeric(1))

  expect_s3_class(res, "spax_raster_field")
  expect_equal(.field_domain(res), c("I", "facility"))
  expect_equal(unname(got), c(2, 4))
})

test_that("Ops.spax_field aligns product-axis raster fields by tuple", {
  tup <- data.frame(J = c("j1", "j1", "j2", "j2"), mode = c("car", "bus", "car", "bus"))
  a <- .mk_prod_field(tup, c(1, 2, 3, 4))
  b <- .mk_prod_field(tup[4:1, ], c(4, 3, 2, 1))

  res <- a + b
  got <- vapply(1:4, function(k) terra::values(.field_data(res))[1, k], numeric(1))

  expect_s3_class(res, "spax_raster_field")
  expect_equal(.field_domain(res), c("I", "J", "mode"))
  expect_equal(unname(got), c(2, 4, 6, 8))
})

test_that("Ops.spax_field supports raster scalar arithmetic", {
  f <- mk_fields()

  add <- f$kernel + 1
  div <- 2 / f$kernel
  pow <- f$kernel ^ 2

  expect_s3_class(add, "spax_raster_field")
  expect_equal(.field_domain(add), .field_domain(f$kernel))
  expect_equal(.field_layer_index(add), .field_layer_index(f$kernel))
  expect_equal(terra::values(.field_data(add)),
               terra::values(.field_data(f$kernel) + 1))
  expect_equal(terra::values(.field_data(div)),
               terra::values(2 / .field_data(f$kernel)))
  expect_equal(terra::values(.field_data(pow)),
               terra::values(.field_data(f$kernel) ^ 2))
})

test_that("Math.spax_field applies pointwise transforms to raster fields", {
  f <- mk_fields()

  logged <- log(f$kernel)
  rooted <- sqrt(f$kernel)

  expect_s3_class(logged, "spax_raster_field")
  expect_equal(.field_domain(logged), .field_domain(f$kernel))
  expect_equal(.field_layer_index(logged), .field_layer_index(f$kernel))
  expect_equal(terra::values(.field_data(logged)),
               terra::values(log(.field_data(f$kernel))))
  expect_equal(terra::values(.field_data(rooted)),
               terra::values(sqrt(.field_data(f$kernel))))
})

test_that("Ops.spax_field does not auto-lift or allow unsupported Ops", {
  f <- mk_fields()

  expect_error(f$kernel * f$ratios, "matching domains")
  expect_error(f$demand * f$ratios, "matching domains")
  expect_error(f$ratios ^ f$ratios, "field \\^ field is not supported")
  expect_error(f$ratios == f$ratios, "unsupported spax_field operator")
  expect_error(f$ratios & TRUE, "unsupported spax_field operator")
  expect_error(cumsum(f$ratios), "unsupported spax_field math transform")
})

test_that(".ae_gather rejects a source that is not single-layer domain I", {
  f <- mk_fields()
  expect_error(.ae_gather(f$kernel, f$kernel), "cell axis")
})

test_that(".ae_aggregate groups product-axis raster fields by retained axes", {
  a <- .mk_prod_field(
    data.frame(J = c("j1", "j1", "j2", "j2"), mode = c("car", "bus", "car", "bus")),
    c(1, 2, 3, 4)
  )
  collapsed <- .ae_aggregate(a, by = c("I", "J"))
  expect_s3_class(collapsed, "spax_raster_field")
  expect_equal(.field_domain(collapsed), c("I", "J"))
  expect_equal(.field_axis_values(collapsed, "J"), c("j1", "j2"))
  got <- vapply(1:2, function(k) terra::values(.field_data(collapsed))[1, k], numeric(1))
  expect_equal(unname(got), c(3, 7))
})

test_that(".ae_aggregate can collapse raster cells and retain product layer axes", {
  a <- .mk_prod_field(
    data.frame(J = c("j1", "j1", "j2", "j2"), mode = c("car", "bus", "car", "bus")),
    c(1, 2, 3, 4)
  )
  out <- .ae_aggregate(a, by = c("J", "mode"))
  expect_s3_class(out, "spax_vector_field")
  expect_equal(.field_domain(out), c("J", "mode"))
  expect_equal(unname(.field_data(out)), c(4, 8, 12, 16))
  expect_equal(
    .field_index_frame(out)[c("J", "mode")],
    data.frame(J = c("j1", "j1", "j2", "j2"),
               mode = c("car", "bus", "car", "bus"))
  )
})

test_that(".ae_aggregate groups product-axis vector fields by retained axes", {
  frame <- data.frame(J = c("j1", "j1", "j2", "j2"),
                      mode = c("car", "bus", "car", "bus"))
  v <- .spax_vector_field(c(1, 2, 3, 4), domain = c("J", "mode"),
                          frame = frame)
  out <- .ae_aggregate(v, by = "J")
  expect_s3_class(out, "spax_vector_field")
  expect_equal(.field_domain(out), "J")
  expect_equal(.field_axis_values(out, "J"), c("j1", "j2"))
  expect_equal(unname(.field_data(out)), c(3, 7))
})

test_that(".ae_lift broadcasts vector fields onto vector templates", {
  base <- .spax_vector_field(c(j1 = 10, j2 = 20), domain = "J")
  frame <- data.frame(J = c("j1", "j1", "j2", "j2"),
                      mode = c("car", "bus", "car", "bus"))
  template <- .spax_vector_field(rep(NA_real_, 4), domain = c("J", "mode"),
                                 frame = frame, role = "template")
  lifted <- .ae_lift(base, template)
  expect_s3_class(lifted, "spax_vector_field")
  expect_equal(.field_domain(lifted), c("J", "mode"))
  expect_equal(unname(.field_data(lifted)), c(10, 10, 20, 20))
})

test_that(".ae_lift broadcasts raster fields across added layer axes", {
  r <- do.call(c, lapply(c(1, 2), function(v) terra::rast(nrows = 2, ncols = 2, vals = v)))
  names(r) <- c("j1", "j2")
  base <- .spax_raster_field(r, domain = c("I", "J"))
  template <- .mk_prod_field(
    data.frame(J = c("j1", "j1", "j2", "j2"), mode = c("car", "bus", "car", "bus")),
    c(0, 0, 0, 0)
  )
  lifted <- .ae_lift(base, template)
  expect_s3_class(lifted, "spax_raster_field")
  got <- vapply(1:4, function(k) terra::values(.field_data(lifted))[1, k], numeric(1))
  expect_equal(unname(got), c(1, 1, 2, 2))
})

test_that(".ae_mask applies lower-dimensional soft masks explicitly", {
  field <- .mk_prod_field(
    data.frame(J = c("j1", "j1", "j2", "j2"), mode = c("car", "bus", "car", "bus")),
    c(10, 20, 30, 40)
  )
  mask <- .spax_vector_field(
    c(1, 0.5, 0),
    domain = c("J", "mode"),
    frame = data.frame(J = c("j1", "j1", "j2"),
                       mode = c("car", "bus", "car"))
  )
  expect_error(.ae_mask(field, mask), "missing axis tuples")

  mask <- .spax_vector_field(
    c(1, 0.5, 0, 0.25),
    domain = c("J", "mode"),
    frame = data.frame(J = c("j1", "j1", "j2", "j2"),
                       mode = c("car", "bus", "car", "bus"))
  )
  masked <- .ae_mask(field, mask)
  got <- vapply(1:4, function(k) terra::values(.field_data(masked))[1, k], numeric(1))
  expect_equal(unname(got), c(10, 10, 0, 10))
  expect_error(.ae_mask(field, .spax_vector_field(c(1.5), domain = "J",
                                                  frame = data.frame(J = "j1"))),
               "\\[0, 1\\]")
})
