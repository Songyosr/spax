# SPAX-002: atom layer (.ae_*) over spax_field

# Random 4 x 4 kernel (I, facility) and demand (I) fields, and facility ratios.
mk_fields <- function() {
  local_test_seed(42)
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

# A 2 x 2 raster field on (I, J, mode) whose layers are constant `vals`.
.mk_prod_field <- function(tuples, vals) {
  r <- do.call(c, lapply(vals, function(v) terra::rast(nrows = 2, ncols = 2, vals = v)))
  names(r) <- paste0("ly", seq_along(vals))
  frame <- data.frame(
    layer = names(r), J = tuples$J, mode = tuples$mode, stringsAsFactors = FALSE
  )
  .spax_raster_field(r, domain = c("I", "J", "mode"), frame = frame)
}

# A 2 x 2 raster field on (I, axis) whose layers are constant `vals`.
.mk_axis_field <- function(ids, vals, axis = "facility") {
  r <- do.call(c, lapply(vals, function(v) terra::rast(nrows = 2, ncols = 2, vals = v)))
  names(r) <- paste0("ly", seq_along(vals))
  frame <- data.frame(layer = names(r), stringsAsFactors = FALSE)
  frame[[axis]] <- ids
  .spax_raster_field(r, domain = c("I", axis), frame = frame)
}

.prod_tuples <- function() {
  data.frame(J = c("j1", "j1", "j2", "j2"), mode = c("car", "bus", "car", "bus"))
}

# Values of a raster field as a plain cell-by-layer matrix, and its first row.
.cell_values <- function(field) unname(terra::values(.field_data(field)))
.first_cell <- function(field) .cell_values(field)[1, ]

# Atom: aggregate -------------------------------------------------------------

test_that(".ae_aggregate keeps the named axes and sums the rest (DEC-006)", {
  f <- mk_fields()
  k <- .cell_values(f$kernel)

  # Summing over I leaves a vector field on the facility axis.
  u <- .ae_aggregate(f$kernel, by = "facility")
  expect_s3_class(u, "spax_vector_field")
  expect_equal(.field_domain(u), "facility")
  expect_equal(.field_axis_values(u, "facility"), c("fac_a", "fac_b"))
  expect_equal(unname(.field_data(u)), colSums(k))

  # Summing over the layer axis leaves a one-layer raster on I.
  a <- .ae_aggregate(f$kernel, by = "I")
  expect_s3_class(a, "spax_raster_field")
  expect_equal(.field_domain(a), "I")
  expect_equal(terra::nlyr(.field_data(a)), 1)
  expect_equal(as.numeric(.cell_values(a)), rowSums(k))

  # No retained axes gives the total; every axis retained gives the field.
  expect_equal(.ae_aggregate(f$kernel), sum(k))
  expect_equal(.ae_aggregate(f$ratios), 4)
  expect_identical(.ae_aggregate(f$kernel, by = c("I", "facility")), f$kernel)
  expect_error(.ae_aggregate(f$kernel, by = c("I", "I")), "unique")
  expect_error(.ae_aggregate(f$kernel, by = "mode"), "subset")
})

test_that(".ae_aggregate groups product-axis raster fields by retained axes", {
  a <- .mk_prod_field(.prod_tuples(), c(1, 2, 3, 4))

  collapsed <- .ae_aggregate(a, by = c("I", "J"))
  expect_s3_class(collapsed, "spax_raster_field")
  expect_equal(.field_domain(collapsed), c("I", "J"))
  expect_equal(.field_axis_values(collapsed, "J"), c("j1", "j2"))
  expect_equal(.first_cell(collapsed), c(3, 7))

  # Collapsing the cell axis too keeps the product layer axes on a vector.
  out <- .ae_aggregate(a, by = c("J", "mode"))
  expect_s3_class(out, "spax_vector_field")
  expect_equal(.field_domain(out), c("J", "mode"))
  expect_equal(unname(.field_data(out)), c(4, 8, 12, 16))
  expect_equal(.field_index_frame(out)[c("J", "mode")], .prod_tuples())

  # A group holding a single layer passes that layer through.
  uneven <- .mk_prod_field(
    data.frame(J = c("j1", "j2", "j2"), mode = c("car", "car", "bus")), c(1, 2, 3)
  )
  expect_equal(.first_cell(.ae_aggregate(uneven, by = c("I", "J"))), c(1, 5))
})

test_that(".ae_aggregate groups product-axis vector fields by retained axes", {
  v <- .spax_vector_field(c(1, 2, 3, 4), domain = c("J", "mode"),
                          frame = .prod_tuples())
  out <- .ae_aggregate(v, by = "J")
  expect_s3_class(out, "spax_vector_field")
  expect_equal(.field_domain(out), "J")
  expect_equal(.field_axis_values(out, "J"), c("j1", "j2"))
  expect_equal(unname(.field_data(out)), c(3, 7))
})

# Pointwise transforms and arithmetic ----------------------------------------

test_that("transforms apply pointwise maps and keep field metadata", {
  f <- mk_fields()
  k <- .cell_values(f$kernel)
  expect_equal(.cell_values(.ae_transform(f$kernel, function(x) x * 2)), k * 2)

  logged <- log(f$kernel)
  expect_s3_class(logged, "spax_raster_field")
  expect_equal(.field_domain(logged), .field_domain(f$kernel))
  expect_equal(.field_layer_index(logged), .field_layer_index(f$kernel))
  expect_equal(.cell_values(logged), log(k))
  expect_equal(.cell_values(sqrt(f$kernel)), sqrt(k))

  a <- .spax_vector_field(c(fac_a = 1, fac_b = 4), domain = "facility",
                          role = "state", meta = list(source = "test"))
  logged <- log(a)
  expect_s3_class(logged, "spax_vector_field")
  expect_equal(unname(.field_data(logged)), log(c(1, 4)))
  expect_equal(unname(.field_data(sqrt(a))), c(1, 2))
  expect_equal(.field_role(logged), "state")
  expect_equal(.field_meta(logged)$source, "test")
})

test_that("Ops.spax_field supports scalar and unary arithmetic in operand order", {
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
  expect_equal(unname(.field_data(-a)), c(-2, -4))
  expect_identical(+a, a)
  expect_equal(.field_role(a + 1), "state")
  expect_equal(.field_meta(a + 1)$source, "test")
  expect_error(a + c(1, 2), "field/scalar")

  f <- mk_fields()
  k <- .cell_values(f$kernel)
  add <- f$kernel + 1
  expect_s3_class(add, "spax_raster_field")
  expect_equal(.field_domain(add), .field_domain(f$kernel))
  expect_equal(.field_layer_index(add), .field_layer_index(f$kernel))
  expect_equal(.cell_values(add), k + 1)
  expect_equal(.cell_values(2 / f$kernel), 2 / k)
  expect_equal(.cell_values(f$kernel ^ 2), k^2)
})

test_that("equal-domain raster combine is cell-by-cell", {
  f <- mk_fields()
  k <- .cell_values(f$kernel)
  expect_equal(.cell_values(.ae_combine(f$kernel, f$kernel, op = `+`)), k + k)
  expect_equal(.cell_values(f$kernel * f$kernel), k * k)
})

test_that("combine and operators align operands by axis tuple, not position", {
  # Each pair stores the same ID -> value map in reversed order; positional
  # alignment would give (3, 3) and (5, 5, 5, 5) below.
  a <- .spax_vector_field(c(fac_a = 1, fac_b = 2), domain = "facility")
  b <- .spax_vector_field(c(fac_b = 20, fac_a = 10), domain = "facility")
  res <- a + b
  expect_s3_class(res, "spax_vector_field")
  expect_equal(.field_axis_values(res, "facility"), c("fac_a", "fac_b"))
  expect_equal(unname(.field_data(res)), c(11, 22))

  a <- .mk_axis_field(c("fac_a", "fac_b"), c(1, 2))
  b <- .mk_axis_field(c("fac_b", "fac_a"), c(2, 1))
  expect_equal(.first_cell(.ae_combine(a, b, op = `+`)), c(2, 4))
  expect_s3_class(a + b, "spax_raster_field")
  expect_equal(.field_domain(a + b), c("I", "facility"))
  expect_equal(.first_cell(a + b), c(2, 4))

  tup <- .prod_tuples()
  a <- .mk_prod_field(tup, c(1, 2, 3, 4))
  b <- .mk_prod_field(tup[4:1, ], c(4, 3, 2, 1))
  expect_equal(.first_cell(.ae_combine(a, b, op = `+`)), c(2, 4, 6, 8))
  expect_equal(.field_domain(a + b), c("I", "J", "mode"))
  expect_equal(.first_cell(a + b), c(2, 4, 6, 8))
})

test_that("operands spanning different axis tuples are rejected", {
  a <- .spax_vector_field(c(fac_a = 1, fac_b = 2), domain = "facility")
  b <- .spax_vector_field(c(fac_a = 10, fac_b = 20, fac_c = 30),
                          domain = "facility")
  expect_error(.ae_combine(a, b, op = `+`), "different axis tuples")
  expect_error(a + b, "different axis tuples")
  expect_error(b + a, "different axis tuples")
  expect_error(
    .mk_axis_field(c("fac_a", "fac_b"), c(1, 2)) +
      .mk_axis_field(c("fac_a", "fac_c"), c(1, 2)),
    "different axis tuples"
  )
})

test_that("a subset-domain vector is broadcast by tuple in operand order (DEC-011)", {
  f <- mk_fields()
  k <- .cell_values(f$kernel)
  scaled <- sweep(k, 2, c(1.5, 2.5), `*`)

  # raster(I, facility) with vector(facility): each layer uses its facility value
  for (out in list(.ae_combine(f$kernel, f$ratios, op = `*`), f$kernel * f$ratios)) {
    expect_s3_class(out, "spax_raster_field")
    expect_equal(.field_domain(out), .field_domain(f$kernel))
    expect_equal(.field_layer_index(out), .field_layer_index(f$kernel))
    expect_equal(.cell_values(out), scaled)
  }
  # Non-commutative ops keep operand order, whichever operand is larger.
  expect_equal(.cell_values(.ae_combine(f$kernel, f$ratios, op = `/`)),
               sweep(k, 2, c(1.5, 2.5), `/`))
  expect_equal(.cell_values(f$ratios / f$kernel), 1 / sweep(k, 2, c(1.5, 2.5), `/`))

  # A vector over a subset of product axes, onto a raster or a vector.
  vj <- .spax_vector_field(c(j2 = 100, j1 = 10), domain = "J")
  prod <- .mk_prod_field(.prod_tuples(), c(1, 2, 3, 4))
  expect_equal(.first_cell(.ae_combine(prod, vj, op = `*`)), c(10, 20, 300, 400))
  vprod <- .spax_vector_field(c(1, 2, 3, 4), domain = c("J", "mode"),
                              frame = .prod_tuples())
  expect_equal(unname(.field_data(.ae_combine(vprod, vj, op = `*`))),
               c(10, 20, 300, 400))
})

test_that("a subset-domain raster is replicated onto the larger raster's layers", {
  big <- .mk_prod_field(.prod_tuples(), c(1, 2, 3, 4))
  sr <- c(terra::rast(nrows = 2, ncols = 2, vals = 10),
          terra::rast(nrows = 2, ncols = 2, vals = 100))
  names(sr) <- c("j1", "j2")
  small <- .spax_raster_field(sr, domain = c("I", "J"))

  # J = j1 layers (1, 2) times 10; J = j2 layers (3, 4) times 100.
  expect_equal(.first_cell(.ae_combine(big, small, op = `*`)), c(10, 20, 300, 400))
  # A cell-axis-only raster multiplies every layer cell by cell.
  cells <- .spax_raster_field(terra::rast(nrows = 2, ncols = 2, vals = c(1, 2, 3, 4)),
                              domain = "I")
  expect_equal(.cell_values(big * cells), outer(c(1, 2, 3, 4), c(1, 2, 3, 4)))
  # Every tuple of the larger raster must be present in the smaller one.
  j1_only <- .spax_raster_field(sr[["j1"]], domain = c("I", "J"))
  expect_error(.ae_combine(big, j1_only, op = `*`), "missing layer tuples")
})

test_that(".ae_combine propagates NA from the raster operand (no lift mask)", {
  local_test_seed(11)
  f <- mk_fields()
  kr <- c(
    terra::rast(nrows = 4, ncols = 4, vals = runif(16)),
    terra::rast(nrows = 4, ncols = 4, vals = runif(16))
  )
  names(kr) <- c("fac_a", "fac_b")
  kr[[1]][1] <- NA
  kernel_na <- .spax_raster_field(kr, domain = c("I", "facility"))

  out <- .ae_combine(kernel_na, f$ratios, op = `*`)
  na_out <- unname(terra::values(is.na(.field_data(out))))
  na_in <- unname(terra::values(is.na(kr)))
  expect_equal(na_out, na_in)   # NA pattern carried by the raster, not imprinted
})

test_that("disjoint domains and unsupported operators are refused", {
  f <- mk_fields()
  # demand lives on I only and the ratios on facility only: combining them
  # would grow the raster (an outer product), which is guarded.
  expect_error(.ae_combine(f$demand, f$ratios, op = `*`), "outer product")
  expect_error(f$demand * f$ratios, "outer product")
  expect_error(f$ratios ^ f$ratios, "not supported")
  expect_error(f$ratios == f$ratios, "unsupported")
  expect_error(f$ratios & TRUE, "unsupported")
  expect_error(cumsum(f$ratios), "unsupported")
})

# FCA verbs -------------------------------------------------------------------

test_that(".ae_gather sums demand-weighted kernels per facility", {
  f <- mk_fields()
  g <- .ae_gather(f$demand, f$kernel)

  expect_s3_class(g, "spax_vector_field")
  expect_equal(.field_axis_values(g, "facility"), c("fac_a", "fac_b"))
  expect_equal(unname(.field_data(g)),
               colSums(as.numeric(.cell_values(f$demand)) * .cell_values(f$kernel)))
  expect_error(.ae_gather(f$kernel, f$kernel), "cell axis")
})

test_that(".ae_spread realizes facility values on cells, aligned by ID", {
  f <- mk_fields()
  # Stored in reverse order: the spread must align by facility ID.
  r <- .spax_vector_field(c(fac_b = 2.5, fac_a = 1.5), domain = "facility")
  s <- .ae_spread(r, f$kernel)

  expect_s3_class(s, "spax_raster_field")
  expect_equal(.field_domain(s), "I")
  expect_equal(as.numeric(.cell_values(s)),
               as.numeric(.cell_values(f$kernel) %*% c(1.5, 2.5)))
})

test_that(".ae_ratio divides zero-safely", {
  num <- .spax_vector_field(c(fac_a = 10, fac_b = 20), domain = "facility")
  den <- .spax_vector_field(c(fac_a = 2, fac_b = 0), domain = "facility")
  expect_equal(unname(.field_data(.ae_ratio(num, den))), c(5, 0))

  num <- .mk_axis_field(c("fac_a", "fac_b"), c(6, 8))
  den <- .mk_axis_field(c("fac_b", "fac_a"), c(0, 3))
  expect_equal(.first_cell(.ae_ratio(num, den)), c(2, 0))
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
  K <- .cell_values(f$kernel)
  raw_U <- colSums(as.numeric(.cell_values(f$demand)) * K)
  expect_equal(as.numeric(.cell_values(A)), as.numeric(K %*% (c(10, 5) / raw_U)))
})

test_that(".ae_normalize makes kernel rows sum to one and infers the cell axis", {
  f <- mk_fields()
  k <- .cell_values(f$kernel)
  n <- .ae_normalize(f$kernel, by = "I", method = "standard")
  expect_equal(.cell_values(n), k / rowSums(k))
  expect_equal(rowSums(.cell_values(n)), rep(1, 16))

  expect_warning(
    inferred <- .ae_normalize(f$kernel, method = "standard"),
    "`by` omitted"
  )
  expect_equal(.cell_values(inferred), .cell_values(n))
  expect_error(.ae_normalize(f$kernel, by = c("I", "I")), "unique")
  expect_error(.ae_normalize(f$kernel, by = "mode"), "subset")
  expect_error(.ae_normalize(f$kernel, by = "I", method = "bogus"), "method")
})

test_that(".ae_normalize groups product-axis raster fields by retained axes", {
  field <- .mk_prod_field(.prod_tuples(), c(1, 2, 3, 4))
  # Layers (j1, car) = 1, (j1, bus) = 2, (j2, car) = 3, (j2, bus) = 4, each
  # constant on four cells. Retained axes define the groups that sum to one:
  # per cell when I is retained, over all cells otherwise.
  cases <- list(
    list(by = c("I", "mode"), first = c(1 / 4, 2 / 6, 3 / 4, 4 / 6),
         groups = list(c(1, 3), c(2, 4))),
    list(by = c("I", "J"), first = c(1 / 3, 2 / 3, 3 / 7, 4 / 7),
         groups = list(1:2, 3:4)),
    list(by = "J", first = c(1 / 12, 2 / 12, 3 / 28, 4 / 28),
         groups = list(1:2, 3:4))
  )
  for (case in cases) {
    normalized <- .ae_normalize(field, by = case$by, method = "standard")
    values <- .cell_values(normalized)
    expect_s3_class(normalized, "spax_raster_field")
    expect_equal(.field_domain(normalized), c("I", "J", "mode"))
    expect_equal(values[1, ], case$first)
    for (g in case$groups) {
      total <- if ("I" %in% case$by) rowSums(values[, g]) else sum(values[, g])
      expect_equal(total, rep(1, length(total)))
    }
  }
})

test_that(".ae_normalize supports vector fields by retained axes", {
  field <- .spax_vector_field(c(1, 2, 3, 4), domain = c("J", "mode"),
                              frame = .prod_tuples())

  by_mode <- .ae_normalize(field, by = "mode", method = "standard")
  expect_s3_class(by_mode, "spax_vector_field")
  expect_equal(unname(.field_data(by_mode)), c(0.25, 1 / 3, 0.75, 2 / 3))

  global <- .ae_normalize(field, method = "standard")
  expect_equal(sum(.field_data(global)), 1)
  expect_equal(unname(.field_data(global)), c(0.1, 0.2, 0.3, 0.4))
})

test_that(".ae_normalize handles outside option and semi normalization by group", {
  field <- .spax_vector_field(c(1, 2, 3, 4), domain = c("J", "mode"),
                              frame = .prod_tuples())

  outside <- .ae_normalize(field, by = "mode", method = "standard", a0 = 1)
  expect_equal(unname(.field_data(outside)), c(0.2, 2 / 7, 0.6, 4 / 7))

  semi_field <- .spax_vector_field(
    c(0.2, 0.8, 0.4, 0.8),
    domain = c("J", "mode"),
    frame = .prod_tuples()
  )
  semi <- .ae_normalize(semi_field, by = "mode", method = "semi")
  expect_equal(unname(.field_data(semi)), c(0.2, 0.5, 0.4, 0.5))
})

# Atoms kept pending a maintainer decision (test audit 2026-10-14): .ae_lift,
# .ae_update and .ae_mask have no caller outside this file's layer. Retiring
# them means deleting this section.

test_that(".ae_lift broadcasts vector fields onto raster and vector templates", {
  f <- mk_fields()
  lifted <- .ae_lift(f$ratios, template = f$kernel)
  expect_s3_class(lifted, "spax_raster_field")
  expect_equal(terra::nlyr(.field_data(lifted)), 2)
  v <- .cell_values(lifted)
  expect_true(all(v[, 1] == 1.5))
  expect_true(all(v[, 2] == 2.5))

  base <- .spax_vector_field(c(j1 = 10, j2 = 20), domain = "J")
  template <- .spax_vector_field(rep(NA_real_, 4), domain = c("J", "mode"),
                                 frame = .prod_tuples(), role = "template")
  lifted <- .ae_lift(base, template)
  expect_s3_class(lifted, "spax_vector_field")
  expect_equal(.field_domain(lifted), c("J", "mode"))
  expect_equal(unname(.field_data(lifted)), c(10, 10, 20, 20))
})

test_that(".ae_lift replicates raster fields across added layer axes", {
  f <- mk_fields()
  lifted <- .ae_lift(f$demand, template = f$kernel)
  expect_equal(terra::nlyr(.field_data(lifted)), 2)
  d <- as.numeric(.cell_values(f$demand))
  expect_equal(.cell_values(lifted), cbind(d, d), ignore_attr = TRUE)

  r <- do.call(c, lapply(c(1, 2), function(v) terra::rast(nrows = 2, ncols = 2, vals = v)))
  names(r) <- c("j1", "j2")
  base <- .spax_raster_field(r, domain = c("I", "J"))
  template <- .mk_prod_field(.prod_tuples(), c(0, 0, 0, 0))
  lifted <- .ae_lift(base, template)
  expect_s3_class(lifted, "spax_raster_field")
  expect_equal(.first_cell(lifted), c(1, 1, 2, 2))
})

test_that(".ae_update applies a damped mix", {
  st <- .spax_vector_field(c(fac_a = 1, fac_b = 1), domain = "facility")
  tg <- .spax_vector_field(c(fac_a = 3, fac_b = 5), domain = "facility")
  up <- .ae_update(st, tg, lambda = 0.5)
  expect_equal(unname(.field_data(up)), c(2, 3))

  expect_error(.ae_update(st, tg, lambda = 0), "lambda")
})

test_that(".ae_mask applies lower-dimensional soft masks explicitly", {
  field <- .mk_prod_field(.prod_tuples(), c(10, 20, 30, 40))
  mask <- .spax_vector_field(
    c(1, 0.5, 0),
    domain = c("J", "mode"),
    frame = .prod_tuples()[1:3, ]
  )
  expect_error(.ae_mask(field, mask), "missing axis tuples")

  mask <- .spax_vector_field(c(1, 0.5, 0, 0.25), domain = c("J", "mode"),
                             frame = .prod_tuples())
  expect_equal(.first_cell(.ae_mask(field, mask)), c(10, 10, 0, 10))
  expect_error(.ae_mask(field, .spax_vector_field(c(1.5), domain = "J",
                                                  frame = data.frame(J = "j1"))),
               "\\[0, 1\\]")
})
