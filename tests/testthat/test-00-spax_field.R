make_field_raster <- function(n = 1) {
  rasters <- lapply(seq_len(n), function(i) {
    r <- terra::rast(nrows = 2, ncols = 2)
    terra::values(r) <- rep(i, terra::ncell(r))
    r
  })
  terra::rast(rasters)
}

test_that("raster field constructs a single-layer I field", {
  r <- make_field_raster(1)

  field <- .create_spax_raster_field(r, domain = "I", role = "demand")

  expect_s3_class(field, "spax_raster_field")
  expect_s3_class(field, "spax_field")
  expect_equal(.field_backend(field), "raster")
  expect_equal(.field_domain(field), "I")
  expect_null(.field_axis_values(field, "I"))
  expect_equal(names(.field_data(field)), "L1")
  expect_equal(.field_layer_index(field), data.frame(layer = "L1"))
})

test_that("raster field lifts semantic layer names into a frame", {
  r <- make_field_raster(2)
  names(r) <- c("H1", "H2")

  field <- .create_spax_raster_field(r, domain = c("I", "J"), role = "kernel")

  expect_equal(names(.field_data(field)), c("L1", "L2"))
  expect_equal(
    .field_layer_index(field),
    data.frame(layer = c("L1", "L2"), J = c("H1", "H2"))
  )
  expect_equal(.field_axis_values(field, "J"), c("H1", "H2"))
  expect_match(.field_meta(field)$provenance, "semantic layer names lifted", all = FALSE)
})

test_that("raster field accepts explicit product-axis frame", {
  r <- make_field_raster(4)
  names(r) <- c("raw1", "raw2", "raw3", "raw4")
  frame <- data.frame(
    layer = names(r),
    J = c("H1", "H1", "H2", "H2"),
    mode = c("car", "walk", "car", "walk")
  )

  field <- .create_spax_raster_field(
    r,
    domain = c("I", "J", "mode"),
    index = list(layer = frame),
    role = "distance"
  )

  expect_equal(names(.field_data(field)), paste0("L", 1:4))
  expect_equal(.field_axis_values(field, "J"), c("H1", "H2"))
  expect_equal(.field_axis_values(field, "mode"), c("car", "walk"))
  expect_equal(
    .field_layer_index(field),
    data.frame(
      layer = paste0("L", 1:4),
      J = c("H1", "H1", "H2", "H2"),
      mode = c("car", "walk", "car", "walk")
    )
  )
})

test_that("raster field allows row-order linkage with a real frame", {
  r <- make_field_raster(2)
  names(r) <- c("raw1", "raw2")
  frame <- data.frame(J = c("H1", "H2"))

  expect_warning(
    field <- .create_spax_raster_field(r, domain = c("I", "J"), frame = frame),
    "assuming row order"
  )

  expect_equal(.field_axis_values(field, "J"), c("H1", "H2"))
  expect_match(.field_meta(field)$provenance, "layer linkage assumed", all = FALSE)
})

test_that("raster field binds a keyed frame by join, not row order", {
  r <- make_field_raster(2)
  names(r) <- c("H1", "H2") # layer 1 = H1, layer 2 = H2

  # Correctly keyed, but rows supplied in reversed order.
  frame <- data.frame(
    layer = c("H2", "H1"),
    J = c("valH2", "valH1"),
    stringsAsFactors = FALSE
  )

  field <- .create_spax_raster_field(r, domain = c("I", "J"), frame = frame)

  # L1 is raster layer 1 (H1) and must carry valH1, not valH2.
  expect_equal(
    .field_layer_index(field),
    data.frame(layer = c("L1", "L2"), J = c("valH1", "valH2"))
  )
})

test_that("raster field rejects a provided key that does not match layer names", {
  r <- make_field_raster(2)
  names(r) <- c("H1", "H2")
  frame <- data.frame(
    layer = c("H1", "H3"),
    J = c("a", "b"),
    stringsAsFactors = FALSE
  )

  expect_error(
    .create_spax_raster_field(r, domain = c("I", "J"), frame = frame),
    "must match raster layer names"
  )
})

test_that("raster field coerces axis labels to character and preserves first appearance", {
  r <- make_field_raster(3)
  names(r) <- c("raw1", "raw2", "raw3")
  frame <- data.frame(
    layer = names(r),
    J = c(2, 1, 2),
    scenario = factor(c("b", "a", "c"))
  )

  field <- .create_spax_raster_field(
    r,
    domain = c("I", "J", "scenario"),
    frame = frame
  )

  expect_equal(.field_axis_values(field, "J"), c("2", "1"))
  expect_equal(.field_axis_values(field, "scenario"), c("b", "a", "c"))
})

test_that("raster field rejects invalid raster domains and layer frames", {
  r2 <- make_field_raster(2)
  names(r2) <- c("lyr.1", "lyr.2")

  expect_error(
    .create_spax_raster_field(r2, domain = "J", allow_positional = TRUE),
    "domain must include I"
  )
  expect_error(
    .create_spax_raster_field(r2, domain = "I"),
    "multi-layer raster fields"
  )
  expect_error(
    .create_spax_raster_field(r2, domain = c("I", "J")),
    "semantic axis IDs are required"
  )

  bad_rows <- data.frame(layer = "a", J = "H1")
  expect_error(
    .create_spax_raster_field(r2, domain = c("I", "J"), frame = bad_rows),
    "one row per raster layer"
  )

  duplicate_keys <- data.frame(layer = c("a", "a"), J = c("H1", "H2"))
  expect_error(
    .create_spax_raster_field(r2, domain = c("I", "J"), frame = duplicate_keys),
    "unique values"
  )

  missing_values <- data.frame(layer = c("a", "b"), J = c("H1", NA))
  expect_error(
    .create_spax_raster_field(r2, domain = c("I", "J"), frame = missing_values),
    "must not contain missing"
  )

  duplicate_coordinates <- data.frame(layer = c("a", "b"), J = c("H1", "H1"))
  expect_error(
    .create_spax_raster_field(r2, domain = c("I", "J"), frame = duplicate_coordinates),
    "duplicate coordinate tuples"
  )
})

test_that("raster field can generate positional IDs only when explicitly allowed", {
  r <- make_field_raster(2)
  names(r) <- c("lyr.1", "lyr.2")

  field <- .create_spax_raster_field(
    r,
    domain = c("I", "draw"),
    allow_positional = TRUE
  )

  expect_equal(.field_axis_values(field, "draw"), c("draw_1", "draw_2"))
  expect_match(.field_meta(field)$provenance, "positional semantic IDs", all = FALSE)
})

test_that("vector field provides minimal axis accessor parity", {
  x <- c(H1 = 10, H2 = 20)

  field <- .create_spax_vector_field(x, domain = "J", role = "supply")

  expect_s3_class(field, "spax_vector_field")
  expect_equal(.field_backend(field), "vector")
  expect_equal(.field_axis_values(field, "J"), c("H1", "H2"))
  expect_error(.create_spax_vector_field(c(10, 20), domain = "J"), "must have names")
})

test_that("spax_field print methods expose compact debugging surfaces", {
  r <- make_field_raster(1)
  raster_field <- .create_spax_raster_field(r, domain = "I")
  vector_field <- .create_spax_vector_field(c(H1 = 10), domain = "J")

  expect_output(print(raster_field), "backend: raster")
  expect_output(print(vector_field), "backend: vector")
})
