# spax_field: raster/vector constructors, coercion, rewrap and accessors.

test_that("fields rekey data to canonical L/V keys and expose accessors", {
  single <- .spax_raster_field(field_rast(1), domain = "I", role = "demand")
  expect_equal(class(single), c("spax_raster_field", "spax_field"))
  expect_equal(.field_backend(single), "raster")
  expect_equal(.field_domain(single), "I")
  expect_equal(.field_role(single), "demand")
  expect_null(.field_axis_values(single, "I"))
  expect_equal(.field_layer_index(single), data.frame(layer = "L1"))

  lifted <- .spax_raster_field(field_rast(2, c("H1", "H2")),
                               domain = c("I", "J"), role = "kernel")
  expect_equal(names(.field_data(lifted)), c("L1", "L2"))
  expect_equal(.field_layer_index(lifted),
               data.frame(layer = c("L1", "L2"), J = c("H1", "H2")))
  expect_equal(.field_layer_axes(lifted), "J")
  expect_equal(unname(terra::values(.field_data(lifted))[1, ]), c(1, 2))

  vec <- .spax_vector_field(c(H1 = 10, H2 = 20), domain = "J", role = "supply")
  expect_equal(class(vec), c("spax_vector_field", "spax_field"))
  expect_equal(.field_backend(vec), "vector")
  expect_equal(.field_data(vec), c(V1 = 10, V2 = 20))
  expect_equal(.field_node_index(vec),
               data.frame(key = c("V1", "V2"), J = c("H1", "H2")))

  expect_error(.field_axis_values(vec, "K"), "axis must be in field domain")
})

test_that("raster layer frames come from explicit frames or allowed positions", {
  product <- data.frame(
    layer = c("raw1", "raw2", "raw3", "raw4"),
    J = c("H1", "H1", "H2", "H2"),
    mode = c("car", "walk", "car", "walk")
  )
  field <- .spax_raster_field(
    field_rast(4, product$layer),
    domain = c("I", "J", "mode"),
    index = list(layer = product),
    role = "distance"
  )
  # Raw layer names give way to the field's own keys.
  expect_equal(names(.field_data(field)), .field_layer_index(field)$layer)
  expect_equal(.field_layer_index(field)[-1], product[-1])
  expect_equal(.field_axis_values(field, "J"), c("H1", "H2"))
  expect_equal(.field_axis_values(field, "mode"), c("car", "walk"))

  # Labels become character and keep their first-appearance order.
  coerced <- .spax_raster_field(
    field_rast(3, c("raw1", "raw2", "raw3")),
    domain = c("I", "J", "scenario"),
    frame = data.frame(layer = c("raw1", "raw2", "raw3"), J = c(2, 1, 2),
                       scenario = factor(c("b", "a", "c")))
  )
  expect_equal(.field_axis_values(coerced, "J"), c("2", "1"))
  expect_equal(.field_axis_values(coerced, "scenario"), c("b", "a", "c"))

  # terra's default layer names carry no IDs: positions only on request.
  expect_error(.spax_raster_field(field_rast(2), domain = c("I", "J")),
               "semantic axis IDs are required")
  positional <- .spax_raster_field(field_rast(2), domain = c("I", "draw"),
                                   allow_positional = TRUE)
  expect_equal(.field_axis_values(positional, "draw"), c("draw_1", "draw_2"))
})

test_that("a keyed layer frame binds by join; a keyless one by row order", {
  r <- field_rast(2, c("H1", "H2"))

  # Keyed rows in reversed order still bind to the matching layer.
  keyed <- .spax_raster_field(
    r, domain = c("I", "J"),
    frame = data.frame(layer = c("H2", "H1"), J = c("valH2", "valH1"))
  )
  expect_equal(.field_layer_index(keyed)$J, c("valH1", "valH2"))
  expect_error(
    .spax_raster_field(r, domain = c("I", "J"),
                       frame = data.frame(layer = c("H1", "H3"), J = c("a", "b"))),
    "must match raster layer names"
  )

  expect_warning(
    keyless <- .spax_raster_field(field_rast(2, c("raw1", "raw2")),
                                  domain = c("I", "J"),
                                  frame = data.frame(J = c("H1", "H2"))),
    "assuming row order"
  )
  expect_equal(.field_axis_values(keyless, "J"), c("H1", "H2"))
})

test_that("raster constructor rejects invalid domains and layer frames", {
  r2 <- field_rast(2)
  bad_frame <- function(...) {
    .spax_raster_field(r2, domain = c("I", "J"), frame = data.frame(...))
  }

  expect_error(.spax_raster_field(r2, domain = "J", allow_positional = TRUE),
               "domain must include I")
  expect_error(.spax_raster_field(r2, domain = "I"), "multi-layer raster fields")
  expect_error(bad_frame(layer = "a", J = "H1"), "one row per raster layer")
  expect_error(bad_frame(layer = c("a", "a"), J = c("H1", "H2")), "unique values")
  expect_error(bad_frame(layer = c("a", NA), J = c("H1", "H2")),
               "must not contain missing")
  expect_error(bad_frame(layer = c("a", "b"), J = c("H1", NA)),
               "must not contain missing")
  expect_error(bad_frame(layer = c("a", "b"), J = c("H1", "H1")),
               "duplicate coordinate tuples")
})

test_that("vector fields bind node frames by key or by position", {
  partial <- .spax_vector_field(
    c(10, 4, 20), domain = c("J", "mode"),
    frame = data.frame(J = c("H1", "H1", "H2"), mode = c("car", "walk", "car"))
  )
  expect_equal(names(.field_data(partial)), field_index_frame(partial)$key)
  expect_equal(field_index_frame(partial)[-1],
               data.frame(J = c("H1", "H1", "H2"), mode = c("car", "walk", "car")))
  expect_equal(.field_axis_values(partial, "mode"), c("car", "walk"))

  # A key column is a join key against the value names.
  keyed <- .spax_vector_field(
    c(a = 10, b = 20), domain = "J",
    frame = data.frame(key = c("b", "a"), J = c("Hb", "Ha"))
  )
  expect_equal(.field_node_index(keyed)$J, c("Ha", "Hb"))

  # Named values with a keyless frame override a real ordering: warn.
  expect_warning(
    .spax_vector_field(c(H1 = 10, H2 = 20), domain = "J",
                       frame = data.frame(J = c("H1", "H2"))),
    "assuming row order"
  )
  expect_no_warning(
    .spax_vector_field(c(10, 20), domain = "J", frame = data.frame(J = c("H1", "H2")))
  )
  expect_equal(
    .field_axis_values(.spax_vector_field(c(10, 20), domain = "J",
                                          allow_positional = TRUE), "J"),
    c("J_1", "J_2")
  )
})

test_that("vector constructor rejects invalid values and node frames", {
  bad_frame <- function(x, ...) {
    .spax_vector_field(x, domain = c("J", "mode"), frame = data.frame(...))
  }

  expect_error(.spax_vector_field(c(10, 20), domain = "J"), "must have names")
  expect_error(.spax_vector_field(c(10, 4), domain = c("J", "mode")),
               "multi-axis vector fields")
  expect_error(bad_frame(c(10, 4), J = "H1", mode = "car"),
               "one row per vector element")
  expect_error(bad_frame(c(10, 4), J = c("H1", "H1"), mode = c("car", "car")),
               "duplicate coordinate tuples")
  expect_error(bad_frame(c(10, 4), J = c("H1", NA), mode = c("car", "walk")),
               "must not contain missing")
  expect_error(bad_frame(c(10, 4), key = c("a", NA), J = c("H1", "H2"),
                         mode = c("car", "car")),
               "key must not contain missing")
  expect_error(bad_frame(c(10, 4), key = c("a", "a"), J = c("H1", "H2"),
                         mode = c("car", "car")),
               "key must contain unique values")
  expect_error(bad_frame(c(a = 10, b = 4), key = c("a", "c"), J = c("H1", "H2"),
                         mode = c("car", "car")),
               "key must match vector names")
  expect_error(.spax_vector_field(matrix(1:2), domain = "J",
                                  allow_positional = TRUE),
               "atomic vector")
})

test_that("common validation rejects bad domains, roles and indexes", {
  r1 <- field_rast(1)
  expect_error(.spax_raster_field(r1, domain = character()),
               "domain must be a non-empty")
  expect_error(.spax_raster_field(r1, domain = c("I", "I")),
               "unique axis names")
  expect_error(.spax_vector_field(c(H1 = 1), domain = "J", role = ""),
               "role must be a non-missing")
  expect_warning(.spax_vector_field(c(H1 = 1), domain = "J", role = "banana"),
                 "not in known spax_field roles")
  # The constructors build their own index; the shared validator guards
  # finished objects.
  expect_error(.chck_spax_field(1, domain = "J", index = list(data.frame())),
               "index must be a named list")
  expect_error(.chck_spax_field(NULL, domain = "J"), "data is required")
})

test_that(".as_spax_field coerces raw inputs and passes fields through", {
  raster_field <- .as_spax_field(field_rast(2, c("H1", "H2")),
                                 domain = c("I", "J"), role = "kernel")
  expect_s3_class(raster_field, "spax_raster_field")
  expect_equal(.field_role(raster_field), "kernel")
  expect_equal(.field_axis_values(raster_field, "J"), c("H1", "H2"))

  positional <- .as_spax_field(field_rast(2), domain = c("I", "facility"),
                               role = "kernel", allow_positional = TRUE)
  expect_equal(.field_axis_values(positional, "facility"),
               c("facility_1", "facility_2"))

  vector_field <- .as_spax_field(c(H1 = 10, H2 = 20), domain = "facility",
                                 role = "supply")
  expect_s3_class(vector_field, "spax_vector_field")
  expect_equal(.field_axis_values(vector_field, "facility"), c("H1", "H2"))

  # A prepared field is trusted once its domain matches, in any order.
  expect_identical(.as_spax_field(raster_field, domain = c("J", "I")),
                   raster_field)
  expect_error(.as_spax_field(raster_field, domain = "I"), "field domain")
  expect_error(.as_spax_field(list(1, 2), domain = "J"), "unsupported input type")
})

test_that(".rewrap_field keeps the structure and rekeys new data", {
  raster_field <- .spax_raster_field(field_rast(2, c("H1", "H2")),
                                     domain = c("I", "J"))
  raster_data <- .field_data(raster_field) * 2
  names(raster_data) <- c("raw_a", "raw_b")
  raster_rewrapped <- .rewrap_field(raster_field, raster_data, role = "map")

  expect_s3_class(raster_rewrapped, "spax_raster_field")
  expect_equal(names(.field_data(raster_rewrapped)),
               .field_layer_index(raster_field)$layer)
  expect_equal(field_index_frame(raster_rewrapped),
               field_index_frame(raster_field))
  expect_equal(.field_role(raster_rewrapped), "map")
  expect_error(.rewrap_field(raster_field, raster_data[[1]]),
               "one layer per index row")

  vector_field <- .spax_vector_field(c(H1 = 10, H2 = 20), domain = "J")
  vector_rewrapped <- .rewrap_field(vector_field, c(11, 21))
  expect_equal(.field_data(vector_rewrapped),
               setNames(c(11, 21), .field_node_index(vector_field)$key))
  expect_equal(field_index_frame(vector_rewrapped),
               field_index_frame(vector_field))
  expect_error(.rewrap_field(vector_field, c(1, 2, 3)), "one value per index row")
})

test_that("print shows the backend and the index frame", {
  raster_field <- .spax_raster_field(field_rast(1), domain = "I")
  vector_field <- .spax_vector_field(c(H1 = 10), domain = "J")

  expect_output(expect_invisible(print(raster_field)),
                "backend: raster(.|\n)*frame:")
  expect_output(print(vector_field), "backend: vector(.|\n)*frame:")
})
