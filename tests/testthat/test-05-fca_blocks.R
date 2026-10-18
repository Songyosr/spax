# The block FCA engine (SPAX-055): facility IDs, row blocks, the raster reader,
# the two-pass engine against base-R algebra, the driver's kept errors, and
# each wrapper under the block engine against the field engine.

gauss_block <- function(sigma) {
  function(b, rows) exp(-b$distance[rows, , drop = FALSE]^2 / (2 * sigma^2))
}

test_that(".fca_ids reads layer names and gives positions to terra defaults", {
  expect_identical(.fca_ids(toy_rast(1:4, nlyr = 2, names = c("a", "b"))), c("a", "b"))
  expect_identical(.fca_ids(toy_rast(1:4, nlyr = 3)),
                   c("facility_1", "facility_2", "facility_3"))
  # One real name is enough to keep every name.
  expect_identical(.fca_ids(toy_rast(1:4, nlyr = 2, names = c("lyr.1", "b"))),
                   c("lyr.1", "b"))
  expect_error(.fca_ids(toy_rast(1:4, nlyr = 2, names = c("a", "a"))),
               "duplicate coordinate tuples")
  expect_error(.fca_ids(1:3, "distance"), "distance")
})

test_that(".raster_blocks plans row blocks and aliases without opening anything", {
  k <- toy_rast(1:12, nrows = 3, ncols = 4, nlyr = 3)
  d <- toy_rast(1:12, nrows = 3, ncols = 4)
  rasters <- list(demand = d, distance = k)

  # 4 columns x 4 layers = 16 values per row.
  one <- .raster_blocks(rasters)
  expect_equal(one$starts, 1L)
  expect_equal(one$n, 3L)
  expect_equal(one$ncol, 4L)
  expect_identical(one$same, c(demand = "demand", distance = "distance"))
  two <- .raster_blocks(rasters, max_values = 32)
  expect_equal(two$starts, c(1L, 3L))
  expect_equal(two$n, c(2L, 1L))
  # A budget below one row still gives one row per block.
  tiny <- .raster_blocks(rasters, max_values = 1)
  expect_equal(tiny$starts, 1:3)
  expect_equal(tiny$n, c(1L, 1L, 1L))
  expect_equal(.raster_blocks(rasters, max_values = 1e9)$n, 3L)

  # An aliased raster counts once: 3 layers x 4 columns = 12 values per row.
  aliased <- list(a = k, b = k, c = k)
  expect_identical(.raster_blocks(aliased, max_values = 24)$same,
                   c(a = "a", b = "a", c = "a"))
  expect_equal(.raster_blocks(aliased, max_values = 24)$n, c(2L, 1L))
  # Equal values in a different object are not an alias.
  copy <- list(a = k, b = terra::rast(k))
  terra::values(copy$b) <- terra::values(k)
  expect_identical(.raster_blocks(copy)$same, c(a = "a", b = "b"))
  # The default budget comes from the option.
  withr::local_options(spax.fca_block_values = 1)
  expect_equal(.raster_blocks(rasters)$n, c(1L, 1L, 1L))
})

test_that(".raster_read gives the values of every source, block by block", {
  dir <- withr::local_tempdir()
  d <- toy_rast(c(1:11, NA), nrows = 3, ncols = 4)
  k <- toy_rast(c(1:30, NA, 32:36), nrows = 3, ncols = 4, nlyr = 3,
                names = c("a", "b", "c"))
  f1 <- file.path(dir, "one.tif")
  f2 <- file.path(dir, "two.tif")
  terra::writeRaster(k[[1:2]], f1, datatype = "FLT8S")
  terra::writeRaster(k[[3]], f2, datatype = "INT2S")
  files <- c(terra::rast(f1), terra::rast(f2))

  read_all <- function(rasters, max_values) {
    blocks <- .raster_blocks(rasters, max_values)
    close <- .raster_open(rasters, blocks$same)
    on.exit(close())
    pieces <- lapply(seq_along(blocks$starts), function(i) {
      .raster_read(rasters, blocks$same, blocks$starts[i], blocks$n[i])
    })
    lapply(stats::setNames(names(rasters), names(rasters)), function(nm) {
      do.call(rbind, lapply(pieces, `[[`, nm))
    })
  }
  sources <- list(
    memory = k,
    file = terra::rast(f1),
    combined = files,
    arithmetic = k * 2
  )
  for (nm in names(sources)) {
    x <- sources[[nm]]
    for (max_values in c(1, 24, 1e6)) {
      got <- read_all(list(demand = d, x = x), max_values)
      expect_equal(got$x, terra::values(x, mat = TRUE), ignore_attr = TRUE,
                   info = paste(nm, max_values))
      expect_equal(got$demand, terra::values(d, mat = TRUE), ignore_attr = TRUE,
                   info = paste(nm, max_values))
    }
  }
  expect_true(terra::inMemory(k) && !terra::inMemory(sources$file))

  # The same object twice is read once and shared; a block is a matrix with one
  # row per cell and one column per layer.
  rasters <- list(demand = d, p = k, q = k)
  blocks <- .raster_blocks(rasters, 24)
  b <- .raster_read(rasters, blocks$same, 2, 2)
  expect_identical(names(b), c("demand", "p", "q"))
  expect_identical(b$p, b$q)
  expect_equal(dim(b$p), c(8L, 3L))
  expect_equal(unname(b$p[, 1]), as.numeric(5:12))
  expect_equal(dim(b$demand), c(8L, 1L))
})

test_that(".raster_open closes what it opened, also after a failure", {
  r <- toy_rast(1:12, nrows = 3, ncols = 4, nlyr = 2)
  rasters <- list(a = r, b = r)
  same <- .raster_blocks(rasters)$same
  close <- .raster_open(rasters, same)
  expect_type(close, "closure")
  expect_silent(close())
  expect_silent(close())
  # A raster that cannot be opened does not leave the earlier ones open.
  bad <- list(a = r, b = 1)
  expect_error(.raster_open(bad, c(a = "a", b = "b")))
})

test_that(".fca_blocks matches the base-R algebra for J = 1 and J = 3", {
  for (J in c(1L, 3L)) {
    f <- fca_block_fixture(J)
    W <- exp(-f$dist^2 / (2 * 2.5^2))
    for (norm in c("identity", "standard", "semi")) {
      for (max_values in list(NULL, 1, 20)) {
        withr::local_options(spax.fca_block_values = max_values)
        got <- .fca_blocks(
          rasters = list(demand = f$demand, distance = f$distance),
          kernels = list(demand = gauss_block(2.5), access = gauss_block(2.5)),
          supply = f$S, normalize = norm
        )
        expect_equal(got, fca_oracle(f$D, W, W, f$S, norm), tolerance = 1e-12,
                     info = paste(J, norm, max_values))
        expect_identical(is.na(got), is.na(fca_oracle(f$D, W, W, f$S, norm)))
      }
    }
  }
})

test_that(".fca_blocks keeps its cell rules and uses separate access kernels", {
  f <- fca_block_fixture(3)
  W <- exp(-f$dist^2 / (2 * 2.5^2))
  # Cell 9 reaches no facility (NA); cell 3 has a missing weight for facility 1
  # that counts as 0; the cell with missing demand (7) still gets access.
  got <- .fca_blocks(
    list(demand = f$demand, distance = f$distance),
    list(demand = gauss_block(2.5), access = gauss_block(2.5)), f$S
  )
  expect_true(all(is.na(got[9, ])))
  expect_false(anyNA(got[7, ]))
  expect_false(anyNA(got[3, ]))
  # An access kernel that differs from the demand kernel (M2SFCA).
  sq <- function(b, rows) gauss_block(2.5)(b, rows)^2
  m2 <- .fca_blocks(
    list(demand = f$demand, distance = f$distance),
    list(demand = gauss_block(2.5), access = sq), f$S
  )
  expect_equal(m2, fca_oracle(f$D, W, W^2, f$S), tolerance = 1e-12)
  # A weight of 0 is finite: a row of zero weights has access 0, not NA.
  zero <- function(b, rows) 0 * b$distance[rows, , drop = FALSE] + 0
  z <- .fca_blocks(list(demand = f$demand, distance = f$distance),
                   list(demand = zero, access = zero), f$S)
  expect_true(all(z[-9, ] == 0))
  expect_true(all(is.na(z[9, ])))
  # A facility that no demand reaches gets ratio 0.
  Kd0 <- function(b, rows) {
    K <- gauss_block(2.5)(b, rows)
    K[, 2] <- 0
    K
  }
  r0 <- .fca_blocks(list(demand = f$demand, distance = f$distance),
                    list(demand = Kd0, access = gauss_block(2.5)), f$S)
  Wd <- W
  Wd[, 2] <- 0
  expect_equal(r0, fca_oracle(f$D, Wd, W, f$S), tolerance = 1e-12)
  # A kernel of the wrong shape is refused.
  expect_error(
    .fca_blocks(list(demand = f$demand, distance = f$distance),
                list(demand = function(b, rows) b$distance[rows, 1, drop = FALSE],
                     access = gauss_block(2.5)), f$S),
    "one row per cell and one column per facility"
  )
})

test_that(".fca_rewrap puts columns on the grid with the given names", {
  tmpl <- toy_rast(1:12, nrows = 3, ncols = 4, nlyr = 3, names = c("a", "b", "c"))
  v <- cbind(1:12, c(NA, 2:12))
  out <- .fca_rewrap(v, tmpl, c("x", "y"))
  expect_identical(names(out), c("x", "y"))
  expect_true(terra::compareGeom(out, tmpl[[1]], stopOnError = FALSE))
  expect_equal(unname(terra::values(out, mat = TRUE)), v)
  expect_equal(terra::nlyr(.fca_rewrap(v[, 1, drop = FALSE], tmpl, "x")), 1)
})

test_that(".compute_fca_blocks binds supply, names layers and checks its inputs", {
  f <- fca_block_fixture(3)
  kernels <- list(demand = gauss_block(2.5), access = gauss_block(2.5))
  W <- exp(-f$dist^2 / (2 * 2.5^2))
  run <- function(...) {
    .compute_fca_blocks(f$demand, rasters = list(distance = f$distance),
                        kernels = kernels, ...)
  }

  matrix_out <- run(supply = f$S)
  expect_s4_class(matrix_out, "SpatRaster")
  expect_identical(names(matrix_out), c("a", "b"))
  expect_equal(unname(terra::values(matrix_out, mat = TRUE)),
               fca_oracle(f$D, W, W, f$S), tolerance = 1e-12)
  expect_identical(names(run(supply = f$S, indicator_names = c("p", "q"))), c("p", "q"))
  expect_identical(names(run(supply = f$S[, 1])), "supply")

  # Supply binds by name for a data frame and a named vector, by position for a
  # matrix without row names and an unnamed vector.
  df <- data.frame(id = rev(f$ids), a = rev(f$S[, "a"]), b = rev(f$S[, "b"]))
  by_id <- run(supply = df, id_col = "id", supply_cols = c("a", "b"))
  expect_equal(terra::values(by_id), terra::values(matrix_out))
  named <- run(supply = rev(f$S[, "a"]))
  expect_equal(terra::values(named)[, 1], terra::values(matrix_out)[, 1])
  by_pos <- run(supply = unname(f$S))
  expect_identical(names(by_pos), c("measure_1", "measure_2"))
  expect_equal(unname(terra::values(by_pos)), unname(terra::values(matrix_out)))
  expect_error(run(supply = c(f1 = 1, f2 = 2, zz = 3)),
               "Some weight layer IDs not found")

  # Facilities without layer names are numbered by position.
  numbered <- f$distance
  names(numbered) <- paste0("lyr.", 1:3)
  out <- .compute_fca_blocks(f$demand, rasters = list(distance = numbered),
                             kernels = kernels,
                             supply = c(facility_1 = 5, facility_2 = 8, facility_3 = 11))
  expect_equal(terra::values(out)[, 1], unname(terra::values(
    run(supply = c(f1 = 5, f2 = 8, f3 = 11)))[, 1]))

  # Kept errors, which `snap` does not remove.
  dup <- f$distance
  names(dup) <- c("a", "a", "b")
  for (snap in c(FALSE, TRUE)) {
    expect_error(.compute_fca_blocks(f$demand, f$S, list(distance = dup), kernels,
                                     snap = snap),
                 "duplicate coordinate tuples")
    expect_error(
      .compute_fca_blocks(c(f$demand, f$demand), f$S, list(distance = f$distance),
                          kernels, snap = snap),
      "multi-layer raster fields must declare at least one non-I axis"
    )
  }
  expect_error(run(supply = f$S, indicator_names = "only_one"),
               "indicator_names must have one name")
  expect_error(run(supply = f$S[1:2, ]), "one row for each facility")
  # The geometry check is unconditional.
  wide <- toy_rast(1:20, nrows = 4, ncols = 5)
  expect_error(.compute_fca_blocks(wide, f$S, list(distance = f$distance), kernels,
                                   snap = TRUE),
               "same number of rows and columns")
  expect_error(.compute_fca_blocks(f$demand, f$S,
                                   list(distance = f$distance, sigma = wide), kernels,
                                   snap = TRUE),
               "demand and sigma must have the same number of rows and columns")
})

test_that("compute_access under the block engine aligns kernels by layer name", {
  withr::local_options(spax.fca_engine = "block")
  f <- fca_block_fixture(3)
  W <- exp(-f$dist^2 / (2 * 2.5^2))
  k <- terra::setValues(f$distance, W)
  names(k) <- f$ids

  expected <- fca_oracle(f$D, W, W, f$S)
  same <- compute_access(f$demand, f$S, k, k)
  expect_identical(names(same), c("a", "b"))
  expect_equal(unname(terra::values(same, mat = TRUE)), expected, tolerance = 1e-12)

  # The access weights in another layer order are matched by name.
  shuffled <- compute_access(f$demand, f$S, k, k[[3:1]])
  expect_equal(terra::values(shuffled), terra::values(same), tolerance = 1e-12)
  W2 <- W[, 3:1]
  asym <- terra::setValues(k, W^2)
  expect_equal(
    unname(terra::values(compute_access(f$demand, f$S, k, asym[[c(2, 3, 1)]]),
                         mat = TRUE)),
    fca_oracle(f$D, W, W^2, f$S), tolerance = 1e-12
  )

  # The same object twice is one alias.
  rasters <- list(demand = f$demand, demand_weights = k, access_weights = k)
  expect_identical(.raster_blocks(rasters)$same[["access_weights"]], "demand_weights")

  other <- k
  names(other) <- c("f1", "f2", "other")
  expect_error(compute_access(f$demand, f$S, k, other),
               "demand and access kernels must span the same facility ids")
  expect_error(compute_access(f$demand, f$S, k, other, snap = TRUE),
               "demand and access kernels must span the same facility ids")
})

test_that("the wrappers keep their errors and names under the block engine", {
  withr::local_options(spax.fca_engine = "block")
  td <- fca_toy()

  # Facility IDs: terra's default names become facility_N and bind by name,
  # data-frame ID or position.
  numbered <- td$distance
  names(numbered) <- c("lyr.1", "lyr.2")
  by_position <- spax_e2sfca(td$demand, unname(td$supply_vector), numbered)
  by_name <- spax_e2sfca(td$demand, c(facility_2 = 15, facility_1 = 10), numbered)
  by_id <- spax_e2sfca(td$demand,
                       data.frame(id = c("facility_2", "facility_1"), s = c(15, 10)),
                       numbered, id_col = "id", supply_cols = "s")
  expect_equal(terra::values(by_name$accessibility),
               terra::values(by_position$accessibility))
  expect_equal(unname(terra::values(by_id$accessibility)),
               unname(terra::values(by_position$accessibility)))
  expect_error(spax_e2sfca(td$demand, c(facility_2 = 15, other = 10), numbered),
               "Some weight layer IDs not found")

  # Duplicate layer names and multi-layer demand, also with snap.
  dup <- td$distance
  names(dup) <- c("a", "a")
  multi <- c(td$demand, td$demand)
  for (snap in c(FALSE, TRUE)) {
    expect_error(spax_e2sfca(td$demand, c(1, 2), dup, snap = snap),
                 "duplicate coordinate tuples")
    expect_error(spax_e2sfca(multi, td$supply_vector, td$distance, snap = snap),
                 "multi-layer raster fields must declare at least one non-I axis")
  }

  # indicator_names of the wrong length.
  expect_error(spax_e2sfca(td$demand, td$supply_matrix, td$distance,
                           indicator_names = "x"),
               "indicator_names")
  # Output layer names.
  named <- spax_e2sfca(td$demand, td$supply_matrix, td$distance,
                       indicator_names = c("d_access", "n_access"))
  expect_identical(names(named$accessibility), c("d_access", "n_access"))
  expect_identical(names(spax_e2sfca(td$demand, td$supply_matrix,
                                     td$distance)$accessibility),
                   c("doctors", "nurses"))
  expect_identical(names(spax_e2sfca(td$demand, td$supply_df, td$distance, id_col = "id",
                                     supply_cols = "nurses")$accessibility), "nurses")
  expect_identical(names(spax_e2sfca(td$demand, td$supply_vector,
                                     td$distance)$accessibility), "supply")
  expect_identical(names(spax_e2sfca(td$demand, unname(td$supply_matrix),
                                     td$distance)$accessibility),
                   c("measure_1", "measure_2"))

  # Negative selection weights are an error unless `snap`.
  negative <- list(method = function(distance, sigma = 1) 1 - distance)
  expect_error(spax_3sfca(td$demand, td$supply_vector, td$distance, negative),
               "weights must be non-negative")
  expect_error(spax_huff2sfca(td$demand, td$supply_vector, td$distance, negative),
               "weights must be non-negative")
  expect_silent(spax_3sfca(td$demand, td$supply_vector, td$distance, negative,
                           snap = TRUE))

  # A vector sigma under snap is refused (without snap it already was).
  expect_error(spax_e2sfca(td$demand, td$supply_vector, td$distance,
                           list(method = "gaussian", sigma = c(1, 2)), snap = TRUE),
               "sigma must be a single number or a SpatRaster")
  expect_error(spax_e2sfca(td$demand, td$supply_vector, td$distance,
                           list(method = "gaussian", sigma = c(1, 2))),
               "sigma")
  # A raster sigma with a declared kernel is refused as today.
  plateau <- decay_kernel(function(distance, sigma) exp(-distance / sigma),
                          name = "plateau", parameter = "scale")
  sig <- terra::rast(td$demand)
  terra::values(sig) <- 2
  expect_error(spax_e2sfca(td$demand, td$supply_vector, td$distance,
                           list(method = plateau, sigma = sig)),
               "a variable catchment needs a built-in method or a plain function")
  # The facility table is built as before.
  expect_equal(spax_e2sfca(td$demand, td$supply_df, td$distance, id_col = "id",
                           supply_cols = c("doctors", "nurses"))$facilities,
               td$supply_df, ignore_attr = TRUE)
})

expect_same_fca <- function(fun, block_values = NULL, info = NULL) {
  both <- fca_engines(fun, block_values)
  as_values <- function(x) {
    unname(terra::values(if (inherits(x, "SpatRaster")) x else x$accessibility,
                         mat = TRUE))
  }
  nm <- function(x) names(if (inherits(x, "SpatRaster")) x else x$accessibility)
  field <- as_values(both$field)
  block <- as_values(both$block)
  expect_identical(is.na(block), is.na(field), info = info)
  expect_equal(block, field, tolerance = 1e-12, info = info)
  expect_identical(nm(both$block), nm(both$field), info = info)
  if (!inherits(both$field, "SpatRaster")) {
    expect_equal(both$block$facilities, both$field$facilities, info = info)
    expect_equal(both$block$parameters, both$field$parameters, info = info)
    expect_equal(both$block$type, both$field$type, info = info)
  }
  invisible(both)
}

test_that("each wrapper gives the same surface under the block and field engines", {
  td <- fca_toy()
  d <- terra::values(td$distance, mat = TRUE)
  d[1, 1] <- NA # cell 1 does not reach facility1
  d[9, ] <- NA # cell 9 reaches no facility
  distance <- terra::setValues(td$distance, d)
  demand <- td$demand
  demand[5] <- NA
  decay <- list(method = "gaussian", sigma = 2.4)

  for (block_values in list(NULL, 1, 6)) {
    info <- paste("block values", format(block_values))
    expect_same_fca(function() spax_2sfca(demand, td$supply_matrix, distance, 3),
                    block_values, info)
    for (norm in c("identity", "standard", "semi")) {
      expect_same_fca(function() spax_e2sfca(demand, td$supply_matrix, distance, decay,
                                             demand_normalize = norm),
                      block_values, paste(info, norm))
      expect_same_fca(function() spax_m2sfca(demand, td$supply_df, distance, decay,
                                             demand_normalize = norm, id_col = "id",
                                             supply_cols = c("nurses", "doctors")),
                      block_values, paste(info, norm))
    }
    expect_same_fca(function() spax_3sfca(demand, td$supply_vector, distance, decay),
                    block_values, info)
    expect_same_fca(function() spax_3sfca(
      demand, td$supply_matrix, distance, decay,
      selection_params = list(method = "exponential", sigma = 0.4)),
      block_values, info)
    expect_same_fca(function() spax_huff2sfca(demand, td$supply_vector, distance,
                                              decay, alpha = 1.5),
                    block_values, info)
    expect_same_fca(function() spax_huff2sfca(
      demand, td$supply_matrix, distance, decay, attractiveness = c(2, 1),
      selection_params = list(method = "power", sigma = 1)),
      block_values, info)
    w <- calc_decay(distance, "gaussian", sigma = 2)
    expect_same_fca(function() compute_access(demand, td$supply_matrix, w, w^2),
                    block_values, info)
    expect_same_fca(function() compute_access(demand, td$supply_matrix, w, w[[2:1]]),
                    block_values, info)
  }
})

test_that("variable catchments, plain functions and declared kernels agree under both engines", {
  td <- fca_toy()
  demand <- td$demand
  sigma1 <- terra::rast(demand)
  terra::values(sigma1) <- seq(1.5, 4, length.out = 9)
  sigma2 <- c(sigma1, sigma1 * 0.8)
  names(sigma2) <- names(td$distance)
  ids <- names(td$distance)

  for (block_values in list(NULL, 1, 6)) {
    run <- function(decay, fun = spax_e2sfca, supply = td$supply_matrix, ...) {
      expect_same_fca(
        function() fun(demand, supply, td$distance, decay, ...),
        block_values, paste(format(block_values), format(decay$method)[1])
      )
    }
    # One sigma layer, or one per facility, with a built-in method.
    run(list(method = "gaussian", sigma = sigma1))
    run(list(method = "exponential", sigma = sigma2))
    run(list(method = "binary", sigma = sigma2), spax_m2sfca)
    run(list(method = "gaussian", sigma = sigma2), spax_3sfca,
        selection_params = list(method = "gaussian", sigma = sigma1))
    run(list(method = decay_kernel("gaussian"), sigma = sigma1))
    # A plain function sees the whole distance raster as today.
    plain <- function(distance, sigma = 3) terra::ifel(distance < sigma, 1, 0)
    run(list(method = plain, sigma = 3))
    run(list(method = plain, sigma = 3), spax_3sfca,
        selection_params = list(method = "gaussian", sigma = 2))
    run(list(method = "gaussian", sigma = 2), spax_huff2sfca, td$supply_vector,
        selection_params = list(method = plain, sigma = 4))
    with_raster <- function(distance, sigma) exp(-distance / sigma)
    run(list(method = with_raster, sigma = sigma1))
    # Declared kernels, as demand and selection method.
    plateau <- decay_kernel(function(distance, sigma, plateau = 2) {
      ifelse(distance <= plateau, 1, exp(-(distance - plateau)^2 / (2 * sigma^2)))
    }, name = "plateau", parameter = "scale", args = list(plateau = 2))
    run(list(method = plateau, sigma = 2), spax_3sfca)
    run(list(method = "gaussian", sigma = 2.4), spax_huff2sfca, td$supply_vector,
        selection_params = list(method = plateau, sigma = 2))
    # The inverse kernel takes its constant through the parameter list.
    run(list(method = "inverse", c = 0.5))
  }
})

test_that("snap leaves the block results unchanged on valid inputs", {
  td <- fca_toy()
  decay <- list(method = "gaussian", sigma = 2.4)
  withr::local_options(spax.fca_engine = "block")
  checked <- spax_3sfca(td$demand, td$supply_matrix, td$distance, decay)
  snapped <- spax_3sfca(td$demand, td$supply_matrix, td$distance, decay, snap = TRUE)
  expect_equal(terra::values(snapped$accessibility),
               terra::values(checked$accessibility))
})

# Decisions D1 and D2 of SPAX-055 ------------------------------------------

d1_message <- paste0(
  "decay weights must be finite where travel is finite (power decay is ",
  "infinite at distance 0); use a positive minimum distance or another kernel"
)

# The toy distance raster with a zero distance in the first cell of the first
# facility, and a missing one in the second cell.
fca_toy_zero <- function() {
  td <- fca_toy()
  v <- terra::values(td$distance, mat = TRUE)
  v[1, 1] <- 0
  v[2, 2] <- NA
  td$distance <- terra::setValues(td$distance, v)
  td
}

test_that("D1 refuses non-finite decay weights at finite distances", {
  td <- fca_toy_zero()
  inv_power <- decay_kernel(function(distance, sigma) distance^(-sigma),
                            name = "invpow", parameter = "exponent",
                            zero_distance = FALSE)
  cases <- list(
    builtin = list(method = "power", sigma = 2),
    inverse = list(method = "inverse", c = 0),
    declared_builtin = list(method = decay_kernel("power"), sigma = 2),
    declared = list(method = inv_power, sigma = 2),
    plain = list(method = function(distance) 1 / distance)
  )
  funs <- list(spax_e2sfca, spax_m2sfca, spax_3sfca)
  for (case in names(cases)) {
    for (snap in c(FALSE, TRUE)) {
      for (fun in funs) {
        expect_error(
          fun(td$demand, td$supply_matrix, td$distance, cases[[case]], snap = snap),
          d1_message, fixed = TRUE, info = paste(case, snap)
        )
      }
      expect_error(
        spax_huff2sfca(td$demand, td$supply_vector, td$distance, cases[[case]],
                       selection_params = list(method = "gaussian", sigma = 2),
                       snap = snap),
        d1_message, fixed = TRUE, info = paste(case, snap)
      )
    }
  }
  # Also with small blocks, and with an infinite distance (an absent edge)
  # and a missing one, which are not checked.
  withr::local_options(spax.fca_block_values = 1)
  expect_error(
    spax_e2sfca(td$demand, td$supply_vector, td$distance,
                list(method = "power", sigma = 2)),
    d1_message, fixed = TRUE
  )
  ok <- fca_toy()
  v <- terra::values(ok$distance, mat = TRUE)
  v[1, 1] <- Inf
  v[2, 2] <- NA
  ok$distance <- terra::setValues(ok$distance, v)
  res <- spax_e2sfca(ok$demand, ok$supply_vector, ok$distance,
                     list(method = "power", sigma = 2))
  expect_true(all(is.finite(terra::values(res$accessibility)[-1, ][
    !is.na(terra::values(res$accessibility)[-1, ])])))
})

test_that("D1 acts on the selection kernel of 3SFCA and Huff", {
  td <- fca_toy_zero()
  # Decay weights are finite everywhere; the selection weights are not.
  decay <- list(method = "gaussian", sigma = 2.4)
  selection <- list(method = "power", sigma = 2)
  for (snap in c(FALSE, TRUE)) {
    expect_error(spax_3sfca(td$demand, td$supply_matrix, td$distance, decay,
                            selection_params = selection, snap = snap),
                 d1_message, fixed = TRUE)
    expect_error(spax_huff2sfca(td$demand, td$supply_vector, td$distance, decay,
                                selection_params = selection, snap = snap),
                 d1_message, fixed = TRUE)
  }
  # A positive minimum distance is the remedy.
  td$distance <- terra::ifel(td$distance < 0.5, 0.5, td$distance)
  expect_no_error(spax_3sfca(td$demand, td$supply_matrix, td$distance, decay,
                             selection_params = selection))
})

test_that("compute_access refuses infinite user weights and allows missing ones", {
  f <- fca_block_fixture(3)
  W <- exp(-f$dist^2 / (2 * 2.5^2))
  k <- terra::setValues(f$distance, W)
  names(k) <- f$ids
  for (bad in c(Inf, -Inf)) {
    for (snap in c(FALSE, TRUE)) {
      inf_demand <- terra::setValues(k, replace(W, 2, bad))
      names(inf_demand) <- f$ids
      expect_error(compute_access(f$demand, f$S, inf_demand, k, snap = snap),
                   "weights must be finite or NA", fixed = TRUE)
      expect_error(compute_access(f$demand, f$S, k, inf_demand, snap = snap),
                   "weights must be finite or NA", fixed = TRUE)
    }
  }
  # A missing weight is allowed (it counts as 0 in a reachable cell).
  Wna <- W
  Wna[3, 1] <- NA
  kna <- terra::setValues(k, Wna)
  names(kna) <- f$ids
  res <- compute_access(f$demand, f$S, kna, kna)
  expected <- fca_oracle(f$D, Wna, Wna, f$S)
  expect_equal(unname(terra::values(res, mat = TRUE)), expected, tolerance = 1e-12)
})

test_that("D2 refuses negative or infinite demand unless snap", {
  td <- fca_toy()
  decay <- list(method = "gaussian", sigma = 2.4)
  msg <- "demand must be finite and nonnegative; use NA for unknown"
  with_cell <- function(value, cell = 9) {
    v <- terra::values(td$demand, mat = FALSE)
    v[cell] <- value
    terra::setValues(td$demand, v)
  }
  funs <- list(spax_e2sfca, spax_m2sfca, spax_3sfca)
  for (value in c(-5, Inf, -Inf)) {
    bad <- with_cell(value)
    for (fun in funs) {
      expect_error(fun(bad, td$supply_matrix, td$distance, decay), msg, fixed = TRUE)
    }
    # The check covers every row of the block, also with one-row blocks.
    withr::with_options(list(spax.fca_block_values = 1), {
      expect_error(spax_e2sfca(bad, td$supply_matrix, td$distance, decay),
                   msg, fixed = TRUE)
    })
    # Under snap the cell is not active: the result is that of a missing cell.
    missing_cell <- with_cell(NA_real_)
    for (fun in funs) {
      snapped <- fun(bad, td$supply_matrix, td$distance, decay, snap = TRUE)
      reference <- fun(missing_cell, td$supply_matrix, td$distance, decay,
                       snap = TRUE)
      expect_equal(terra::values(snapped$accessibility),
                   terra::values(reference$accessibility), tolerance = 1e-12)
    }
  }
  expect_error(spax_2sfca(with_cell(-5), td$supply_matrix, td$distance, 3), msg,
               fixed = TRUE)
  # Zero demand and missing demand stay valid.
  expect_no_error(spax_e2sfca(with_cell(0), td$supply_matrix, td$distance, decay))
  expect_no_error(spax_e2sfca(with_cell(NA_real_), td$supply_matrix, td$distance, decay))
  # compute_access has the same rule.
  f <- fca_block_fixture(3)
  W <- exp(-f$dist^2 / (2 * 2.5^2))
  k <- terra::setValues(f$distance, W)
  names(k) <- f$ids
  bad <- terra::setValues(f$demand, replace(terra::values(f$demand, mat = FALSE), 1, -1))
  expect_error(compute_access(bad, f$S, k, k), msg, fixed = TRUE)
})

test_that("a missing sigma cell gives no weight, as in the field engine", {
  f <- fca_toy()
  sigma <- terra::rast(f$demand)
  terra::values(sigma) <- 8
  sigma[2] <- NA
  params <- list(method = "gaussian", sigma = sigma)
  run <- function(engine) {
    withr::local_options(spax.fca_engine = engine)
    terra::values(spax_e2sfca(f$demand, f$supply_vector, f$distance,
                              decay_params = params)$accessibility)
  }
  block <- expect_no_error(run("block"))
  field <- run("field")
  expect_identical(unname(is.na(block)), unname(is.na(field)))
  expect_equal(unname(block), unname(field), tolerance = 1e-12)
})
