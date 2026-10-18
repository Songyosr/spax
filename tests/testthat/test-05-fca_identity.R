# Identity suite of the block FCA engine (SPAX-055 T2).
#
# (a) Every FCA wrapper gives the same surface under the field engine and the
#     block engine (option spax.fca_engine), on the raw shipped Cornwall
#     example and on the 3 x 3 toy, with and without missing data, for blocks of
#     1 row, 3 rows and the default plan: the same NA pattern and values within
#     1e-12.
# (b) The block engine against the independent base-R fca_oracle() on the full
#     Cornwall matrices.
# (c) The block engine against surfaces computed with the field engine at
#     commit 664b821 and stored in fixtures/fca_cornwall_reference.rds.
#
# Parts (b) and (c) stay when the field engine is removed (T4); part (a) goes.
# Set SPAX_IDENTITY_LOG to a file path to append the largest differences found.

# --- helpers -----------------------------------------------------------------

# Block values that give `rows` raster rows per block when the block plan reads
# the demand layer, the distance layers and `extra` further layers.
block_values_for <- function(d, rows, extra = 0) {
  terra::ncol(d$distance) * (1 + terra::nlyr(d$distance) + extra) * rows
}

# The largest differences between two surfaces, for the log.
fca_difference <- function(field, block) {
  f <- c(field)
  b <- c(block)
  ok <- !is.na(f) & !is.na(b) & f != 0
  c(mean_rel = sum(abs(b - f), na.rm = TRUE) / sum(abs(f), na.rm = TRUE),
    max_cell_rel = if (any(ok)) max(abs(b[ok] - f[ok]) / abs(f[ok])) else 0,
    max_abs_at_zero = max(0, abs(b[!is.na(f) & !is.na(b) & f == 0]), na.rm = TRUE))
}

fca_values <- function(x) {
  unname(terra::values(if (inherits(x, "SpatRaster")) x else x$accessibility,
                       mat = TRUE))
}

# Run `fun` once under the field engine and under the block engine for each
# block size; require the same NA pattern, layer names, facility table and
# parameters, and values within 1e-12.
expect_identity <- function(label, fun, d, extra = 0) {
  field <- withr::with_options(list(spax.fca_engine = "field"), fun())
  field_values <- fca_values(field)
  field_names <- names(if (inherits(field, "SpatRaster")) field else field$accessibility)
  sizes <- list(one_row = block_values_for(d, 1, extra),
                three_rows = block_values_for(d, 3, extra),
                default = NULL)
  for (size in names(sizes)) {
    block <- withr::with_options(
      list(spax.fca_engine = "block", spax.fca_block_values = sizes[[size]]), fun()
    )
    info <- paste(d$label, label, size)
    block_values <- fca_values(block)
    expect_identical(is.na(block_values), is.na(field_values), info = info)
    expect_equal(block_values, field_values, tolerance = 1e-12, info = info)
    expect_identical(
      names(if (inherits(block, "SpatRaster")) block else block$accessibility),
      field_names, info = info
    )
    if (!inherits(field, "SpatRaster")) {
      expect_equal(block$facilities, field$facilities, info = info)
      expect_equal(block$parameters, field$parameters, info = info)
      expect_equal(block$type, field$type, info = info)
    }
    log <- Sys.getenv("SPAX_IDENTITY_LOG")
    if (nzchar(log)) {
      diff <- fca_difference(field_values, block_values)
      cat(sprintf("%s | %s | %s | mean_rel %.3g | max_cell_rel %.3g | max_abs_at_zero %.3g | na %d\n",
                  d$label, label, size, diff[["mean_rel"]], diff[["max_cell_rel"]],
                  diff[["max_abs_at_zero"]], sum(is.na(field_values))),
          file = log, append = TRUE)
    }
  }
  invisible(NULL)
}

# --- data --------------------------------------------------------------------

# The raw shipped Cornwall example: 132 x 271 cells, 55 practices.
cornwall_raw <- function() {
  pop <- read_spax_example("cornwall_pop.tif")
  travel <- read_spax_example("cornwall_gp_travel.tif")
  supply <- sf::st_drop_geometry(cornwall_gp)
  # Keep the practices that have a travel layer (all of them in the shipped
  # example), in the order of the layers.
  supply <- supply[supply$practice %in% names(travel), ]
  list(
    label = "cornwall", demand = pop, distance = travel, supply = supply,
    id_col = "practice", supply_col = "gp_fte_extg",
    supply_cols2 = c("gp_fte_extg", "nurse_fte"),
    sigma = 10, threshold = 15, selection_sigma = 6, plain_threshold = 20,
    inverse_c = 2, attractiveness = seq(1, 3, length.out = terra::nlyr(travel))
  )
}

# Cells from `from` to `n` in steps of `by`; none when `from` is beyond `n`.
cells_from <- function(from, n, by) if (from > n) integer() else seq(from, n, by = by)

# The same data with missing travel pairs, unreachable cells, a few infinite
# distances and missing and zero demand, injected on fixed cell patterns.
with_missing <- function(d) {
  v <- terra::values(d$distance, mat = TRUE)
  n <- nrow(v)
  j <- col(v)
  r <- row(v)
  v[(r + 7 * j) %% 10 == 0] <- NA # about 10% of the pairs
  v[cells_from(1, n, 97), ] <- NA # cells that reach no facility
  v[cells_from(5, n, 211), ] <- Inf # cells at infinite distance from all
  distance <- terra::setValues(d$distance, v)
  p <- terra::values(d$demand, mat = TRUE)[, 1]
  p[cells_from(3, length(p), 53)] <- NA
  p[cells_from(11, length(p), 89)] <- 0
  d$demand <- terra::setValues(d$demand, p)
  d$distance <- distance
  d$label <- paste(d$label, "missing")
  d
}

toy_data <- function() {
  td <- fca_toy()
  list(
    label = "toy", demand = td$demand, distance = td$distance,
    supply = td$supply_df, id_col = "id", supply_col = "doctors",
    supply_cols2 = c("doctors", "nurses"),
    sigma = 2.4, threshold = 3, selection_sigma = 0.9, plain_threshold = 3,
    inverse_c = 0.5, attractiveness = c(2, 1)
  )
}

# --- cases -------------------------------------------------------------------

expect_wrapper_identity <- function(d) {
  demand <- d$demand
  distance <- d$distance
  supply <- d$supply
  id <- d$id_col
  one <- d$supply_col
  two <- d$supply_cols2
  gauss <- list(method = "gaussian", sigma = d$sigma)
  # A supply table in another row order binds by facility id.
  flipped <- supply[rev(seq_len(nrow(supply))), ]
  J <- terra::nlyr(distance)

  expect_identity("2sfca", d = d, function() {
    spax_2sfca(demand, supply, distance, d$threshold, id_col = id, supply_cols = one)
  })
  for (norm in c("identity", "standard", "semi")) {
    expect_identity(paste("e2sfca gaussian", norm), d = d, function() {
      spax_e2sfca(demand, supply, distance, gauss, demand_normalize = norm,
                  id_col = id, supply_cols = one)
    })
  }
  expect_identity("e2sfca flipped supply", d = d, function() {
    spax_e2sfca(demand, flipped, distance, gauss, id_col = id, supply_cols = one)
  })
  expect_identity("e2sfca two supply columns", d = d, function() {
    spax_e2sfca(demand, supply, distance, gauss, demand_normalize = "standard",
                id_col = id, supply_cols = two)
  })
  for (kernel in list(list(method = "exponential", sigma = d$sigma),
                      list(method = "inverse", c = d$inverse_c),
                      list(method = "binary", sigma = d$threshold),
                      list(method = "power", sigma = 1))) {
    # Both normalizations for the smooth kernels, identity for the others.
    norms <- if (kernel$method %in% c("exponential", "inverse")) {
      c("identity", "standard")
    } else {
      "identity"
    }
    for (norm in norms) {
      expect_identity(paste("e2sfca", kernel$method, norm), d = d, function() {
        spax_e2sfca(demand, supply, distance, kernel, demand_normalize = norm,
                    id_col = id, supply_cols = one)
      })
    }
  }
  for (norm in c("identity", "standard")) {
    expect_identity(paste("m2sfca", norm), d = d, function() {
      spax_m2sfca(demand, supply, distance, gauss, demand_normalize = norm,
                  id_col = id, supply_cols = two)
    })
  }
  expect_identity("3sfca selection = decay", d = d, function() {
    spax_3sfca(demand, supply, distance, gauss, id_col = id, supply_cols = one)
  })
  expect_identity("3sfca exponential selection", d = d, function() {
    spax_3sfca(demand, supply, distance, gauss,
               selection_params = list(method = "exponential",
                                       sigma = d$selection_sigma),
               id_col = id, supply_cols = two)
  })
  expect_identity("huff2sfca default attractiveness", d = d, function() {
    spax_huff2sfca(demand, supply, distance, gauss, id_col = id, supply_cols = one)
  })
  expect_identity("huff2sfca attractiveness alpha 2", d = d, function() {
    spax_huff2sfca(demand, supply, distance, gauss, attractiveness = d$attractiveness,
                   alpha = 2, id_col = id, supply_cols = two)
  })

  # Variable catchments: one sigma layer, and one per facility.
  sigma1 <- terra::rast(demand)
  terra::values(sigma1) <- seq(d$sigma * 0.5, d$sigma * 1.6, length.out = terra::ncell(demand))
  factors <- seq(0.7, 1.3, length.out = J)
  sigma_each <- terra::rast(lapply(seq_len(J), function(j) sigma1 * factors[j]))
  names(sigma_each) <- names(distance)
  expect_identity("e2sfca one sigma layer", d = d, extra = 1, function() {
    spax_e2sfca(demand, supply, distance, list(method = "gaussian", sigma = sigma1),
                demand_normalize = "standard", id_col = id, supply_cols = one)
  })
  expect_identity("e2sfca sigma layer per facility", d = d, extra = J, function() {
    spax_e2sfca(demand, supply, distance, list(method = "exponential", sigma = sigma_each),
                id_col = id, supply_cols = one)
  })
  expect_identity("3sfca variable catchments", d = d, extra = J + 1, function() {
    spax_3sfca(demand, supply, distance, list(method = "gaussian", sigma = sigma_each),
               selection_params = list(method = "gaussian", sigma = sigma1),
               id_col = id, supply_cols = one)
  })

  # A plain function written with terra::ifel() sees the whole distance raster.
  plain <- function(distance, sigma = 3) terra::ifel(distance < sigma, 1, 0)
  expect_identity("e2sfca plain ifel function", d = d, extra = J, function() {
    spax_e2sfca(demand, supply, distance,
                list(method = plain, sigma = d$plain_threshold),
                demand_normalize = "standard", id_col = id, supply_cols = one)
  })
  expect_identity("3sfca plain ifel selection", d = d, extra = J, function() {
    spax_3sfca(demand, supply, distance, gauss,
               selection_params = list(method = plain, sigma = d$plain_threshold),
               id_col = id, supply_cols = one)
  })

  # Declared kernels, as demand and as selection method.
  plateau <- decay_kernel(function(distance, sigma, plateau = 2) {
    ifelse(distance <= plateau, 1, exp(-(distance - plateau)^2 / (2 * sigma^2)))
  }, name = "plateau", parameter = "scale", args = list(plateau = 2))
  expect_identity("e2sfca declared kernel", d = d, function() {
    spax_e2sfca(demand, supply, distance, list(method = plateau, sigma = d$sigma),
                demand_normalize = "standard", id_col = id, supply_cols = one)
  })
  expect_identity("huff2sfca declared selection kernel", d = d, function() {
    spax_huff2sfca(demand, supply, distance, gauss,
                   selection_params = list(method = plateau, sigma = d$sigma),
                   id_col = id, supply_cols = one)
  })

  # compute_access(): access weights in another layer order, matched by name.
  w <- calc_decay(distance, "gaussian", sigma = d$sigma)
  w2 <- w^2
  names(w2) <- names(w)
  expect_identity("compute_access reordered access weights", d = d, extra = J, function() {
    compute_access(demand, supply, w, w2[[rev(seq_len(J))]], id_col = id,
                   supply_cols = two)
  })
  expect_identity("compute_access same weights twice", d = d, extra = 0, function() {
    compute_access(demand, supply, w, w, id_col = id, supply_cols = one)
  })
}

# --- tests -------------------------------------------------------------------

test_that("the block engine equals the field engine on the raw Cornwall example", {
  expect_wrapper_identity(cornwall_raw())
})

test_that("the block engine equals the field engine on Cornwall with missing data", {
  # A window of the grid (rows 20-110, columns 60-230) keeps the file quick; the
  # full grid is covered by the raw example above.
  d <- cornwall_raw()
  window <- terra::ext(terra::xFromCol(d$demand, 60) - 1e-9,
                       terra::xFromCol(d$demand, 230) + 1e-9,
                       terra::yFromRow(d$demand, 110) - 1e-9,
                       terra::yFromRow(d$demand, 20) + 1e-9)
  d$demand <- terra::crop(d$demand, window)
  d$distance <- terra::crop(d$distance, window)
  expect_equal(dim(d$demand)[1:2], c(91, 171))
  d <- with_missing(d)
  expect_true(anyNA(terra::values(d$demand)))
  expect_wrapper_identity(d)
})

test_that("the block engine equals the field engine on the toy fixtures", {
  expect_wrapper_identity(toy_data())
  expect_wrapper_identity(with_missing(toy_data()))
})

test_that("the block engine equals the independent base-R oracle on every Cornwall cell", {
  d <- cornwall_raw()
  withr::local_options(spax.fca_engine = "block")
  distance <- terra::values(d$distance, mat = TRUE)
  D <- terra::values(d$demand, mat = TRUE)[, 1]
  S <- d$supply$gp_fte_extg[match(names(d$distance), d$supply$practice)]
  W <- exp(-distance^2 / (2 * d$sigma^2))
  expect_equal(nrow(W), terra::ncell(d$demand))
  for (norm in c("identity", "standard")) {
    got <- spax_e2sfca(d$demand, d$supply, d$distance,
                       list(method = "gaussian", sigma = d$sigma),
                       demand_normalize = norm, id_col = d$id_col,
                       supply_cols = d$supply_col)
    expected <- fca_oracle(D, W, W, S, norm)
    expect_identical(is.na(fca_values(got)), is.na(expected), info = norm)
    expect_equal(fca_values(got), expected, tolerance = 1e-12, info = norm)
  }
})

test_that("the block engine reproduces the field-engine reference surfaces of Cornwall", {
  ref <- readRDS(test_path("fixtures", "fca_cornwall_reference.rds"))
  expect_named(ref, c("2sfca_15", "e2sfca_gaussian10_standard", "3sfca_gaussian10",
                      "huff2sfca_gaussian10"))
  d <- cornwall_raw()
  withr::local_options(spax.fca_engine = "block")
  gauss <- list(method = "gaussian", sigma = 10)
  got <- list(
    `2sfca_15` = spax_2sfca(d$demand, d$supply, d$distance, 15,
                            id_col = d$id_col, supply_cols = d$supply_col),
    e2sfca_gaussian10_standard = spax_e2sfca(
      d$demand, d$supply, d$distance, gauss, demand_normalize = "standard",
      id_col = d$id_col, supply_cols = d$supply_col),
    `3sfca_gaussian10` = spax_3sfca(d$demand, d$supply, d$distance, gauss,
                                    id_col = d$id_col, supply_cols = d$supply_col),
    huff2sfca_gaussian10 = spax_huff2sfca(d$demand, d$supply, d$distance, gauss,
                                          id_col = d$id_col, supply_cols = d$supply_col)
  )
  for (nm in names(ref)) {
    v <- fca_values(got[[nm]])[, 1]
    expect_identical(which(is.na(v)), ref[[nm]]$na, info = nm)
    finite <- v[!is.na(v)]
    expect_length(finite, length(ref[[nm]]$values))
    # Relative to the reference, cell by cell (the reference is rounded to 12
    # significant digits).
    expect_true(all(abs(finite - ref[[nm]]$values) <= 1e-11 * abs(ref[[nm]]$values)),
                info = nm)
  }
})
