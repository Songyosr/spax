# FCA indices on numeric data (SPAX-060, V3): spax(prep_data(demand, supply, travel),
# spec_<index>()) runs the engine of the raster route on the active origins, fed from
# memory. The oracle is the raster route itself: at every active cell the access values of
# the numeric data equal those of the rasters, with the same cells missing. The Cornwall
# cases read the shipped example grids.

expect_error_exact <- function(object, message) {
  # the whole message, not a part of it
  got <- tryCatch({ object; NULL }, error = function(e) conditionMessage(e))
  expect_identical(got, message)
}

# Cornwall: the raster data and the numeric data made from it in two ways (the
# converter of allocation models, and prep_data() on the extracted values), memoized.
.numeric_cache <- new.env(parent = emptyenv())
cornwall_numeric <- function() {
  if (!is.null(.numeric_cache$cornwall)) return(.numeric_cache$cornwall)
  pop <- read_spax_example("cornwall_pop.tif")
  travel <- read_spax_example("cornwall_gp_travel.tif")
  supply <- sf::st_drop_geometry(cornwall_gp)
  raster <- prep_data(pop, supply, travel, id_col = "practice", supply_cols = "gp_fte_extg")
  converted <- .raster_to_origins(raster, spec_clm())
  # prep_data() on the values of every cell: unknown demand is excluded and missing
  # travel is an absent pair, the semantics of the raster route.
  demand <- terra::values(pop)[, 1]
  distance <- terra::values(travel, mat = TRUE)
  ids <- as.character(seq_along(demand))
  names(demand) <- ids
  rownames(distance) <- ids
  prepared <- prep_data(demand, stats::setNames(supply$gp_fte_extg[match(names(travel),
                                                                         supply$practice)],
                                                names(travel)),
                        distance, missing_demand = "exclude", missing_travel = "legacy_zero")
  .numeric_cache$cornwall <- list(raster = raster, converted = converted, prepared = prepared)
}

# The access of the raster route at the active cells of the numeric data, as a vector.
raster_values_at <- function(result, cells, layer = 1L) {
  unname(terra::values(result$accessibility, mat = TRUE)[cells, layer])
}

fca_specs <- function() {
  list(
    e2sfca = spec_e2sfca(decay_gaussian(10)),
    e2sfca_standard = spec_e2sfca(decay_gaussian(10), normalize = "standard"),
    e2sfca_semi = spec_e2sfca(decay_exponential(6), normalize = "semi"),
    m2sfca = spec_m2sfca(decay_gaussian(10)),
    `3sfca` = spec_3sfca(decay_gaussian(10)),
    `3sfca_selection` = spec_3sfca(decay_gaussian(10), selection = decay_exponential(6)),
    huff2sfca = spec_huff2sfca(decay_gaussian(10)),
    huff2sfca_alpha = spec_huff2sfca(decay_gaussian(10), alpha = 1.5),
    `2sfca` = spec_2sfca(15),
    inverse = spec_e2sfca(decay_inverse()),
    step = spec_m2sfca(decay_step(20))
  )
}

# the oracle on Cornwall ----------------------------------------------------------------

test_that("numeric access equals raster access at every active cell, with the same cells missing", {
  skip_on_cran()
  k <- cornwall_numeric()
  cells <- k$converted$cell_index
  expect_length(cells, 6985L)
  for (nm in names(fca_specs())) {
    spec <- fca_specs()[[nm]]
    want <- raster_values_at(spax(k$raster, spec), cells)
    for (source in c("converted", "prepared")) {
      data <- k[[source]]
      got <- spax(data, spec)
      expect_identical(unname(is.na(got$access[, 1L])), is.na(want), info = paste(nm, source))
      expect_equal(unname(got$access[, 1L]), want, tolerance = 1e-12, info = paste(nm, source))
    }
  }
})

test_that("the cells that reach no practice are missing in both modes, 51 of them in Cornwall", {
  skip_on_cran()
  k <- cornwall_numeric()
  got <- spax(k$converted, spec_e2sfca(decay_gaussian(10)))
  expect_equal(sum(is.na(got$access)), 51L)
  # prep_data() on the values of every cell has the same active origins and the same cells
  expect_identical(unname(k$prepared$demand_kept_index), k$converted$cell_index)
  expect_identical(rownames(got$access), as.character(k$converted$cell_index))
  expect_identical(rownames(spax(k$prepared, spec_e2sfca(decay_gaussian(10)))$access),
                   rownames(got$access))
})

test_that("a declared kernel gives the same access on numeric and raster data", {
  skip_on_cran()
  k <- cornwall_numeric()
  kernel <- decay_kernel(function(distance, sigma) exp(-distance / sigma),
                         name = "pexp", parameter = "scale")
  spec <- spec_e2sfca(decay_custom(kernel, 8))
  want <- raster_values_at(spax(k$raster, spec), k$converted$cell_index)
  got <- spax(k$converted, spec)$access[, 1L]
  expect_identical(unname(is.na(got)), is.na(want))
  expect_equal(unname(got), want, tolerance = 1e-12)
})

# demand groups ---------------------------------------------------------------------------

# Two demand groups on the 3 x 4 grid with three facilities. Demand is missing in cell 7
# for both groups, zero for one group in cells 3 and 12 and for the other in 5, and the
# groups travel differently: b cannot reach cell 4 and nobody reaches cell 9. A cell with
# demand missing in one group only is not in this fixture: the numeric data cannot have it.
group_raster_fixture <- function() {
  f <- fca_block_fixture(3)
  Da <- f$D
  Da[12] <- 0
  Db <- c(5, 15, 0, 25, 35, 45, NA, 65, 75, 85, 95, 0)
  dist_b <- f$dist * 1.5
  dist_b[4, ] <- NA
  list(
    demand = list(a = toy_rast(Da, nrows = 3, ncols = 4),
                  b = toy_rast(Db, nrows = 3, ncols = 4)),
    travel = list(a = f$distance,
                  b = terra::setValues(f$distance, dist_b)),
    supply = stats::setNames(f$S[, 1L], f$ids),
    shared = f$distance, Da = Da, Db = Db, dist_a = f$dist, dist_b = dist_b, ids = f$ids
  )
}

# The same data as numeric inputs of every cell (prep_data() with unknown demand excluded).
group_numeric_fixture <- function(f, shared = FALSE) {
  cells <- as.character(1:12)
  distance_of <- function(m) {
    dimnames(m) <- list(cells, f$ids)
    m
  }
  prep_data(list(a = stats::setNames(f$Da, cells), b = stats::setNames(f$Db, cells)),
            f$supply,
            if (shared) distance_of(f$dist_a) else
              list(a = distance_of(f$dist_a), b = distance_of(f$dist_b)),
            missing_demand = "exclude", missing_travel = "legacy_zero")
}

group_specs <- function() {
  list(
    e2sfca = spec_e2sfca(list(a = decay_gaussian(2), b = decay_exponential(3)),
                         normalize = "standard"),
    m2sfca = spec_m2sfca(decay_gaussian(3)),
    `3sfca` = spec_3sfca(decay_gaussian(3), selection = list(a = decay_gaussian(2),
                                                             b = decay_gaussian(4))),
    huff2sfca = spec_huff2sfca(decay_gaussian(3), alpha = 2),
    `2sfca` = spec_2sfca(c(a = 3, b = 4))
  )
}

test_that("grouped numeric access equals grouped raster access: groups and the overall layer", {
  f <- group_raster_fixture()
  raster <- prep_data(f$demand, f$supply, f$travel)
  dnum <- group_numeric_fixture(f)
  converted <- .raster_to_origins(raster, spec_clm())
  expect_identical(converted$cell_index, c(1:6, 8:11))
  cells <- converted$cell_index
  expect_identical(unname(dnum$demand_kept_index), cells)
  for (nm in names(group_specs())) {
    spec <- group_specs()[[nm]]
    want <- terra::values(spax(raster, spec)$accessibility, mat = TRUE)[cells, , drop = FALSE]
    for (data in list(converted, dnum)) {
      got <- spax(data, spec)
      expect_identical(colnames(got$access), c("supply.a", "supply.b", "supply.all"), info = nm)
      expect_identical(unname(is.na(got$access)), unname(is.na(want)), info = nm)
      expect_equal(unname(got$access), unname(want), tolerance = 1e-12, info = nm)
    }
  }
})

test_that("grouped numeric access with travel shared by the groups equals the raster route", {
  f <- group_raster_fixture()
  raster <- prep_data(f$demand, f$supply, f$shared)
  dnum <- group_numeric_fixture(f, shared = TRUE)
  converted <- .raster_to_origins(raster, spec_clm())
  cells <- converted$cell_index
  spec <- spec_e2sfca(decay_gaussian(3))
  want <- terra::values(spax(raster, spec)$accessibility, mat = TRUE)[cells, , drop = FALSE]
  for (data in list(converted, dnum)) {
    got <- spax(data, spec)$access
    expect_identical(unname(is.na(got)), unname(is.na(want)))
    expect_equal(unname(got), unname(want), tolerance = 1e-12)
  }
})

test_that("the overall layer of numeric groups counts a group that cannot reach a cell as 0", {
  f <- group_raster_fixture()
  dnum <- group_numeric_fixture(f)
  got <- spax(dnum, spec_e2sfca(decay_gaussian(3)))
  # cell 4: group b cannot reach it (NA travel to every facility), so b has no value
  # there and the overall access is the demand-weighted mean with b counted as 0
  expect_true(is.na(got$access["4", "supply.b"]))
  expect_equal(unname(got$access["4", "supply.all"]),
               unname(f$Da[4] * got$access["4", "supply.a"] / (f$Da[4] + f$Db[4])))
  expect_identical(got$parameters$groups, c("a", "b"))
  expect_identical(got$parameters$layers$layer, c("supply.a", "supply.b", "supply.all"))
})

# the result -------------------------------------------------------------------------------

test_that("an FCA result on numeric data is a spax_access_values with the parts of the wrappers", {
  f <- group_raster_fixture()
  dnum <- group_numeric_fixture(f)
  got <- spax(dnum, spec_huff2sfca(decay_gaussian(3), alpha = 2))
  expect_s3_class(got, c("spax_access_values", "spax_result"), exact = TRUE)
  expect_named(got, c("access", "type", "parameters", "facilities", "call"))
  expect_identical(got$type, "Huff-2SFCA")
  expect_identical(names(got$parameters),
                   c("decay_params", "selection_params", "attractiveness", "alpha",
                     "groups", "layers"))
  expect_identical(got$parameters$decay_params, list(method = "gaussian", sigma = 3))
  expect_identical(got$parameters$selection_params, list(method = "gaussian", sigma = 3))
  expect_identical(got$parameters$attractiveness, stats::setNames(c(5, 8, 11), f$ids))
  expect_identical(got$parameters$alpha, 2)
  expect_identical(got$facilities$id, f$ids)
  expect_identical(got$facilities$supply, c(5, 8, 11))
  expect_identical(rownames(got$access), dnum$origin_ids)
  expect_true(is.matrix(got$access))
  expect_identical(got$call, quote(spax(data = dnum, model = spec_huff2sfca(decay_gaussian(3),
                                                                              alpha = 2))))
  # E2SFCA: decay and normalization only
  e2 <- spax(dnum, spec_e2sfca(decay_gaussian(3), normalize = "semi"))
  expect_identical(names(e2$parameters), c("decay_params", "demand_normalize", "groups",
                                           "layers"))
  expect_identical(e2$parameters$demand_normalize, "semi")
  expect_identical(e2$type, "E2SFCA")
  # 2SFCA: the binary decay of the threshold
  two <- spax(dnum, spec_2sfca(c(a = 3, b = 4)))
  expect_identical(two$type, "2SFCA")
  expect_identical(two$parameters$decay_params,
                   list(a = list(method = "binary", sigma = 3),
                        b = list(method = "binary", sigma = 4)))
})

test_that("an ungrouped numeric result has no groups or layers and one column, supply", {
  td <- fca_toy()
  dm <- prep_data(c(a = 10, b = 20, c = 30),
                  stats::setNames(c(4, 6), c("f1", "f2")),
                  matrix(c(1, 2, 3, 3, 2, 1), 3, 2, dimnames = list(c("a", "b", "c"), c("f1", "f2"))))
  got <- spax(dm, spec_e2sfca(decay_gaussian(2)))
  expect_identical(names(got$parameters), c("decay_params", "demand_normalize"))
  expect_identical(colnames(got$access), "supply")
  expect_identical(rownames(got$access), c("a", "b", "c"))
  # by hand: weights, load, ratio, access
  W <- exp(-matrix(c(1, 2, 3, 3, 2, 1), 3, 2)^2 / 8)
  R <- c(4, 6) / colSums(c(10, 20, 30) * W)
  expect_equal(unname(got$access[, 1L]), as.vector(W %*% R), tolerance = 1e-14)
})

test_that("numeric data with no origin that reaches a facility gives missing access", {
  dm <- prep_data(c(a = 10, b = 20), c(f1 = 4, f2 = 6),
                  matrix(c(1, NA, 2, NA), 2, 2, dimnames = list(c("a", "b"), c("f1", "f2"))),
                  missing_travel = "legacy_zero")
  got <- spax(dm, spec_e2sfca(decay_gaussian(2)))
  expect_true(is.na(got$access["b", 1L]))
  expect_false(is.na(got$access["a", 1L]))
})

test_that("Huff-2SFCA on numeric data takes a named attractiveness by facility", {
  f <- group_raster_fixture()
  dnum <- group_numeric_fixture(f)
  attract <- c(f3 = 3, f1 = 1, f2 = 2)
  got <- spax(dnum, spec_huff2sfca(decay_gaussian(3), attractiveness = attract))
  expect_identical(got$parameters$attractiveness, c(f1 = 1, f2 = 2, f3 = 3))
  # the same attractiveness on rasters
  raster <- prep_data(f$demand, f$supply, f$travel)
  want <- spax(raster, spec_huff2sfca(decay_gaussian(3), attractiveness = attract))
  expect_identical(want$parameters$attractiveness, got$parameters$attractiveness)
  cells <- .raster_to_origins(raster, spec_clm())$cell_index
  expect_equal(unname(got$access),
               unname(terra::values(want$accessibility, mat = TRUE)[cells, ]), tolerance = 1e-12)
  expect_error_exact(
    spax(dnum, spec_huff2sfca(decay_gaussian(3), attractiveness = c(f1 = 1, f2 = 2))),
    "names of attractiveness must be the facility IDs of the data, one for each facility")
  expect_error_exact(
    spax(dnum, spec_huff2sfca(decay_gaussian(3), attractiveness = c(f1 = 1, f2 = 2, x = 3))),
    "names of attractiveness must be the facility IDs of the data, one for each facility")
})

# refusals and bindings --------------------------------------------------------------------------

test_that("a raster sigma is refused on numeric data", {
  f <- group_raster_fixture()
  dnum <- group_numeric_fixture(f)
  sigma <- toy_rast(c(2, 3, 4), nrows = 3, ncols = 4)
  message <- "a raster sigma needs raster data"
  expect_error_exact(spax(dnum, spec_e2sfca(decay_gaussian(sigma))), message)
  expect_error_exact(spax(dnum, spec_3sfca(decay_gaussian(3),
                                              selection = decay_exponential(sigma))), message)
  expect_error_exact(spax(dnum, spec_m2sfca(list(a = decay_gaussian(sigma),
                                                    b = decay_gaussian(3)))), message)
  # on rasters it is allowed
  raster <- prep_data(f$demand, f$supply, f$travel)
  expect_s3_class(spax(raster, spec_e2sfca(decay_gaussian(sigma))), "spax")
})

test_that("an index on numeric data is computed, not fitted", {
  f <- group_raster_fixture()
  dnum <- group_numeric_fixture(f)
  spec <- spec_e2sfca(decay_gaussian(3))
  expect_error_exact(spax(dnum, spec, observed = obs_totals(c(f1 = 1, f2 = 2, f3 = 3))),
                     paste0("FCA indices are computed, not fitted; to fit how demand chooses ",
                            "facilities use spec_huff() or spec_clm()"))
  expect_error_exact(spax(dnum, spec, starts = c(sigma = 1)),
                     "starts, bounds and fitting controls apply to allocation models")
  expect_error_exact(spax(dnum, spec, maxit = 3),
                     "starts, bounds and fitting controls apply to allocation models")
})

test_that("group arguments are bound to the groups of numeric data as for rasters", {
  f <- group_raster_fixture()
  dnum <- group_numeric_fixture(f)
  # a list by group on ungrouped data
  dm <- prep_data(c(a = 10, b = 20), c(f1 = 4, f2 = 6),
                  matrix(c(1, 2, 2, 1), 2, 2, dimnames = list(c("a", "b"), c("f1", "f2"))))
  expect_error_exact(
    spax(dm, spec_e2sfca(list(a = decay_gaussian(2), b = decay_gaussian(3)))),
    "decay needs data with demand groups")
  expect_error_exact(spax(dm, spec_2sfca(c(a = 3, b = 4))),
                     "threshold needs data with demand groups")
  # names that differ from the groups of the data
  expect_error_exact(
    spax(dnum, spec_e2sfca(list(a = decay_gaussian(2), z = decay_gaussian(3)))),
    "decay is given for groups a, z but the data has groups a, b")
  # a list given in another order is put in the order of the groups
  swapped <- spax(dnum, spec_e2sfca(list(b = decay_gaussian(3), a = decay_gaussian(2))))
  ordered <- spax(dnum, spec_e2sfca(list(a = decay_gaussian(2), b = decay_gaussian(3))))
  expect_identical(swapped$access, ordered$access)
  expect_identical(names(swapped$parameters$decay_params), c("a", "b"))
})

test_that("a plain decay function needs raster data", {
  f <- group_raster_fixture()
  dnum <- group_numeric_fixture(f)
  expect_error_exact(
    .fca_group_kernels("e2sfca", f$dist_a, list(method = function(distance, sigma) 1 / distance,
                                                sigma = 1)),
    "a plain decay function needs raster data; declare a kernel with decay_kernel()")
  expect_error_exact(.fca_group_kernels("bogus", f$dist_a, list(method = "gaussian", sigma = 1)),
                     "unknown FCA type 'bogus'")
})

# methods ----------------------------------------------------------------------------------------

test_that("print() and format() of an FCA result on numeric data give exact lines", {
  f <- group_raster_fixture()
  dnum <- group_numeric_fixture(f)
  got <- spax(dnum, spec_e2sfca(list(a = decay_gaussian(2), b = decay_exponential(3)),
                                   normalize = "standard"))
  lines <- c(
    "<spax access> E2SFCA, 3 layers at 10 origins x 3 facilities",
    "  layers      supply.a, supply.b, supply.all",
    "  decay       by group: a gaussian (sigma = 2), b exponential (sigma = 3)",
    "  normalize   standard"
  )
  expect_identical(format(got), lines)
  expect_identical(capture.output(print(got)), lines)
  capture.output(returned <- print(got))
  expect_identical(returned, got)
  one <- prep_data(c(a = 10, b = 20), c(f1 = 4, f2 = 6),
                   matrix(c(1, 2, 2, 1), 2, 2, dimnames = list(c("a", "b"), c("f1", "f2"))))
  expect_identical(
    format(spax(one, spec_3sfca(decay_gaussian(2)))),
    c("<spax access> 3SFCA, 1 layer at 2 origins x 2 facilities",
      "  layers      supply",
      "  decay       gaussian (sigma = 2)"))
})

test_that("summary() gives min, mean and max per layer and the coverage", {
  one <- prep_data(c(a = 10, b = 20, c = 30), c(f1 = 4, f2 = 6),
                   matrix(c(1, NA, 3, 3, NA, 1), 3, 2, dimnames = list(c("a", "b", "c"),
                                                                       c("f1", "f2"))),
                   missing_travel = "legacy_zero")
  got <- spax(one, spec_2sfca(2))
  s <- summary(got)
  expect_s3_class(s, "summary.spax_access_values")
  expect_identical(s$type, "2SFCA")
  expect_identical(s$origins, 3L)
  values <- got$access[, 1L]
  expect_identical(s$layers$layer, "supply")
  expect_identical(s$layers$n, 2L)
  expect_equal(s$layers$min, min(values, na.rm = TRUE))
  expect_equal(s$layers$mean, mean(values, na.rm = TRUE))
  expect_equal(s$layers$max, max(values, na.rm = TRUE))
  expect_identical(s$layers$covered, sum(values > 0, na.rm = TRUE))
  expect_equal(s$layers$proportion, sum(values > 0, na.rm = TRUE) / 2)
  out <- capture.output(print(s))
  expect_identical(out[1:3], c("<spax access summary> 2SFCA", "  origins     3",
                               "  decay       binary (sigma = 2)"))
  expect_true(any(grepl("with_value", out, fixed = TRUE)))
})

test_that("as.data.frame() gives the origin ID and one column per layer", {
  f <- group_raster_fixture()
  dnum <- group_numeric_fixture(f)
  got <- spax(dnum, spec_e2sfca(decay_gaussian(3)))
  df <- as.data.frame(got)
  expect_identical(names(df), c("origin_id", "supply.a", "supply.b", "supply.all"))
  expect_identical(df$origin_id, dnum$origin_ids)
  expect_equal(df$supply.all, unname(got$access[, "supply.all"]))
  expect_identical(rownames(df), as.character(seq_len(nrow(df))))
  expect_identical(rownames(as.data.frame(got, row.names = dnum$origin_ids)),
                   dnum$origin_ids)
})

# the in-memory block -------------------------------------------------------------------------------

test_that(".fca_blocks() takes an in-memory block and checks it", {
  f <- fca_block_fixture(3)
  keys <- list(demand = f$D, distance = f$dist)
  kernels <- .fca_group_kernels("e2sfca", f$dist, list(method = "gaussian", sigma = 3))
  expect_null(kernels$rasters)
  out <- .fca_blocks(NULL, kernels$kernels, matrix(c(5, 8, 11), ncol = 1L), block = keys)
  expect_identical(dim(out), c(12L, 1L))
  # the raster of the same data gives the same values
  r <- .fca_group_kernels("e2sfca", f$distance, list(method = "gaussian", sigma = 3))
  rasters <- .fca_merge_rasters(c(list(demand = f$demand), r$rasters))
  expect_equal(.fca_blocks(rasters, r$kernels, matrix(c(5, 8, 11), ncol = 1L)), out,
               tolerance = 1e-14)
  # a missing demand key and unequal numbers of rows are internal errors
  expect_error_exact(.fca_blocks(NULL, kernels$kernels, matrix(1, 3, 1),
                                 block = list(distance = f$dist)),
                     "internal error: the block has no 'demand'")
  expect_error_exact(.fca_blocks(NULL, kernels$kernels, matrix(1, 3, 1),
                                 block = list(demand = f$D[1:5], distance = f$dist)),
                     "internal error: every element of a block needs one row per origin")
  expect_error_exact(.fca_blocks(NULL, kernels$kernels, matrix(1, 3, 1), block = list(1)),
                     "internal error: a block must be a list with unique names")
})
