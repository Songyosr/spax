# Allocation models on raster data (SPAX-060, V3): spax(prep_data(<raster>, ...), spec_clm())
# converts the cells with demand to origins on each call (.raster_to_origins()) and then
# follows the path of numeric data. The converter is checked against the legacy raster
# substrate (.interaction_substrate()) and against numeric preparation of the same values;
# the Cornwall cases read the shipped example grids and are skipped on CRAN.

expect_error_exact <- function(object, message) {
  # the whole message, not a part of it
  got <- tryCatch({ object; NULL }, error = function(e) conditionMessage(e))
  expect_identical(got, message)
}

# The 3 x 4 grid of the block engine fixture: demand is missing in cell 7 and zero in
# cell 5, cell 9 reaches no facility and cell 3 does not reach facility f1.
single_fixture <- function() {
  f <- fca_block_fixture(3)
  supply <- stats::setNames(f$S[, 1L], f$ids)
  c(f, list(supply = supply, raster = prep_data(f$demand, supply, f$distance)))
}

# Two demand groups on the same grid: demand is missing in cell 7 for both and zero for one
# of them in cells 3, 5 and 12; group b cannot reach cell 4 and nobody reaches cell 9.
group_fixture_v3 <- function() {
  f <- fca_block_fixture(3)
  Da <- f$D
  Da[12] <- 0
  Db <- c(5, 15, 0, 25, 35, 45, NA, 65, 75, 85, 95, 0)
  dist_b <- f$dist * 1.5
  dist_b[4, ] <- NA
  list(
    demand = list(a = toy_rast(Da, nrows = 3, ncols = 4),
                  b = toy_rast(Db, nrows = 3, ncols = 4)),
    travel = list(a = f$distance, b = terra::setValues(f$distance, dist_b)),
    shared = f$distance,
    supply = stats::setNames(f$S[, 1L], f$ids), ids = f$ids,
    Da = Da, Db = Db, dist_a = f$dist, dist_b = dist_b
  )
}

# The cornwall example: mainland practices, the grids cropped to the mainland, demand 1.25
# times the residents of a cell (the registered residents of the practices, 599,000,
# exceed the residents of the grid, 541,000, and an outside option needs demand above the
# counts) and the totals of registrations by practice.
.origins_cache <- new.env(parent = emptyenv())
cornwall_mainland <- function() {
  if (!is.null(.origins_cache$mainland)) return(.origins_cache$mainland)
  gp <- sf::st_drop_geometry(cornwall_gp[!cornwall_gp$scilly, ])
  pop <- read_spax_example("cornwall_pop.tif")
  travel <- read_spax_example("cornwall_gp_travel.tif")
  mainland <- terra::ext(-6, terra::xmax(pop), terra::ymin(pop), terra::ymax(pop))
  pop <- terra::crop(pop, mainland) * 1.25
  travel <- terra::crop(travel[[gp$practice]], mainland)
  totals <- tapply(cornwall_flows$patients, cornwall_flows$practice, sum)[gp$practice]
  .origins_cache$mainland <- list(
    gp = gp, pop = pop, travel = travel,
    supply = stats::setNames(gp$gp_fte_extg, gp$practice),
    totals = stats::setNames(as.numeric(totals), gp$practice),
    data = prep_data(pop, gp, travel, id_col = "practice", supply_cols = "gp_fte_extg")
  )
}

# the converter against the legacy raster substrate -----------------------------------------

# The fields that mean the same in both: demand, travel with the missing pairs in the same
# places, supply, IDs, the cell of every origin and the travel flags.
expect_same_substrate <- function(o, s, info = NULL) {
  expect_identical(o$D_active, s$D_active, info = info)
  # Missing travel is NA here and NaN in the substrate (terra::extract); the pairs are the
  # same, so compare the values with the missing ones set apart.
  expect_identical(is.na(unname(o$distance_active)), is.na(unname(s$distance_active)),
                   info = info)
  without_missing <- function(m) {
    m[is.na(m)] <- -1
    unname(m)
  }
  expect_identical(without_missing(o$distance_active), without_missing(s$distance_active),
                   info = info)
  expect_equal(o$S, s$S, info = info)
  expect_identical(o$facility_ids, s$facility_ids, info = info)
  expect_identical(o$origin_ids, s$origin_ids, info = info)
  # the grid cell of every active origin
  expect_identical(o$cell_index[o$demand_kept_index], s$cell_index[s$demand_kept_index],
                   info = info)
  expect_identical(unname(o$active_unknown_travel), unname(s$active_unknown_travel), info = info)
  expect_identical(o$active_travel_status, s$active_travel_status, info = info)
  expect_identical(o$template, s$template, info = info)
}

test_that("the converter gives the active cells, their travel and the cell mapping of the substrate", {
  f <- single_fixture()
  o <- .raster_to_origins(f$raster, spec_clm())
  s <- .interaction_substrate(f$demand, f$supply, f$distance)
  expect_s3_class(o, "ae_prepared_inputs")
  expect_same_substrate(o, s)
  # cells with known positive demand: 7 (missing) and 5 (zero) are not active
  expect_identical(o$cell_index, c(1:4, 6L, 8:12))
  expect_identical(o$origin_ids, as.character(o$cell_index))
  # the registry holds the active cells only; unreachable cell 9 is kept, with all
  # travel missing, and cell 3 reaches f2 and f3
  expect_identical(o$all_origin_ids, o$origin_ids)
  expect_true(all(o$origin_status == "active"))
  expect_identical(unname(o$distance_active["9", ]), rep(NA_real_, 3L))
  expect_identical(unname(o$distance_active["3", ]), c(NA, 7, 3))
  expect_identical(o$input_policy, list(source = "raster", missing_demand = "error",
                                        missing_travel = "legacy_zero",
                                        kernel_domain = "strict"))
  expect_identical(o$supply_cols, "supply")
})

test_that("the converter equals the substrate on the Cornwall grids, with the same fitted theta", {
  skip_on_cran()
  k <- cornwall_mainland()
  o <- .raster_to_origins(k$data, spec_clm())
  s <- .interaction_substrate(k$pop, k$supply, k$travel)
  expect_same_substrate(o, s)
  expect_identical(dim(o$distance_active), c(6912L, 54L))
  # the same model and starts on both give the same estimates
  fit <- function(inputs) {
    fit_allocation(clm_allocation(inputs, family = "gaussian", fit_v0 = TRUE), k$totals,
                   rbind(start = c(sigma = 20, v0 = 100)), loss = "poisson",
                   gradient = TRUE, control = list(maxit = 15))
  }
  fa <- fit(o)
  fb <- fit(s)
  expect_identical(stats::coef(fa), stats::coef(fb))
  expect_equal(fa$best$loss, fb$best$loss, tolerance = 1e-12)
})

test_that("the full Cornwall grid keeps the cells that reach no practice as origins", {
  skip_on_cran()
  pop <- read_spax_example("cornwall_pop.tif")
  travel <- read_spax_example("cornwall_gp_travel.tif")
  gp <- sf::st_drop_geometry(cornwall_gp)
  data <- prep_data(pop, gp, travel, id_col = "practice", supply_cols = "gp_fte_extg")
  o <- .raster_to_origins(data, spec_clm())
  s <- .interaction_substrate(pop, stats::setNames(gp$gp_fte_extg, gp$practice), travel)
  expect_same_substrate(o, s)
  expect_length(o$origin_ids, 6985L)
  expect_equal(sum(rowSums(is.na(o$distance_active)) == ncol(o$distance_active)), 51L)
  expect_equal(sum(o$active_unknown_travel), sum(s$active_unknown_travel))
})

# conversion details ------------------------------------------------------------------------------

test_that("origin IDs are integers as text: cell 100000 is not 1e+05", {
  dem <- terra::rast(nrows = 400, ncols = 300, xmin = 0, xmax = 300, ymin = 0, ymax = 400)
  v <- rep(NA_real_, terra::ncell(dem))
  v[c(1, 99999, 100000, 100001, 120000)] <- c(5, 6, 7, 8, 9)
  terra::values(dem) <- v
  travel <- terra::rast(dem, nlyrs = 2)
  terra::values(travel) <- 1 + seq_len(2 * terra::ncell(dem)) %% 7
  names(travel) <- c("x", "y")
  o <- .raster_to_origins(prep_data(dem, c(x = 3, y = 4), travel), spec_clm())
  expect_identical(o$origin_ids, c("1", "99999", "100000", "100001", "120000"))
  expect_identical(o$cell_index, c(1L, 99999L, 100000L, 100001L, 120000L))
  expect_type(o$cell_index, "integer")
})

test_that("the conversion is the same in one block as in many, and the same on every call", {
  f <- single_fixture()
  one <- .raster_to_origins(f$raster, spec_clm())
  expect_identical(.raster_to_origins(f$raster, spec_clm()), one)
  for (values in c(1, 5, 11, 40)) {
    withr::local_options(list(spax.fca_block_values = values))
    expect_identical(.raster_to_origins(f$raster, spec_clm()), one, info = format(values))
  }
  g <- group_fixture_v3()
  graster <- prep_data(g$demand, g$supply, g$travel)
  gone <- .raster_to_origins(graster, spec_clm())
  withr::local_options(list(spax.fca_block_values = 1))
  expect_identical(.raster_to_origins(graster, spec_clm()), gone)
})

test_that("the demand rule of the engine applies: negative or infinite demand is an error", {
  f <- single_fixture()
  message <- "demand must be finite and nonnegative; use NA for unknown"
  for (bad in c(-1, Inf, -Inf)) {
    D <- f$D
    D[4] <- bad
    data <- prep_data(toy_rast(D, nrows = 3, ncols = 4), f$supply, f$distance)
    expect_error_exact(.raster_to_origins(data, spec_clm()), message)
    expect_error_exact(spax(data, spec_clm()), message)
  }
  # in one group of several
  g <- group_fixture_v3()
  Db <- g$Db
  Db[2] <- -3
  data <- prep_data(list(a = g$demand$a, b = toy_rast(Db, nrows = 3, ncols = 4)), g$supply,
                    g$travel)
  expect_error_exact(.raster_to_origins(data, spec_clm()), message)
  # but zero and missing demand are fine
  expect_no_error(.raster_to_origins(single_fixture()$raster, spec_clm()))
})

test_that("a grid with no cell of positive demand cannot be converted", {
  f <- single_fixture()
  data <- prep_data(toy_rast(c(0, NA), nrows = 3, ncols = 4), f$supply, f$distance)
  expect_error_exact(.raster_to_origins(data, spec_clm()), "no cell has positive demand")
})

test_that("an allocation model uses one supply measure", {
  f <- single_fixture()
  supply <- data.frame(id = f$ids, a = c(5, 8, 11), b = c(1, 2, 3))
  both <- prep_data(f$demand, supply, f$distance, id_col = "id", supply_cols = c("a", "b"))
  message <- paste0("the allocation model uses one supply measure: choose it with ",
                    "prep_data(supply_cols = )")
  expect_error_exact(spax(both, spec_clm()), message)
  expect_error_exact(.raster_to_origins(both, spec_huff()), message)
  # one column of a table is the supply, and a matrix of one column too
  one <- prep_data(f$demand, supply, f$distance, id_col = "id", supply_cols = "b")
  expect_equal(as.vector(.raster_to_origins(one, spec_clm())$S), c(1, 2, 3))
  matrix_supply <- matrix(c(5, 8, 11), 3, 1, dimnames = list(f$ids, "a"))
  expect_equal(as.vector(.raster_to_origins(prep_data(f$demand, matrix_supply, f$distance),
                                            spec_clm())$S), c(5, 8, 11))
  # FCA specifications keep several measures
  expect_s3_class(spax(both, spec_e2sfca(decay_gaussian(3))), "spax")
})

test_that("facility IDs are the layer names, or facility_N when the names are terra defaults", {
  f <- single_fixture()
  travel <- f$distance
  names(travel) <- paste0("lyr.", 1:3)
  o <- .raster_to_origins(prep_data(f$demand, c(5, 8, 11), travel), spec_clm())
  expect_identical(o$facility_ids, c("facility_1", "facility_2", "facility_3"))
  expect_identical(o$facility_ids, .fca_ids(travel))
  expect_identical(colnames(o$distance_active), o$facility_ids)
  expect_identical(.raster_to_origins(f$raster, spec_clm())$facility_ids, f$ids)
})

test_that("supply is matched to the layers by facility ID, whatever its order", {
  f <- single_fixture()
  supply <- data.frame(id = rev(f$ids), s = c(11, 8, 5))
  data <- prep_data(f$demand, supply, f$distance, id_col = "id", supply_cols = "s")
  expect_equal(as.vector(.raster_to_origins(data, spec_clm())$S), c(5, 8, 11))
})

test_that("the metadata of the data go to the prepared inputs", {
  f <- single_fixture()
  data <- prep_data(f$demand, f$supply, f$distance,
                    metadata = list(demand_units = "residents", travel_units = "minutes"))
  o <- .raster_to_origins(data, spec_clm())
  expect_identical(o$input_metadata$demand_units, "residents")
  expect_identical(o$input_metadata$travel_units, "minutes")
  expect_true(is.na(o$input_metadata$supply_units))
})

test_that("the converter needs raster data and an allocation specification", {
  f <- single_fixture()
  expect_error(.raster_to_origins(f$demand, spec_clm()), "internal error")
  expect_error(.raster_to_origins(f$raster, spec_e2sfca(decay_gaussian(3))), "internal error")
})

# demand groups -----------------------------------------------------------------------------------

test_that("grouped raster data convert to the groups of numeric preparation of the same values", {
  g <- group_fixture_v3()
  cells <- as.character(1:12)
  as_matrix <- function(m) {
    dimnames(m) <- list(cells, g$ids)
    m
  }
  numeric <- prep_data(list(a = stats::setNames(g$Da, cells), b = stats::setNames(g$Db, cells)),
                       g$supply, list(a = as_matrix(g$dist_a), b = as_matrix(g$dist_b)),
                       missing_demand = "exclude", missing_travel = "legacy_zero")
  o <- .raster_to_origins(prep_data(g$demand, g$supply, g$travel), spec_clm())
  # cells 7 (missing in both) and 12 (zero in both) are not active
  expect_identical(o$cell_index, c(1:6, 8:11))
  expect_identical(o$group_ids, c("a", "b"))
  expect_identical(o$origin_ids, numeric$origin_ids)
  expect_identical(o$D_active, numeric$D_active)
  expect_identical(o$D_groups, numeric$D_groups)
  expect_identical(o$distance_groups, numeric$distance_groups)
  expect_null(o$distance_active)
  expect_identical(o$active_travel_status, numeric$active_travel_status)
  expect_identical(o$active_travel_status_groups, numeric$active_travel_status_groups)
  expect_identical(o$active_unknown_travel, numeric$active_unknown_travel)
  expect_identical(o$input_policy$source, "raster")
  # travel shared by the groups is one matrix
  shared <- .raster_to_origins(prep_data(g$demand, g$supply, g$shared), spec_clm())
  expect_false(is.null(shared$distance_active))
  expect_identical(unname(shared$distance_active), unname(o$distance_groups$a))
})

test_that("a cell with demand missing in one group only is not active", {
  g <- group_fixture_v3()
  Da <- g$Da
  Da[2] <- NA
  data <- prep_data(list(a = toy_rast(Da, nrows = 3, ncols = 4), b = g$demand$b), g$supply,
                    g$travel)
  o <- .raster_to_origins(data, spec_clm())
  expect_false(2L %in% o$cell_index)
  expect_identical(o$cell_index, c(1L, 3:6, 8:11))
})

test_that("groups with their own travel stacks that are the same raster read it once", {
  g <- group_fixture_v3()
  data <- prep_data(g$demand, g$supply, list(a = g$shared, b = g$shared))
  o <- .raster_to_origins(data, spec_clm())
  expect_identical(o$distance_groups$a, o$distance_groups$b)
  expect_identical(o$distance_groups$a, .raster_to_origins(prep_data(g$demand, g$supply,
                                                                    g$shared),
                                                           spec_clm())$distance_active)
})

test_that("a grouped model on raster data gives the results of the same numeric data", {
  g <- group_fixture_v3()
  cells <- as.character(1:12)
  as_matrix <- function(m) {
    dimnames(m) <- list(cells, g$ids)
    m
  }
  numeric <- prep_data(list(a = stats::setNames(g$Da, cells), b = stats::setNames(g$Db, cells)),
                       g$supply, list(a = as_matrix(g$dist_a), b = as_matrix(g$dist_b)),
                       missing_demand = "exclude", missing_travel = "legacy_zero")
  raster <- prep_data(g$demand, g$supply, g$travel)
  spec <- spec_clm(decay = list(a = decay_gaussian(2), b = decay_gaussian(3)), outside = 1.5)
  got <- spax(raster, spec)
  want <- spax(numeric, spec)
  expect_equal(as.data.frame(got, type = "origins"), as.data.frame(want, type = "origins"),
               tolerance = 1e-12)
  expect_equal(as.data.frame(got, type = "facilities"), as.data.frame(want, type = "facilities"),
               tolerance = 1e-12)
  expect_equal(unclass(coef(got)), unclass(coef(want)))
})

# allocation on raster data, end to end -------------------------------------------------------------

test_that("an evaluated model on raster data equals the same model on the converted cells", {
  f <- single_fixture()
  spec <- spec_clm(decay_gaussian(3), outside = 2, beta = 1)
  got <- spax(f$raster, spec)
  expect_s3_class(got, c("spax_allocation", "spax_result"), exact = TRUE)
  want <- spax(.raster_to_origins(f$raster, spec), spec)
  expect_identical(got$model$substrate, want$model$substrate)
  expect_identical(got$theta, want$theta)
  expect_identical(got$evaluation$outputs, want$evaluation$outputs)
  origins <- as.data.frame(got, type = "origins")
  expect_identical(origins$origin_id, as.character(c(1:4, 6L, 8:12)))
  # the active cell that reaches no practice keeps its demand: all of it is outside
  expect_equal(origins$outside_share[origins$origin_id == "9"], 1)
  expect_equal(origins$access[origins$origin_id == "9"], 0)
  expect_identical(got$model$substrate$input_policy$source, "raster")
  # a result of raster data prints as numeric data does
  expect_identical(capture.output(print(got))[5L], "  data        10 origins x 3 facilities")
})

test_that("a prediction from a model on raster data works (the snapshot accepts the policy)", {
  f <- single_fixture()
  x <- spax(f$raster, spec_clm(decay_gaussian(3), outside = 2, beta = 1))
  requested <- f$dist[1:2, ]
  dimnames(requested) <- list(c("a", "b"), f$ids)
  p <- predict(x, travel = requested)
  expect_s3_class(p, "ae_clm_prediction")
  expect_identical(rownames(p$outputs$allocation), c("a", "b"))
  # the allocation of requested cells with the travel of cells 1 and 2 is that of the origins
  full <- evaluate_allocation(x$model, x$theta, requested_outputs = "allocation")$outputs$allocation
  expect_equal(unname(p$outputs$allocation), unname(full[1:2, ]), tolerance = 1e-12)
})

test_that("a model fitted on raster data to practice totals runs the default search", {
  skip_on_cran()
  k <- cornwall_mainland()
  x <- spax(k$data, spec_clm(), observed = obs_totals(k$totals))
  expect_s3_class(x, c("spax_allocation", "spax_result"), exact = TRUE)
  expect_identical(x$mode, "fit")
  expect_false(is.null(x$theta))
  expect_named(coef(x), c("sigma", "v0"))
  expect_identical(capture.output(print(x))[5L], "  data        6,912 origins x 54 facilities")
  expect_identical(x$model$substrate$origin_ids,
                   as.character(x$model$substrate$cell_index))
  # the origins of the fit are those of the conversion
  converted <- spax(.raster_to_origins(k$data, spec_clm()),
                    spec_clm(decay_gaussian(10), outside = 5, beta = 1))
  expect_identical(x$model$substrate, converted$model$substrate)
})

test_that("fits on the same raster data have the same travel matrix and can be compared", {
  skip_on_cran()
  k <- cornwall_mainland()
  clm <- spax(k$data, spec_clm(), observed = obs_totals(k$totals),
              starts = rbind(start = c(sigma = 20, v0 = 100)))
  huff <- spax(k$data, spec_huff(), observed = obs_totals(k$totals),
               starts = rbind(start = c(sigma = 2, beta = 1)))
  expect_identical(clm$model$substrate$distance_active, huff$model$substrate$distance_active)
  expect_identical(clm$model$substrate$origin_ids, huff$model$substrate$origin_ids)
  comparison <- compare_allocations(list(clm = clm, huff = huff))
  expect_s3_class(comparison, "ae_allocation_comparison")
  expect_identical(comparison$table$specification, c("clm", "huff"))
})

test_that("flows keyed by cell number align, and counts at other cells are dropped with one message", {
  skip_on_cran()
  k <- cornwall_mainland()
  truth <- spax(k$data, spec_clm(decay_gaussian(8), outside = 40, beta = 1))
  flows <- as.data.frame(truth, type = "flows")
  withr::local_seed(11)
  counts <- data.frame(origin = as.integer(flows$origin_id), facility = flows$facility_id,
                       count = stats::rpois(nrow(flows), flows$flow))
  # cells 1 and 2 have no demand: they are outside the data
  outside <- data.frame(origin = c(1L, 2L), facility = k$gp$practice[1], count = c(5, 7))
  observed <- obs_flows(rbind(counts, outside), origin = "origin", facility = "facility",
                        count = "count")
  expect_message(
    fit <- spax(k$data, spec_clm(), observed = observed,
                starts = rbind(start = c(sigma = 10, v0 = 30))),
    "observed: 12 counts from 2 origins outside the data or left out by it are dropped",
    fixed = TRUE)
  expect_identical(fit$observed$output, "flow")
  expect_identical(rownames(fit$observed$target), fit$model$substrate$origin_ids)
  expect_equal(sum(fit$observed$target, na.rm = TRUE), sum(counts$count))
  expect_true(all(is.finite(coef(fit))))
  # the same counts keyed by text ("843" and the like) align the same
  as_text <- rbind(counts, outside)
  as_text$origin <- as.character(as_text$origin)
  expect_identical(
    suppressMessages(spax(k$data, spec_clm(), starts = rbind(start = c(sigma = 10, v0 = 30)),
                          observed = obs_flows(as_text, origin = "origin", facility = "facility",
                                               count = "count")))$observed$target,
    fit$observed$target)
})

test_that("a model fitted on one call of raster data equals the fit on another", {
  f <- single_fixture()
  observed <- obs_totals(c(f1 = 60, f2 = 120, f3 = 180))
  starts <- rbind(start = c(sigma = 3, v0 = 5))
  a <- spax(f$raster, spec_clm(), observed = observed, starts = starts)
  b <- spax(f$raster, spec_clm(), observed = observed, starts = starts)
  expect_identical(a$model$substrate, b$model$substrate)
  expect_identical(coef(a), coef(b))
})
