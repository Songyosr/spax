# Checked numeric allocation inputs (prepare_allocation()). Raster inputs and
# the SAE/HAAE providers are tested in test-06-legacy_providers.R; reload in a
# fresh process in test-90-allocation_portability.R.

.prepared_fixture <- function() {
  ids <- paste0("o", 1:6)
  D <- stats::setNames(c(8, 0, NA, 12, 9, 0), ids)
  M <- matrix(c(1, 2, 3, 2, Inf, -10, 4, 3, 2, 1, Inf, NA), 6, 2,
              dimnames = list(ids, c("b", "a")))
  included <- stats::setNames(c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE), ids)
  list(D = D, S = c(a = 15, b = 20), M = M, ids = ids, included = included)
}

.prepare_fixture <- function(f, ...) {
  prepare_allocation(f$D, f$S, f$M, included = f$included, missing_demand = "exclude", ...)
}

test_that("preparation aligns independent axes and records distinct load support", {
  f <- .prepared_fixture()
  p <- .prepare_fixture(f)
  expect_s3_class(p, "ae_prepared_inputs")
  expect_identical(p$origin_status, c("active", "zero", "unknown", "excluded", "active", "zero"))
  expect_identical(p$demand_kept_index, c(1L, 5L))
  expect_identical(p$origin_ids, p$all_origin_ids[p$demand_kept_index])
  expect_identical(p$origin_ids, c("o1", "o5"))
  expect_identical(p$active_travel_status, c("known", "all_absent"))
  expect_equal(unname(p$distance_active), unname(f$M[c(1, 5), ]))
  expect_equal(as.vector(p$S), c(20, 15))
  expect_null(p$template)
  expect_null(p$cell_index)
  expect_false("distance" %in% names(p))
  expect_true(all(vapply(p$input_metadata, is.na, logical(1))))
  rows <- c(5, 1, 6, 3, 2, 4)
  demand_order <- c(4, 6, 2, 1, 5, 3)
  q <- prepare_allocation(f$D[demand_order], rev(f$S), f$M[rows, c(2, 1)],
    origin_ids = f$ids, facility_ids = c("b", "a"),
    included = f$included[rev(seq_along(f$included))], missing_demand = "exclude")
  expect_identical(q, p)
  unnamed_mask <- unname(f$included[demand_order])
  q <- prepare_allocation(f$D[demand_order], f$S, f$M, origin_ids = f$ids,
    included = unnamed_mask, missing_demand = "exclude")
  expect_identical(q, p)
})

test_that("checked support validates only active travel and explicit missing policies", {
  f <- .prepared_fixture()
  expect_error(prepare_allocation(f$D, f$S, f$M), "missing demand")
  p <- .prepare_fixture(f) # negative/unknown omitted-row travel is not read as fit support
  f$M[1, 1] <- NA_real_
  expect_error(.prepare_fixture(f), "active travel is missing")
  q <- .prepare_fixture(f, missing_travel = "legacy_zero")
  expect_true(q$active_unknown_travel[1])
  expect_identical(q$active_travel_status, c("unknown", "all_absent"))
  for (bad in c(-1, -Inf)) {
    f$M[1, 1] <- bad
    expect_error(.prepare_fixture(f, missing_travel = "legacy_zero"), "nonnegative")
  }
  for (bad in c(-1, Inf, -Inf)) {
    f$D[1] <- bad
    expect_error(.prepare_fixture(f), "demand must be finite")
  }
})

test_that("identity, shape, supply and metadata errors fail at preparation", {
  f <- .prepared_fixture()
  for (bad in list(c("o1", "o1", f$ids[-(1:2)]), c("", f$ids[-1]),
                   c(NA, f$ids[-1]))) {
    expect_error(prepare_allocation(f$D, f$S, f$M, origin_ids = bad), "ID")
  }
  expect_error(prepare_allocation(unname(f$D), f$S, f$M), "origin_ids")
  expect_error(prepare_allocation(as.character(f$D), f$S, f$M, origin_ids = f$ids),
               "demand must be a numeric vector")
  for (bad in list(rep(1, 6), c(TRUE, NA, TRUE, TRUE, TRUE, TRUE), rep(TRUE, 5))) {
    expect_error(prepare_allocation(f$D, f$S, f$M, included = bad), "included must be")
  }
  wrong <- f$M; rownames(wrong)[1] <- "unknown"
  expect_error(prepare_allocation(f$D, f$S, wrong, missing_demand = "exclude"), "exactly match")
  expect_error(prepare_allocation(f$D, f$S, t(f$M), facility_ids = c("b", "a")), "origin-by-facility")
  expect_error(prepare_allocation(f$D, f$S[-1], f$M), "supply must")
  for (bad in c(-1, NA, Inf)) {
    f$S[1] <- bad
    expect_error(.prepare_fixture(f), "supply must be finite")
  }
  f <- .prepared_fixture()
  expect_error(.prepare_fixture(f, metadata = list(travel_units = "")), "nonblank")
  expect_error(.prepare_fixture(f, metadata = list(units = "minutes")), "metadata must")
  p <- .prepare_fixture(f, metadata = list(demand_units = "visits", demand_period = "year",
                                           travel_units = "minutes"))
  expect_identical(p$input_metadata$demand_units, "visits")
  expect_true(is.na(p$input_metadata$supply_period))
  cov <- .problem_coverage(clm_allocation(p, v0 = 5), c(sigma = 2))
  expect_identical(cov$input_metadata, p$input_metadata)
  expect_identical(cov$input_policy, p$input_policy)
})

test_that("checked CLM rejects invalid finite kernels and attractiveness transforms", {
  p <- prepare_allocation(c(o1 = 1), c(f1 = 2), matrix(0, 1, 1, dimnames = list("o1", "f1")))
  expect_error(.bind_theta(clm_allocation(p, family = "power"), c(sigma = 2)), "strictly positive")
  expect_error(.bind_theta(clm_allocation(p), c(sigma = 0)), "positive sigma")
  for (raw in list(list(supply = c(f1 = 2)), list(distance = matrix(1)), list(supply_cols = "x"))) {
    expect_error(do.call(.huff_problem, c(list(p), raw)), "cannot be combined")
  }
  tiny <- prepare_allocation(c(o1 = 1), c(f1 = 2), matrix(1e-200, 1, 1, dimnames = list("o1", "f1")))
  expect_error(.bind_theta(clm_allocation(tiny, family = "power"), c(sigma = 2)),
               "kernel is invalid on finite")
  p <- prepare_allocation(c(o1 = 1), c(f1 = 1e200), matrix(1, 1, 1, dimnames = list("o1", "f1")))
  expect_error(clm_allocation(p, beta = 2), "attractiveness is nonfinite")
  q <- clm_allocation(p, fit_beta = TRUE)
  expect_error(.bind_theta(q, c(sigma = 2, beta = 0)), "beta.*positive")
  expect_error(.bind_theta(q, c(sigma = 2, beta = 2)), "attractiveness is nonfinite")
  absent <- prepare_allocation(c(o1 = 1), c(f1 = 2), matrix(Inf, 1, 1, dimnames = list("o1", "f1")))
  out <- .solve_problem(clm_allocation(absent, v0 = 3), c(sigma = 2))$outputs
  expect_equal(unname(out$access), 0)
  expect_equal(unname(out$outside_share), 1)
  out <- .solve_problem(clm_allocation(absent), c(sigma = 2))$outputs
  expect_equal(unname(out$access), 0)
  expect_equal(unname(out$outside_share), 0) # existing guarded, unresolved denominator
})

test_that("numeric coverage joins origins by ID and has no raster surface", {
  f <- .prepared_fixture()
  problem <- clm_allocation(.prepare_fixture(f), v0 = 5)
  cov <- .problem_coverage(problem, c(sigma = 2), norm = 3)
  expect_identical(cov$cells$origin_id, c("o1", "o5"))
  expect_identical(cov$cells$registry_index, c(1L, 5L))
  expect_false("cell" %in% names(cov$cells))
  expect_s3_class(summary(cov), "summary.ae_coverage")
  zones <- data.frame(origin_id = c("o5", "o1"), zone = c("south", "north"))
  tab <- .coverage_aggregate(cov, zones)
  expect_equal(tab$demand[match(c("north", "south"), tab$zone)], c(8, 9))
  expect_error(.coverage_aggregate(cov, data.frame(cell = c(1, 5), zone = "x")), "spatial metadata")
  expect_error(.coverage_surface(cov), "surface metadata")
  expect_error(.problem_output_surface(problem, c(sigma = 2), problem$state$init, "access"),
               "surface metadata")
  # A fit on prepared numeric inputs keeps their declarations but has no map.
  entry <- alloc_prediction_sources()[["gaussian fixed"]]
  expect_true("access" %in% .fit_available_surfaces(entry$fit))
  expect_error(.fit_output_surface(entry$fit, "access"), "surface metadata")
  expect_identical(entry$fit$coverage_meta$input_metadata,
                   entry$case$model$substrate$input_metadata)
})

test_that("spatial mapping is independent of registry order and validates both zone keys", {
  f <- .prepared_fixture()
  template <- terra::rast(nrows = 2, ncols = 4, xmin = 20, xmax = 24, ymin = 0, ymax = 2)
  cells <- stats::setNames(c(7, 3, 8, 4, 2, 5), f$ids)
  p <- .prepare_fixture(f, spatial = list(template = template, cell_index = cells[c(5, 1, 6, 2, 4, 3)]))
  expect_s3_class(p$template, "ae_spatial_template")
  expect_identical(p$cell_index, unname(cells))
  cov <- .problem_coverage(clm_allocation(p, v0 = 5), c(sigma = 2), norm = 3)
  expect_equal(cov$cells$cell, c(7, 2))
  map <- .coverage_surface(cov)
  expect_equal(terra::values(map)[c(7, 2), 1], cov$cells$A, ignore_attr = TRUE)
  expect_true(all(is.na(terra::values(map)[-c(7, 2), 1])))
  both <- data.frame(origin_id = c("o5", "o1"), cell = c(2, 7), zone = c("s", "n"))
  expect_no_error(.coverage_aggregate(cov, both))
  both$cell <- rev(both$cell)
  expect_error(.coverage_aggregate(cov, both), "disagree")
  for (bad in list(c(1, 1, 2, 3, 4, 5), c(0, 2, 3, 4, 5, 6), c(9, 2, 3, 4, 5, 6),
                   c(2.5, 3, 4, 5, 6, 7))) {
    expect_error(.prepare_fixture(f, spatial = list(template = template, cell_index = bad)),
                 "unique valid")
  }
  wrong <- cells; names(wrong)[1] <- "other"
  expect_error(.prepare_fixture(f, spatial = list(template = template, cell_index = wrong)),
               "exactly match")
  expect_error(.prepare_fixture(f, spatial = list(template = c(template, template), cell_index = cells)),
               "one-layer")
  expect_error(.prepare_fixture(f, spatial = list(template = template)), "template and cell_index")
})
