.prepared_fixture <- function() {
  ids <- paste0("o", 1:6)
  D <- stats::setNames(c(8, 0, NA, 12, 9, 0), ids)
  M <- matrix(c(1, 2, 3, 2, Inf, -10, 4, 3, 2, 1, Inf, NA), 6, 2,
              dimnames = list(ids, c("b", "a")))
  included <- stats::setNames(c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE), ids)
  list(D = D, S = c(a = 15, b = 20), M = M, ids = ids, included = included)
}

.prepare_fixture <- function(f, ...) {
  .prepare_allocation(f$D, f$S, f$M, included = f$included,
                      missing_demand = "exclude", ...)
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
  q <- .prepare_allocation(f$D[demand_order], rev(f$S), f$M[rows, c(2, 1)],
    origin_ids = f$ids, facility_ids = c("b", "a"),
    included = f$included[rev(seq_along(f$included))], missing_demand = "exclude")
  expect_identical(q, p)
  unnamed_mask <- unname(f$included[demand_order])
  q <- .prepare_allocation(f$D[demand_order], f$S, f$M, origin_ids = f$ids,
    included = unnamed_mask, missing_demand = "exclude")
  expect_identical(q, p)
})

test_that("checked support validates only active travel and explicit missing policies", {
  f <- .prepared_fixture()
  expect_error(.prepare_allocation(f$D, f$S, f$M), "missing demand")
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
    expect_error(.prepare_allocation(f$D, f$S, f$M, origin_ids = bad), "ID")
  }
  expect_error(.prepare_allocation(unname(f$D), f$S, f$M), "origin_ids")
  wrong <- f$M; rownames(wrong)[1] <- "unknown"
  expect_error(.prepare_allocation(f$D, f$S, wrong, missing_demand = "exclude"), "exactly match")
  expect_error(.prepare_allocation(f$D, f$S, t(f$M), facility_ids = c("b", "a")), "origin-by-facility")
  expect_error(.prepare_allocation(f$D, f$S[-1], f$M), "supply must")
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
  problem <- .huff_problem(p, v0 = 5)
  cov <- .problem_coverage(problem, c(sigma = 2))
  expect_identical(cov$input_metadata, p$input_metadata)
  expect_identical(cov$input_policy, p$input_policy)
})

test_that("all providers accept prepared inputs and reproduce matched raster numerics", {
  d <- terra::rast(nrows = 1, ncols = 4)
  terra::values(d) <- c(10, 0, NA, 20)
  dist <- c(d, d); names(dist) <- c("b", "a")
  terra::values(dist) <- cbind(c(0, -1, NA, 3), c(2, NA, -1, NA))
  S <- c(a = 15, b = 30)
  D <- stats::setNames(terra::values(d)[, 1], as.character(1:4))
  M <- terra::values(dist); rownames(M) <- names(D)
  prepared <- .prepare_allocation(D, S, M, missing_demand = "exclude", missing_travel = "legacy_zero")
  for (constructor in list(.huff_problem, .sae_problem, .haae_problem)) {
    a <- constructor(d, S, dist)
    b <- constructor(prepared)
    theta <- c(sigma = 2)
    ka <- .bind_theta(a, theta)$plan$Kd_active
    kb <- .bind_theta(b, theta)$plan$Kd_active
    expect_equal(unname(ka), unname(kb), tolerance = 1e-14)
    ao <- .problem_outputs_at(a, theta, a$state$init)
    bo <- .problem_outputs_at(b, theta, b$state$init)
    expect_equal(lapply(ao, unname), lapply(bo, unname), tolerance = 1e-12)
    expect_error(constructor(prepared, supply = S), "cannot be combined")
    expect_error(constructor(prepared, distance = dist), "cannot be combined")
    expect_error(constructor(prepared, supply_cols = "x"), "cannot be combined")
  }
  legacy <- .interaction_substrate(d, S, dist)
  expect_identical(legacy$input_policy$missing_demand, "legacy_positive_fit")
  expect_identical(legacy$input_policy$missing_travel, "legacy_zero")
  terra::values(d) <- c(-1, Inf, NA, 20)
  legacy <- .interaction_substrate(d, S, dist)
  expect_identical(legacy$origin_status, c("invalid_omitted", "invalid_omitted", "unknown", "active"))
  shifted <- terra::shift(dist, dx = 90)
  expect_error(.interaction_substrate(d, S, shifted), "align|extent")
})

test_that("strict providers reject invalid finite kernel and attractiveness transforms", {
  p <- .prepare_allocation(c(o1 = 1), c(f1 = 2), matrix(0, 1, 1, dimnames = list("o1", "f1")))
  for (constructor in list(.huff_problem, .sae_problem, .haae_problem)) {
    q <- constructor(p, family = "power")
    expect_error(.bind_theta(q, c(sigma = 2)), "strictly positive")
    expect_error(.bind_theta(constructor(p), c(sigma = 0)), "positive sigma")
  }
  p <- .prepare_allocation(c(o1 = 1), c(f1 = 1e200), matrix(1, 1, 1, dimnames = list("o1", "f1")))
  expect_error(.huff_problem(p, beta = 2), "attractiveness is nonfinite")
  q <- .huff_problem(p, fit_beta = TRUE)
  expect_error(.bind_theta(q, c(sigma = 2, beta = 0)), "beta.*positive")
  expect_error(.bind_theta(q, c(sigma = 2, beta = 2)), "attractiveness is nonfinite")
  for (constructor in list(.sae_problem, .haae_problem)) {
    expect_error(.bind_theta(constructor(p, kappa = 1e200), c(sigma = 2)), "scaled supply is nonfinite")
  }
  absent <- .prepare_allocation(c(o1 = 1), c(f1 = 2), matrix(Inf, 1, 1, dimnames = list("o1", "f1")))
  out <- .solve_problem(.huff_problem(absent, v0 = 3), c(sigma = 2))$outputs
  expect_equal(unname(out$access), 0)
  expect_equal(unname(out$outside_share), 1)
  out <- .solve_problem(.huff_problem(absent), c(sigma = 2))$outputs
  expect_equal(unname(out$access), 0)
  expect_equal(unname(out$outside_share), 0) # existing guarded, unresolved denominator
})

test_that("numeric coverage joins origins and summarizes without a raster", {
  f <- .prepared_fixture()
  p <- .prepare_fixture(f)
  problem <- .huff_problem(p, v0 = 5)
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
  expect_error(.problem_output_surface(problem, c(sigma = 2), problem$state$init, "access"), "surface metadata")
  y <- .solve_problem(problem, c(sigma = 2))$outputs$utilization
  fit <- .fit_problem_nfxp(problem, y, c(sigma = 2), c(sigma = .5), c(sigma = 5))
  expect_true("access" %in% .fit_available_surfaces(fit))
  expect_error(.fit_output_surface(fit, "access"), "surface metadata")
  expect_identical(fit$coverage_meta$input_metadata, p$input_metadata)
})

test_that("spatial mapping is independent of registry order and validates both zone keys", {
  f <- .prepared_fixture()
  template <- terra::rast(nrows = 2, ncols = 4, xmin = 20, xmax = 24, ymin = 0, ymax = 2)
  cells <- stats::setNames(c(7, 3, 8, 4, 2, 5), f$ids)
  p <- .prepare_fixture(f, spatial = list(template = template, cell_index = cells[c(5, 1, 6, 2, 4, 3)]))
  expect_s3_class(p$template, "ae_spatial_template")
  expect_identical(p$cell_index, unname(cells))
  problem <- .huff_problem(p, v0 = 5)
  cov <- .problem_coverage(problem, c(sigma = 2), norm = 3)
  expect_equal(cov$cells$cell, c(7, 2))
  map <- .coverage_surface(cov)
  expect_equal(terra::values(map)[c(7, 2), 1], cov$cells$A, ignore_attr = TRUE)
  expect_true(all(is.na(terra::values(map)[-c(7, 2), 1])))
  both <- data.frame(origin_id = c("o5", "o1"), cell = c(2, 7), zone = c("s", "n"))
  expect_no_error(.coverage_aggregate(cov, both))
  both$cell <- rev(both$cell)
  expect_error(.coverage_aggregate(cov, both), "disagree")
  for (bad in list(c(1, 1, 2, 3, 4, 5), c(0, 2, 3, 4, 5, 6), c(9, 2, 3, 4, 5, 6))) {
    expect_error(.prepare_fixture(f, spatial = list(template = template, cell_index = bad)), "unique valid")
  }
  wrong <- cells; names(wrong)[1] <- "other"
  expect_error(.prepare_fixture(f, spatial = list(template = template, cell_index = wrong)), "exactly match")
  expect_error(.prepare_fixture(f, spatial = list(template = c(template, template), cell_index = cells)), "one-layer")

  # A fresh R process proves no live terra pointer or source raster is needed.
  saved <- tempfile(fileext = ".rds"); result <- tempfile(fileext = ".rds")
  script <- tempfile(fileext = ".R")
  on.exit(unlink(c(saved, result, script)), add = TRUE)
  saveRDS(list(prepared = p, coverage = cov), saved)
  package_path <- getNamespaceInfo(asNamespace("spax"), "path")
  package_mode <- if (pkgload::is_dev_package("spax")) "source" else "installed"
  writeLines(c("a <- commandArgs(TRUE)",
    "if (a[4] == 'source') pkgload::load_all(a[1], quiet = TRUE) else library('spax', lib.loc = dirname(a[1]))",
    "x <- readRDS(a[2])", "m <- spax:::.coverage_surface(x$coverage)",
    "q <- spax:::.huff_problem(x$prepared, v0 = 5)",
    "z <- spax:::.problem_coverage(q, c(sigma = 2), norm = 3)",
    "saveRDS(list(values = terra::values(m), extent = as.vector(terra::ext(m)),",
    "             crs = terra::crs(m), cells = z$cells), a[3])"), script)
  status <- system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(script), shQuote(package_path),
      shQuote(saved), shQuote(result), package_mode), stdout = TRUE, stderr = TRUE)
  expect_null(attr(status, "status"), info = paste(status, collapse = "\n"))
  restored <- readRDS(result)
  expect_equal(restored$values, terra::values(map))
  expect_equal(restored$extent, as.vector(terra::ext(template)))
  expect_identical(restored$crs, terra::crs(template))
  expect_equal(restored$cells, cov$cells)
})

test_that("prepared reuse performs no further raster extraction", {
  d <- terra::rast(nrows = 1, ncols = 4); terra::values(d) <- c(5, 0, NA, 7)
  distance <- c(d, d); terra::values(distance) <- cbind(1:4, 4:1)
  names(distance) <- c("a", "b")
  original <- terra::extract; calls <- 0L
  testthat::local_mocked_bindings(extract = function(...) {
    calls <<- calls + 1L; original(...)
  }, .package = "terra")
  p <- .interaction_substrate(d, c(a = 10, b = 20), distance)
  expect_equal(calls, 1L)
  for (constructor in list(.huff_problem, .sae_problem, .haae_problem)) {
    problem <- constructor(p)
    .problem_outputs_at(problem, c(sigma = 2), problem$state$init)
  }
  expect_equal(calls, 1L)
})
