.extraction_fixture <- function() {
  demand <- terra::rast(nrows = 2, ncols = 3)
  names(demand) <- "demand"
  terra::values(demand) <- c(10, 0, NA, 20, 30, 5)
  distance <- c(demand, demand)
  names(distance) <- c("a", "b")
  terra::values(distance) <- cbind(c(0, 1, 2, NA, 5, Inf),
                                   c(3, 2, 1, NA, 4, 6))
  list(demand = demand, distance = distance, supply = c(b = 25, a = 50))
}

test_that("selective distances equal full extraction, including missing and zero edges", {
  f <- .extraction_fixture()
  full <- terra::values(f$distance, mat = TRUE)
  p <- .interaction_substrate(f$demand, f$supply, f$distance)
  expect_identical(p$demand_kept_index, c(1L, 4L, 5L, 6L))
  expect_identical(p$distance_active, full[p$demand_kept_index, , drop = FALSE])
  expect_equal(p$D_active, c(10, 20, 30, 5))
  expect_equal(as.vector(p$S), c(50, 25))
  expect_equal(p$facility_ids, c("a", "b"))
  expect_equal(p$distance_active[1, 1], 0, ignore_attr = TRUE)
  expect_true(all(is.na(p$distance_active[2, ])))
  expect_true(is.infinite(p$distance_active[4, 1]))
})

test_that("partial support never requests the full distance values matrix", {
  f <- .extraction_fixture()
  original_values <- terra::values
  original_extract <- terra::extract
  extracted <- NULL
  testthat::local_mocked_bindings(
    values = function(x, ...) {
      if (terra::nlyr(x) > 1) stop("unexpected full distance read")
      original_values(x, ...)
    },
    extract = function(x, y, ...) {
      extracted <<- y
      original_extract(x, y, ...)
    }, .package = "terra")
  p <- .interaction_substrate(f$demand, f$supply, f$distance)
  expect_identical(extracted, p$demand_kept_index)
  expect_equal(nrow(p$distance_active), 4)
})

test_that("file-backed, single-layer, all-active and empty extraction preserve shape", {
  f <- .extraction_fixture()
  path <- tempfile(fileext = ".tif")
  # Use finite/NA values here: GeoTIFF handling of Inf is driver-specific.
  v <- terra::values(f$distance); v[is.infinite(v)] <- NA_real_
  terra::values(f$distance) <- v
  disk <- terra::writeRaster(f$distance, path, overwrite = TRUE, datatype = "FLT8S")
  on.exit(unlink(path), add = TRUE)
  names(disk) <- names(f$distance)
  full_disk <- terra::values(disk, mat = TRUE)
  expect_identical(is.na(full_disk), is.na(v))
  p <- .interaction_substrate(f$demand, f$supply, disk)
  expect_identical(p$distance_active, full_disk[p$demand_kept_index, , drop = FALSE])
  single <- .interaction_substrate(f$demand, c(a = 50), disk[[1]])
  expect_equal(dim(single$distance_active), c(4, 1))
  terra::values(f$demand) <- seq_len(6)
  all <- .interaction_substrate(f$demand, f$supply, disk)
  expect_identical(all$distance_active, full_disk)
  terra::values(f$demand) <- c(0, NA, 0, NA, 0, NA)
  empty <- .interaction_substrate(f$demand, f$supply, disk)
  expect_equal(dim(empty$distance_active), c(0, 2))
  expect_length(empty$demand_kept_index, 0)
})

test_that("preparation refreshes changed support and facility order without imputing maps", {
  f <- .extraction_fixture()
  p <- .huff_problem(f$demand, f$supply, f$distance, v0 = 10)
  before <- .problem_coverage(p, c(sigma = 2))
  v <- terra::values(.coverage_surface(before, "rho"))[, 1]
  expect_true(is.na(v[2])) # valid zero demand, not a prediction
  expect_true(is.na(v[3])) # unknown demand, not silently converted to zero
  expect_equal(unname(v[4]), 0) # positive demand with no reachable facility
  terra::values(f$demand)[2, 1] <- 15
  distance <- f$distance[[c(2, 1)]]
  q <- .huff_problem(f$demand, f$supply, distance, v0 = 10)
  expect_identical(q$substrate$demand_kept_index, c(1L, 2L, 4L, 5L, 6L))
  expect_identical(q$substrate$facility_ids, c("b", "a"))
  expect_identical(q$substrate$distance_active,
                   terra::values(distance)[q$substrate$demand_kept_index, , drop = FALSE])
  after <- .problem_coverage(q, c(sigma = 2))
  expect_true(after$cells$rho[after$cells$cell == 2] > 0)
  expect_equal(before$cells$rho, after$cells$rho[match(before$cells$cell, after$cells$cell)],
                 tolerance = 1e-12)
})
