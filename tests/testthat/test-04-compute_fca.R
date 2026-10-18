# compute_fca(): the internal driver behind the public FCA wrappers (SPAX-055).
# The two-pass engine, facility IDs and the kept errors are covered in
# test-05-fca_blocks.R; here the driver is checked against direct algebra.

# Gaussian kernel (sigma = 2.4) whose cell sums fall on both sides of 1 (cell
# 1 sums to 1.031, so "semi" differs from "identity"), with cell 9 reaching no
# facility, cell 3 reaching only facility2 and the demand of cell 5 missing.
compute_fca_inputs <- function() {
  td <- fca_toy()
  K <- exp(-unname(terra::values(td$distance))^2 / (2 * 2.4^2))
  K[9, ] <- NA
  K[3, 1] <- NA
  D <- terra::values(td$demand)[, 1]
  D[5] <- NA
  distance <- terra::setValues(td$distance, K)
  # `distance` holds the kernel values themselves; the kernels read them.
  kernels <- list(
    demand = function(b, rows) b$distance[rows, , drop = FALSE],
    access = function(b, rows) b$distance[rows, , drop = FALSE]
  )
  c(td, list(D = D, K = K, demand_na = terra::setValues(td$demand, D),
             kernel_raster = distance, kernels = kernels))
}

run_fca <- function(f, supply, ..., demand = f$demand_na, kernels = f$kernels) {
  compute_fca(demand, supply, list(distance = f$kernel_raster), kernels, ...)
}

test_that("compute_fca gives the hand-computed access of one measure", {
  # One facility, three cells in a row: U = 10 * 1 + 20 * 0.5 + 30 * 0.25 = 27.5,
  # R = 7 / 27.5 and A = R * (1, 0.5, 0.25).
  demand <- terra::rast(nrows = 1, ncols = 3)
  terra::values(demand) <- c(10, 20, 30)
  w <- terra::rast(demand)
  terra::values(w) <- c(1, 0.5, 0.25)
  names(w) <- "f"
  kernel <- function(b, rows) b$w[rows, , drop = FALSE]
  out <- compute_fca(demand, c(f = 7), list(w = w),
                     list(demand = kernel, access = kernel))
  expect_s4_class(out, "SpatRaster")
  expect_equal(names(out), "supply")
  expect_equal(unname(terra::values(out)[, 1]), 7 / 27.5 * c(1, 0.5, 0.25))
})

test_that("compute_fca matches direct algebra for every normalization and supply", {
  f <- compute_fca_inputs()
  supplies <- list(
    vector = list(supply = f$supply_vector),
    matrix = list(supply = f$supply_matrix),
    data_frame = list(supply = f$supply_df[2:1, ], id_col = "id",
                      supply_cols = c("nurses", "doctors"))
  )
  expected_S <- list(vector = f$supply_vector, matrix = f$supply_matrix,
                     data_frame = f$supply_matrix[, 2:1])

  results <- list()
  for (norm in c("identity", "standard", "semi")) {
    for (shape in names(supplies)) {
      result <- do.call(run_fca, c(list(f), supplies[[shape]],
                                   list(demand_normalize = norm)))
      results[[norm]] <- unname(terra::values(result))
      expect_equal(results[[norm]],
                   fca_oracle(f$D, f$K, f$K, expected_S[[shape]], norm),
                   info = paste(norm, shape))
    }
  }
  # Guard the fixture: the three normalizations must give different results.
  expect_false(isTRUE(all.equal(results$semi, results$identity)))
  expect_false(isTRUE(all.equal(results$semi, results$standard)))

  # A facility that no demand reaches gets a ratio of 0, not Inf or NaN, even
  # though the access step still reaches it.
  Kd <- f$K
  Kd[, 2] <- 0
  kernels <- list(
    demand = function(b, rows) {
      matrix(Kd[rows, ], nrow = length(rows))
    },
    access = f$kernels$access
  )
  expect_equal(
    unname(terra::values(run_fca(f, f$supply_vector, kernels = kernels))),
    fca_oracle(f$D, Kd, f$K, f$supply_vector)
  )
})

test_that("compute_fca names the layers and keeps the grid of the first raster", {
  f <- compute_fca_inputs()
  two <- run_fca(f, f$supply_matrix, demand_normalize = "semi")
  expect_equal(names(two), c("doctors", "nurses"))
  expect_true(terra::compareGeom(two, f$kernel_raster, stopOnError = FALSE))
  one <- run_fca(f, f$supply_vector)
  expect_equal(names(one), "supply")
  expect_equal(terra::nlyr(one), 1)

  named <- run_fca(f, f$supply_matrix,
                   indicator_names = c("doc_access", "nurse_access"), snap = TRUE)
  expect_equal(names(named), c("doc_access", "nurse_access"))
  expect_equal(terra::values(named), terra::values(run_fca(f, f$supply_matrix)),
               ignore_attr = TRUE)

  # A supply table in another row order binds by facility ID.
  reversed <- run_fca(f, f$supply_df[2:1, ], id_col = "id",
                      supply_cols = "doctors", indicator_names = "gp_access")
  expect_equal(names(reversed), "gp_access")
  expect_equal(unname(terra::values(reversed)),
               fca_oracle(f$D, f$K, f$K, c(10, 15)))
})

test_that("compute_fca refuses multi-layer demand and wrong supply", {
  f <- compute_fca_inputs()
  for (snap in c(FALSE, TRUE)) {
    expect_error(run_fca(f, f$supply_vector, demand = c(f$demand_na, f$demand_na),
                         snap = snap),
                 "multi-layer raster fields must declare at least one non-I axis")
  }
  expect_error(run_fca(f, f$supply_vector, indicator_names = c("a", "b")),
               "indicator_names must have one name")
  expect_error(run_fca(f, c(facility1 = 10, facility3 = 15)),
               "Some weight layer IDs not found")
})

test_that(".help_process_supply binds supply values to facility IDs", {
  ids <- c("facility1", "facility2")
  td <- fca_toy()

  # Data frames and named vectors bind by ID; matrices and unnamed vectors by
  # position.
  from_df <- .help_process_supply(td$supply_df[2:1, ], id_col = "id",
                                  supply_cols = c("doctors", "nurses"),
                                  weight_ids = ids)
  expect_equal(from_df$values, td$supply_matrix, ignore_attr = TRUE)
  expect_equal(from_df[c("ids", "cols")],
               list(ids = ids, cols = c("doctors", "nurses")))
  from_vector <- .help_process_supply(c(facility2 = 45, facility1 = 30),
                                      weight_ids = ids)
  expect_equal(as.numeric(from_vector$values), c(30, 45))
  expect_equal(from_vector[c("ids", "cols")], list(ids = ids, cols = "supply"))
  expect_equal(.help_process_supply(td$supply_matrix, weight_ids = ids)$ids, ids)
  expect_equal(.help_process_supply(c(30, 45), weight_ids = ids)$ids, ids)

  # Without kernel IDs the supply order and generated names are kept.
  expect_equal(.help_process_supply(td$supply_df, id_col = "id",
                                    supply_cols = "nurses")$ids, ids)
  unnamed <- .help_process_supply(unname(td$supply_matrix))
  expect_equal(unnamed[c("ids", "cols")],
               list(ids = c("facility_1", "facility_2"),
                    cols = c("measure_1", "measure_2")))
  expect_equal(.help_process_supply(c(30, 45))$ids, c("facility_1", "facility_2"))

  # A missing ID is an error, not a silent misalignment.
  expect_error(.help_process_supply(c(facility1 = 30, other = 45), weight_ids = ids),
               "not found in supply data")
  expect_error(.help_process_supply(td$supply_df[1, ], id_col = "id",
                                    supply_cols = "doctors", weight_ids = ids),
               "not found in supply data")
  expect_error(.help_process_supply(factor(c("a", "b"))),
               "Unsupported supply data type")
})
