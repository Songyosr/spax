# compute_fca(): the typed FCA engine behind the public wrappers, and the
# supply processing that binds supply values to kernel facility IDs.

# Gaussian kernel (sigma = 2.4) whose cell sums fall on both sides of 1, with
# cell 9 reaching no facility, cell 1 reaching only facility2 and the demand
# of cell 5 missing.
compute_fca_inputs <- function() {
  td <- fca_toy()
  K <- exp(-unname(terra::values(td$distance))^2 / (2 * 2.4^2))
  K[9, ] <- NA
  K[1, 1] <- NA
  D <- terra::values(td$demand)[, 1]
  D[5] <- NA
  c(td, list(D = D, K = K, demand_na = terra::setValues(td$demand, D),
             kernel = terra::setValues(td$distance, K)))
}

kernel_field <- function(kernel, axis = "facility") {
  .spax_raster_field(kernel, domain = c("I", axis),
                     frame = data.frame(layer = names(kernel),
                                        setNames(list(names(kernel)), axis)),
                     role = "kernel")
}

fca_values <- function(field) unname(terra::values(.fca_result_raster(field)))

test_that("compute_fca matches the FCA oracle for every normalization and supply", {
  f <- compute_fca_inputs()
  supplies <- list(
    vector = list(supply = f$supply_vector),
    matrix = list(supply = f$supply_matrix),
    data_frame = list(supply = f$supply_df[2:1, ], id_col = "id",
                      supply_cols = c("nurses", "doctors"))
  )
  expected_S <- list(vector = f$supply_vector, matrix = f$supply_matrix,
                     data_frame = f$supply_matrix[, 2:1])

  for (norm in c("identity", "standard", "semi")) {
    for (shape in names(supplies)) {
      result <- do.call(compute_fca, c(
        list(demand = f$demand_na, demand_kernel = f$kernel,
             access_kernel = f$kernel, demand_normalize = norm),
        supplies[[shape]]
      ))
      expect_equal(fca_values(result),
                   fca_oracle(f$D, f$K, f$K, expected_S[[shape]], norm),
                   info = paste(norm, shape))
    }
  }

  # A facility that no demand reaches gets a ratio of 0, not Inf or NaN.
  Kz <- f$K
  Kz[, 2] <- 0
  expect_equal(
    fca_values(compute_fca(f$demand_na, f$supply_vector,
                           terra::setValues(f$kernel, Kz),
                           terra::setValues(f$kernel, Kz))),
    fca_oracle(f$D, Kz, Kz, f$supply_vector)
  )
})

test_that("compute_fca returns a named realization field through either door", {
  f <- compute_fca_inputs()
  raw <- compute_fca(f$demand_na, f$supply_matrix, f$kernel, f$kernel,
                     demand_normalize = "semi")
  expect_s3_class(raw, "spax_raster_field")
  expect_equal(.field_domain(raw), c("I", "measure"))
  expect_equal(names(.fca_result_raster(raw)), c("doctors", "nurses"))

  single <- compute_fca(f$demand_na, f$supply_vector, f$kernel, f$kernel)
  expect_equal(.field_domain(single), "I")
  expect_equal(names(.fca_result_raster(single)), "supply")

  # Prepared fields give the same result as raw rasters.
  fields <- compute_fca(.spax_raster_field(f$demand_na, domain = "I"),
                        f$supply_matrix, kernel_field(f$kernel),
                        kernel_field(f$kernel), demand_normalize = "semi")
  expect_equal(fca_values(fields), fca_values(raw))

  named <- compute_fca(f$demand_na, f$supply_matrix, f$kernel, f$kernel,
                       indicator_names = c("doc_access", "nurse_access"),
                       snap = TRUE)
  expect_equal(names(.fca_result_raster(named)), c("doc_access", "nurse_access"))
  expect_equal(fca_values(named), fca_values(compute_fca(
    f$demand_na, f$supply_matrix, f$kernel, f$kernel)))
})

test_that("a supply field binds to kernel facilities by ID", {
  f <- compute_fca_inputs()
  reversed <- .spax_vector_field(c(facility2 = 15, facility1 = 10),
                                 domain = "facility", role = "supply")

  result <- compute_fca(f$demand_na, reversed, f$kernel, f$kernel,
                        indicator_names = "gp_access")
  expect_equal(names(.fca_result_raster(result)), "gp_access")
  expect_equal(fca_values(result), fca_oracle(f$D, f$K, f$K, c(10, 15)))

  expect_error(
    compute_fca(f$demand_na, .spax_vector_field(c(facility1 = 10, facility2 = 15),
                                                domain = "site"),
                f$kernel, f$kernel),
    "same axis as the kernels"
  )
  expect_error(
    compute_fca(f$demand_na, .spax_vector_field(c(facility1 = 10, facility3 = 15),
                                                domain = "facility"),
                f$kernel, f$kernel),
    "same facility ids as the kernels"
  )
})

test_that("compute_fca refuses kernels on different facilities or axes", {
  f <- compute_fca_inputs()
  other_ids <- f$kernel
  names(other_ids) <- c("facility1", "facility3")

  expect_error(compute_fca(f$demand_na, f$supply_vector, f$kernel, other_ids),
               "same facility ids")
  # compute_fca coerces both kernels to the facility axis, so the axis check
  # is reached through the pair validator.
  expect_error(.fca_check_kernel_pair(kernel_field(f$kernel),
                                      kernel_field(f$kernel, axis = "site")),
               "same non-I axis")
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
