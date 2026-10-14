# Compact matrix executor (R/05-executor.R). The primitives serve the AE
# models; the compact FCA executor at the end is benchmark-only.

# Executor primitives ---------------------------------------------------------

test_that(".contract, .scale and .safe_ratio do plain matrix algebra", {
  K <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2) # [2 x 3]
  v_rows <- c(10, 100)
  v_cols <- c(2, 3, 4)

  # over = "rows" sums over dim 1 (gather); over = "cols" over dim 2 (spread).
  expect_equal(.contract(v_rows, K, over = "rows"), colSums(K * v_rows))
  expect_equal(.contract(v_cols, K, over = "cols"),
               rowSums(K * rep(v_cols, each = 2)))
  expect_equal(.contract(cbind(v_cols, 1), K, over = "cols"),
               cbind(rowSums(K * rep(v_cols, each = 2)), rowSums(K)),
               ignore_attr = TRUE)

  expect_equal(.scale(K, v_rows, "rows"), sweep(K, 1, v_rows, "*"))
  expect_equal(.scale(K, v_cols, "cols"), sweep(K, 2, v_cols, "*"))
  expect_error(.scale(K, v_rows, "diagonal"), "should be one of")

  expect_equal(.safe_ratio(c(2, 4, 6, 0), c(1, 0, 3, 0), zero = 0), c(2, 0, 2, 0))
  expect_equal(.safe_ratio(c(2, 4), c(1, 0), zero = -1), c(2, -1))
})

test_that(".normalize divides over a margin and the FCA policy wraps it", {
  K <- matrix(c(1, 0, 2, 3, 0, 1), nrow = 2) # row sums 3, 4
  expect_equal(.normalize(K, over = "cols"), sweep(K, 1, rowSums(K), "/"))
  expect_equal(.normalize(K, over = "rows"), sweep(K, 2, colSums(K), "/"))
  # zero-sum guard
  expect_equal(.normalize(rbind(c(0, 0), c(1, 1)), over = "cols"),
               rbind(c(0, 0), c(0.5, 0.5)))

  K2 <- rbind(c(0.2, 0.3), c(2, 1))
  expect_equal(.fca_normalize_kernel(K2, "identity"), K2)
  expect_equal(.fca_normalize_kernel(K2, "standard"),
               rbind(c(0.4, 0.6), c(2 / 3, 1 / 3)))
  expect_equal(.fca_normalize_kernel(K2, "semi"),
               rbind(c(0.2, 0.3), c(2 / 3, 1 / 3)))
  expect_error(.fca_normalize_kernel(K2, "bad"), "should be one of")
})

test_that("sparse Matrix inputs give the same primitives as dense ones", {
  skip_if_not_installed("Matrix")
  K <- matrix(c(1, 0, 2, 3, 0, 1), nrow = 2)
  Ksp <- Matrix::Matrix(K, sparse = TRUE)
  dense <- function(x) unname(as.matrix(x))

  expect_equal(dense(.scale(Ksp, c(10, 100), "rows")), .scale(K, c(10, 100), "rows"))
  expect_equal(dense(.scale(Ksp, c(2, 3, 4), "cols")), .scale(K, c(2, 3, 4), "cols"))
  for (over in c("rows", "cols")) {
    expect_equal(dense(.normalize(Ksp, over = over)), .normalize(K, over = over),
                 info = over)
  }
  for (m in c("identity", "standard", "semi")) {
    expect_equal(dense(.fca_normalize_kernel(Ksp, m)), .fca_normalize_kernel(K, m),
                 info = m)
  }
  expect_equal(.contract(c(10, 100), Ksp, over = "rows"),
               .contract(c(10, 100), K, over = "rows"))
})

test_that(".rewrap_cells writes values back onto the template", {
  template <- toy_rast(0, 2, 2)

  full <- .rewrap_cells(c(1, 2, 3, 4), template)
  expect_true(terra::compareGeom(full, template))
  expect_equal(terra::values(full)[, 1], c(1, 2, 3, 4))
  # Only the kept cells are filled; the rest are NA.
  kept <- .rewrap_cells(c(7, 9), template, kept_cell_index = c(2, 4))
  expect_equal(terra::values(kept)[, 1], c(NA, 7, NA, 9))
})

# Benchmark-only compact FCA executor ------------------------------------------
# .fca_compact_plan(), .compute_fca_plan() and .compute_fca_matrix() are used
# only by benchmarks (maintainer decision pending). Delete this section
# together with them.

test_that(".compute_fca_matrix matches the FCA oracle for every normalization", {
  td <- fca_toy()
  K <- exp(-unname(terra::values(td$distance))^2 / (2 * 2.4^2))
  K[9, ] <- NA # unreachable cell
  D <- terra::values(td$demand)[, 1]
  D[c(2, 5)] <- c(0, NA) # inactive demand rows are compacted away
  demand <- terra::setValues(td$demand, D)
  kernel <- terra::setValues(td$distance, K)
  S <- unname(td$supply_matrix)

  for (norm in c("identity", "standard", "semi")) {
    matx <- .compute_fca_matrix(demand, td$supply_matrix, kernel, kernel,
                                demand_normalize = norm)
    expect_equal(names(matx), c("doctors", "nurses"))
    expect_equal(unname(terra::values(matx)), fca_oracle(D, K, K, S, norm),
                 info = norm)
  }
  single <- .compute_fca_matrix(demand, td$supply_vector, kernel, kernel,
                                indicator_names = "gp_access")
  expect_equal(names(single), "gp_access")
  expect_equal(unname(terra::values(single)), fca_oracle(D, K, K, S[, 1]))

  # The terra-free core takes sparse kernels unchanged.
  skip_if_not_installed("Matrix")
  plan <- .fca_compact_plan(demand, td$supply_matrix, kernel, kernel,
                            demand_normalize = "standard")
  sparse <- plan
  sparse$Kd_active <- Matrix::Matrix(plan$Kd_active, sparse = TRUE)
  sparse$Ka_kept <- Matrix::Matrix(plan$Ka_kept, sparse = TRUE)
  expect_equal(lapply(.compute_fca_plan(sparse), as.numeric),
               .compute_fca_plan(plan), tolerance = 1e-10)
})

test_that("the compact executor batches demand layers or supply measures, not both", {
  td <- fca_toy()
  K <- exp(-unname(terra::values(td$distance))^2 / 8)
  kernel <- terra::setValues(td$distance, K)
  D <- terra::values(td$demand)[, 1]
  demand_batch <- c(td$demand, td$demand * 2)
  names(demand_batch) <- c("children", "adults")

  batched <- .compute_fca_matrix(demand_batch, td$supply_vector, kernel, kernel,
                                 demand_normalize = "standard")
  expect_equal(names(batched), c("children", "adults"))
  expect_equal(
    unname(terra::values(batched)),
    cbind(fca_oracle(D, K, K, td$supply_vector, "standard"),
          fca_oracle(2 * D, K, K, td$supply_vector, "standard"))
  )
  expect_error(.fca_compact_plan(demand_batch, td$supply_matrix, kernel, kernel),
               "product-axis semantics")
})
