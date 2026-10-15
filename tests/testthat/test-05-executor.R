# Compact matrix executor (R/05-executor.R): primitives and the compact FCA
# plan that the AE models build on.

# Executor primitives ---------------------------------------------------------

test_that(".contract and .scale do plain matrix algebra", {
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

# Compact FCA plan ------------------------------------------------------------

test_that("the compact FCA plan names its measures and refuses mixed batches", {
  td <- fca_toy()
  kernel <- terra::setValues(td$distance,
                             exp(-unname(terra::values(td$distance))^2 / 8))
  demand_batch <- c(td$demand, td$demand * 2)
  names(demand_batch) <- c("children", "adults")

  # Measures are the supply columns, the demand layers of a batch, or the
  # given indicator names.
  expect_equal(.fca_compact_plan(td$demand, td$supply_matrix, kernel,
                                 kernel)$measures, c("doctors", "nurses"))
  expect_equal(.fca_compact_plan(demand_batch, td$supply_vector, kernel,
                                 kernel)$measures, c("children", "adults"))
  expect_equal(.fca_compact_plan(td$demand, td$supply_matrix, kernel, kernel,
                                 indicator_names = c("a", "b"))$measures,
               c("a", "b"))
  expect_error(.fca_compact_plan(demand_batch, td$supply_matrix, kernel, kernel),
               "product-axis semantics")
})

test_that("the compact FCA plan keeps only active demand rows for the gather", {
  td <- fca_toy()
  K <- exp(-unname(terra::values(td$distance))^2 / 8)
  K[9, ] <- NA # unreachable cell
  D <- terra::values(td$demand)[, 1]
  D[c(2, 5)] <- c(0, NA) # inactive demand
  plan <- .fca_compact_plan(terra::setValues(td$demand, D), td$supply_vector,
                            terra::setValues(td$distance, K),
                            terra::setValues(td$distance, K))
  keep <- c(1, 3, 4, 6, 7, 8, 9)
  expected_K <- K[keep, ]
  expected_K[is.na(expected_K)] <- 0
  expect_equal(plan$demand_kept_index, keep)
  expect_equal(unname(plan$D_active), D[keep])
  expect_equal(unname(plan$Kd_active), expected_K)
})
