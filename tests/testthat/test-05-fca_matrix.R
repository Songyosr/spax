# SPAX-020/007: private compact-matrix FCA path == compute_fca (engine-instance #1)

.mk_fca_matrix_data <- function() {
  demand <- terra::rast(nrows = 3, ncols = 3)
  terra::values(demand) <- c(10, 20, 30, 40, 50, 60, 70, 80, 90)

  dist1 <- terra::rast(nrows = 3, ncols = 3)
  dist2 <- terra::rast(nrows = 3, ncols = 3)
  terra::values(dist1) <- c(1, 2, 3, 2, 3, 4, 3, 4, 5)
  terra::values(dist2) <- c(5, 4, 3, 4, 3, 2, 3, 2, 1)
  distance <- c(dist1, dist2)
  names(distance) <- c("facility1", "facility2")

  supply_matrix <- matrix(
    c(10, 15, 20, 25),
    nrow = 2,
    dimnames = list(c("facility1", "facility2"), c("doctors", "nurses"))
  )
  supply_vector <- c(facility1 = 10, facility2 = 15)

  list(
    demand = demand, distance = distance,
    supply_matrix = supply_matrix, supply_vector = supply_vector
  )
}

test_that(".compute_fca_matrix equals compute_fca (single measure)", {
  td <- .mk_fca_matrix_data()
  weights <- calc_decay(td$distance, method = "gaussian", sigma = 2)

  golden <- .fca_result_raster(compute_fca(
    demand = td$demand, supply = td$supply_vector,
    demand_kernel = weights, access_kernel = weights,
    demand_normalize = "standard"
  ))
  matx <- .compute_fca_matrix(
    demand = td$demand, supply = td$supply_vector,
    demand_kernel = weights, access_kernel = weights,
    demand_normalize = "standard"
  )

  expect_equal(terra::values(matx), terra::values(golden), tolerance = 1e-8)
})

test_that(".compute_fca_matrix equals compute_fca (multi-measure names + values)", {
  td <- .mk_fca_matrix_data()
  weights <- calc_decay(td$distance, method = "gaussian", sigma = 2)

  golden <- .fca_result_raster(compute_fca(
    demand = td$demand, supply = td$supply_matrix,
    demand_kernel = weights, access_kernel = weights,
    demand_normalize = "semi"
  ))
  matx <- .compute_fca_matrix(
    demand = td$demand, supply = td$supply_matrix,
    demand_kernel = weights, access_kernel = weights,
    demand_normalize = "semi"
  )

  expect_equal(names(matx), c("doctors", "nurses"))
  expect_equal(terra::values(matx), terra::values(golden), tolerance = 1e-8)
})

test_that(".compute_fca_matrix equals compute_fca across normalize methods", {
  td <- .mk_fca_matrix_data()
  weights <- calc_decay(td$distance, method = "gaussian", sigma = 2)

  for (m in c("identity", "standard", "semi")) {
    golden <- .fca_result_raster(compute_fca(
      demand = td$demand, supply = td$supply_matrix,
      demand_kernel = weights, access_kernel = weights,
      demand_normalize = m
    ))
    matx <- .compute_fca_matrix(
      demand = td$demand, supply = td$supply_matrix,
      demand_kernel = weights, access_kernel = weights,
      demand_normalize = m
    )
    expect_equal(terra::values(matx), terra::values(golden),
                 tolerance = 1e-8, info = paste("normalize =", m))
  }
})

test_that(".k_contract(over='rows') reproduces the gather (terra::global) reduction", {
  td <- .mk_fca_matrix_data()
  weights <- calc_decay(td$distance, method = "gaussian", sigma = 2)

  D <- terra::values(td$demand)[, 1]
  K <- terra::values(weights, mat = TRUE)

  u_matrix <- .k_contract(D, K, over = "rows")
  u_terra  <- gather_weighted(td$demand, weights, simplify = TRUE)

  expect_equal(unname(u_matrix), unname(as.numeric(u_terra)), tolerance = 1e-8)
})

test_that(".compute_fca_plan is terra-free and a sparse kernel plugs in identically", {
  skip_if_not_installed("Matrix")
  td <- .mk_fca_matrix_data()
  weights <- calc_decay(td$distance, method = "gaussian", sigma = 2)

  plan <- .fca_compact_plan(
    td$demand, td$supply_matrix, weights, weights, demand_normalize = "standard"
  )
  dense <- .compute_fca_plan(plan)

  # swap the kernels for sparse Matrix -- same plan, same primitives, no terra
  plan_sparse <- plan
  plan_sparse$Kd_active <- Matrix::Matrix(plan$Kd_active, sparse = TRUE)
  plan_sparse$Ka_kept   <- Matrix::Matrix(plan$Ka_kept, sparse = TRUE)
  sparse <- .compute_fca_plan(plan_sparse)

  expect_equal(lapply(sparse, as.numeric), lapply(dense, as.numeric),
               tolerance = 1e-10)
})

test_that(".k_ratio is zero-safe and .k_normalize matches by-cell semantics", {
  expect_equal(.k_ratio(c(2, 4, 6), c(1, 0, 3), zero = 0), c(2, 0, 2))

  K <- matrix(c(1, 0, 2, 3, 0, 1), nrow = 2)  # rows sum to 1+2+0=3 ... [2 x 3]
  rs <- rowSums(K)
  # standard: each row divides by its row sum
  expect_equal(.k_normalize(K, "standard"), sweep(K, 1, rs, "/"))
  # identity: untouched
  expect_equal(.k_normalize(K, "identity"), K)
  # standard zero-row guard
  Z <- rbind(c(0, 0), c(1, 1))
  expect_equal(.k_normalize(Z, "standard"), rbind(c(0, 0), c(0.5, 0.5)))
})

test_that(".k_scale scales rows/cols identically for base and sparse matrices", {
  skip_if_not_installed("Matrix")
  K  <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)   # [2 x 3]
  sr <- c(10, 100)                              # per row
  sc <- c(2, 3, 4)                              # per col
  Ksp <- Matrix::Matrix(K, sparse = TRUE)

  expect_equal(.k_scale(K, sr, "rows"), sweep(K, 1, sr, "*"))
  expect_equal(.k_scale(K, sc, "cols"), sweep(K, 2, sc, "*"))
  expect_equal(unname(as.matrix(.k_scale(Ksp, sr, "rows"))), sweep(K, 1, sr, "*"))
  expect_equal(unname(as.matrix(.k_scale(Ksp, sc, "cols"))), sweep(K, 2, sc, "*"))
})

test_that(".k_normalize is dense/sparse-clean (diagonal scaling)", {
  skip_if_not_installed("Matrix")
  K   <- matrix(c(1, 0, 2, 3, 0, 1), nrow = 2)
  Ksp <- Matrix::Matrix(K, sparse = TRUE)
  for (m in c("identity", "standard", "semi")) {
    expect_equal(unname(as.matrix(.k_normalize(Ksp, m))),
                 unname(as.matrix(.k_normalize(K, m))),
                 info = paste("normalize =", m))
  }
})
