# M2SFCA (Delamater 2013): step 2 weights ratios by the SQUARED decay.
# Closes the last ring-0 hole; expressed as the shared recipe with access = W^2.

.mk_m2_data <- function() {
  demand <- terra::rast(nrows = 3, ncols = 3)
  terra::values(demand) <- c(10, 20, 30, 40, 50, 60, 70, 80, 90)
  d1 <- terra::rast(nrows = 3, ncols = 3); terra::values(d1) <- c(1, 2, 3, 2, 3, 4, 3, 4, 5)
  d2 <- terra::rast(nrows = 3, ncols = 3); terra::values(d2) <- c(5, 4, 3, 4, 3, 2, 3, 2, 1)
  distance <- c(d1, d2); names(distance) <- c("f1", "f2")
  supply <- c(f1 = 10, f2 = 15)
  list(demand = demand, distance = distance, supply = supply)
}

test_that("spax_m2sfca == compute_fca with a squared access kernel", {
  td <- .mk_m2_data()
  W <- calc_decay(td$distance, method = "gaussian", sigma = 2)

  m2 <- spax_m2sfca(td$demand, td$supply, td$distance,
                    decay_params = list(method = "gaussian", sigma = 2),
                    demand_normalize = "identity")
  via_fca <- .fca_result_raster(compute_fca(
    td$demand, td$supply, demand_kernel = W, access_kernel = W * W,
    demand_normalize = "identity"
  ))

  expect_s3_class(m2, "spax")
  expect_equal(terra::values(m2$accessibility), terra::values(via_fca))
})

test_that("spax_m2sfca matches the manual sum_j R_j W_ij^2 formula", {
  td <- .mk_m2_data()
  W <- calc_decay(td$distance, method = "gaussian", sigma = 2)

  # step 1 (== E2SFCA): R_j = S_j / sum_i D_i W_ij
  U <- gather_weighted(td$demand, W, simplify = TRUE)
  R <- td$supply[names(U)] / U
  # step 2 (M2SFCA): A_i = sum_j R_j W_ij^2
  manual <- spread_weighted(matrix(R, ncol = 1, dimnames = list(names(R), "supply")), W * W)

  m2 <- spax_m2sfca(td$demand, td$supply, td$distance,
                    decay_params = list(method = "gaussian", sigma = 2),
                    demand_normalize = "identity")

  expect_equal(unname(terra::values(m2$accessibility)), unname(terra::values(manual)))
})

test_that("M2SFCA differs from E2SFCA (the squared weight has an effect)", {
  td <- .mk_m2_data()
  args <- list(demand = td$demand, supply = td$supply, distance = td$distance,
               decay_params = list(method = "gaussian", sigma = 2),
               demand_normalize = "identity")
  m2 <- do.call(spax_m2sfca, args)
  e2 <- do.call(spax_e2sfca, args)
  expect_false(isTRUE(all.equal(
    terra::values(m2$accessibility), terra::values(e2$accessibility)
  )))
})
