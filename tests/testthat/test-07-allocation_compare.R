# compare_allocations() and zone coverage tables. Expected values come from
# clm_oracle() (helper-clm.R).

compare_case <- function() {
  alloc_memo("compare_case", {
    f <- alloc_small_case()
    gauss <- alloc_small_fit()
    expo_model <- clm_allocation(f$inputs, family = "exponential", fit_v0 = TRUE)
    expo <- fit_allocation(expo_model, f$observed, rbind(a = c(sigma = 0.5, v0 = 10)),
                           c(sigma = 0.01, v0 = 0.1), c(sigma = 5, v0 = 200),
                           loss = "poisson", control = list(maxit = 100))
    list(f = f, gauss = gauss, expo = expo, expo_model = expo_model)
  })
}

test_that("regional values match the oracle and loss differences are like for like", {
  k <- compare_case()
  f <- k$f
  cmp <- compare_allocations(list(gauss = k$gauss, expo = k$expo),
                             models = list(gauss = f$model, expo = k$expo_model))
  tab <- as.data.frame(cmp)
  expect_identical(tab$specification, c("gauss", "expo"))
  D <- f$demand[f$active]
  o <- clm_oracle(f$travel[f$active, , drop = FALSE], D, f$supply,
                  coef(k$gauss)[["sigma"]], coef(k$gauss)[["v0"]])
  rho <- sum(D * o$rho) / sum(D)
  A <- sum(D * o$A) / sum(D)
  expect_equal(tab$contact[1], rho, tolerance = 1e-8)
  expect_equal(tab$intensity[1], A, tolerance = 1e-8)
  expect_equal(tab$capacity_per_contact[1], A / rho, tolerance = 1e-8)
  expect_equal(tab$cv_contact[1], sd(o$rho) / mean(o$rho), tolerance = 1e-8)
  expect_equal(tab$loss_difference, c(0, k$expo$best$loss - k$gauss$best$loss))
  expect_true(all(tab$comparable))
  expect_output(print(cmp), "reference: gauss")
})

test_that("loss differences are withheld when inputs are not verified or differ", {
  k <- compare_case()
  f <- k$f
  no_models <- as.data.frame(compare_allocations(list(gauss = k$gauss, expo = k$expo)))
  expect_true(all(is.na(no_models$loss_difference)))
  expect_match(no_models$not_comparable[2], "travel not checked")
  other <- fit_allocation(f$model, f$observed * 2, f$starts, f$lower, f$upper,
                          loss = "poisson", control = list(maxit = 100))
  tab <- as.data.frame(compare_allocations(list(a = k$gauss, b = other),
                                           models = list(a = f$model, b = f$model)))
  expect_true(is.na(tab$loss_difference[2]))
  expect_match(tab$not_comparable[2], "observed")
  expect_output(print(compare_allocations(list(a = k$gauss, b = other),
                                          models = list(a = f$model, b = f$model))),
                "like-for-like")
})

test_that("zone summaries keep the identity and report class changes", {
  k <- compare_case()
  f <- k$f
  zones <- data.frame(origin_id = c("o1", "o3", "o5"), zone = c("A", "A", "B"))
  cmp <- compare_allocations(list(gauss = k$gauss, expo = k$expo), zones = zones,
                             coverage_args = list(adequacy_cut = 0.5))
  z <- as.data.frame(cmp, type = "zones")
  expect_equal(nrow(z), 4)
  expect_equal(z$A, z$rho * z$abar, tolerance = 1e-12)
  expect_true(all(cmp$table$zones_changed == "none"))
  # The same aggregation is available on a saved coverage object.
  ev <- evaluate_allocation(f$model, coef(k$gauss), coverage_args = list(adequacy_cut = 0.5))
  zc <- as.data.frame(ev$coverage, type = "zones", zones = zones)
  expect_equal(zc$rho, z$rho[z$specification == "gauss"], tolerance = 1e-8)
  expect_equal(zc$quadrant, z$quadrant[z$specification == "gauss"])
  expect_error(as.data.frame(ev$coverage, type = "zones"), "requires a zones")
  expect_error(as.data.frame(ev$coverage, zones = zones), "only to type")
  expect_error(as.data.frame(compare_allocations(list(a = k$gauss, b = k$expo)), type = "zones"),
               "no zone summaries")
})

test_that("invalid calls fail clearly", {
  k <- compare_case()
  f <- k$f
  expect_error(compare_allocations(list(k$gauss, k$expo)), "named list")
  expect_error(compare_allocations(list(a = k$gauss)), "at least two")
  expect_error(compare_allocations(list(a = k$gauss, b = k$expo), reference = "c"), "reference")
  expect_error(compare_allocations(list(a = k$gauss, b = k$expo), models = list(a = f$model)),
               "same names")
  expect_error(compare_allocations(list(a = k$gauss, b = k$expo), coverage_args = list(bad = 1)),
               "coverage_args")
  expect_error(compare_allocations(list(a = k$gauss, b = alloc_failed_search(f))),
               "no eligible allocation fit")
  wrong <- clm_allocation(prepare_allocation(f$demand, f$supply * 2, f$travel,
                                             missing_demand = "exclude"), fit_v0 = TRUE)
  expect_error(compare_allocations(list(a = k$gauss, b = k$expo),
                                   models = list(a = wrong, b = k$expo_model)),
               "does not belong")
})
