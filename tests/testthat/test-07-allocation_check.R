# check_allocation(): comparators and implied trips. Expected values come from
# clm_oracle() (helper-clm.R) and hand-computed Poisson losses, not internals.

poisson_nll <- function(U, Y) sum(pmax(U, 1e-9) - Y * log(pmax(U, 1e-9)))

test_that("a distance-generated fit beats both comparators", {
  f <- alloc_small_case()
  fit <- alloc_small_fit()
  out <- check_allocation(f$model, fit)
  expect_s3_class(out, "ae_allocation_check")
  tab <- as.data.frame(out)
  expect_identical(tab$comparator, c("fitted", "no_decay", "capacity_only"))
  expect_equal(tab$loss[1], fit$best$loss, tolerance = 1e-8)
  expect_true(all(tab$loss_difference[2:3] > 0))
  expect_false(any(out$flags))
  expect_equal(tab$loss_difference[1], 0)
  expect_output(print(out), "better than both comparators")
})

test_that("comparator losses and trips match independent calculations", {
  f <- alloc_small_case()
  fit <- alloc_small_fit()
  out <- check_allocation(f$model, fit, trip_breaks = c(2, 4))
  tab <- as.data.frame(out)
  Y <- f$observed
  # Capacity only: the Poisson optimum matches the observed total.
  U_cap <- f$supply * sum(Y) / sum(f$supply)
  expect_equal(tab$loss[3], poisson_nll(U_cap, Y), tolerance = 1e-6)
  # No decay: a flat kernel with the reported outside mass.
  v0 <- out$parameters$no_decay[["v0"]]
  travel <- f$travel[f$active, , drop = FALSE]
  flat <- clm_oracle(travel, f$demand[f$active], f$supply, sigma = 1e8, v0 = v0)
  expect_equal(tab$loss[2], poisson_nll(flat$utilization, Y), tolerance = 1e-6)
  expect_equal(tab$contact[2], sum(f$demand[f$active] * flat$rho) / sum(f$demand[f$active]),
               tolerance = 1e-6)
  # Fitted trips: flow-weighted travel-time quantiles and exceedance shares.
  truth <- clm_oracle(travel, f$demand[f$active], f$supply, coef(fit)[["sigma"]],
                      coef(fit)[["v0"]])
  w <- as.vector(truth$flow); t <- as.vector(travel); o <- order(t)
  cw <- cumsum(w[o]) / sum(w)
  expect_equal(tab$trip_median[1], t[o][which(cw >= 0.5)[1]])
  expect_equal(tab$trips_over_2[1], sum(w[t > 2]) / sum(w), tolerance = 1e-8)
  expect_equal(tab$trips_over_4[1], sum(w[t > 4]) / sum(w), tolerance = 1e-8)
})

test_that("counts that track capacity are flagged as not identifying decay", {
  f <- alloc_small_case()
  fit <- fit_allocation(f$model, f$supply * 3, f$starts, f$lower, f$upper,
                        loss = "poisson", control = list(maxit = 100))
  out <- check_allocation(f$model, fit)
  expect_true(out$flags[["capacity_only_fits_at_least_as_well"]])
  expect_output(print(out), "without distance decay")
})

test_that("absent edges are excluded from trips and reported", {
  demand <- c(a = 50, b = 40, c = 30)
  supply <- c(x = 20, y = 30)
  travel <- matrix(c(1, 3, Inf, 4, 2, 1), 3, 2, dimnames = list(names(demand), names(supply)))
  model <- clm_allocation(prepare_allocation(demand, supply, travel), fit_v0 = TRUE)
  Y <- c(x = 25, y = 45)
  fit <- fit_allocation(model, Y, rbind(s1 = c(sigma = 2, v0 = 5)),
                        c(sigma = 0.5, v0 = 0.1), c(sigma = 20, v0 = 200),
                        loss = "poisson", control = list(maxit = 100))
  tab <- as.data.frame(check_allocation(model, fit))
  # Only the capacity-only allocation sends flow along the absent edge (c -> x).
  expect_equal(tab$trips_absent_edge[1:2], c(0, 0))
  expect_equal(tab$trips_absent_edge[3], (30 * 20 / 50) / sum(demand), tolerance = 1e-8)
})

test_that("flow targets have no capacity-only comparator", {
  f <- alloc_small_case()
  fit <- fit_allocation(f$model, f$flow[, names(f$supply)], f$starts, f$lower, f$upper,
                        output = "flow", loss = "poisson", control = list(maxit = 100))
  out <- check_allocation(f$model, fit)
  tab <- as.data.frame(out)
  expect_true(all(is.na(tab[tab$comparator == "capacity_only", c("loss", "trip_median")])))
  expect_false(out$flags[["capacity_only_fits_at_least_as_well"]])
  expect_output(print(out), "facility totals only")
})

test_that("invalid calls fail clearly", {
  f <- alloc_small_case()
  fit <- alloc_small_fit()
  expect_error(check_allocation(f$model, alloc_failed_search(f)), "no eligible allocation fit")
  expect_error(check_allocation(f$model, fit, trip_breaks = c(0, 5)), "trip_breaks")
  other <- clm_allocation(f$inputs, family = "exponential", fit_v0 = TRUE)
  expect_error(check_allocation(other, fit), "does not belong")
  expect_error(check_allocation(f$model, fit$best), "ae_multistart_fit")
})
