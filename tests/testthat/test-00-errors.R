# Errors name the call that the user made, never an internal helper (.stop(),
# .user_call()).

error_of <- function(expr) tryCatch(expr, error = identity)

test_that("an error found by an internal helper names the exported call", {
  f <- alloc_small_case()
  # the supply names do not match the travel columns
  wrong <- setNames(f$supply, c("a", "b", "c"))
  err <- error_of(prep_data(f$demand, wrong, f$travel, missing_demand = "exclude"))
  expect_s3_class(err, "error")
  expect_identical(conditionCall(err)[[1L]], quote(prep_data))
  # a namespaced call keeps its namespace
  err <- error_of(spax::prep_data(f$demand, wrong, f$travel, missing_demand = "exclude"))
  expect_identical(conditionCall(err)[[1L]], quote(spax::prep_data))
})

test_that("the outermost spax call is named, not the functions it calls", {
  f <- alloc_small_case()
  wrong <- setNames(f$supply, c("a", "b", "c"))
  # spax_clm() calls prep_data() and spax() in turn
  err <- error_of(spax_clm(f$demand, wrong, f$travel,
                           data_args = list(missing_demand = "exclude")))
  expect_s3_class(err, "error")
  expect_identical(conditionCall(err)[[1L]], quote(spax_clm))
})

test_that("an error inside a spax method names the generic call", {
  f <- alloc_small_case()
  d <- prep_data(f$demand, f$supply, f$travel, missing_demand = "exclude")
  result <- spax(d, spec_clm(decay = decay_gaussian(sigma = 3), outside = 10))
  # travel to two of the three facilities only
  err <- error_of(predict(result, f$travel[, 1:2]))
  expect_s3_class(err, "error")
  expect_identical(conditionCall(err)[[1L]], quote(predict))
})

test_that(".stop() builds the message as stop() does and passes conditions on", {
  outside_spax <- function() .stop("ids: ", c("a", "b"), 3)
  err <- error_of(outside_spax())
  expect_identical(conditionMessage(err), "ids: ab3")
  # no spax call on the stack: no call is named
  expect_null(conditionCall(err))
  expect_null(conditionCall(error_of(.stop("x", call. = FALSE))))
  # a condition object is signalled as it is, with its class
  expect_error(.stop(errorCondition("kept", class = "spax_test_condition")),
               class = "spax_test_condition")
})
