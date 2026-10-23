# Errors name the call that the user made, never an internal helper (.stop(),
# .user_call()).

error_of <- function(expr) tryCatch(expr, error = identity)

# A function as a user writes it, with the variables it needs. Tests run in an
# environment inside the spax namespace, so a function defined in a test would
# count as spax code; a user's functions live under the global environment.
as_user_code <- function(.fun, ...) {
  env <- new.env(parent = globalenv())
  vars <- list(...)
  for (name in names(vars)) assign(name, vars[[name]], envir = env)
  environment(.fun) <- env
  .fun
}

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

test_that("the user's call is named, not the spax functions it calls", {
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

test_that("calls through lapply(), do.call(), lazy arguments and callbacks", {
  f <- alloc_small_case()
  wrong <- setNames(f$supply, c("a", "b", "c"))
  # a function given to lapply(): the call that lapply() makes, as base R names it
  err <- error_of(lapply(list(f$demand), prep_data, supply = wrong, travel = f$travel,
                         missing_demand = "exclude"))
  expect_identical(conditionCall(err)[[1L]], quote(FUN))
  # a call inside the user's own function given to lapply()
  user_fun <- as_user_code(function(s) {
    prep_data(f$demand, s, f$travel, missing_demand = "exclude")
  }, f = f)
  err <- error_of(lapply(list(wrong), user_fun))
  expect_identical(conditionCall(err)[[1L]], quote(prep_data))
  # do.call() with the function itself: named by its spax name
  err <- error_of(do.call(prep_data, list(f$demand, wrong, f$travel,
                                          missing_demand = "exclude")))
  expect_identical(conditionCall(err)[[1L]], quote(prep_data))
  # an argument evaluated later: the call that failed, not the call it is given to
  err <- error_of(spax(prep_data(f$demand, wrong, f$travel, missing_demand = "exclude"),
                       spec_clm(decay = decay_gaussian(sigma = 3), outside = 10)))
  expect_identical(conditionCall(err)[[1L]], quote(prep_data))
  # a user's function that spax calls back, which itself calls spax
  my_decay <- as_user_code(function(distance, sigma, ...) {
    calc_decay(distance, method = "bogus")
  })
  err <- error_of(calc_decay(1:3, method = my_decay, sigma = 1))
  expect_identical(conditionCall(err)[[1L]], quote(calc_decay))
  expect_identical(conditionCall(err)$method, "bogus")
  # a method reached through lapply(): the call that lapply() makes to the generic
  d <- prep_data(f$demand, f$supply, f$travel, missing_demand = "exclude")
  result <- spax(d, spec_clm(decay = decay_gaussian(sigma = 3), outside = 10))
  err <- error_of(lapply(list(result), predict, travel = f$travel[, 1:2]))
  expect_identical(conditionCall(err)[[1L]], quote(FUN))
  # the generic given to do.call() as an object: named by the generic
  err <- error_of(do.call(predict, list(result, f$travel[, 1:2])))
  expect_identical(conditionCall(err)[[1L]], quote(predict))
})

