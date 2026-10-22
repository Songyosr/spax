# Raster data and FCA specifications on spax() (SPAX-060, V2): the shared runner
# .fca_run(), prep_data() with rasters, and the identity of spax(prep_data(...),
# spec_<index>()) with the one-shot spax_<index>() for every index. The one-shots
# keep their direct path; spax() reaches the same engine through the same runner.

expect_message_exact <- function(object, message) {
  # the whole message, not a part of it
  got <- tryCatch({ object; NULL }, error = function(e) conditionMessage(e))
  expect_identical(got, message)
}

# A kernel declared with decay_kernel() for the declared-kernel cases.
exp_kernel <- function() {
  decay_kernel(function(distance, sigma) exp(-distance / sigma),
               name = "pexp", parameter = "scale")
}

# One result equals another in everything but the call: values (and so the NA
# pattern), layer names, type, parameters, facilities and class.
expect_same_result <- function(got, want, info = NULL) {
  gv <- terra::values(got$accessibility)
  wv <- terra::values(want$accessibility)
  expect_identical(gv, wv, info = info)
  expect_identical(is.na(gv), is.na(wv), info = info)
  expect_identical(names(got$accessibility), names(want$accessibility), info = info)
  expect_identical(got$type, want$type, info = info)
  expect_identical(got$parameters, want$parameters, info = info)
  expect_identical(got$facilities, want$facilities, info = info)
  expect_identical(class(got), class(want), info = info)
}

# The inputs of a case as the one-shots take them and as prep_data() does.
fca_case <- function(demand, supply, travel, id_col = NULL, supply_cols = NULL) {
  list(demand = demand, supply = supply, travel = travel, id_col = id_col,
       supply_cols = supply_cols)
}

fca_case_data <- function(k) {
  prep_data(k$demand, k$supply, k$travel, id_col = k$id_col, supply_cols = k$supply_cols)
}

# The one-shot `fun` on the case; `...` are its other arguments.
fca_one_shot <- function(fun, k, ...) {
  do.call(fun, c(list(demand = k$demand, supply = k$supply, distance = k$travel), list(...),
                 if (!is.null(k$id_col)) list(id_col = k$id_col),
                 if (!is.null(k$supply_cols)) list(supply_cols = k$supply_cols)))
}

toy_case <- function() {
  td <- fca_toy()
  fca_case(td$demand, td$supply_df, td$distance, "id", c("doctors", "nurses"))
}

# The 3 x 4 block fixture with its matrix of two supply measures.
block_case <- function() {
  f <- fca_block_fixture(3)
  fca_case(f$demand, f$S, f$distance)
}

# Two demand groups on the 3 x 4 grid with three facilities and two supply measures:
# group a travels as in fca_block_fixture(), group b has its own slower travel that
# cannot reach cell 4, and zero demand in cells 3 and 12 (where a's demand is zero too).
# The same data as the group fixture of test-05-fca_groups.R.
group_fixture <- function() {
  f <- fca_block_fixture(3)
  Da <- f$D
  Da[12] <- 0
  Db <- c(5, 15, 0, 25, 35, 45, 55, 65, 75, 85, 95, 0)
  dist_b <- f$dist * 1.5
  dist_b[4, ] <- NA
  c(f, list(
    demand_a = toy_rast(Da, nrows = 3, ncols = 4),
    demand_b = toy_rast(Db, nrows = 3, ncols = 4),
    travel_a = f$distance,
    travel_b = terra::setValues(f$distance, dist_b)
  ))
}

# The groups with their own travel, and the matrix of two supply measures.
group_case <- function() {
  f <- group_fixture()
  fca_case(list(a = f$demand_a, b = f$demand_b), f$S, list(a = f$travel_a, b = f$travel_b))
}

gauss <- function(sigma) list(method = "gaussian", sigma = sigma)

# the shared runner -------------------------------------------------------------------

test_that(".fca_kind_info() maps each kind to the runner type, label and parameters", {
  expect_identical(.fca_kind_info("2sfca")$type, "e2sfca")
  expect_identical(.fca_kind_info("e2sfca")$type, "e2sfca")
  expect_identical(.fca_kind_info("m2sfca")$type, "m2sfca")
  expect_identical(.fca_kind_info("3sfca")$type, "3sfca")
  expect_identical(.fca_kind_info("huff2sfca")$type, "huff")
  expect_identical(vapply(c("2sfca", "e2sfca", "m2sfca", "3sfca", "huff2sfca"),
                          function(k) .fca_kind_info(k)$label, ""),
                   c(`2sfca` = "2SFCA", e2sfca = "E2SFCA", m2sfca = "M2SFCA",
                     `3sfca` = "3SFCA", huff2sfca = "Huff-2SFCA"))
  expect_identical(.fca_kind_info("2sfca")$parameter_names,
                   c("decay_params", "demand_normalize"))
  expect_identical(.fca_kind_info("3sfca")$parameter_names,
                   c("decay_params", "selection_params"))
  expect_identical(.fca_kind_info("huff2sfca")$parameter_names,
                   c("decay_params", "selection_params", "attractiveness", "alpha"))
  expect_message_exact(.fca_kind_info("bogus"), "unknown FCA kind 'bogus'")
  expect_message_exact(.fca_kind_info("huff"), "unknown FCA kind 'huff'")
})

test_that(".fca_run() stores the call as given and returns a spax object", {
  td <- fca_toy()
  call <- quote(some_caller(x = 1))
  r <- .fca_run("e2sfca", td$demand, td$supply_vector, td$distance, gauss(2), call = call)
  expect_identical(r$call, call)
  expect_identical(class(r), c("spax", "spax_result"))
  expect_identical(r$type, "E2SFCA")
  expect_identical(names(r$parameters), c("decay_params", "demand_normalize"))
  expect_null(.fca_run("e2sfca", td$demand, td$supply_vector, td$distance, gauss(2))$call)
  expect_message_exact(
    .fca_run("bogus", td$demand, td$supply_vector, td$distance, gauss(2)),
    "unknown FCA kind 'bogus'")
})

test_that(".fca_run() resolves the attractiveness of Huff-2SFCA as the wrapper does", {
  td <- fca_toy()
  r <- .fca_run("huff2sfca", td$demand, td$supply_vector, td$distance, gauss(2),
                selection_params = gauss(2))
  expect_identical(r$parameters$attractiveness, c(facility1 = 10, facility2 = 15))
  expect_identical(names(r$parameters),
                   c("decay_params", "selection_params", "attractiveness", "alpha"))
  # The 3SFCA and the plain indices ignore attractiveness and alpha.
  r3 <- .fca_run("3sfca", td$demand, td$supply_vector, td$distance, gauss(2),
                 selection_params = gauss(2), attractiveness = c(1, 1), alpha = 5)
  expect_identical(names(r3$parameters), c("decay_params", "selection_params"))
  expect_message_exact(
    .fca_run("huff2sfca", td$demand, td$supply_df, td$distance, gauss(2),
             selection_params = gauss(2), id_col = "id", supply_cols = c("doctors", "nurses")),
    "give attractiveness when supply has several measures")
})

test_that(".chck_e2sfca() is its data part followed by its decay part", {
  td <- fca_toy()
  # the whole check as before
  expect_true(.chck_e2sfca(td$demand, td$supply_df, td$distance, gauss(2), "identity",
                           "id", "doctors"))
  expect_true(.chck_e2sfca_data(td$demand, td$supply_df, td$distance, "id", "doctors"))
  expect_true(.chck_e2sfca_decay(td$demand, gauss(2), "identity"))
  # the data part knows nothing about the decay
  expect_message_exact(
    .chck_e2sfca_data(td$demand, td$supply_df, td$distance[[1]], "id", "doctors"),
    "Length of distance layers (1) must match length of facilities (2)")
  expect_message_exact(
    .chck_e2sfca_data(td$demand, td$supply_df, td$distance[[1]], "id", "doctors",
                      arg = "travel"),
    "Length of travel layers (1) must match length of facilities (2)")
  expect_message_exact(.chck_e2sfca_decay(td$demand, list(sigma = 2), "identity"),
                       "decay_params must include 'method'")
  expect_message_exact(.chck_e2sfca_decay(td$demand, gauss(2), "bogus"),
                       "demand_normalize must be one of: identity, standard, semi")
  # one fault, one message through the composite
  expect_message_exact(
    .chck_e2sfca(td$demand, td$supply_df, td$distance[[1]], gauss(2), "identity", "id",
                 "doctors"),
    "Length of distance layers (1) must match length of facilities (2)")
  expect_message_exact(
    .chck_e2sfca(td$demand, td$supply_df, td$distance, list(sigma = 2), "identity", "id",
                 "doctors"),
    "decay_params must include 'method'")
})

# the identity of spax(prep_data(), spec) with the one-shot ----------------------------

test_that("every index on spax() equals its one-shot: data frame supply, two measures", {
  k <- toy_case()
  d <- fca_case_data(k)
  expect_same_result(spax(d, spec_2sfca(3)), fca_one_shot(spax_2sfca, k, threshold = 3))
  expect_same_result(
    spax(d, spec_e2sfca(decay_gaussian(2))),
    fca_one_shot(spax_e2sfca, k, decay_params = gauss(2)))
  for (norm in c("standard", "semi")) {
    expect_same_result(
      spax(d, spec_e2sfca(decay_exponential(0.4), norm)),
      fca_one_shot(spax_e2sfca, k, decay_params = list(method = "exponential", sigma = 0.4),
                   demand_normalize = norm), info = norm)
    expect_same_result(
      spax(d, spec_m2sfca(decay_gaussian(1.5), norm)),
      fca_one_shot(spax_m2sfca, k, decay_params = gauss(1.5), demand_normalize = norm),
      info = norm)
  }
  expect_same_result(
    spax(d, spec_3sfca(decay_gaussian(2))),
    fca_one_shot(spax_3sfca, k, decay_params = gauss(2)))
  expect_same_result(
    spax(d, spec_3sfca(decay_gaussian(2), decay_step(3))),
    fca_one_shot(spax_3sfca, k, decay_params = gauss(2),
                 selection_params = list(method = "binary", sigma = 3)))
  # attractiveness needs a vector: the supply has two measures
  attract <- c(facility1 = 2, facility2 = 1)
  expect_same_result(
    spax(d, spec_huff2sfca(decay_gaussian(2), attractiveness = attract, alpha = 1.5)),
    fca_one_shot(spax_huff2sfca, k, decay_params = gauss(2), attractiveness = attract,
                 alpha = 1.5))
  expect_message_exact(spax(d, spec_huff2sfca(decay_gaussian(2))),
                       "give attractiveness when supply has several measures")
  # all of them give layers named by the supply columns
  expect_identical(names(spax(d, spec_e2sfca(decay_gaussian(2)))$accessibility),
                   c("doctors", "nurses"))
})

test_that("every index on spax() equals its one-shot: one measure, matrix and vector supply", {
  td <- fca_toy()
  for (supply in list(td$supply_vector, td$supply_matrix[, "doctors", drop = FALSE])) {
    k <- fca_case(td$demand, supply, td$distance)
    d <- fca_case_data(k)
    expect_same_result(spax(d, spec_2sfca(2.5)), fca_one_shot(spax_2sfca, k, threshold = 2.5))
    expect_same_result(spax(d, spec_e2sfca(decay_gaussian(2), "semi")),
                       fca_one_shot(spax_e2sfca, k, decay_params = gauss(2),
                                    demand_normalize = "semi"))
    expect_same_result(spax(d, spec_m2sfca(decay_gaussian(2))),
                       fca_one_shot(spax_m2sfca, k, decay_params = gauss(2)))
    expect_same_result(spax(d, spec_3sfca(decay_gaussian(2))),
                       fca_one_shot(spax_3sfca, k, decay_params = gauss(2)))
    # the attractiveness is the supply measure by default
    expect_same_result(spax(d, spec_huff2sfca(decay_gaussian(2))),
                       fca_one_shot(spax_huff2sfca, k, decay_params = gauss(2)))
    expect_same_result(spax(d, spec_huff2sfca(decay_gaussian(2), alpha = 0)),
                       fca_one_shot(spax_huff2sfca, k, decay_params = gauss(2), alpha = 0))
  }
})

test_that("every index on spax() equals its one-shot: missing demand and unreachable cells", {
  k <- block_case()
  d <- fca_case_data(k)
  expect_same_result(spax(d, spec_2sfca(4)), fca_one_shot(spax_2sfca, k, threshold = 4))
  expect_same_result(spax(d, spec_e2sfca(decay_gaussian(2.5), "standard")),
                     fca_one_shot(spax_e2sfca, k, decay_params = gauss(2.5),
                                  demand_normalize = "standard"))
  expect_same_result(spax(d, spec_m2sfca(decay_exponential(0.3), "semi")),
                     fca_one_shot(spax_m2sfca, k,
                                  decay_params = list(method = "exponential", sigma = 0.3),
                                  demand_normalize = "semi"))
  expect_same_result(spax(d, spec_3sfca(decay_gaussian(2.5))),
                     fca_one_shot(spax_3sfca, k, decay_params = gauss(2.5)))
  attract <- c(f1 = 3, f2 = 1, f3 = 2)
  expect_same_result(
    spax(d, spec_huff2sfca(decay_gaussian(2.5), decay_exponential(0.5), attract, 2)),
    fca_one_shot(spax_huff2sfca, k, decay_params = gauss(2.5),
                 selection_params = list(method = "exponential", sigma = 0.5),
                 attractiveness = attract, alpha = 2))
  # the layers are the supply measures of the matrix
  expect_identical(names(spax(d, spec_e2sfca(decay_gaussian(2)))$accessibility), c("a", "b"))
})

test_that("step, inverse, declared and raster decays equal the one-shots", {
  k <- block_case()
  d <- fca_case_data(k)
  expect_same_result(spax(d, spec_e2sfca(decay_step(3))),
                     fca_one_shot(spax_e2sfca, k, decay_params = list(method = "binary",
                                                                      sigma = 3)))
  expect_same_result(spax(d, spec_e2sfca(decay_inverse())),
                     fca_one_shot(spax_e2sfca, k, decay_params = list(method = "inverse")))
  expect_same_result(spax(d, spec_3sfca(decay_gaussian(2), decay_inverse())),
                     fca_one_shot(spax_3sfca, k, decay_params = gauss(2),
                                  selection_params = list(method = "inverse")))
  kernel <- exp_kernel()
  expect_same_result(spax(d, spec_e2sfca(decay_custom(kernel, 2))),
                     fca_one_shot(spax_e2sfca, k, decay_params = list(method = kernel,
                                                                      sigma = 2)))
  # a variable catchment: sigma is a raster (one layer here)
  sigma <- toy_rast(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), nrows = 3, ncols = 4)
  expect_same_result(spax(d, spec_e2sfca(decay_gaussian(sigma))),
                     fca_one_shot(spax_e2sfca, k, decay_params = list(method = "gaussian",
                                                                      sigma = sigma)))
  expect_same_result(spax(d, spec_m2sfca(decay_exponential(sigma), "standard")),
                     fca_one_shot(spax_m2sfca, k,
                                  decay_params = list(method = "exponential", sigma = sigma),
                                  demand_normalize = "standard"))
  expect_same_result(spax(d, spec_3sfca(decay_power(sigma))),
                     fca_one_shot(spax_3sfca, k,
                                  decay_params = list(method = "power", sigma = sigma)))
  # and a raster with one layer per facility
  per_facility <- toy_rast(seq(1, 5, length.out = 12), nrows = 3, ncols = 4, nlyr = 3)
  names(per_facility) <- c("f1", "f2", "f3")
  expect_same_result(spax(d, spec_e2sfca(decay_gaussian(per_facility))),
                     fca_one_shot(spax_e2sfca, k, decay_params = list(method = "gaussian",
                                                                      sigma = per_facility)))
})

test_that("a selection that is NULL and one that repeats the decay give the same index", {
  k <- block_case()
  d <- fca_case_data(k)
  same <- spax(d, spec_3sfca(decay_gaussian(2.5)))
  expect_same_result(same, spax(d, spec_3sfca(decay_gaussian(2.5), decay_gaussian(2.5))))
  # the stored parameters of the NULL selection are the decay itself
  expect_identical(same$parameters$selection_params, same$parameters$decay_params)
  other <- spax(d, spec_3sfca(decay_gaussian(2.5), decay_gaussian(4)))
  expect_false(identical(terra::values(other$accessibility),
                         terra::values(same$accessibility)))
  expect_identical(other$parameters$selection_params, gauss(4))
})

test_that("indices with demand groups equal the one-shots, with thresholds and decays by group", {
  k <- group_case()
  d <- fca_case_data(k)
  wrap_decay <- list(a = gauss(2.5), b = gauss(4))
  by_group <- list(a = decay_gaussian(2.5), b = decay_gaussian(4))
  expect_same_result(spax(d, spec_2sfca(c(a = 3, b = 5))),
                     fca_one_shot(spax_2sfca, k, threshold = c(a = 3, b = 5)))
  expect_same_result(spax(d, spec_2sfca(3)), fca_one_shot(spax_2sfca, k, threshold = 3))
  for (norm in c("identity", "standard", "semi")) {
    expect_same_result(spax(d, spec_e2sfca(by_group, norm)),
                       fca_one_shot(spax_e2sfca, k, decay_params = wrap_decay,
                                    demand_normalize = norm), info = norm)
  }
  expect_same_result(spax(d, spec_e2sfca(decay_gaussian(3))),
                     fca_one_shot(spax_e2sfca, k, decay_params = gauss(3)))
  expect_same_result(spax(d, spec_m2sfca(by_group, "standard")),
                     fca_one_shot(spax_m2sfca, k, decay_params = wrap_decay,
                                  demand_normalize = "standard"))
  expect_same_result(spax(d, spec_3sfca(by_group)),
                     fca_one_shot(spax_3sfca, k, decay_params = wrap_decay))
  expect_same_result(
    spax(d, spec_3sfca(by_group, list(a = decay_step(3), b = decay_inverse()))),
    fca_one_shot(spax_3sfca, k, decay_params = wrap_decay,
                 selection_params = list(a = list(method = "binary", sigma = 3),
                                         b = list(method = "inverse"))))
  # a shared decay with a selection by group
  expect_same_result(
    spax(d, spec_3sfca(decay_gaussian(3), list(a = decay_step(2), b = decay_step(4)))),
    fca_one_shot(spax_3sfca, k, decay_params = gauss(3),
                 selection_params = list(a = list(method = "binary", sigma = 2),
                                         b = list(method = "binary", sigma = 4))))
  attract <- c(f1 = 1, f2 = 2, f3 = 3)
  expect_same_result(
    spax(d, spec_huff2sfca(by_group, attractiveness = attract, alpha = 1.5)),
    fca_one_shot(spax_huff2sfca, k, decay_params = wrap_decay, attractiveness = attract,
                 alpha = 1.5))
  expect_identical(spax(d, spec_e2sfca(by_group))$parameters$groups, c("a", "b"))
  expect_identical(names(spax(d, spec_e2sfca(by_group))$accessibility),
                   c("a.a", "a.b", "a.all", "b.a", "b.b", "b.all"))
})

test_that("group lists and vectors are put in the order of the data's groups", {
  k <- group_case()
  d <- fca_case_data(k)
  wrap_decay <- list(a = gauss(2.5), b = gauss(4))
  swapped <- spax(d, spec_e2sfca(list(b = decay_gaussian(4), a = decay_gaussian(2.5))))
  expect_same_result(swapped, fca_one_shot(spax_e2sfca, k, decay_params = wrap_decay))
  expect_identical(names(swapped$parameters$decay_params), c("a", "b"))
  expect_same_result(spax(d, spec_2sfca(c(b = 5, a = 3))),
                     fca_one_shot(spax_2sfca, k, threshold = c(a = 3, b = 5)))
  expect_same_result(
    spax(d, spec_3sfca(decay_gaussian(3), list(b = decay_step(4), a = decay_step(2)))),
    fca_one_shot(spax_3sfca, k, decay_params = gauss(3),
                 selection_params = list(a = list(method = "binary", sigma = 2),
                                         b = list(method = "binary", sigma = 4))))
  # the specification itself is not changed
  spec <- spec_2sfca(c(b = 5, a = 3))
  spax(d, spec)
  expect_identical(spec$threshold, c(b = 5, a = 3))
})

test_that("a group list of one group is bound like any other", {
  f <- group_fixture()
  k <- fca_case(list(a = f$demand_a), f$S, f$travel_a)
  d <- fca_case_data(k)
  expect_identical(d$groups, "a")
  expect_same_result(spax(d, spec_e2sfca(list(a = decay_gaussian(2.5)))),
                     fca_one_shot(spax_e2sfca, k, decay_params = list(a = gauss(2.5))))
  expect_same_result(spax(d, spec_2sfca(c(a = 3))),
                     fca_one_shot(spax_2sfca, k, threshold = c(a = 3)))
})

test_that("travel by group in the data is used by group", {
  f <- group_fixture()
  # one shared stack for both groups
  shared <- fca_case(list(a = f$demand_a, b = f$demand_b), f$S, f$travel_a)
  expect_same_result(spax(fca_case_data(shared), spec_e2sfca(decay_gaussian(2.5))),
                     fca_one_shot(spax_e2sfca, shared, decay_params = gauss(2.5)))
  # a different stack for each: different values
  by_group <- group_case()
  expect_false(identical(
    terra::values(spax(fca_case_data(shared), spec_e2sfca(decay_gaussian(2.5)))$accessibility),
    terra::values(spax(fca_case_data(by_group), spec_e2sfca(decay_gaussian(2.5)))$accessibility)))
})

test_that("the Cornwall rasters give the one-shot results with the same NA positions", {
  # Slow; skipped in CRAN checks, run locally and in CI
  skip_on_cran()
  pop <- read_spax_example("cornwall_pop.tif")
  travel <- read_spax_example("cornwall_gp_travel.tif")
  supply <- sf::st_drop_geometry(cornwall_gp)
  supply <- supply[supply$practice %in% names(travel), ]
  k <- fca_case(pop, supply, travel, "practice", "gp_fte_extg")
  d <- fca_case_data(k)
  e2 <- spax(d, spec_e2sfca(decay_gaussian(10), "semi"))
  expect_same_result(e2, fca_one_shot(spax_e2sfca, k, decay_params = gauss(10),
                                      demand_normalize = "semi"))
  # the cells that reach no practice are NA, as before
  expect_true(anyNA(terra::values(e2$accessibility)))
  expect_same_result(spax(d, spec_2sfca(15)), fca_one_shot(spax_2sfca, k, threshold = 15))
  expect_same_result(spax(d, spec_3sfca(decay_gaussian(10))),
                     fca_one_shot(spax_3sfca, k, decay_params = gauss(10)))
  expect_same_result(spax(d, spec_huff2sfca(decay_gaussian(10), alpha = 1)),
                     fca_one_shot(spax_huff2sfca, k, decay_params = gauss(10)))
  expect_same_result(spax(d, spec_m2sfca(decay_exponential(0.2), "standard")),
                     fca_one_shot(spax_m2sfca, k,
                                  decay_params = list(method = "exponential", sigma = 0.2),
                                  demand_normalize = "standard"))
})

# call and class -----------------------------------------------------------------------

test_that("every one-shot records its own call, spax() records its own", {
  td <- fca_toy()
  r2 <- spax_2sfca(td$demand, td$supply_vector, td$distance, 3)
  expect_identical(r2$call[[1L]], quote(spax_2sfca))
  expect_identical(r2$call$threshold, 3)
  expect_identical(spax_e2sfca(td$demand, td$supply_vector, td$distance)$call[[1L]],
                   quote(spax_e2sfca))
  expect_identical(spax_m2sfca(td$demand, td$supply_vector, td$distance)$call[[1L]],
                   quote(spax_m2sfca))
  expect_identical(spax_3sfca(td$demand, td$supply_vector, td$distance)$call[[1L]],
                   quote(spax_3sfca))
  expect_identical(spax_huff2sfca(td$demand, td$supply_vector, td$distance)$call[[1L]],
                   quote(spax_huff2sfca))
  # also with snap, which skips the validation
  expect_identical(
    spax_2sfca(td$demand, td$supply_vector, td$distance, 3, snap = TRUE)$call[[1L]],
    quote(spax_2sfca))
  d <- prep_data(td$demand, td$supply_vector, td$distance)
  x <- spax(d, spec_e2sfca(decay_gaussian(2)))
  expect_identical(x$call[[1L]], quote(spax))
  expect_identical(x$call$data, quote(d))
  expect_identical(x$call$model, quote(spec_e2sfca(decay_gaussian(2))))
})

test_that("FCA results are spax and spax_result objects", {
  td <- fca_toy()
  d <- prep_data(td$demand, td$supply_vector, td$distance)
  results <- list(
    spax_2sfca(td$demand, td$supply_vector, td$distance, 3),
    spax_e2sfca(td$demand, td$supply_vector, td$distance),
    spax_m2sfca(td$demand, td$supply_vector, td$distance),
    spax_3sfca(td$demand, td$supply_vector, td$distance),
    spax_huff2sfca(td$demand, td$supply_vector, td$distance),
    spax_e2sfca(td$demand, td$supply_vector, td$distance, snap = TRUE),
    spax(d, spec_e2sfca()),
    spax(d, spec_huff2sfca())
  )
  for (r in results) {
    expect_identical(class(r), c("spax", "spax_result"))
    expect_true(inherits(r, "spax_result"))
  }
})

test_that("an FCA result of spax() saves and reads like the one-shot's", {
  skip_if_no_proj()
  td <- fca_toy()
  d <- prep_data(td$demand, td$supply_df, td$distance, id_col = "id", supply_cols = "doctors")
  x <- spax(d, spec_e2sfca(decay_gaussian(2)))
  tmp <- withr::local_tempdir()
  save_spax(x, file.path(tmp, "x"))
  y <- read_spax(file.path(tmp, "x"))
  expect_identical(class(y), c("spax", "spax_result"))
  expect_identical(y$parameters, x$parameters)
  expect_equal(terra::values(y$accessibility), terra::values(x$accessibility),
               tolerance = 1e-6)
})

# prep_data(): dispatch ---------------------------------------------------------------

test_that("prep_data() takes rasters, lists of rasters, numbers and lists of numbers", {
  td <- fca_toy()
  # rasters
  expect_s3_class(prep_data(td$demand, td$supply_vector, td$distance), "spax_raster_data")
  groups <- list(a = td$demand, b = td$demand * 2)
  expect_s3_class(prep_data(groups, td$supply_vector, td$distance), "spax_raster_data")
  expect_s3_class(prep_data(list(a = td$demand), td$supply_vector, td$distance),
                  "spax_raster_data")
  # numbers: as before
  f <- alloc_huff_case()
  expect_s3_class(prep_data(f$demand, f$supply, f$distance), "ae_prepared_inputs")
  expect_false(inherits(prep_data(f$demand, f$supply, f$distance), "spax_data"))
  grouped <- prep_data(list(a = f$demand, b = f$demand * 2), f$supply, f$distance)
  expect_s3_class(grouped, "ae_prepared_inputs")
  expect_identical(grouped$group_ids, c("a", "b"))
  # the class of the raster data
  expect_identical(class(prep_data(td$demand, td$supply_vector, td$distance)),
                   c("spax_raster_data", "spax_data"))
})

test_that("a list that mixes rasters with other elements is an error", {
  td <- fca_toy()
  mixed <- "demand must be a raster, a numeric vector, or a named list of either"
  expect_message_exact(prep_data(list(a = td$demand, b = c(1, 2)), td$supply_vector,
                                 td$distance), mixed)
  expect_message_exact(prep_data(list(a = c(x = 1), b = td$demand), td$supply_vector,
                                 td$distance), mixed)
  expect_message_exact(prep_data(list(td$demand, "a"), td$supply_vector, td$distance), mixed)
  expect_message_exact(prep_data(list(a = td$demand, b = NULL, c = td$demand),
                                 td$supply_vector, td$distance), mixed)
  # a list with no raster is numeric and goes through the numeric checks
  err <- tryCatch(prep_data(list(a = c(x = 1), b = c(x = 2)), c(f = 1), matrix(1)),
                  error = function(e) conditionMessage(e))
  expect_false(identical(err, mixed))
})

test_that("numeric data refuse id_col and supply_cols", {
  f <- alloc_huff_case()
  text <- "id_col and supply_cols are for a supply table with raster data"
  expect_message_exact(prep_data(f$demand, f$supply, f$distance, id_col = "id"), text)
  expect_message_exact(prep_data(f$demand, f$supply, f$distance, supply_cols = "x"), text)
  expect_message_exact(prep_data(f$demand, f$supply, f$distance, id_col = "id",
                                 supply_cols = "x"), text)
  # NULL is the same as not given
  expect_identical(prep_data(f$demand, f$supply, f$distance, id_col = NULL, supply_cols = NULL),
                   prep_data(f$demand, f$supply, f$distance))
  # grouped numeric data too
  expect_message_exact(
    prep_data(list(a = f$demand), f$supply, f$distance, id_col = "id"), text)
})

test_that("raster data refuse the arguments of numeric data", {
  td <- fca_toy()
  call <- function(...) prep_data(td$demand, td$supply_vector, td$distance, ...)
  expect_message_exact(call(origin_ids = "a"), "origin_ids is for numeric data")
  expect_message_exact(call(facility_ids = "a"), "facility_ids is for numeric data")
  expect_message_exact(call(included = TRUE), "included is for numeric data")
  expect_message_exact(call(missing_demand = "exclude"), "missing_demand is for numeric data")
  expect_message_exact(call(missing_travel = "legacy_zero"),
                       "missing_travel is for numeric data")
  expect_message_exact(call(spatial = list()), "spatial is for numeric data")
  # given is given, also with the default value or NULL
  expect_message_exact(call(missing_demand = "error"), "missing_demand is for numeric data")
  expect_message_exact(call(origin_ids = NULL), "origin_ids is for numeric data")
  expect_message_exact(call(spatial = NULL), "spatial is for numeric data")
  # the first of several, in the order of the arguments
  expect_message_exact(call(spatial = NULL, included = TRUE), "included is for numeric data")
  expect_message_exact(call(missing_travel = "error", origin_ids = "a"),
                       "origin_ids is for numeric data")
  # metadata and the supply table are for raster data too
  expect_no_error(call(metadata = list(travel_units = "minutes")))
})

# prep_data(): the raster object ---------------------------------------------------------

test_that("raster data hold the inputs and describe the grid", {
  td <- fca_toy()
  d <- prep_data(td$demand, td$supply_df, td$distance, id_col = "id",
                 supply_cols = c("doctors", "nurses"),
                 metadata = list(travel_units = "minutes"))
  expect_identical(names(d), c("demand", "travel", "supply", "id_col", "supply_cols",
                               "facility_ids", "groups", "metadata", "grid"))
  expect_identical(class(d), c("spax_raster_data", "spax_data"))
  expect_true(.same_raster(d$demand, td$demand))
  expect_true(.same_raster(d$travel, td$distance))
  expect_identical(d$supply, td$supply_df)
  expect_identical(d$id_col, "id")
  expect_identical(d$supply_cols, c("doctors", "nurses"))
  expect_identical(d$facility_ids, c("facility1", "facility2"))
  expect_null(d$groups)
  expect_identical(d$metadata$travel_units, "minutes")
  expect_identical(d$metadata$demand_units, NA_character_)
  expect_identical(names(d$metadata), c("demand_units", "demand_period", "supply_units",
                                        "supply_period", "travel_units"))
  expect_identical(d$grid$dim, c(nrow = 3, ncol = 3))
  expect_identical(d$grid$res, terra::res(td$demand))
  expect_identical(d$grid$crs, terra::crs(td$demand))
  expect_true(.raster_data_alive(d))
})

test_that("raster data with demand groups hold the lists and the group IDs", {
  k <- group_case()
  d <- fca_case_data(k)
  expect_identical(d$groups, c("a", "b"))
  expect_identical(d$facility_ids, c("f1", "f2", "f3"))
  expect_identical(names(d$demand), c("a", "b"))
  expect_identical(names(d$travel), c("a", "b"))
  expect_identical(d$grid$dim, c(nrow = 3, ncol = 4))
  expect_true(.raster_data_alive(d))
})

test_that("raster data without layer names have positional facility IDs", {
  td <- fca_toy()
  travel <- td$distance
  names(travel) <- c("lyr.1", "lyr.2")
  d <- prep_data(td$demand, c(5, 6), travel)
  expect_identical(d$facility_ids, c("facility_1", "facility_2"))
})

test_that("raster data are checked with the checks of the FCA wrappers", {
  td <- fca_toy()
  ok <- function(...) {
    args <- list(demand = td$demand, supply = td$supply_df, travel = td$distance,
                 id_col = "id", supply_cols = "doctors")
    args[names(list(...))] <- list(...)
    do.call(prep_data, args)
  }
  expect_s3_class(ok(), "spax_raster_data")
  # the demand structure
  expect_message_exact(ok(demand = c(td$demand, td$demand)),
                       "give demand groups as a named list of single-layer rasters")
  expect_message_exact(ok(demand = list(td$demand, td$demand)),
                       "demand group names must be unique, non-empty, not NA, not \"all\" and without \".\"")
  expect_message_exact(ok(demand = list(all = td$demand)),
                       "demand group names must be unique, non-empty, not NA, not \"all\" and without \".\"")
  other_grid <- terra::rast(nrows = 4, ncols = 4)
  terra::values(other_grid) <- 1
  expect_message_exact(ok(demand = list(a = td$demand, b = other_grid)),
                       "all demand groups must have the same geometry")
  # the travel stack
  expect_message_exact(ok(travel = c(1, 2)),
                       "travel must be one of the following classes: SpatRaster")
  expect_message_exact(ok(travel = td$distance[[1]]),
                       "Length of travel layers (1) must match length of facilities (2)")
  expect_message_exact(ok(travel = terra::rast(nrows = 4, ncols = 4, nlyrs = 2)),
                       "demand and travel must have the same geometry (resolution, extent, and CRS)")
  dup <- td$distance
  names(dup) <- c("a", "a")
  expect_message_exact(ok(travel = dup), "index$layer contains duplicate coordinate tuples")
  # the supply table
  expect_message_exact(ok(id_col = NULL), "id_col must be specified when supply is a data.frame")
  expect_message_exact(ok(supply_cols = NULL),
                       "supply_cols must be specified when supply is a data.frame")
  expect_message_exact(ok(supply_cols = "nope"), "Column(s) not found in supply: nope")
  expect_message_exact(ok(supply = rbind(td$supply_df, td$supply_df)),
                       "Values in id_col must be unique")
  expect_message_exact(ok(supply = td$supply_df[1, ]),
                       "Length of travel layers (2) must match length of facilities (1)")
  expect_message_exact(ok(supply = sf::st_sf(td$supply_df,
                                             geometry = sf::st_sfc(sf::st_point(0:1),
                                                                   sf::st_point(1:2)))),
                       "Supply data is an sf object. Please use st_drop_geometry() first")
  # the metadata
  expect_message_exact(ok(metadata = list(bogus = "x")),
                       "metadata must be a named list of demand/supply units and periods or travel_units")
})

test_that("raster data check the travel stacks of the groups", {
  f <- group_fixture()
  demand <- list(a = f$demand_a, b = f$demand_b)
  renamed <- f$travel_b
  names(renamed) <- c("f1", "f3", "f2")
  expect_message_exact(
    prep_data(demand, f$S, list(a = f$travel_a, b = renamed)),
    paste0("travel layers of every group must name the same facilities in the same order; ",
           "reorder with x[[names(first)]]"))
  expect_message_exact(
    prep_data(demand, f$S, list(a = f$travel_a)),
    "travel: give one element per group: a, b")
  expect_message_exact(
    prep_data(demand, f$S, list(a = f$travel_a, b = c(1, 2))),
    "travel must be one of the following classes: SpatRaster")
})

test_that("prep_data() reads no raster values: the demand rule belongs to the engine", {
  f <- fca_block_fixture(3)
  # negative demand: prepared, refused when an index runs (decision D2)
  negative <- terra::setValues(f$demand, c(10, -20, 30, 40, 0, 60, NA, 80, 90, 100, 110, 120))
  d <- prep_data(negative, f$S, f$distance)
  expect_s3_class(d, "spax_raster_data")
  expect_error(spax(d, spec_e2sfca(decay_gaussian(2))),
               "demand must be finite and nonnegative; use NA for unknown", fixed = TRUE)
  expect_error(spax_e2sfca(negative, f$S, f$distance, gauss(2)),
               "demand must be finite and nonnegative; use NA for unknown", fixed = TRUE)
  # missing demand and unreachable cells are fine
  expect_s3_class(prep_data(f$demand, f$S, f$distance), "spax_raster_data")
})

# prep_data(): printing -------------------------------------------------------------------

test_that("raster data print as documented", {
  td <- fca_toy()
  d <- prep_data(td$demand, td$supply_df, td$distance, id_col = "id", supply_cols = "doctors")
  expected <- c(
    "<spax data> raster 3 x 3 cells x 2 facilities; supply doctors",
    "  demand    1 layer; travel 2 layers",
    "  note      holds terra rasters: prepare the data again after saveRDS()")
  expect_identical(format(d), expected)
  shown <- NULL
  out <- capture.output(shown <- withVisible(print(d)))
  expect_identical(out, expected)
  expect_false(shown$visible)
  expect_identical(shown$value, d)
  # several supply measures
  two <- prep_data(td$demand, td$supply_df, td$distance, id_col = "id",
                   supply_cols = c("doctors", "nurses"))
  expect_identical(format(two)[1L],
                   "<spax data> raster 3 x 3 cells x 2 facilities; supply doctors, nurses")
  # a matrix names its measures, a vector is "supply", and a single facility is singular
  expect_identical(format(prep_data(td$demand, td$supply_matrix, td$distance))[1L],
                   "<spax data> raster 3 x 3 cells x 2 facilities; supply doctors, nurses")
  expect_identical(format(prep_data(td$demand, td$supply_vector, td$distance))[1L],
                   "<spax data> raster 3 x 3 cells x 2 facilities; supply supply")
  one <- prep_data(td$demand, c(facility1 = 4), td$distance[[1]])
  expect_identical(format(one)[1:2],
                   c("<spax data> raster 3 x 3 cells x 1 facility; supply supply",
                     "  demand    1 layer; travel 1 layer"))
  unnamed <- matrix(c(1, 2), ncol = 1)
  expect_identical(format(prep_data(td$demand, unnamed, td$distance))[1L],
                   "<spax data> raster 3 x 3 cells x 2 facilities; supply measure_1")
})

test_that("raster data with groups and unit labels print their groups and labels", {
  k <- group_case()
  d <- fca_case_data(k)
  expect_identical(format(d)[2L], "  demand    2 groups: a, b; travel 3 layers per group")
  shared <- prep_data(k$demand, k$supply, k$travel$a)
  expect_identical(format(shared)[2L], "  demand    2 groups: a, b; travel 3 layers")
  one <- prep_data(list(a = k$demand$a), k$supply, k$travel$a)
  expect_identical(format(one)[2L], "  demand    1 group: a; travel 3 layers")
  labelled <- prep_data(k$demand, k$supply, k$travel,
                        metadata = list(demand_units = "people", demand_period = "2021",
                                        travel_units = "minutes"))
  expect_identical(format(labelled)[3L], "  units     demand people (2021); travel minutes")
  expect_length(format(labelled), 4L)
  expect_length(format(d), 3L)
})

test_that("a large grid prints thousands with a comma", {
  # geometry only: prep_data() reads no values
  big <- terra::rast(nrows = 1200, ncols = 1800)
  stack <- terra::rast(big, nlyrs = 2)
  names(stack) <- c("a", "b")
  d <- prep_data(big, c(a = 1, b = 2), stack)
  expect_identical(format(d)[1L],
                   "<spax data> raster 1,200 x 1,800 cells x 2 facilities; supply supply")
})

# prep_data(): the rasters do not survive saveRDS() ---------------------------------------

test_that("raster data whose rasters are gone are detected and refused by spax()", {
  # Slow; skipped in CRAN checks, run locally and in CI
  skip_on_cran()
  td <- fca_toy()
  d <- prep_data(td$demand, td$supply_df, td$distance, id_col = "id", supply_cols = "doctors")
  expect_true(.raster_data_alive(d))
  file <- withr::local_tempfile(fileext = ".rds")
  saveRDS(d, file)
  back <- readRDS(file)
  expect_false(.raster_data_alive(back))
  expect_s3_class(back, "spax_raster_data")
  gone <- paste0("the rasters of this data are no longer available (saved and reloaded?): ",
                 "prepare the data again")
  expect_message_exact(spax(back, spec_e2sfca(decay_gaussian(2))), gone)
  # it prints (the note says why), and the plain fields are still there
  expect_identical(format(back), format(d))
  expect_identical(back$facility_ids, d$facility_ids)
  expect_identical(back$supply, d$supply)
  # preparing the data again works
  again <- prep_data(td$demand, back$supply, td$distance, id_col = back$id_col,
                     supply_cols = back$supply_cols)
  expect_true(.raster_data_alive(again))
  expect_s3_class(spax(again, spec_e2sfca(decay_gaussian(2))), "spax")
})

test_that("raster data with groups are dead after a round trip too, and so is each part", {
  # Slow; skipped in CRAN checks, run locally and in CI
  skip_on_cran()
  k <- group_case()
  d <- fca_case_data(k)
  file <- withr::local_tempfile(fileext = ".rds")
  saveRDS(d, file)
  expect_false(.raster_data_alive(readRDS(file)))
  # one dead raster is enough
  back <- readRDS(file)
  mixed <- d
  mixed$travel <- back$travel
  expect_false(.raster_data_alive(mixed))
  mixed <- d
  mixed$demand <- back$demand
  expect_false(.raster_data_alive(mixed))
  expect_true(.raster_data_alive(d))
})

# spax(): dispatch ------------------------------------------------------------------------

test_that("spax() checks the data, the model and the rasters in that order", {
  # Slow; skipped in CRAN checks, run locally and in CI
  skip_on_cran()
  td <- fca_toy()
  d <- prep_data(td$demand, td$supply_vector, td$distance)
  back <- {
    file <- withr::local_tempfile(fileext = ".rds")
    saveRDS(d, file)
    readRDS(file)
  }
  # 1. data before the model
  expect_message_exact(spax(list(), "x"), "data must come from prep_data()")
  expect_message_exact(spax(td$demand, spec_e2sfca()), "data must come from prep_data()")
  expect_message_exact(spax(back$supply, spec_e2sfca()), "data must come from prep_data()")
  # 2. the model before the rasters
  model <- paste0("model must be a model specification such as spec_e2sfca(), ",
                  "spec_huff() or spec_clm()")
  expect_message_exact(spax(d, "e2sfca"), model)
  expect_message_exact(spax(d, decay_gaussian(2)), model)
  expect_message_exact(spax(back, "e2sfca"), model)
  expect_message_exact(spax(d, list(kind = "e2sfca")), model)
  # 3. the rasters before the refusals of an FCA index
  gone <- paste0("the rasters of this data are no longer available (saved and reloaded?): ",
                 "prepare the data again")
  expect_message_exact(spax(back, spec_e2sfca(), observed = 1), gone)
  expect_message_exact(spax(back, spec_clm()), gone)
})

test_that("an FCA index is computed, not fitted: observed, starts, bounds and ... are refused", {
  td <- fca_toy()
  d <- prep_data(td$demand, td$supply_vector, td$distance)
  fit <- paste0("FCA indices are computed, not fitted; to fit how demand chooses facilities ",
                "use spec_huff() or spec_clm()")
  controls <- "starts, bounds and fitting controls apply to allocation models"
  spec <- spec_e2sfca(decay_gaussian(2))
  expect_message_exact(spax(d, spec, observed = obs_totals(c(facility1 = 1, facility2 = 2))),
                       fit)
  expect_message_exact(spax(d, spec, observed = c(facility1 = 1, facility2 = 2)), fit)
  expect_message_exact(spax(d, spec, starts = c(sigma = 1)), controls)
  expect_message_exact(spax(d, spec, lower = c(sigma = 1)), controls)
  expect_message_exact(spax(d, spec, upper = c(sigma = 1)), controls)
  expect_message_exact(spax(d, spec, gradient = TRUE), controls)
  expect_message_exact(spax(d, spec, loss = "poisson"), controls)
  expect_message_exact(spax(d, spec, 1), fit)
  # observed comes first
  expect_message_exact(spax(d, spec, observed = 1, starts = c(sigma = 1)), fit)
  # every kind
  for (s in list(spec_2sfca(3), spec_m2sfca(decay_gaussian(2)), spec_3sfca(decay_gaussian(2)),
                 spec_huff2sfca(decay_gaussian(2)))) {
    expect_message_exact(spax(d, s, observed = 1), fit)
    expect_message_exact(spax(d, s, starts = 1), controls)
  }
  # nothing of this is raised without the extra arguments
  expect_s3_class(spax(d, spec), "spax")
})

test_that("the refusals of an FCA index apply to numeric data too", {
  f <- alloc_huff_case()
  d <- prep_data(f$demand, f$supply, f$distance)
  spec <- spec_e2sfca(decay_gaussian(2))
  expect_message_exact(spax(d, spec, observed = 1),
                       paste0("FCA indices are computed, not fitted; to fit how demand ",
                              "chooses facilities use spec_huff() or spec_clm()"))
  expect_message_exact(spax(d, spec, starts = 1),
                       "starts, bounds and fitting controls apply to allocation models")
})

test_that("the two combinations that waited for V3 now run (see test-08-spax_numeric_fca.R)", {
  td <- fca_toy()
  raster <- prep_data(td$demand, td$supply_vector, td$distance)
  f <- alloc_huff_case()
  numeric <- prep_data(f$demand, f$supply, f$distance)
  expect_s3_class(spax(numeric, spec_e2sfca(decay_gaussian(2))), "spax_access_values")
  expect_s3_class(spax(numeric, spec_2sfca(3)), "spax_access_values")
  expect_s3_class(spax(raster, spec_clm(decay_gaussian(2), outside = 1, beta = 1)),
                  "spax_allocation")
  expect_s3_class(spax(raster, spec_huff(decay_gaussian(2), beta = 1)), "spax_allocation")
  # the allocation route on numeric data is untouched
  expect_s3_class(spax(numeric, spec_clm(decay_gaussian(8), outside = 0.5, beta = 1)),
                  "spax_allocation")
})

test_that("group arguments need data with groups and must name its groups", {
  td <- fca_toy()
  ungrouped <- prep_data(td$demand, td$supply_vector, td$distance)
  expect_message_exact(spax(ungrouped, spec_e2sfca(list(a = decay_gaussian(2)))),
                       "decay needs data with demand groups")
  expect_message_exact(spax(ungrouped, spec_3sfca(decay_gaussian(2), list(a = decay_step(2)))),
                       "selection needs data with demand groups")
  expect_message_exact(spax(ungrouped, spec_2sfca(c(a = 3, b = 4))),
                       "threshold needs data with demand groups")
  expect_message_exact(spax(ungrouped, spec_huff2sfca(list(a = decay_gaussian(2)))),
                       "decay needs data with demand groups")
  # a decay by group is checked before the selection and the threshold has its own kind
  expect_message_exact(
    spax(ungrouped, spec_3sfca(list(a = decay_gaussian(2)), list(a = decay_step(2)))),
    "decay needs data with demand groups")

  d <- fca_case_data(group_case())
  expect_message_exact(
    spax(d, spec_e2sfca(list(a = decay_gaussian(2), c = decay_gaussian(3)))),
    "decay is given for groups a, c but the data has groups a, b")
  expect_message_exact(
    spax(d, spec_e2sfca(list(b = decay_gaussian(2)))),
    "decay is given for groups b but the data has groups a, b")
  expect_message_exact(
    spax(d, spec_e2sfca(list(a = decay_gaussian(2), b = decay_gaussian(3),
                             c = decay_gaussian(4)))),
    "decay is given for groups a, b, c but the data has groups a, b")
  expect_message_exact(
    spax(d, spec_3sfca(decay_gaussian(2), list(x = decay_step(2), y = decay_step(3)))),
    "selection is given for groups x, y but the data has groups a, b")
  expect_message_exact(
    spax(d, spec_2sfca(c(a = 3, c = 4))),
    "threshold is given for groups a, c but the data has groups a, b")
  # shared arguments are fine with groups
  expect_s3_class(spax(d, spec_e2sfca(decay_gaussian(2))), "spax")
  expect_s3_class(spax(d, spec_2sfca(3)), "spax")
})

test_that("an attractiveness is shared by the groups and checked as the wrapper does", {
  k <- group_case()
  d <- fca_case_data(k)
  expect_message_exact(
    spax(d, spec_huff2sfca(decay_gaussian(2), attractiveness = c(a = 1, b = 2, c = 3))),
    "names of attractiveness must be the distance layer names, one for each facility")
  expect_message_exact(
    spax(d, spec_huff2sfca(decay_gaussian(2))),
    "give attractiveness when supply has several measures")
  expect_error(
    fca_one_shot(spax_huff2sfca, k, decay_params = gauss(2)),
    "give attractiveness when supply has several measures", fixed = TRUE)
})

test_that("the result of spax() on raster data is the object of the one-shot", {
  td <- fca_toy()
  d <- prep_data(td$demand, td$supply_df, td$distance, id_col = "id", supply_cols = "doctors")
  x <- spax(d, spec_e2sfca(decay_gaussian(2)))
  expect_named(x, c("accessibility", "type", "parameters", "facilities", "iterations",
                    "variations", "call"))
  expect_s4_class(x$accessibility, "SpatRaster")
  expect_identical(x$type, "E2SFCA")
  expect_identical(x$parameters, list(decay_params = gauss(2), demand_normalize = "identity"))
  expect_identical(x$facilities, data.frame(id = td$supply_df$id, doctors = td$supply_df$doctors))
  expect_null(x$iterations)
  expect_null(x$variations)
  # its methods work
  expect_output(print(x), "<spax access> E2SFCA, 1 layer on 3 x 3 cells", fixed = TRUE)
  expect_s3_class(summary(x), "summary.spax")
})

test_that("a result is the same whether the data are used once or several times", {
  td <- fca_toy()
  d <- prep_data(td$demand, td$supply_df, td$distance, id_col = "id", supply_cols = "doctors")
  first <- spax(d, spec_e2sfca(decay_gaussian(2)))
  other <- spax(d, spec_m2sfca(decay_gaussian(3)))
  again <- spax(d, spec_e2sfca(decay_gaussian(2)))
  expect_identical(terra::values(first$accessibility), terra::values(again$accessibility))
  expect_false(identical(terra::values(first$accessibility), terra::values(other$accessibility)))
  # the data are not changed by running
  expect_identical(format(d), format(prep_data(td$demand, td$supply_df, td$distance,
                                               id_col = "id", supply_cols = "doctors")))
})

test_that("allocation specifications saved with spax 0.5.0 (class spax_spec alone) keep working", {
  old <- spec_clm()
  class(old) <- "spax_spec"
  expect_identical(format(old), format(spec_clm()))
  expect_identical(capture.output(print(old)), capture.output(print(spec_clm())))
  expect_identical(class(.upgrade_spec(old)), c("spax_allocation_spec", "spax_spec"))
  # an object that is not an allocation specification is left alone
  other <- structure(list(kind = "other"), class = "spax_spec")
  expect_identical(.upgrade_spec(other), other)
  # spax() takes the old class
  case <- alloc_small_case()
  fit_it <- function(spec) {
    suppressMessages(spax(case$inputs, spec, observed = obs_totals(case$observed),
                          starts = case$starts, lower = case$lower, upper = case$upper))
  }
  fresh <- fit_it(spec_clm())
  saved <- fit_it(old)
  expect_identical(coef(saved), coef(fresh))
  # a result saved with 0.5.0 prints its specification block
  saved$spec <- old
  expect_identical(capture.output(print(saved))[1:4], capture.output(print(fresh))[1:4])
})

test_that("a raster sigma saved and reloaded is refused with the package's message", {
  td <- fca_toy()
  d <- prep_data(td$demand, td$supply_vector, td$distance)
  sig <- terra::rast(td$demand)
  terra::values(sig) <- 2
  spec <- spec_e2sfca(decay_gaussian(sig))
  expect_s3_class(spax(d, spec), "spax")
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(spec, path)
  expect_message_exact(spax(d, readRDS(path)),
                       paste0("the raster sigma of this specification is no longer ",
                              "available (saved and reloaded?): give the decay its raster again"))
})

test_that("one named threshold on data without groups is a plain threshold", {
  td <- fca_toy()
  d <- prep_data(td$demand, td$supply_vector, td$distance)
  named <- spax(d, spec_2sfca(threshold = c("50%" = 2)))
  plain <- spax(d, spec_2sfca(threshold = 2))
  expect_identical(terra::values(named$accessibility), terra::values(plain$accessibility))
  # two named thresholds still need groups
  expect_message_exact(spax(d, spec_2sfca(threshold = c(a = 2, b = 3))),
                       "threshold needs data with demand groups")
})
