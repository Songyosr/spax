# Binding a model specification to prepared data (.bind_spax_spec()), prep_data() and
# the printed lines of prepared inputs and bound models. Fit mode must give the model
# that the matching clm_allocation() call gives; evaluate mode must evaluate to the
# same outputs. Printed output is compared as exact lines, not with snapshots.

# Fixtures -------------------------------------------------------------------------

# Sixteen origins and three facilities (alloc_huff_case()) with the demand split
# into three groups, as in the grouped tests of test-06-allocation_groups.R. Travel is
# shared by the groups or given by group (scaled by 1, 1.1 and 1.3).
bind_groups <- c("young", "mid", "old")

bind_split <- function(demand, young_at = NULL) {
  out <- list(young = demand * 0.2, mid = demand * 0.5, old = demand * 0.3)
  if (!is.null(young_at)) out$young[names(young_at)] <- young_at
  out
}

bind_grouped <- function(per_group_travel = FALSE, demand = NULL, travel = NULL) {
  f <- alloc_huff_case()
  if (is.null(demand)) demand <- bind_split(f$demand)
  if (is.null(travel)) travel <- f$distance
  if (per_group_travel) {
    travel <- list(young = travel, mid = travel * 1.1, old = travel * 1.3)
  }
  prepare_allocation(demand, f$supply, travel)
}

# The same data with a travel time of 0 at origin o2 and facility1. `young_at_o2` is
# the demand of the young group at o2, where the other groups have demand.
bind_zero_travel <- function(young_at_o2, per_group_travel = FALSE) {
  f <- alloc_huff_case()
  travel <- f$distance
  travel["o2", "facility1"] <- 0
  bind_grouped(per_group_travel, bind_split(f$demand, c(o2 = young_at_o2)), travel)
}

bind_plateau <- function() {
  decay_kernel(function(distance, sigma, plateau = 5) {
    ifelse(distance <= plateau, 1, exp(-(distance - plateau)^2 / (2 * sigma^2)))
  }, name = "plateau", parameter = "scale", lower = 1, upper = 20, args = list(plateau = 5))
}

# Mainland Cornwall, as in the README: registered residents, GP FTE and travel minutes.
bind_cornwall <- function(...) {
  areas <- sf::st_drop_geometry(cornwall_lsoa[!cornwall_lsoa$scilly, ])
  gp <- sf::st_drop_geometry(cornwall_gp[!cornwall_gp$scilly, ])
  travel <- cornwall_travel[areas$lsoa, gp$practice]
  list(demand = stats::setNames(areas$registered, areas$lsoa),
       supply = stats::setNames(gp$gp_fte_extg, gp$practice), travel = travel)
}

bind_error <- function(expr) {
  tryCatch({ expr; NULL }, error = function(e) conditionMessage(e))
}

# The bound model equals the one a clm_allocation() call declares, field by field
# (the bind closures differ by their environments).
expect_same_model <- function(bound, reference) {
  for (field in c("model", "substrate", "state", "theta")) {
    expect_identical(bound$problem[[field]], reference[[field]], label = field)
  }
  expect_identical(bound$problem$metadata$spec, reference$metadata$spec)
}

# Fit-mode identity rows (4.4) ------------------------------------------------------

test_that("fit mode gives the model of the matching clm_allocation() call: ungrouped", {
  inputs <- alloc_huff_case()$inputs
  plateau <- bind_plateau()
  rows <- list(
    default_clm = list(spec_clm(), clm_allocation(inputs, fit_v0 = TRUE)),
    exponential_outside = list(
      spec_clm(decay_exponential(), outside = 0.3),
      clm_allocation(inputs, family = "exponential", v0 = 0.3)),
    huff = list(spec_huff(), huff_allocation(inputs)),
    plateau = list(spec_clm(decay_custom(plateau)),
                   clm_allocation(inputs, family = plateau, fit_v0 = TRUE)),
    plateau_as_kernel = list(spec_clm(plateau),
                             clm_allocation(inputs, family = plateau, fit_v0 = TRUE)),
    builtin_kernel = list(spec_clm(decay_kernel("gaussian")),
                          clm_allocation(inputs, family = "gaussian", fit_v0 = TRUE)),
    builtin_name = list(spec_clm("exponential"),
                        clm_allocation(inputs, family = "exponential", fit_v0 = TRUE)),
    beta_fit = list(spec_clm(beta = "fit"),
                    clm_allocation(inputs, fit_v0 = TRUE, fit_beta = TRUE)),
    kappa_beta = list(spec_clm(kappa = 2, beta = 0.5),
                      clm_allocation(inputs, fit_v0 = TRUE, kappa = 2, beta = 0.5)),
    no_outside = list(spec_clm(outside = 0), clm_allocation(inputs, v0 = 0)),
    huff_exponential = list(
      spec_huff(decay_exponential(), beta = 1.5, kappa = 3),
      clm_allocation(inputs, family = "exponential", kappa = 3, beta = 1.5, v0 = 0)))
  for (row in names(rows)) {
    bound <- .bind_spax_spec(rows[[row]][[1L]], inputs)
    expect_identical(bound$mode, "fit", label = row)
    expect_null(bound$theta_given, label = row)
    expect_same_model(bound, rows[[row]][[2L]])
  }
  # the declared kernel is kept as a declaration, a built-in one as its name
  expect_identical(.bind_spax_spec(spec_clm(plateau), inputs)$problem$metadata$spec$kernel,
                   plateau)
  expect_null(.bind_spax_spec(spec_clm(decay_kernel("gaussian")), inputs)$problem$metadata$spec[["kernel", exact = TRUE]])
})

test_that("fit mode gives the model of the matching clm_allocation() call: grouped", {
  shared <- bind_grouped()
  by_group_travel <- bind_grouped(per_group_travel = TRUE)
  plateau <- bind_plateau()
  rows <- list(
    by_sigma = list(spec_clm(by_group = "sigma"),
                    clm_allocation(shared, fit_v0 = TRUE, by_group = "sigma")),
    by_sigma_v0 = list(spec_clm(by_group = c("sigma", "v0")),
                       clm_allocation(shared, fit_v0 = TRUE, by_group = c("sigma", "v0"))),
    huff_by_sigma = list(spec_huff(by_group = "sigma"),
                         huff_allocation(shared, by_group = "sigma")),
    v0_vector = list(
      spec_clm(outside = c(old = 0.1, young = 0.2, mid = 0.3)),
      clm_allocation(shared, v0 = c(young = 0.2, mid = 0.3, old = 0.1))),
    v0_all_numbers_list = list(
      spec_clm(outside = list(old = 0.1, young = 0.2, mid = 0.3)),
      clm_allocation(shared, v0 = c(young = 0.2, mid = 0.3, old = 0.1))),
    mixed_outside = list(
      spec_clm(outside = list(young = "fit", mid = 0.2, old = "fit")),
      clm_allocation(shared, fit_v0 = TRUE, by_group = "v0", fixed = c(v0.mid = 0.2))),
    all_fit_outside = list(
      spec_clm(outside = list(young = "fit", mid = "fit", old = "fit")),
      clm_allocation(shared, fit_v0 = TRUE, by_group = "v0")),
    shared_held_sigma = list(spec_clm(decay_gaussian(5)),
                             clm_allocation(shared, fit_v0 = TRUE, fixed = c(sigma = 5))),
    beta_fit = list(spec_clm(beta = "fit", by_group = "sigma"),
                    clm_allocation(shared, fit_v0 = TRUE, fit_beta = TRUE,
                                   by_group = "sigma")),
    kappa_beta = list(spec_clm(kappa = 2, beta = 0.5),
                      clm_allocation(shared, fit_v0 = TRUE, kappa = 2, beta = 0.5)))
  for (row in names(rows)) {
    bound <- .bind_spax_spec(rows[[row]][[1L]], shared)
    expect_identical(bound$mode, "fit", label = row)
    expect_same_model(bound, rows[[row]][[2L]])
  }
  # one held sigma among per-group families, with shared and with per-group travel
  families <- list(old = decay_gaussian(), young = decay_exponential(0.3), mid = decay_gaussian())
  for (data in list(shared, by_group_travel)) {
    expect_same_model(
      .bind_spax_spec(spec_clm(families), data),
      clm_allocation(data, family = list(young = "exponential", mid = "gaussian",
                                         old = "gaussian"),
                     fit_v0 = TRUE, fixed = c(sigma.young = 0.3)))
    expect_same_model(
      .bind_spax_spec(spec_clm(list(young = decay_gaussian(), mid = decay_custom(plateau),
                                    old = decay_power(1.5))), data),
      clm_allocation(data, family = list(young = "gaussian", mid = plateau, old = "power"),
                     fit_v0 = TRUE, fixed = c(sigma.old = 1.5)))
  }
})

test_that("a decay list in fit mode holds its numbers and fits its \"fit\" elements", {
  data <- bind_grouped()
  spec <- spec_clm(list(young = decay_gaussian(3), mid = decay_gaussian(),
                        old = decay_gaussian(5)), outside = 0.1)
  b <- .bind_spax_spec(spec, data)
  expect_identical(b$mode, "fit")
  expect_identical(b$problem$theta$names, "sigma.mid")
  expect_same_model(b, clm_allocation(
    data, family = list(young = "gaussian", mid = "gaussian", old = "gaussian"), v0 = 0.1,
    fixed = c(sigma.young = 3, sigma.old = 5)))
  expect_identical(b$held, c(sigma.young = 3, sigma.old = 5, v0 = 0.1, beta = 1, kappa = 1))
  # all numbers but a fitted beta: only beta is free
  all_held <- .bind_spax_spec(spec_huff(list(young = decay_power(1), mid = decay_power(2),
                                             old = decay_power(3))), data)
  expect_identical(all_held$mode, "fit")
  expect_identical(all_held$problem$theta$names, "beta")
  expect_identical(all_held$held, c(sigma.young = 1, sigma.mid = 2, sigma.old = 3,
                                    v0 = 0, kappa = 1))
})

# Evaluate-mode rows (4.4) ------------------------------------------------------------

test_that("evaluate mode evaluates like the matching clm_allocation() call", {
  inputs <- alloc_huff_case()$inputs
  shared <- bind_grouped()
  v0 <- c(old = 0.1, young = 0.2, mid = 0.3)
  numbers <- list(young = decay_gaussian(3), mid = decay_gaussian(4), old = decay_gaussian(5))
  rows <- list(
    grouped_gaussian = list(
      spec_clm(decay_gaussian(5), outside = 0.1), shared,
      clm_allocation(shared, v0 = 0.1), c(sigma = 5)),
    decay_list = list(
      spec_clm(numbers, outside = 0.1), shared,
      clm_allocation(shared, family = list(young = "gaussian", mid = "gaussian",
                                           old = "gaussian"), v0 = 0.1),
      c(sigma.young = 3, sigma.mid = 4, sigma.old = 5)),
    huff_grouped = list(
      spec_huff(decay_power(2), beta = 1), shared,
      clm_allocation(shared, family = "power", v0 = 0), c(sigma = 2)),
    huff_ungrouped = list(
      spec_huff(decay_power(2), beta = 1), inputs,
      clm_allocation(inputs, family = "power", v0 = 0), c(sigma = 2)),
    v0_vector = list(
      spec_clm(decay_gaussian(5), outside = v0), shared,
      clm_allocation(shared, v0 = v0[bind_groups]), c(sigma = 5)),
    ungrouped_gaussian = list(
      spec_clm(decay_gaussian(4), outside = 0.2, beta = 0.8, kappa = 2), inputs,
      clm_allocation(inputs, v0 = 0.2, beta = 0.8, kappa = 2), c(sigma = 4)),
    list_with_v0_numbers = list(
      spec_clm(numbers, outside = list(young = 0.1, mid = 0.2, old = 0.3)), shared,
      clm_allocation(shared, family = list(young = "gaussian", mid = "gaussian",
                                           old = "gaussian"),
                     v0 = c(young = 0.1, mid = 0.2, old = 0.3)),
      c(sigma.young = 3, sigma.mid = 4, sigma.old = 5)))
  for (row in names(rows)) {
    spec <- rows[[row]][[1L]]
    b <- .bind_spax_spec(spec, rows[[row]][[2L]])
    reference <- rows[[row]][[3L]]
    theta <- rows[[row]][[4L]]
    expect_identical(b$mode, "evaluate", label = row)
    expect_identical(b$theta_given, theta, label = row)
    expect_identical(b$problem$theta$names, names(theta), label = row)
    expect_identical(evaluate_allocation(b$problem, b$theta_given)$outputs,
                     evaluate_allocation(reference, theta)$outputs, label = row)
    # nothing is fixed in evaluate mode
    expect_identical(length(b$problem$metadata$spec[["fixed", exact = TRUE]]), 0L, label = row)
  }
})

# The returned values ------------------------------------------------------------------

test_that("the result has problem, mode, theta_given and held in that order", {
  b <- .bind_spax_spec(spec_clm(), alloc_huff_case()$inputs)
  expect_identical(names(b), c("problem", "mode", "theta_given", "held"))
  expect_s3_class(b$problem, "ae_problem")
  # a NULL theta_given stays in the list
  expect_true("theta_given" %in% names(b))
  expect_null(b$theta_given)
})

test_that("held lists the values that are neither fitted nor given", {
  inputs <- alloc_huff_case()$inputs
  shared <- bind_grouped()
  held <- function(spec, data) .bind_spax_spec(spec, data)$held
  # ungrouped
  expect_identical(held(spec_clm(), inputs), c(beta = 1, kappa = 1))
  expect_identical(held(spec_huff(), inputs), c(v0 = 0, kappa = 1))
  expect_identical(held(spec_clm(decay_exponential(), outside = 0.3), inputs),
                   c(v0 = 0.3, beta = 1, kappa = 1))
  expect_identical(held(spec_clm(beta = "fit"), inputs), c(kappa = 1))
  expect_identical(held(spec_clm(kappa = 2, beta = 0.5), inputs), c(beta = 0.5, kappa = 2))
  expect_identical(held(spec_clm(outside = 0), inputs), c(v0 = 0, beta = 1, kappa = 1))
  # grouped
  expect_identical(held(spec_clm(decay_gaussian(5)), shared),
                   c(sigma = 5, beta = 1, kappa = 1))
  expect_identical(held(spec_clm(list(old = decay_gaussian(), young = decay_exponential(0.3),
                                      mid = decay_gaussian())), shared),
                   c(sigma.young = 0.3, beta = 1, kappa = 1))
  expect_identical(held(spec_clm(outside = c(old = 0.1, young = 0.2, mid = 0.3)), shared),
                   c(v0.young = 0.2, v0.mid = 0.3, v0.old = 0.1, beta = 1, kappa = 1))
  expect_identical(held(spec_clm(outside = list(young = "fit", mid = 0.2, old = "fit")), shared),
                   c(v0.mid = 0.2, beta = 1, kappa = 1))
  # in evaluate mode sigma is given as theta, not held
  b <- .bind_spax_spec(spec_clm(decay_gaussian(5), outside = 0.1), shared)
  expect_identical(b$theta_given, c(sigma = 5))
  expect_identical(b$held, c(v0 = 0.1, beta = 1, kappa = 1))
  b <- .bind_spax_spec(spec_clm(list(young = decay_gaussian(3), mid = decay_gaussian(4),
                                     old = decay_gaussian(5)), outside = 0.1), shared)
  expect_identical(b$theta_given, c(sigma.young = 3, sigma.mid = 4, sigma.old = 5))
  expect_identical(b$held, c(v0 = 0.1, beta = 1, kappa = 1))
  expect_identical(.bind_spax_spec(spec_huff(decay_power(2), beta = 1), inputs)$held,
                   c(v0 = 0, beta = 1, kappa = 1))
})

test_that("the mode is evaluate exactly when no parameter is free", {
  inputs <- alloc_huff_case()$inputs
  shared <- bind_grouped()
  mode <- function(spec, data = inputs) .bind_spax_spec(spec, data)$mode
  expect_identical(mode(spec_clm()), "fit")
  expect_identical(mode(spec_clm(decay_gaussian(4), outside = 0.1)), "evaluate")
  expect_identical(mode(spec_clm(decay_gaussian(4), outside = "fit", beta = 1), shared), "fit")
  expect_identical(mode(spec_clm(decay_gaussian(4), outside = 0.1, beta = "fit"), shared), "fit")
  expect_identical(mode(spec_huff(decay_power(2), beta = 1)), "evaluate")
  expect_identical(mode(spec_huff(decay_power(2)), shared), "fit")
  # a decay list with one "fit" element is fitted
  expect_identical(mode(spec_clm(list(young = decay_gaussian(3), mid = decay_gaussian(),
                                      old = decay_gaussian(5)), outside = 0.1), shared), "fit")
  # one group's weight fitted in a list of outside weights
  expect_identical(mode(spec_clm(decay_gaussian(3),
                                 outside = list(young = "fit", mid = 0.2, old = 0.1)), shared),
                   "fit")
})

# Binding errors, in order -----------------------------------------------------------

test_that("ungrouped data refuse group arguments in the order decay, outside, by_group", {
  inputs <- alloc_huff_case()$inputs
  needs <- function(arg) {
    paste0(arg, " needs data with demand groups (prep_data() with demand as a named ",
           "list by group)")
  }
  by_group_decay <- list(a = decay_gaussian(), b = decay_gaussian())
  everything <- spec_clm(by_group_decay, outside = c(a = 0.1, b = 0.2), by_group = "sigma")
  expect_identical(bind_error(.bind_spax_spec(everything, inputs)), needs("decay"))
  expect_identical(
    bind_error(.bind_spax_spec(spec_clm(outside = c(a = 0.1, b = 0.2), by_group = "sigma"),
                               inputs)), needs("outside"))
  expect_identical(
    bind_error(.bind_spax_spec(spec_clm(outside = list(a = "fit", b = 0)), inputs)),
    needs("outside"))
  # a named vector of length 1 is by group
  expect_identical(
    bind_error(.bind_spax_spec(spec_clm(outside = c(a = 0.1)), inputs)), needs("outside"))
  expect_identical(bind_error(.bind_spax_spec(spec_clm(by_group = "sigma"), inputs)),
                   needs("by_group"))
  expect_identical(bind_error(.bind_spax_spec(spec_huff(by_group = "sigma"), inputs)),
                   needs("by_group"))
  expect_identical(bind_error(.bind_spax_spec(spec_clm(by_group = "v0"), inputs)),
                   needs("by_group"))
  expect_identical(bind_error(.bind_spax_spec(spec_huff(by_group_decay), inputs)),
                   needs("decay"))
})

test_that("grouped data refuse a decay or outside given for other groups, decay first", {
  shared <- bind_grouped()
  msg <- function(arg, given) {
    paste0(arg, " is given for groups ", given, " but the data has groups young, mid, old")
  }
  wrong <- list(young = decay_gaussian(), old = decay_gaussian())
  expect_identical(bind_error(.bind_spax_spec(spec_clm(wrong), shared)),
                   msg("decay", "young, old"))
  expect_identical(
    bind_error(.bind_spax_spec(spec_clm(outside = c(young = 0.1, old = 0.2)), shared)),
    msg("outside", "young, old"))
  expect_identical(
    bind_error(.bind_spax_spec(spec_clm(outside = list(young = "fit", teen = 0)), shared)),
    msg("outside", "young, teen"))
  expect_identical(
    bind_error(.bind_spax_spec(spec_clm(
      list(young = decay_gaussian(), mid = decay_gaussian(), old = decay_gaussian(),
           extra = decay_gaussian())), shared)),
    msg("decay", "young, mid, old, extra"))
  # the decay is checked before the outside weights
  expect_identical(
    bind_error(.bind_spax_spec(spec_clm(wrong, outside = c(young = 0.1, old = 0.2)), shared)),
    msg("decay", "young, old"))
  # the names are compared as sets: any order is accepted
  expect_null(bind_error(.bind_spax_spec(
    spec_clm(list(old = decay_gaussian(), young = decay_gaussian(), mid = decay_gaussian())),
    shared)))
})

test_that("an ungrouped sigma is held only when nothing else is fitted", {
  inputs <- alloc_huff_case()$inputs
  msg <- paste0("in this version sigma can be held only when nothing else is fitted, or ",
                "with demand groups; give the decay sigma = \"fit\", or give beta and ",
                "outside values to evaluate at given parameters")
  expect_identical(bind_error(.bind_spax_spec(spec_clm(decay_gaussian(4)), inputs)), msg)
  expect_identical(bind_error(.bind_spax_spec(spec_huff(decay_power(2)), inputs)), msg)
  expect_identical(
    bind_error(.bind_spax_spec(spec_clm(decay_gaussian(4), outside = 0.1, beta = "fit"),
                               inputs)), msg)
  # everything given: evaluate mode, no error
  expect_null(bind_error(.bind_spax_spec(spec_clm(decay_gaussian(4), outside = 0.1), inputs)))
  # with demand groups the held sigma goes through `fixed`
  expect_null(bind_error(.bind_spax_spec(spec_clm(decay_gaussian(4)), bind_grouped())))
})

test_that("the group check comes first, then the held sigma, then the travel domain", {
  f <- alloc_huff_case()
  travel <- f$distance
  travel["o2", "facility1"] <- 0
  data <- prepare_allocation(f$demand, f$supply, travel)
  needs_groups <- paste0("by_group needs data with demand groups (prep_data() with demand ",
                         "as a named list by group)")
  # every error applies to the same data with a zero travel time
  expect_identical(bind_error(.bind_spax_spec(spec_huff(by_group = "sigma"), data)),
                   needs_groups)
  expect_match(bind_error(.bind_spax_spec(spec_huff(decay_power(2)), data)),
               "^in this version sigma can be held only")
  expect_match(bind_error(.bind_spax_spec(spec_huff(), data)),
               "^power decay needs positive travel times")
  # on grouped data the group names are checked before the travel
  zero <- bind_zero_travel(young_at_o2 = 10)
  expect_match(bind_error(.bind_spax_spec(spec_huff(list(young = decay_power()), beta = 1),
                                          zero)),
               "^decay is given for groups young but the data has groups young, mid, old$")
})

# D8: a kernel that is undefined at zero refuses a zero travel time ------------------------

test_that("D8 ungrouped: power decay refuses a zero travel time, gaussian accepts it", {
  f <- alloc_huff_case()
  travel <- f$distance
  travel["o2", "facility1"] <- 0
  data <- prepare_allocation(f$demand, f$supply, travel)
  msg <- function(n, name = "power") {
    paste0(name, " decay needs positive travel times, and ", n,
           if (n == 1) " origin-facility pair has " else " origin-facility pairs have ",
           "travel 0; use decay_exponential() or decay_gaussian(), or set a minimum ",
           "travel time (for example pmax(travel, 1))")
  }
  expect_identical(bind_error(.bind_spax_spec(spec_huff(), data)), msg(1))
  expect_identical(bind_error(.bind_spax_spec(spec_clm(decay_power()), data)), msg(1))
  expect_identical(bind_error(.bind_spax_spec(spec_huff(decay_power(2), beta = 1), data)),
                   msg(1))
  expect_null(bind_error(.bind_spax_spec(spec_clm(), data)))
  expect_null(bind_error(.bind_spax_spec(spec_clm(decay_exponential()), data)))
  expect_null(bind_error(.bind_spax_spec(spec_huff(decay_gaussian()), data)))
  # the number of pairs counts every zero
  more <- travel
  more[c("o3", "o9"), "facility2"] <- 0
  expect_identical(bind_error(.bind_spax_spec(
    spec_huff(), prepare_allocation(f$demand, f$supply, more))), msg(3))
  # a declared kernel is refused when it is undefined at zero, and says its name
  strict <- decay_kernel(function(distance, sigma) exp(-sigma * log(distance + 1)^2),
                         name = "logsq", parameter = "rate", zero_distance = FALSE)
  expect_identical(bind_error(.bind_spax_spec(spec_clm(strict), data)), msg(1, "logsq"))
  allowed <- decay_kernel(function(distance, sigma) exp(-sigma * log(distance + 1)^2),
                          name = "logsq", parameter = "rate")
  expect_null(bind_error(.bind_spax_spec(spec_clm(allowed), data)))
  # a zero without finite travel is not a zero travel time: an absent edge is Inf
  absent <- f$distance
  absent["o2", "facility1"] <- Inf
  expect_null(bind_error(.bind_spax_spec(
    spec_huff(), prepare_allocation(f$demand, f$supply, absent))))
})

test_that("D8 ungrouped: Cornwall travel with one zero", {
  skip_on_cran()
  cw <- bind_cornwall()
  msg <- paste0("power decay needs positive travel times, and 1 origin-facility pair has ",
                "travel 0; use decay_exponential() or decay_gaussian(), or set a minimum ",
                "travel time (for example pmax(travel, 1))")
  expect_null(bind_error(.bind_spax_spec(spec_huff(),
    prep_data(cw$demand, cw$supply, cw$travel))))
  travel <- cw$travel
  travel[7, 3] <- 0
  data <- prep_data(cw$demand, cw$supply, travel)
  expect_identical(bind_error(.bind_spax_spec(spec_huff(), data)), msg)
  expect_null(bind_error(.bind_spax_spec(spec_huff(decay_gaussian()), data)))
  expect_null(bind_error(.bind_spax_spec(spec_clm(), data)))
  # a minimum travel time repairs it
  expect_null(bind_error(.bind_spax_spec(spec_huff(), prep_data(
    cw$demand, cw$supply, pmax(travel, 1)))))
})

test_that("D8 grouped: a zero only on a row where the power group has no demand is accepted", {
  # young has no demand at o2, where the other groups do and travel is 0
  for (per_group in c(FALSE, TRUE)) {
    data <- bind_zero_travel(young_at_o2 = 0, per_group_travel = per_group)
    young_power <- list(young = decay_power(), mid = decay_gaussian(), old = decay_gaussian())
    expect_null(bind_error(.bind_spax_spec(spec_clm(young_power), data)))
    expect_null(bind_error(.bind_spax_spec(spec_huff(young_power), data)))
    # the other groups have demand at o2: a power decay there is refused
    old_power <- list(young = decay_gaussian(), mid = decay_gaussian(), old = decay_power())
    msg <- bind_error(.bind_spax_spec(spec_clm(old_power), data))
    expect_identical(msg, paste0("power decay needs positive travel times, and 1 ",
      "origin-facility pair has travel 0; use decay_exponential() or decay_gaussian(), ",
      "or set a minimum travel time (for example pmax(travel, 1))"))
    # a shared power decay applies to every group, so it is refused too
    expect_match(bind_error(.bind_spax_spec(spec_huff(), data)),
                 "^power decay needs positive travel times, and 1 origin-facility pair has")
    # the engine accepts what the check accepts
    expect_no_error(evaluate_allocation(
      .bind_spax_spec(spec_clm(young_power, outside = 0.1), data)$problem,
      c(sigma.young = 2, sigma.mid = 3, sigma.old = 4)))
  }
  # a positive demand of the power group on the zero-travel row is refused
  for (per_group in c(FALSE, TRUE)) {
    data <- bind_zero_travel(young_at_o2 = 10, per_group_travel = per_group)
    young_power <- list(young = decay_power(), mid = decay_gaussian(), old = decay_gaussian())
    expect_match(bind_error(.bind_spax_spec(spec_clm(young_power), data)),
                 "^power decay needs positive travel times, and 1 origin-facility pair has")
  }
})

test_that("D8 grouped: the zero is read in the group's own travel", {
  # the zero is in the young group's travel only; mid and old travel stay positive
  f <- alloc_huff_case()
  travel_young <- f$distance
  travel_young["o2", "facility1"] <- 0
  travel <- list(young = travel_young, mid = f$distance * 1.1, old = f$distance * 1.3)
  demand <- bind_split(f$demand)
  data <- prepare_allocation(demand, f$supply, travel)
  power_old <- list(young = decay_gaussian(), mid = decay_gaussian(), old = decay_power())
  expect_null(bind_error(.bind_spax_spec(spec_clm(power_old), data)))
  power_young <- list(young = decay_power(), mid = decay_gaussian(), old = decay_gaussian())
  expect_match(bind_error(.bind_spax_spec(spec_clm(power_young), data)),
               "^power decay needs positive travel times, and 1 origin-facility pair has")
  # with no demand for the young group at o2 its zero is not checked
  demand0 <- bind_split(f$demand, c(o2 = 0))
  data0 <- prepare_allocation(demand0, f$supply, travel)
  expect_null(bind_error(.bind_spax_spec(spec_clm(power_young), data0)))
})

# prep_data() ----------------------------------------------------------------------------

test_that("prep_data() is prepare_allocation() with travel for distance", {
  expect_identical(names(formals(prep_data)),
    c("demand", "supply", "travel", "origin_ids", "facility_ids", "included",
      "missing_demand", "missing_travel", "spatial", "metadata"))
  expect_identical(formals(prep_data)$missing_demand, quote(c("error", "exclude")))
  expect_identical(formals(prep_data)$missing_travel, quote(c("error", "legacy_zero")))
  f <- alloc_small_case()
  metadata <- list(demand_units = "requests", travel_units = "minutes")
  expect_identical(
    prep_data(f$demand, f$supply, f$travel, missing_demand = "exclude", metadata = metadata),
    prepare_allocation(f$demand, f$supply, f$travel, missing_demand = "exclude",
                       metadata = metadata))
  expect_s3_class(prep_data(f$demand, f$supply, f$travel, missing_demand = "exclude"),
                  "ae_prepared_inputs")
  # every argument is passed through
  included <- c(TRUE, TRUE, FALSE, TRUE, TRUE)
  ids <- names(f$demand)
  expect_identical(
    prep_data(unname(f$demand), unname(f$supply), unname(f$travel),
              origin_ids = ids, facility_ids = names(f$supply),
              included = included, missing_demand = "exclude"),
    prepare_allocation(unname(f$demand), unname(f$supply), unname(f$travel),
                       origin_ids = ids, facility_ids = names(f$supply),
                       included = included, missing_demand = "exclude"))
  travel <- f$travel
  travel[1, 2] <- NA
  expect_identical(
    prep_data(f$demand, f$supply, travel, missing_demand = "exclude",
              missing_travel = "legacy_zero"),
    prepare_allocation(f$demand, f$supply, travel, missing_demand = "exclude",
                       missing_travel = "legacy_zero"))
  # the same errors
  expect_identical(bind_error(prep_data(f$demand, f$supply, f$travel)),
                   bind_error(prepare_allocation(f$demand, f$supply, f$travel)))
  expect_match(bind_error(prep_data(f$demand, f$supply, f$travel)),
               "included missing demand requires missing_demand = 'exclude'")
  expect_identical(bind_error(prep_data(f$demand, f$supply, f$travel,
                                        missing_demand = "drop")),
                   bind_error(prepare_allocation(f$demand, f$supply, f$travel,
                                                 missing_demand = "drop")))
  # grouped demand and travel by group
  g <- alloc_huff_case()
  demand <- bind_split(g$demand)
  travel <- list(young = g$distance, mid = g$distance * 1.1, old = g$distance * 1.3)
  expect_identical(prep_data(demand, g$supply, travel),
                   prepare_allocation(demand, g$supply, travel))
  expect_identical(prep_data(demand, g$supply, g$distance),
                   prepare_allocation(demand, g$supply, g$distance))
})

test_that("prep_data() output prints and binds like the step functions' inputs", {
  f <- alloc_huff_case()
  data <- prep_data(f$demand, f$supply, f$distance)
  expect_identical(.bind_spax_spec(spec_clm(), data)$problem$substrate,
                   clm_allocation(f$inputs, fit_v0 = TRUE)$substrate)
})

# Printed prepared inputs -----------------------------------------------------------------

test_that("prepared inputs print their origins, labels, unknown travel and policies", {
  f <- alloc_small_case()
  expect_identical(capture.output(print(f$inputs)), c(
    "<spax data> 5 origins (3 active; 1 with zero demand, 1 with unknown demand) x 3 facilities",
    "  demand      130 requests (period1)",
    "  supply      slots (period1)",
    "  travel      minutes; unknown for 0 active origins",
    "  policies    missing demand: exclude; missing travel: error"))
  expect_identical(capture.output(print(f$inputs)), format(f$inputs))
  shown <- NULL
  capture.output(shown <- withVisible(print(f$inputs)))
  expect_false(shown$visible)
  expect_identical(shown$value, f$inputs)
})

test_that("prepared inputs without labels print only the total demand and the unknown count", {
  g <- alloc_huff_case()
  expect_identical(capture.output(print(g$inputs)), c(
    "<spax data> 16 origins (16 active) x 3 facilities",
    "  demand      460",
    "  travel      unknown for 0 active origins",
    "  policies    missing demand: error; missing travel: error"))
  # a label given alone: units without a period, and a period without units
  d <- prep_data(g$demand, g$supply, g$distance,
                 metadata = list(demand_period = "2026", supply_units = "beds"))
  expect_identical(capture.output(print(d)), c(
    "<spax data> 16 origins (16 active) x 3 facilities",
    "  demand      460 (2026)",
    "  supply      beds",
    "  travel      unknown for 0 active origins",
    "  policies    missing demand: error; missing travel: error"))
  d <- prep_data(g$demand, g$supply, g$distance,
                 metadata = list(travel_units = "km", supply_period = "June 2026"))
  expect_identical(capture.output(print(d)), c(
    "<spax data> 16 origins (16 active) x 3 facilities",
    "  demand      460",
    "  supply      (June 2026)",
    "  travel      km; unknown for 0 active origins",
    "  policies    missing demand: error; missing travel: error"))
})

test_that("prepared inputs print the count of active origins with unknown travel", {
  f <- alloc_small_case()
  travel <- f$travel
  travel["o1", "f1"] <- NA
  one <- prep_data(f$demand, f$supply, travel, missing_demand = "exclude",
                   missing_travel = "legacy_zero", metadata = list(travel_units = "minutes"))
  expect_identical(capture.output(print(one)), c(
    "<spax data> 5 origins (3 active; 1 with zero demand, 1 with unknown demand) x 3 facilities",
    "  demand      130",
    "  travel      minutes; unknown for 1 active origin",
    "  policies    missing demand: exclude; missing travel: legacy_zero"))
  travel["o5", "f3"] <- NA
  two <- prep_data(f$demand, f$supply, travel, missing_demand = "exclude",
                   missing_travel = "legacy_zero")
  expect_identical(capture.output(print(two))[3], "  travel      unknown for 2 active origins")
  # unknown travel of an origin that is not active does not count
  travel <- f$travel
  travel["o2", "f1"] <- NA
  zero <- prep_data(f$demand, f$supply, travel, missing_demand = "exclude",
                    missing_travel = "legacy_zero")
  expect_identical(capture.output(print(zero))[3], "  travel      unknown for 0 active origins")
})

test_that("prepared inputs list the demand groups and count the excluded origins", {
  g <- alloc_huff_case()
  demand <- bind_split(g$demand)
  data <- prep_data(demand, g$supply, g$distance,
                    metadata = list(demand_units = "residents", travel_units = "minutes"))
  expect_identical(capture.output(print(data)), c(
    "<spax data> 16 origins (16 active) x 3 facilities",
    "  groups      young, mid, old",
    "  demand      460 residents",
    "  travel      minutes; unknown for 0 active origins",
    "  policies    missing demand: error; missing travel: error"))
  # origins without demand in every group are excluded; so are those left out
  demand$young["o1"] <- demand$mid["o1"] <- demand$old["o1"] <- 0
  included <- rep(TRUE, 16)
  included[2] <- FALSE
  data <- prep_data(demand, g$supply, g$distance, included = included)
  expect_identical(capture.output(print(data))[1],
    "<spax data> 16 origins (14 active; 1 with zero demand, 1 excluded) x 3 facilities")
  # grouped unknown travel counts the active origins unknown for a group with demand
  travel <- g$distance
  travel["o5", "facility2"] <- NA
  data <- prep_data(demand, g$supply, travel, missing_travel = "legacy_zero")
  expect_identical(capture.output(print(data))[4], "  travel      unknown for 1 active origin")
})

test_that("prepared inputs say why origins are not active, in a fixed order", {
  f <- alloc_small_case()
  # zero and unknown demand
  expect_identical(capture.output(print(f$inputs))[1],
    "<spax data> 5 origins (3 active; 1 with zero demand, 1 with unknown demand) x 3 facilities")
  # only the parts that occur; one origin each of the four kinds, in this order
  demand <- c(o1 = 40, o2 = 0, o3 = 55, o4 = NA, o5 = 35, o6 = 20, o7 = 10)
  travel <- matrix(1 + (seq_len(21) %% 5), 7, 3,
                   dimnames = list(names(demand), c("f1", "f2", "f3")))
  data <- prep_data(demand, c(f1 = 5, f2 = 6, f3 = 7), travel, missing_demand = "exclude",
                    included = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE))
  expect_identical(data$origin_status,
                   c("active", "zero", "active", "unknown", "active", "active", "excluded"))
  expect_identical(capture.output(print(data))[1], paste0("<spax data> 7 origins (4 active; ",
    "1 with zero demand, 1 with unknown demand, 1 excluded) x 3 facilities"))
  # the registry status that preparation refuses today is worded as well
  data$origin_status[7L] <- "invalid_omitted"
  expect_identical(capture.output(print(data))[1], paste0("<spax data> 7 origins (4 active; ",
    "1 with zero demand, 1 with unknown demand, 1 with invalid demand) x 3 facilities"))
  # inputs without the registry fall back on the old count
  data$origin_status <- NULL
  expect_identical(capture.output(print(data))[1],
                   "<spax data> 7 origins (4 active; 3 excluded) x 3 facilities")
})

test_that("the Cornwall example prints as in the design", {
  skip_on_cran()
  cw <- bind_cornwall()
  data <- prep_data(cw$demand, cw$supply, cw$travel, metadata = list(
    demand_units = "registered residents", demand_period = "July 2026",
    supply_units = "GP FTE excluding GPs in training", supply_period = "June 2026",
    travel_units = "minutes"))
  expect_identical(capture.output(print(data)), c(
    "<spax data> 336 origins (336 active) x 54 facilities",
    "  demand      604,471 registered residents (July 2026)",
    "  supply      GP FTE excluding GPs in training (June 2026)",
    "  travel      minutes; unknown for 0 active origins",
    "  policies    missing demand: error; missing travel: error"))
})

test_that("a fractional or large total keeps seven significant digits with separators", {
  g <- alloc_huff_case()
  scaled <- prep_data(g$demand * 1234.5678, g$supply, g$distance)
  expect_identical(capture.output(print(scaled))[2], "  demand      567,901.2")
  big <- prep_data(g$demand * 1e6, g$supply, g$distance)
  expect_identical(capture.output(print(big))[2], "  demand      460,000,000")
})

# Printed bound models -------------------------------------------------------------------

test_that("a bound Huff model prints like its specification plus a data line", {
  inputs <- alloc_huff_case()$inputs
  bound <- .bind_spax_spec(spec_huff(), inputs)$problem
  expect_identical(capture.output(print(bound)), c(
    "<spax model> Huff model (no outside option)",
    "  decay       power (sigma = exponent): fit",
    "  attraction  (kappa * supply)^beta: kappa = 1, beta = fit",
    "  outside     none",
    "  data        16 origins x 3 facilities"))
  expect_identical(capture.output(print(bound)), format(bound))
  shown <- NULL
  capture.output(shown <- withVisible(print(bound)))
  expect_false(shown$visible)
  expect_identical(shown$value, bound)
  # the same lines as the specification, apart from the first line and the data line
  spec_lines <- format(spec_huff())
  expect_identical(format(bound)[2:4], spec_lines[2:4])
  # the same Huff model through the step function
  expect_identical(format(huff_allocation(inputs)), format(bound))
})

test_that("a bound conditional logit model prints its decay, attraction and outside weight", {
  inputs <- alloc_huff_case()$inputs
  show <- function(spec) format(.bind_spax_spec(spec, inputs)$problem)
  expect_identical(show(spec_clm()), c(
    "<spax model> conditional logit with an outside option",
    "  decay       gaussian (sigma = travel scale): fit",
    "  attraction  (kappa * supply)^beta: kappa = 1, beta = 1",
    "  outside     v0: fit",
    "  data        16 origins x 3 facilities"))
  expect_identical(show(spec_clm(decay_exponential(), outside = 0.3, beta = "fit",
                                 kappa = 2.5)), c(
    "<spax model> conditional logit with an outside option",
    "  decay       exponential (sigma = rate): fit",
    "  attraction  (kappa * supply)^beta: kappa = 2.5, beta = fit",
    "  outside     v0: 0.3",
    "  data        16 origins x 3 facilities"))
  # no outside option: the same model as the Huff model
  expect_identical(show(spec_clm(decay_power(), outside = 0, beta = "fit"))[1:4],
                   format(.bind_spax_spec(spec_huff(), inputs)$problem)[1:4])
  # a model with every parameter given binds sigma as free, so its decay shows as fit
  expect_identical(show(spec_clm(decay_gaussian(4), outside = 0.1, beta = 0.8))[2:4], c(
    "  decay       gaussian (sigma = travel scale): fit",
    "  attraction  (kappa * supply)^beta: kappa = 1, beta = 0.8",
    "  outside     v0: 0.1"))
  # the model line follows the specification's rule
  expect_identical(show(spec_clm(outside = 0))[1], "<spax model> Huff model (no outside option)")
  # the step function gives the same lines as the specification
  expect_identical(format(clm_allocation(inputs, fit_v0 = TRUE)), show(spec_clm()))
})

test_that("a bound model with a declared kernel prints the kernel's name and parameter", {
  inputs <- alloc_huff_case()$inputs
  plateau <- bind_plateau()
  expect_identical(format(.bind_spax_spec(spec_clm(plateau), inputs)$problem), c(
    "<spax model> conditional logit with an outside option",
    "  decay       plateau (sigma = scale): fit",
    "  attraction  (kappa * supply)^beta: kappa = 1, beta = 1",
    "  outside     v0: fit",
    "  data        16 origins x 3 facilities"))
  # a built-in kernel declaration prints as the built-in kernel
  expect_identical(format(.bind_spax_spec(spec_clm(decay_kernel("exponential")),
                                          inputs)$problem)[2],
                   "  decay       exponential (sigma = rate): fit")
})

test_that("a bound grouped model prints its decay and outside weights by group", {
  shared <- bind_grouped()
  show <- function(spec, data = shared) format(.bind_spax_spec(spec, data)$problem)
  data_line <- "  data        16 origins x 3 facilities"
  attraction <- "  attraction  (kappa * supply)^beta: kappa = 1, beta = 1"
  # shared parameters print as an ungrouped model does
  expect_identical(show(spec_clm()), c(
    "<spax model> conditional logit with an outside option",
    "  decay       gaussian (sigma = travel scale): fit", attraction,
    "  outside     v0: fit", data_line))
  # one fitted kernel with a sigma and a v0 for each group
  expect_identical(show(spec_clm(by_group = c("sigma", "v0"))), c(
    "<spax model> conditional logit with an outside option",
    "  decay       gaussian (sigma = travel scale): fit by group", attraction,
    "  outside     v0: fit by group", data_line))
  expect_identical(show(spec_huff(by_group = "sigma"))[1:4], c(
    "<spax model> Huff model (no outside option)",
    "  decay       power (sigma = exponent): fit by group",
    "  attraction  (kappa * supply)^beta: kappa = 1, beta = fit",
    "  outside     none"))
  # a shared held sigma goes through `fixed`
  expect_identical(show(spec_clm(decay_gaussian(5)))[2],
                   "  decay       gaussian (sigma = travel scale): 5")
  # kernels by group with one held sigma
  families <- list(young = decay_gaussian(), mid = decay_exponential(0.3), old = decay_power())
  expect_identical(show(spec_clm(families, outside = 0.2)), c(
    "<spax model> conditional logit with an outside option",
    "  decay       by group: young gaussian (fit), mid exponential (0.3), old power (fit)",
    attraction, "  outside     v0: 0.2", data_line))
  # the same line as the specification
  expect_identical(show(spec_clm(families, outside = 0.2))[2:4],
                   format(spec_clm(families, outside = 0.2))[2:4])
  # outside weights by group: held, mixed and all fitted
  expect_identical(show(spec_clm(outside = c(old = 0.1, young = 0.2, mid = 0.3)))[4],
                   "  outside     v0 by group: young 0.2, mid 0.3, old 0.1")
  expect_identical(show(spec_clm(outside = list(young = "fit", mid = 0.2, old = "fit")))[4],
                   "  outside     v0 by group: young fit, mid 0.2, old fit")
  expect_identical(show(spec_clm(outside = list(young = "fit", mid = "fit", old = "fit")))[4],
                   "  outside     v0: fit by group")
  expect_identical(show(spec_clm(outside = c(young = 0, mid = 0, old = 0)))[1:4][c(1, 4)], c(
    "<spax model> Huff model (no outside option)", "  outside     none"))
  # the lines of the specification, for every grouped row
  rows <- list(spec_clm(by_group = c("sigma", "v0")), spec_huff(by_group = "sigma"),
               spec_clm(outside = list(young = "fit", mid = 0.2, old = "fit")),
               spec_clm(outside = c(young = 0.2, mid = 0.3, old = 0.1)),
               spec_clm(families, outside = 0.2))
  # (a specification lists its by-group values in the order given, a model in the order
  # of the groups of its data)
  for (spec in rows) expect_identical(show(spec)[1:4], sub("<spax spec>", "<spax model>",
                                                           format(spec)[1:4], fixed = TRUE))
  # the step function: a held sigma among fitted ones
  m <- clm_allocation(shared, by_group = "sigma", fit_v0 = TRUE, fixed = c(sigma.mid = 3))
  expect_identical(format(m)[2],
                   "  decay       by group: young gaussian (fit), mid gaussian (3), old gaussian (fit)")
})

test_that("a bound model with a held outside weight and a fixed beta prints their values", {
  shared <- bind_grouped()
  m <- clm_allocation(shared, fit_v0 = TRUE, fit_beta = TRUE, fixed = c(beta = 0.7))
  expect_identical(format(m)[3], "  attraction  (kappa * supply)^beta: kappa = 1, beta = 0.7")
  m <- clm_allocation(shared, fit_v0 = TRUE, fixed = c(v0 = 2))
  expect_identical(format(m)[4], "  outside     v0: 2")
})

test_that("legacy and comparator models print one line and their parameters", {
  f <- alloc_huff_case()
  legacy <- .huff_problem(f$inputs, allocation = "huff_decay")
  expect_identical(capture.output(print(legacy)), c(
    "<ae_problem> huff (huff_decay, family gaussian)", "parameters: sigma"))
  expect_identical(format(.huff_problem(f$inputs, family = "exponential", fit_v0 = TRUE,
                                        allocation = "huff_decay")), c(
    "<ae_problem> huff (huff_decay, family exponential)", "parameters: sigma, v0"))
  shown <- NULL
  capture.output(shown <- withVisible(print(legacy)))
  expect_false(shown$visible)
  # the no-decay comparator of check_allocation()
  none <- .no_decay_model(clm_allocation(f$inputs, fit_v0 = TRUE, fit_beta = TRUE))
  expect_identical(capture.output(print(none)), c(
    "<ae_problem> huff (clm, family none)", "parameters: sigma, v0, beta"))
  # a model from before the allocation was recorded (SPAX-033) is a huff_decay model
  old <- .huff_problem(f$inputs, family = "gaussian", allocation = "huff_decay")
  old$metadata$spec$allocation <- NULL
  expect_identical(format(old), c("<ae_problem> huff (huff_decay, family gaussian)",
                                  "parameters: sigma"))
  # the bound model of the step workflow is not a fallback
  expect_identical(format(clm_allocation(f$inputs, fit_v0 = TRUE))[1],
                   "<spax model> conditional logit with an outside option")
})

# Cornwall: binding the examples of the design ---------------------------------------------

test_that("Cornwall binds to the models of the step functions and prints", {
  skip_on_cran()
  cw <- bind_cornwall()
  data <- prep_data(cw$demand, cw$supply, cw$travel)
  expect_same_model(.bind_spax_spec(spec_clm(), data), clm_allocation(data, fit_v0 = TRUE))
  expect_same_model(.bind_spax_spec(spec_huff(), data), huff_allocation(data))
  expect_same_model(.bind_spax_spec(spec_clm(decay_exponential(), outside = 0.3), data),
                    clm_allocation(data, family = "exponential", v0 = 0.3))
  expect_identical(capture.output(print(.bind_spax_spec(spec_huff(), data)$problem)), c(
    "<spax model> Huff model (no outside option)",
    "  decay       power (sigma = exponent): fit",
    "  attraction  (kappa * supply)^beta: kappa = 1, beta = fit",
    "  outside     none",
    "  data        336 origins x 54 facilities"))
  b <- .bind_spax_spec(spec_huff(decay_power(1.5), beta = 1), data)
  expect_identical(b$mode, "evaluate")
  expect_identical(evaluate_allocation(b$problem, b$theta_given)$outputs,
                   evaluate_allocation(clm_allocation(data, family = "power", v0 = 0),
                                       c(sigma = 1.5))$outputs)
})
