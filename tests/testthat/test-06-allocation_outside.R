# Outside counts as model outputs (SPAX-059, T1): flow_outside, utilization_outside
# and utilization_outside_by_group of the corrected CLM, their target identity,
# the fits to them against independent oracles written out in this file, and the
# refusals. The step layer takes the observed outside count as the entry
# "(outside)" of the target and fits it with the ordinary Poisson loss.

# Twelve origins on a line and four facilities (not alphabetical). The observed
# flows are the expected flows at `truth` times a deterministic wobble, so the
# maximum likelihood estimate differs from `truth` and the fit must be compared
# with an oracle, not with the truth. Every origin's outside count is known by
# subtraction.
.oc_case <- function() {
  x <- 1:12
  pos <- c(f2 = 2, f1 = 5, f4 = 8, f3 = 11)
  oid <- paste0("o", x)
  travel <- abs(outer(x, pos, "-")) + 0.5
  dimnames(travel) <- list(oid, names(pos))
  demand <- stats::setNames(100 + 10 * (x %% 4), oid)
  supply <- c(f2 = 20, f1 = 35, f4 = 15, f3 = 30)
  truth <- c(sigma = 2.5, v0 = 15)
  o <- clm_oracle(travel, demand, supply, truth[["sigma"]], truth[["v0"]])
  wobble <- 1 + 0.08 * sin(outer(x, seq_along(pos), function(i, j) i + 2 * j))
  flows <- o$flow * wobble
  dimnames(flows) <- dimnames(travel)
  list(travel = travel, demand = demand, supply = supply, truth = truth, flows = flows,
       outside = demand - rowSums(flows),
       inputs = prepare_allocation(demand, supply, travel))
}

.oc_target <- function(case) cbind(case$flows, "(outside)" = case$outside)

.oc_starts <- rbind(a = c(sigma = 2, v0 = 8), b = c(sigma = 4, v0 = 30))
.oc_lower <- c(sigma = 0.5, v0 = 0.5)
.oc_upper <- c(sigma = 20, v0 = 200)
.oc_control <- list(maxit = 300, factr = 1e3, pgtol = 1e-5)

.oc_fit <- function(model, observed, output, ...) {
  fit_allocation(model, observed, .oc_starts, .oc_lower, .oc_upper, output = output,
                 loss = "poisson", gradient = TRUE, control = .oc_control, ...)
}

# The observed outside count of each origin, relative to the demand, stays positive.
test_that("the outside-count fixture has known positive outside counts", {
  case <- .oc_case()
  expect_true(all(case$outside > 0))
  expect_equal(unname(rowSums(case$flows) + case$outside), unname(case$demand))
})

test_that("outside outputs equal the hand algebra of the choice model", {
  case <- .oc_case()
  model <- clm_allocation(case$inputs, fit_v0 = TRUE)
  e <- evaluate_allocation(model, case$truth,
    requested_outputs = c("flow", "flow_outside", "utilization_outside"))
  o <- clm_oracle(case$travel, case$demand, case$supply, 2.5, 15)
  expected_outside <- case$demand * o$outside_share
  fo <- e$outputs$flow_outside
  expect_identical(dim(fo), c(12L, 5L))
  expect_identical(rownames(fo), rownames(case$travel))
  expect_identical(colnames(fo), c(colnames(case$travel), "(outside)"))
  expect_equal(unname(fo[, 1:4]), unname(o$flow), tolerance = 1e-12)
  expect_equal(unname(fo[, 5]), unname(expected_outside), tolerance = 1e-12)
  # every row adds up to the demand of its origin
  expect_equal(unname(rowSums(fo)), unname(case$demand), tolerance = 1e-12)
  uo <- e$outputs$utilization_outside
  expect_length(uo, 5L)
  expect_equal(unname(uo), c(o$utilization, sum(expected_outside)), tolerance = 1e-12)
  expect_equal(sum(uo), sum(case$demand), tolerance = 1e-12)
  # the plain outputs are the inside part, and the outside outputs exist only on request
  expect_identical(e$outputs$flow, e$outputs$flow_outside[, 1:4])
  plain <- evaluate_allocation(model, case$truth)
  expect_false(any(names(.outside_outputs) %in% names(plain$outputs)))
  # without an outside option the outside count is 0 and the rows still add up
  huff <- clm_allocation(case$inputs)
  eh <- evaluate_allocation(huff, c(sigma = 2.5), requested_outputs = "flow_outside")
  expect_equal(unname(eh$outputs$flow_outside[, 5]), rep(0, 12))
  expect_equal(unname(rowSums(eh$outputs$flow_outside)), unname(case$demand), tolerance = 1e-12)
})

test_that("outside outputs are declared with axes, meanings and no state dependence", {
  case <- .oc_case()
  model <- clm_allocation(case$inputs, fit_v0 = TRUE)
  spec <- model$metadata$spec
  expect_identical(spec$output_axes[["flow_outside"]], "origin_facility_outside")
  expect_identical(spec$output_axes[["utilization_outside"]], "facility_outside")
  expect_identical(spec$output_meanings[["flow_outside"]], "expected_count")
  expect_identical(spec$output_meanings[["utilization_outside"]], "expected_count")
  expect_false("utilization_outside_by_group" %in% names(spec$output_axes))
  step <- .bind_theta(model, case$truth)
  expect_identical(step$state_independent_outputs,
                   c("flow", "flow_outside", "utilization_outside"))
  # identity: the outside entry closes the facility axis
  id <- .problem_output_identity(model, "flow_outside")
  expect_identical(id$axes, c("origin", "facility_outside"))
  expect_identical(id$ids$origin, rownames(case$travel))
  expect_identical(id$ids$facility_outside, c(colnames(case$travel), "(outside)"))
  id <- .problem_output_identity(model, "utilization_outside")
  expect_identical(id$axes, "facility_outside")
  expect_identical(id$ids[[1L]], c(colnames(case$travel), "(outside)"))
  # the legacy huff_decay form declares none of them
  legacy <- .huff_problem(case$inputs, v0 = 5, allocation = "huff_decay")
  expect_false(any(names(.outside_outputs) %in% names(legacy$metadata$spec$output_axes)))
  expect_null(legacy$metadata$spec$output_meanings)
  # the sensitivity (finite differences, as for flow) matches a numerical Jacobian
  theta <- c(sigma = 2.2, v0 = 12)
  x <- model$state$init
  for (out in c("flow_outside", "utilization_outside")) {
    S <- .problem_output_sensitivity(model, theta, x, out)
    value <- function(th) as.numeric(evaluate_allocation(model, th,
      requested_outputs = out)$outputs[[out]])
    numeric <- vapply(names(theta), function(nm) {
      h <- 1e-5 * theta[[nm]]
      up <- down <- theta
      up[[nm]] <- up[[nm]] + h
      down[[nm]] <- down[[nm]] - h
      (value(up) - value(down)) / (2 * h)
    }, numeric(length(value(theta))))
    expect_equal(unname(S), unname(numeric), tolerance = 1e-5)
  }
})

test_that("a facility called (outside) makes the outside outputs unavailable", {
  case <- .oc_case()
  travel <- case$travel
  colnames(travel)[2] <- "(outside)"
  supply <- case$supply
  names(supply)[2] <- "(outside)"
  model <- clm_allocation(prepare_allocation(case$demand, supply, travel), fit_v0 = TRUE)
  msg <- "a facility ID is \"\\(outside\\)\", which names the outside column"
  for (out in c("flow_outside", "utilization_outside")) {
    expect_error(.problem_output_identity(model, out), msg)
  }
  target <- cbind(case$flows, "(outside)" = case$outside)
  expect_error(.oc_fit(model, target, "flow_outside"), msg)
  expect_error(fit_allocation(model, target, output = "flow_outside", loss = "poisson"), msg)
  # the inside outputs of the same model are not affected
  expect_identical(.problem_output_identity(model, "flow")$ids$facility, colnames(travel))
})

test_that("flows: the fit is the multinomial maximum likelihood estimate", {
  case <- .oc_case()
  model <- clm_allocation(case$inputs, fit_v0 = TRUE)
  # the log-likelihood of the origins' choices written out in the test
  loglik <- function(par) {
    o <- clm_oracle(case$travel, case$demand, case$supply, exp(par[1]), exp(par[2]))
    -(sum(case$flows * log(o$probability)) + sum(case$outside * log(o$outside_share)))
  }
  oracle <- exp(stats::optim(log(c(2, 8)), loglik, method = "BFGS",
                             control = list(reltol = 1e-14, maxit = 1000))$par)
  target <- .oc_target(case)
  fit <- .oc_fit(model, target, "flow_outside")
  expect_equal(unname(coef(fit)), oracle, tolerance = 1e-4)
  expect_identical(fit$best$output, "flow_outside")
  # the modelled total outside count equals the observed one (score equation of log v0)
  modelled <- fitted(fit)
  expect_identical(colnames(modelled), c(colnames(case$travel), "(outside)"))
  expect_equal(sum(modelled[, "(outside)"]), sum(case$outside), tolerance = 1e-6)
  # the Poisson loss on all cells is the multinomial one up to a constant
  expect_equal(fit$best$loss - loglik(log(coef(fit))),
               fit$best$loss - loglik(log(oracle)), tolerance = 1e-6)
  # the default search gets there too, from starts it finds
  searched <- fit_allocation(model, target, output = "flow_outside", loss = "poisson")
  expect_equal(unname(coef(searched)), oracle, tolerance = 1e-4)
  expect_s3_class(searched, "ae_multistart_fit")
  expect_true(is.list(searched$default_search))
})

test_that("flows: names are matched by ID in any order, the outside column too", {
  case <- .oc_case()
  model <- clm_allocation(case$inputs, fit_v0 = TRUE)
  target <- .oc_target(case)
  base <- .oc_fit(model, target, "flow_outside")
  shuffled <- target[rev(rownames(target)), c("(outside)", "f3", "f1", "f4", "f2")]
  other <- .oc_fit(model, shuffled, "flow_outside")
  expect_equal(coef(other), coef(base), tolerance = 1e-10)
  expect_equal(other$best$loss, base$best$loss, tolerance = 1e-10)
  expect_identical(dimnames(fitted(other)), dimnames(fitted(base)))
  # a missing or extra entry is refused
  expect_error(.oc_fit(model, target[, -5], "flow_outside"), "does not match|exactly match")
  wrong <- target
  colnames(wrong)[5] <- "(rest)"
  expect_error(.oc_fit(model, wrong, "flow_outside"), "exactly match")
  negative <- target
  negative[2, 5] <- -1
  expect_error(.oc_fit(model, negative, "flow_outside"), "nonnegative")
})

test_that("flows: NA outside counts carry no outside term", {
  case <- .oc_case()
  model <- clm_allocation(case$inputs, fit_v0 = TRUE)
  target <- .oc_target(case)
  target[c(3, 7), "(outside)"] <- NA
  known <- !is.na(target[, "(outside)"])
  # the Poisson loss on the facility cells of every origin and on the outside cell of
  # the origins with a known outside count (rows with NA outside carry no outside term,
  # so the loss is not the multinomial one there)
  poisson <- function(par) {
    o <- clm_oracle(case$travel, case$demand, case$supply, exp(par[1]), exp(par[2]))
    f <- case$demand * o$probability
    f0 <- case$demand * o$outside_share
    sum(f - case$flows * log(f)) + sum(f0[known] - case$outside[known] * log(f0[known]))
  }
  oracle <- exp(stats::optim(log(c(2, 8)), poisson, method = "BFGS",
                             control = list(reltol = 1e-14, maxit = 1000))$par)
  fit <- .oc_fit(model, target, "flow_outside")
  expect_equal(unname(coef(fit)), oracle, tolerance = 1e-4)
  expect_identical(fit$best$n_observed, as.integer(48L + 10L))
  # an NA outside count is not a zero: setting it to 0 gives another estimate
  zero <- target
  zero[c(3, 7), "(outside)"] <- 0
  expect_gt(max(abs(coef(.oc_fit(model, zero, "flow_outside")) / coef(fit) - 1)), 1e-3)
  # NA everywhere: the facility counts alone, the same fit as output = "flow"
  none <- target
  none[, "(outside)"] <- NA
  inside <- .oc_fit(model, none, "flow_outside")
  flow <- .oc_fit(model, case$flows, "flow")
  expect_equal(coef(inside), coef(flow), tolerance = 1e-8)
  expect_equal(inside$best$loss, flow$best$loss, tolerance = 1e-8)
})

test_that("totals: an explicit outside total is fitted with the pooled Poisson loss", {
  case <- .oc_case()
  model <- clm_allocation(case$inputs, fit_v0 = TRUE)
  totals <- colSums(case$flows)
  # an outside total that is not the difference of the demand and the counts:
  # nothing here derives it by subtraction, and no matching total is claimed
  y0 <- 0.8 * (sum(case$demand) - sum(totals))
  loss <- function(par) {
    o <- clm_oracle(case$travel, case$demand, case$supply, exp(par[1]), exp(par[2]))
    mu <- o$utilization
    mu0 <- sum(case$demand) - sum(mu)
    sum(mu - totals * log(mu)) + (mu0 - y0 * log(mu0))
  }
  oracle <- exp(stats::optim(log(c(2, 8)), loss, method = "BFGS",
                             control = list(reltol = 1e-14, maxit = 1000))$par)
  target <- c(totals, "(outside)" = y0)
  fit <- .oc_fit(model, target, "utilization_outside")
  expect_equal(unname(coef(fit)), oracle, tolerance = 1e-4)
  expect_length(fitted(fit), 5L)
  expect_identical(names(fitted(fit)), c(names(case$supply), "(outside)"))
  # in any order, the outside element anywhere
  mixed <- target[c(3, 5, 1, 4, 2)]
  expect_equal(coef(.oc_fit(model, mixed, "utilization_outside")), coef(fit), tolerance = 1e-10)
  # NA for the outside total: the facility totals alone
  na_outside <- target
  na_outside[["(outside)"]] <- NA
  alone <- .oc_fit(model, na_outside, "utilization_outside")
  plain <- .oc_fit(model, totals, "utilization")
  expect_equal(coef(alone), coef(plain), tolerance = 1e-8)
  expect_error(.oc_fit(model, c(totals, "(outside)" = -1), "utilization_outside"), "nonnegative")
})

test_that("the default search reads the facility counts, not the outside entries", {
  case <- .oc_case()
  model <- clm_allocation(case$inputs, fit_v0 = TRUE)
  with_outside <- fit_allocation(model, .oc_target(case), output = "flow_outside",
                                 loss = "poisson")
  inside_only <- fit_allocation(model, case$flows, output = "flow", loss = "poisson")
  # the v0 seeds (and the sigma grid) come from the inside counts only
  columns <- c("point", "sigma", "v0")
  expect_equal(with_outside$default_search$grid[, columns],
               inside_only$default_search$grid[, columns], tolerance = 1e-12)
  expect_equal(with_outside$default_search$sigma_range, inside_only$default_search$sigma_range)
  # the same for totals
  totals <- colSums(case$flows)
  by_totals <- fit_allocation(model, c(totals, "(outside)" = sum(case$outside)),
                              output = "utilization_outside", loss = "poisson")
  plain <- fit_allocation(model, totals, output = "utilization", loss = "poisson")
  expect_equal(by_totals$default_search$grid[, columns], plain$default_search$grid[, columns],
               tolerance = 1e-12)
  # the guard (counts reaching the total demand) reads the inside counts too
  full <- case$flows / rowSums(case$flows) * case$demand
  expect_error(
    fit_allocation(model, cbind(full, "(outside)" = 0), output = "flow_outside",
                   loss = "poisson"),
    "reach the total demand")
  # an outside count alone does not count as an observed count
  expect_error(
    fit_allocation(model, cbind(case$flows * NA, "(outside)" = case$outside),
                   output = "flow_outside", loss = "poisson"),
    "at least one non-missing|nothing to calibrate|all zero or unobserved")
})

test_that("outside outputs are refused for a model without an outside option", {
  case <- .oc_case()
  target <- .oc_target(case)
  msg <- "outside counts need a model with an outside option"
  for (model in list(clm_allocation(case$inputs), huff_allocation(case$inputs),
                     clm_allocation(case$inputs, v0 = 0, fit_v0 = FALSE))) {
    # by default search and with explicit starts
    expect_error(fit_allocation(model, target, output = "flow_outside", loss = "poisson"), msg)
    expect_error(fit_allocation(model, c(colSums(case$flows), "(outside)" = 1),
      output = "utilization_outside", loss = "poisson"), msg)
    expect_error(fit_allocation(model, target, rbind(a = c(sigma = 2)), c(sigma = 0.5),
      c(sigma = 20), output = "flow_outside", loss = "poisson"), msg)
    expect_error(.bind_problem_target(model, target, "flow_outside"), msg)
  }
  # a positive outside weight held at a value is an outside option
  held <- clm_allocation(case$inputs, v0 = 10)
  fit <- fit_allocation(held, target, rbind(a = c(sigma = 2)), c(sigma = 0.5),
    c(sigma = 20), output = "flow_outside", loss = "poisson", gradient = TRUE)
  expect_s3_class(fit, "ae_multistart_fit")
  expect_false(is.null(fit$best))
  # other refusals: the by-group output needs demand groups, and a model built by
  # a version without the outputs does not declare them
  fitted_model <- clm_allocation(case$inputs, fit_v0 = TRUE)
  expect_error(.oc_fit(fitted_model, cbind(totals = colSums(case$flows)),
    "utilization_outside_by_group"), "needs a model with demand groups")
  expect_error(fit_allocation(fitted_model, cbind(totals = colSums(case$flows)),
    output = "utilization_outside_by_group", loss = "poisson"), "needs a model with demand groups")
  # (a saved model keeps the specification and the step it was built with)
  old <- fitted_model
  declared <- names(old$metadata$spec$output_axes)
  keep <- !declared %in% names(.outside_outputs)
  old$metadata$spec$output_axes <- old$metadata$spec$output_axes[keep]
  old$metadata$spec$output_meanings <- old$metadata$spec$output_meanings[
    !names(old$metadata$spec$output_meanings) %in% names(.outside_outputs)]
  # the checks and the bootstrap rebuild the step from the stored specification
  old$bind <- .compile_huff_map(old$metadata$spec, old$substrate)$bind
  expect_identical(.bind_theta(old, c(sigma = 2, v0 = 5))$state_independent_outputs, "flow")
  # the bootstrap rebuilds the step from the specification as well
  expect_identical(.bind_theta(.allocation_bootstrap_model(old),
                               c(sigma = 2, v0 = 5))$state_independent_outputs, "flow")
  expect_error(.oc_fit(old, target, "flow_outside"),
               "'flow_outside' is not declared by this model: rebuild")
  expect_error(fit_allocation(old, target, output = "flow_outside", loss = "poisson"),
               "not declared by this model")
  # ... and keeps working for every other output, including the checks that rebuild
  # the step from the specification
  flow <- .oc_fit(old, case$flows, "flow")
  current <- .oc_fit(fitted_model, case$flows, "flow")
  expect_equal(coef(flow), coef(current), tolerance = 1e-12)
  expect_equal(as.data.frame(check_allocation(old, flow)),
               as.data.frame(check_allocation(fitted_model, current)), tolerance = 1e-8)
})

test_that("the facility bootstrap refuses fits with outside counts", {
  case <- .oc_case()
  model <- clm_allocation(case$inputs, fit_v0 = TRUE)
  totals <- colSums(case$flows)
  fit <- .oc_fit(model, c(totals, "(outside)" = sum(case$outside)), "utilization_outside")
  expect_error(
    bootstrap_allocation(model, fit, observation = list(event = "visits", period = "year",
      sampling = "independent_facilities"), seed = 1),
    "the facility-case bootstrap is defined for facility totals without outside counts")
  fit_flow <- .oc_fit(model, .oc_target(case), "flow_outside")
  expect_error(
    bootstrap_allocation(model, fit_flow, observation = list(event = "visits", period = "year",
      sampling = "independent_facilities"), seed = 1),
    "the facility-case bootstrap is defined for facility totals without outside counts")
})

test_that("fits to the other outputs are unchanged", {
  # Values from the same fits on develop @ ca4153d, before the outside outputs.
  case <- .oc_case()
  model <- clm_allocation(case$inputs, fit_v0 = TRUE)
  fit_flow <- fit_allocation(model, case$flows, .oc_starts, .oc_lower, .oc_upper,
    output = "flow", loss = "poisson", gradient = TRUE,
    control = list(maxit = 300, factr = 1e3, pgtol = 1e-2))
  expect_equal(unname(coef(fit_flow)), c(2.50003569009813, 15.234359630588), tolerance = 1e-8)
  expect_equal(fit_flow$best$loss, -2506.79509577966, tolerance = 1e-10)
  fit_total <- fit_allocation(model, colSums(case$flows), .oc_starts, .oc_lower, .oc_upper,
    output = "utilization", loss = "poisson", gradient = TRUE,
    control = list(maxit = 300, factr = 1e3, pgtol = 1e-2))
  expect_equal(unname(coef(fit_total)), c(2.60967365143617, 15.8751609520974), tolerance = 1e-8)
  expect_equal(fit_total$best$loss, -4458.08629446525, tolerance = 1e-10)
  fit_sse <- fit_allocation(model, colSums(case$flows), .oc_starts, .oc_lower, .oc_upper)
  expect_equal(unname(coef(fit_sse)), c(2.61109807295168, 15.8869240114673), tolerance = 1e-8)
  expect_equal(fit_sse$best$loss, 0.0566547722078156, tolerance = 1e-10)
  # the fits do not carry the outside outputs unless they were the target
  expect_false(any(names(.outside_outputs) %in% names(fit_flow$best$outputs)))
})

test_that("check and compare work on outside-count fits, also after saveRDS/readRDS", {
  case <- .oc_case()
  gaussian <- clm_allocation(case$inputs, fit_v0 = TRUE)
  exponential <- clm_allocation(case$inputs, family = "exponential", fit_v0 = TRUE)
  target <- .oc_target(case)
  fit_g <- .oc_fit(gaussian, target, "flow_outside")
  fit_e <- fit_allocation(exponential, target, rbind(a = c(sigma = 0.5, v0 = 8)),
    c(sigma = 0.01, v0 = 0.5), c(sigma = 20, v0 = 200), output = "flow_outside",
    loss = "poisson", gradient = TRUE, control = .oc_control)
  check <- check_allocation(gaussian, fit_g)
  expect_s3_class(check, "ae_allocation_check")
  expect_equal(check$comparators$loss[check$comparators$comparator == "fitted"],
               fit_g$best$loss, tolerance = 1e-6)
  comparison <- compare_allocations(list(gaussian = fit_g, exponential = fit_e),
    models = list(gaussian = gaussian, exponential = exponential))
  expect_true(all(comparison$table$comparable))
  expect_equal(comparison$table$loss_difference[2], fit_e$best$loss - fit_g$best$loss,
               tolerance = 1e-10)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(list(gaussian = gaussian, exponential = exponential, fit_g = fit_g, fit_e = fit_e), path)
  back <- readRDS(path)
  expect_equal(as.data.frame(check_allocation(back$gaussian, back$fit_g)),
               as.data.frame(check), tolerance = 1e-8)
  again <- compare_allocations(list(gaussian = back$fit_g, exponential = back$fit_e),
    models = list(gaussian = back$gaussian, exponential = back$exponential))
  expect_equal(as.data.frame(again), as.data.frame(comparison), tolerance = 1e-8)
  # a flow fit and an outside-count fit are not like for like: no loss difference
  flow <- .oc_fit(gaussian, case$flows, "flow")
  mixed <- compare_allocations(list(inside = flow, outside = fit_g),
    models = list(inside = gaussian, outside = gaussian))
  expect_false(mixed$table$comparable[2])
  expect_true(is.na(mixed$table$loss_difference[2]))
  # the Poisson floor machinery is the ordinary one: the floor of the fit is reported
  expect_equal(check$poisson_floor[["eps"]], 1e-250)
  expect_false(check$flags[["positive_counts_below_poisson_floor"]])
})

# Demand groups ---------------------------------------------------------------

# Two groups on the same system, in the order old, young; origins and facilities
# are given in another order than the travel matrix.
.oc_group_case <- function() {
  case <- .oc_case()
  young <- round(0.4 * case$demand)
  old <- case$demand - young
  Dg <- cbind(young = young, old = old)
  truth <- c(sigma.young = 4, sigma.old = 2, v0.young = 6, v0.old = 20)
  by_group <- function(sigma, v0) {
    r <- lapply(c("young", "old"), function(g) {
      clm_oracle(case$travel, Dg[, g], case$supply, sigma[[g]], v0[[g]])
    })
    names(r) <- c("young", "old")
    list(utilization = vapply(r, function(o) o$utilization, numeric(4)),
         outside = vapply(c("young", "old"), function(g) {
           sum(Dg[, g] * r[[g]]$outside_share)
         }, numeric(1)))
  }
  expected <- by_group(c(young = 4, old = 2), c(young = 6, old = 20))
  rownames(expected$utilization) <- colnames(case$travel)
  exact <- rbind(expected$utilization, "(outside)" = expected$outside)
  permuted_origins <- rev(rownames(case$travel))
  permuted_facilities <- c("f3", "f2", "f1", "f4")
  inputs <- prepare_allocation(
    list(old = old[permuted_origins], young = young[permuted_origins]),
    case$supply[permuted_facilities], case$travel)
  list(case = case, Dg = Dg, truth = truth, by_group = by_group, exact = exact,
       inputs = inputs)
}

test_that("grouped outside counts equal the hand algebra and map groups and origins by ID", {
  g <- .oc_group_case()
  model <- clm_allocation(g$inputs, fit_v0 = TRUE, by_group = c("sigma", "v0"))
  spec <- model$metadata$spec
  expect_identical(spec$output_axes[["utilization_outside_by_group"]], "facility_outside_group")
  expect_identical(spec$output_meanings[["utilization_outside_by_group"]], "expected_count")
  expect_identical(spec$output_axes[["flow_outside"]], "origin_facility_outside")
  theta <- g$truth[model$theta$names]
  e <- evaluate_allocation(model, theta, requested_outputs = c("utilization_outside_by_group",
    "utilization_by_group", "flow_outside", "utilization_outside"))
  # the model orders its groups as given (old, young) and its facilities by travel
  uo <- e$outputs$utilization_outside_by_group
  expect_identical(colnames(uo), c("old", "young"))
  expect_identical(rownames(uo), c(colnames(g$case$travel), "(outside)"))
  expect_equal(uo, g$exact[rownames(uo), colnames(uo)], tolerance = 1e-12)
  # each group's column adds up to its demand
  expect_equal(colSums(uo), colSums(g$Dg)[colnames(uo)], tolerance = 1e-12)
  # the pooled outputs: the outside count of the pooled flows is the sum over groups
  expect_equal(unname(e$outputs$utilization_outside), unname(c(rowSums(g$exact[1:4, ]),
    sum(g$exact[5, ]))), tolerance = 1e-12)
  fo <- e$outputs$flow_outside
  expect_equal(unname(rowSums(fo)), unname(g$inputs$D_active), tolerance = 1e-12)
  expect_identical(colnames(fo)[5], "(outside)")
  # a grouped model saved before the outputs: the step rebuilt from its specification
  # names only the outputs that specification declares
  old <- model
  gone <- names(.outside_outputs)
  old$metadata$spec$output_axes <- old$metadata$spec$output_axes[
    !names(old$metadata$spec$output_axes) %in% gone]
  old$metadata$spec$output_meanings <- old$metadata$spec$output_meanings[
    !names(old$metadata$spec$output_meanings) %in% gone]
  old$bind <- .compile_huff_map(old$metadata$spec, old$substrate)$bind
  expect_identical(.bind_theta(old, theta)$state_independent_outputs,
    c("flow", "utilization_by_group", "access_by_group", "outside_share_by_group"))
  # state independence is declared for all three
  step <- .bind_theta(model, theta)
  expect_true(all(names(.outside_outputs) %in% step$state_independent_outputs))
  id <- .problem_output_identity(model, "utilization_outside_by_group")
  expect_identical(id$axes, c("facility_outside", "group"))
  expect_identical(id$ids$group, c("old", "young"))
})

test_that("grouped totals by group are fitted with their outside row, in any order", {
  g <- .oc_group_case()
  model <- clm_allocation(g$inputs, fit_v0 = TRUE, by_group = c("sigma", "v0"))
  theta <- g$truth[model$theta$names]
  # observed counts: the expected ones times a wobble; each group's outside count is
  # its demand less its counts
  wobble <- 1 + 0.05 * sin(outer(1:4, 1:2, `+`))
  observed <- g$exact
  observed[1:4, ] <- g$exact[1:4, ] * wobble
  observed["(outside)", ] <- colSums(g$Dg) - colSums(observed[1:4, ])
  stopifnot(all(observed["(outside)", ] > 0))
  # the oracle: the pooled Poisson likelihood over facility cells and one outside cell per group
  loss <- function(par) {
    th <- exp(par)
    names(th) <- names(theta)
    e <- g$by_group(c(young = th[["sigma.young"]], old = th[["sigma.old"]]),
                    c(young = th[["v0.young"]], old = th[["v0.old"]]))
    mu <- rbind(e$utilization, "(outside)" = e$outside)
    sum(mu - observed * log(mu))
  }
  oracle <- exp(stats::optim(log(theta * 1.3), loss, method = "BFGS",
                             control = list(reltol = 1e-14, maxit = 2000))$par)
  starts <- rbind(a = theta * 1.3)
  fit_one <- function(target) {
    fit_allocation(model, target, starts, theta / 20, theta * 20,
      output = "utilization_outside_by_group", loss = "poisson", gradient = TRUE,
      control = list(maxit = 300, factr = 1e3, pgtol = 1e-5))
  }
  canonical <- observed[c("f2", "f1", "f4", "f3", "(outside)"), c("old", "young")]
  fit <- fit_one(canonical)
  expect_equal(coef(fit)[names(theta)], oracle, tolerance = 1e-4)
  # facilities and groups in other orders, the outside row in the middle
  shuffled <- observed[c("f1", "(outside)", "f4", "f2", "f3"), c("young", "old")]
  other <- fit_one(shuffled)
  expect_equal(coef(other), coef(fit), tolerance = 1e-10)
  expect_identical(dimnames(fitted(other)), dimnames(fitted(fit)))
  # the group-labelled axes must be named
  expect_error(fit_one(unname(canonical)), "must be named")
  # a group without an observed facility count is refused, even with its outside count
  empty <- canonical
  empty[1:4, "young"] <- NA
  expect_error(fit_one(empty), "group-specific parameters")
  # NA outside counts carry no outside term
  na_outside <- canonical
  na_outside["(outside)", "old"] <- NA
  expect_s3_class(fit_one(na_outside), "ae_multistart_fit")
  # the default search works on the by-group target as well; with twelve origins the
  # loss of the old group is nearly flat along a ridge of its two parameters, where
  # the search stops (as it does for the facility counts alone), so the parameters
  # of the other group and the loss are compared
  searched <- fit_allocation(model, shuffled, output = "utilization_outside_by_group",
                             loss = "poisson")
  expect_s3_class(searched, "ae_multistart_fit")
  young <- c("sigma.young", "v0.young")
  expect_equal(coef(searched)[young], coef(fit)[young], tolerance = 5e-3)
  expect_lt(searched$best$loss - fit$best$loss, 0.5)
})

test_that("a group's outside count is finite where the group has no demand and no travel", {
  ids <- paste0("o", 1:6)
  Da <- stats::setNames(c(10, 0, 0, 7, 4, 6), ids)
  Db <- stats::setNames(c(5, 0, 8, 0, NA, 9), ids)
  M <- matrix(c(1, 2, 3, 4, 5, 6, 2, 3, 4, 5, 6, 7, 3, 4, 5, 6, 7, 8), 6, 3,
              dimnames = list(ids, c("f2", "f1", "f3")))
  Mb <- M
  Mb["o4", ] <- c(NA, -1, NA)
  supply <- c(f1 = 20, f2 = 15, f3 = 5)
  p <- prepare_allocation(list(a = Da, b = Db), supply, list(a = M, b = Mb),
                          missing_demand = "exclude")
  model <- clm_allocation(p, by_group = "sigma", v0 = 2)
  e <- evaluate_allocation(model, c(sigma.a = 3, sigma.b = 4),
    requested_outputs = c("utilization_outside_by_group", "outside_share_by_group"))
  expect_true(is.na(e$outputs$outside_share_by_group["o4", "b"]))
  uo <- e$outputs$utilization_outside_by_group
  expect_true(all(is.finite(uo)))
  rows <- p$origin_ids
  oracle <- clm_group_oracle(list(a = M[rows, ], b = M[rows, ]), p$D_groups,
                             as.vector(p$S), sigma = c(a = 3, b = 4), v0 = 2)
  expect_equal(unname(uo["(outside)", ]),
               unname(colSums(p$D_groups * oracle$outside_share_by_group)), tolerance = 1e-12)
  expect_equal(colSums(uo), colSums(p$D_groups), tolerance = 1e-12)
})

test_that("a group whose outside weight is held at 0 reports its positive outside count", {
  g <- .oc_group_case()
  model <- clm_allocation(g$inputs, fit_v0 = TRUE, by_group = c("sigma", "v0"),
                          fixed = c(v0.old = 0))
  expect_identical(model$theta$names, c("sigma.old", "sigma.young", "v0.young"))
  target <- g$exact[c("f2", "f1", "f4", "f3", "(outside)"), c("old", "young")]
  starts <- rbind(a = c(sigma.old = 2, sigma.young = 4, v0.young = 6))
  # the held group has no outside count to explain a positive observed one: the
  # existing Poisson-floor warning reports it
  warned <- testthat::capture_warnings(
    fit_allocation(model, target, starts, c(sigma.old = 0.5, sigma.young = 0.5, v0.young = 0.5),
      c(sigma.old = 20, sigma.young = 20, v0.young = 100),
      output = "utilization_outside_by_group", loss = "poisson", gradient = TRUE))
  expect_true(any(grepl("below the Poisson floor", warned)))
  # an outside count where the outside weight is 0 is among the causes it names
  expect_true(any(grepl(paste0("Look for observed counts on absent edges, at facilities with ",
    "zero supply or on an outside count where the outside weight is 0, or use a kernel ",
    "with a heavier tail."), warned, fixed = TRUE)))
})

test_that("the Poisson floor message names an outside count only for the outside outputs", {
  floor_info <- c(eps = 1e-250, positive_below = 2)
  for (output in names(.outside_outputs)) {
    expect_match(.poisson_floor_message(floor_info, output),
                 "or on an outside count where the outside weight is 0", fixed = TRUE)
  }
  for (output in list("flow", "utilization", "utilization_by_group", NULL)) {
    expect_identical(.poisson_floor_message(floor_info, output),
      paste0("2 cells with positive observed counts are modelled below the Poisson floor ",
             "(eps = 1e-250, zero included); the loss is capped there, so the fit is not the ",
             "Poisson maximum likelihood estimate. Look for observed counts on absent edges ",
             "or at facilities with zero supply, or use a kernel with a heavier tail."))
  }
  # the same text without an output, as before
  expect_identical(.poisson_floor_message(floor_info), .poisson_floor_message(floor_info, "flow"))
  # check_allocation() prints it with the output of the fit
  case <- .oc_case()
  model <- clm_allocation(case$inputs, fit_v0 = TRUE)
  fit <- .oc_fit(model, .oc_target(case), "flow_outside")
  check <- check_allocation(model, fit)
  expect_identical(check$output, "flow_outside")
  check$flags[["positive_counts_below_poisson_floor"]] <- TRUE
  check$poisson_floor <- c(eps = 1e-250, positive_below = 2)
  printed <- paste(capture.output(print(check)), collapse = " ")
  expect_match(printed, "or on an outside count where the outside weight is 0", fixed = TRUE)
})

test_that("outside counts are fitted with the built-in Poisson loss only", {
  case <- .oc_case()
  model <- clm_allocation(case$inputs, fit_v0 = TRUE)
  g <- .oc_group_case()
  gmodel <- clm_allocation(g$inputs, fit_v0 = TRUE, by_group = c("sigma", "v0"))
  gtheta <- g$truth[gmodel$theta$names]
  msg <- "outside counts are fitted with loss = \"poisson\""
  custom <- function(predicted, observed, ...) sum((predicted - observed)^2)
  custom_grad <- function(predicted, observed, sensitivity, ...) {
    as.vector(crossprod(sensitivity, 2 * (predicted - observed)))
  }
  setups <- list(
    flow_outside = list(model = model, target = .oc_target(case), starts = .oc_starts,
                        lower = .oc_lower, upper = .oc_upper),
    utilization_outside = list(model = model,
      target = c(colSums(case$flows), "(outside)" = sum(case$outside)), starts = .oc_starts,
      lower = .oc_lower, upper = .oc_upper),
    utilization_outside_by_group = list(model = gmodel, target = g$exact,
      starts = rbind(a = gtheta * 1.3), lower = gtheta / 20, upper = gtheta * 20))
  for (output in names(setups)) {
    s <- setups[[output]]
    # given starts and no loss: the default weighted SSE is refused
    expect_error(fit_allocation(s$model, s$target, s$starts, s$lower, s$upper, output = output),
                 msg, fixed = TRUE)
    # a named weighted SSE, with and without starts
    expect_error(fit_allocation(s$model, s$target, s$starts, s$lower, s$upper, output = output,
                                loss = "weighted_sse"), msg, fixed = TRUE)
    expect_error(fit_allocation(s$model, s$target, output = output, loss = "weighted_sse"),
                 msg, fixed = TRUE)
    # a custom loss function, with and without its gradient and starts
    expect_error(fit_allocation(s$model, s$target, s$starts, s$lower, s$upper, output = output,
                                loss = custom), msg, fixed = TRUE)
    expect_error(fit_allocation(s$model, s$target, s$starts, s$lower, s$upper, output = output,
                                loss = custom, loss_grad = custom_grad, gradient = TRUE),
                 msg, fixed = TRUE)
    expect_error(fit_allocation(s$model, s$target, output = output, loss = custom),
                 msg, fixed = TRUE)
    # the built-in Poisson loss is fitted, also named by a partial match
    fit <- fit_allocation(s$model, s$target, s$starts, s$lower, s$upper, output = output,
                          loss = "pois", gradient = TRUE, control = .oc_control)
    expect_s3_class(fit, "ae_multistart_fit")
    expect_false(is.null(fit$best))
  }
  # the other outputs keep the default weighted SSE and custom losses
  expect_s3_class(fit_allocation(model, case$flows, .oc_starts, .oc_lower, .oc_upper,
                                 output = "flow"), "ae_multistart_fit")
  expect_s3_class(fit_allocation(model, case$flows, .oc_starts, .oc_lower, .oc_upper,
                                 output = "flow", loss = custom), "ae_multistart_fit")
  # without starts and without a named loss, the search asks for the loss first
  expect_error(fit_allocation(model, .oc_target(case), output = "flow_outside"),
               "choose the loss when starts are not given", fixed = TRUE)
})

# Cornwall -------------------------------------------------------------------

# Mainland Cornwall with registered demand: every registered resident is registered
# somewhere, so each origin's outside count is its registered demand less its
# registrations at the mainland practices.
.oc_cornwall <- function() {
  alloc_memo("outside_cornwall", {
    areas <- sf::st_drop_geometry(cornwall_lsoa[!cornwall_lsoa$scilly, ])
    gp <- sf::st_drop_geometry(cornwall_gp[!cornwall_gp$scilly, ])
    travel <- cornwall_travel[areas$lsoa, gp$practice]
    demand <- stats::setNames(areas$registered, areas$lsoa)
    supply <- stats::setNames(gp$gp_fte_extg, gp$practice)
    keep <- cornwall_flows$lsoa %in% areas$lsoa & cornwall_flows$practice %in% gp$practice
    flows <- matrix(0, nrow(travel), ncol(travel), dimnames = dimnames(travel))
    cf <- cornwall_flows[keep, ]
    flows[cbind(match(cf$lsoa, rownames(flows)), match(cf$practice, colnames(flows)))] <-
      cf$patients
    outside <- demand - rowSums(flows)
    inputs <- prepare_allocation(demand, supply, travel)
    model <- clm_allocation(inputs, family = "gaussian", fit_v0 = TRUE)
    fit <- fit_allocation(model, cbind(flows, "(outside)" = outside), output = "flow_outside",
                          loss = "poisson")
    list(demand = demand, supply = supply, travel = travel, flows = flows,
         outside = outside, inputs = inputs, model = model, fit = fit)
  })
}

test_that("Cornwall flows with registered demand give the multinomial estimates", {
  skip_on_cran()
  cw <- .oc_cornwall()
  expect_true(all(cw$outside >= 0))
  expect_equal(sum(cw$outside), 5497)
  # the Poisson regression with origin effects and an outside alternative gives
  # sigma 4.6194 and v0 0.04864
  expect_equal(unname(coef(cw$fit)), c(4.6194, 0.04864), tolerance = 5e-4)
  # the modelled total outside count equals the observed one
  modelled <- sum(fitted(cw$fit)[, "(outside)"])
  expect_equal(modelled, sum(cw$outside), tolerance = 1e-6)
  expect_equal(modelled / sum(cw$demand), 0.009094, tolerance = 1e-3)
  # the inside fit that leaves the outside counts out is another estimate
  inside <- fit_allocation(cw$model, cw$flows, output = "flow", loss = "poisson")
  expect_equal(unname(coef(inside)), c(4.591, 0.02733), tolerance = 1e-3)
})

test_that("Cornwall grouped totals by group recover the parameters that made them", {
  skip_on_cran()
  cw <- .oc_cornwall()
  local_test_seed(2)
  share <- stats::runif(length(cw$demand), 0.3, 0.7)
  groups <- list(young = round(cw$demand * share), old = cw$demand - round(cw$demand * share))
  inputs <- prepare_allocation(groups, cw$supply, cw$travel)
  model <- clm_allocation(inputs, family = "gaussian", fit_v0 = TRUE, by_group = c("sigma", "v0"))
  truth <- c(sigma.young = 6, sigma.old = 3, v0.young = 0.05, v0.old = 0.01)[model$theta$names]
  e <- evaluate_allocation(model, truth, requested_outputs = "utilization_outside_by_group")
  observed <- round(e$outputs$utilization_outside_by_group)
  observed["(outside)", ] <- colSums(inputs$D_groups) - colSums(observed[-nrow(observed), ])
  fit <- fit_allocation(model, observed[c(55L, seq_len(54L)), c("old", "young")],
    output = "utilization_outside_by_group", loss = "poisson")
  expect_equal(coef(fit)[names(truth)], truth, tolerance = 1e-3)
  modelled <- evaluate_allocation(model, coef(fit),
    requested_outputs = "utilization_outside_by_group")$outputs$utilization_outside_by_group
  expect_equal(modelled["(outside)", ], observed["(outside)", colnames(modelled)],
               tolerance = 1e-3)
})
