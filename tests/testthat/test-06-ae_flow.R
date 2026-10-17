# Static CLM on the b, a, c identity inputs (1 x 6). `disconnected` cuts the
# last retained origin and facility c off.
.flow_fixture <- function(family = "gaussian", disconnected = FALSE,
                          v0 = 6, fit_v0 = TRUE, fit_beta = FALSE,
                          allocation = "clm") {
  f <- ae_identity_inputs(6)
  if (disconnected) {
    travel <- terra::values(f$distance)
    travel[6, ] <- Inf
    travel[, 3] <- Inf
    terra::values(f$distance) <- travel
  }
  p <- .huff_problem(f$demand, f$supply, f$distance,
    family = family, v0 = v0, fit_v0 = fit_v0, fit_beta = fit_beta,
    allocation = allocation)
  theta <- c(sigma = 2, if (fit_v0) c(v0 = v0), if (fit_beta) c(beta = 1.2))
  list(p = p, theta = theta)
}

# Independent CLM algebra (helper-clm.R) on the problem's retained support.
.flow_oracle <- function(p, theta) {
  s <- p$substrate
  spec <- p$metadata$spec
  clm_oracle(s$distance_active, s$D_active, as.numeric(s$S), theta[["sigma"]],
             v0 = if (spec$fit_v0) theta[["v0"]] else spec$v0,
             family = spec$family,
             beta = if (spec$fit_beta) theta[["beta"]] else spec$beta,
             kappa = spec$kappa)
}

# Analytic d flow / d theta, written from scratch. With score s_ij = K_ij a_j
# and flow_ij = D_i s_ij / (sum_k s_ik + v0), a parameter that moves log s_ij
# by G_ij moves flow_ij by D_i P_ij (G_ij - sum_k P_ik G_ik); v0 enters only
# the denominator.
.flow_derivative <- function(p, theta) {
  s <- p$substrate
  spec <- p$metadata$spec
  ref <- .flow_oracle(p, theta)
  P <- ref$probability
  D <- s$D_active
  d <- s$distance_active
  shift <- function(G) D * P * (G - rowSums(P * G))
  G <- switch(spec$family, gaussian = d^2 / theta[["sigma"]]^3,
              exponential = -d, power = -log(d))
  G[P == 0] <- 0 # unreachable edges carry no probability
  cols <- list(sigma = shift(G))
  if (spec$fit_v0) cols$v0 <- -D * P / ref$denom
  if (spec$fit_beta) {
    log_a <- log(spec$kappa * as.numeric(s$S))
    cols$beta <- shift(matrix(log_a, nrow(P), ncol(P), byrow = TRUE))
  }
  vapply(cols[names(theta)], as.numeric, numeric(length(P)))
}

test_that("CLM expected flow obeys independent count algebra for every family", {
  for (family in c("gaussian", "exponential", "power")) {
    for (outside in c(0, 4)) {
      f <- .flow_fixture(family, disconnected = TRUE, v0 = outside,
                         fit_v0 = outside > 0, fit_beta = TRUE)
      p <- f$p
      th <- f$theta
      out <- .solve_problem(p, th, requested_outputs = "flow")$outputs
      ref <- .flow_oracle(p, th)
      expect_equal(out$allocation, ref$probability, tolerance = 1e-12)
      expect_equal(out$flow, ref$flow, tolerance = 1e-12)
      expect_equal(as.numeric(colSums(out$flow)), out$utilization, tolerance = 1e-12)
      expect_equal(as.numeric(rowSums(out$flow)), p$substrate$D_active * out$access,
                   tolerance = 1e-12)
      expect_equal(unname(c(out$flow[4, ], out$flow[, 3])), rep(0, 7))
      # With v0=0 the disconnected row retains legacy zero mass, not a
      # normalized conditional distribution or an inferred outside category.
      expect_equal(out$access[4] + out$outside_share[4], as.numeric(outside > 0))
      sens <- .problem_output_sensitivity(p, th, p$state$init, "flow")
      expect_equal(unname(sens), unname(.flow_derivative(p, th)), tolerance = 1e-5)
    }
  }
  # Declarations and the requested-output accessor do not depend on family.
  expect_identical(p$metadata$spec$output_axes[["flow"]], "origin_facility")
  expect_identical(p$metadata$spec$output_meanings[["flow"]], "expected_count")
  expect_equal(.problem_outputs_at(p, th, p$state$init, "flow")$flow, out$flow)
})

test_that("flow fits recover parameters from masked fractional counts", {
  f <- .flow_fixture(fit_beta = TRUE)
  y <- .flow_oracle(f$p, f$theta)$flow
  dimnames(y) <- list(f$p$substrate$origin_ids, f$p$substrate$facility_ids)
  y[1, 2] <- y[3, 1] <- NA_real_
  fit <- .fit_problem_nfxp(f$p, y, init = c(sigma = 2.2, v0 = 5, beta = 1),
    lower = c(sigma = .5, v0 = .1, beta = .2),
    upper = c(sigma = 5, v0 = 20, beta = 3),
    output = "flow", loss = .poisson_loss, loss_grad = .poisson_gradient,
    loss_args = list(), gradient = TRUE, control = list(maxit = 75))

  expect_identical(fit$convergence, 0L)
  expect_equal(fit$theta, f$theta, tolerance = 1e-3)
  expect_identical(fit$target_mask, as.numeric(!is.na(y)) == 1)
  expect_equal(fit$loss, .poisson_loss(as.numeric(fit$predicted)[fit$target_mask],
                                       as.numeric(y)[fit$target_mask]))
  expect_identical(fit$output_meanings[["allocation"]], "inside_choice_probability")
  expect_identical(fit$output_axes[["flow"]], "origin_facility")
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(fit, path)
  restored <- readRDS(path)
  testthat::local_mocked_bindings(
    .clm_flow = function(...) stop("unexpected materialization"),
    .bind_theta = function(...) stop("unexpected bind"))
  expect_identical(.fit_output(restored, "flow"), fit$outputs$flow)
  expect_identical(restored$output_meanings, fit$output_meanings)
})

test_that("Poisson flow gradients match the floored loss when cells fall below eps", {
  # Ten origins on a line and three practices; a few long trips are observed
  # where the Gaussian kernel puts expected flow just below the 1e-9 floor.
  x <- seq(0, 18, by = 2)
  site <- c(f1 = 1, f2 = 9, f3 = 17)
  travel <- abs(outer(x, site, "-"))
  dimnames(travel) <- list(paste0("o", seq_along(x)), names(site))
  demand <- setNames(rep(c(60, 90), length.out = length(x)), rownames(travel))
  supply <- c(f1 = 20, f2 = 30, f3 = 25)
  observed <- round(clm_oracle(travel, demand, supply, sigma = 1.5, v0 = 2)$flow)
  observed["o7", "f1"] <- 1
  observed["o4", "f3"] <- 2
  observed["o1", "f3"] <- 1
  model <- clm_allocation(prepare_allocation(demand, supply, travel), fit_v0 = TRUE)
  y <- as.numeric(observed)
  flow_at <- function(theta) {
    .solve_problem(model, theta, keep_history = FALSE, warn = FALSE,
                   diagnostics = FALSE, requested_outputs = "flow")
  }
  loss_at <- function(theta) .poisson_loss(as.numeric(flow_at(theta)$outputs$flow), y)

  theta <- c(sigma = 1.5, v0 = 2)
  solved <- flow_at(theta)
  mu <- as.numeric(solved$outputs$flow)
  expect_true(any(mu > 0 & mu < 1e-9 & y > 0))
  sensitivity <- .problem_output_sensitivity(model, theta, solved$x_star, output = "flow")
  expect_equal(.poisson_gradient(mu, y, sensitivity),
               as.vector(fd_jacobian(loss_at, theta)), tolerance = 1e-6)

  # Sanity check, not a regression check: with only three cells on the floor
  # the old gradient also fitted this case; the Cornwall guide fit is the case
  # where it split the starts (DEC-029). The fit names the old floor 1e-9, since
  # at the default floor nothing here is floored.
  starts <- rbind(a = c(sigma = 1, v0 = 1), b = c(sigma = 3, v0 = 5))
  fits <- lapply(c(analytic = TRUE, numerical = FALSE), function(gradient) {
    expect_warning(
      fit <- fit_allocation(model, observed, starts,
        lower = c(sigma = 0.3, v0 = 0.01), upper = c(sigma = 10, v0 = 50),
        output = "flow", loss = "poisson", loss_args = list(eps = 1e-9),
        gradient = gradient, control = list(maxit = 200, factr = 10, pgtol = 1e-10)),
      "below the Poisson floor")
    fit
  })
  for (fit in fits) {
    expect_identical(fit$table$optim_convergence, c(0L, 0L))
    expect_false(fit$loss_disagreement)
  }
  expect_equal(fits$analytic$theta, fits$numerical$theta, tolerance = 1e-5)
})

test_that("default solves and fits never materialize optional flow", {
  f <- .flow_fixture()
  observed <- as.numeric(colSums(.flow_oracle(f$p, f$theta)$flow))
  testthat::local_mocked_bindings(.clm_flow = function(...) stop("unexpected flow"))
  solved <- .solve_problem(f$p, f$theta)
  expect_false("flow" %in% names(solved$outputs))
  fit <- .fit_problem_nfxp(f$p, observed, c(sigma = 2.2, v0 = 5),
    c(sigma = .5, v0 = .1), c(sigma = 5, v0 = 20), gradient = TRUE)
  expect_identical(fit$convergence, 0L)
  expect_false("flow" %in% names(fit$outputs))
  expect_error(.fit_output(fit, "flow"), "does not contain")
  expect_identical(fit$loss_fn, .weighted_sse_loss)
  expect_identical(fit$loss_args, list(eta = 1))
})

test_that("explicit state independence bypasses state derivative construction", {
  f <- .flow_fixture()
  original <- f$p$bind
  f$p$bind <- function(theta) {
    step <- original(theta)
    step$jac_state <- step$jac_param <- step$jac_output_state <- function(...) {
      stop("unexpected state derivative")
    }
    step
  }
  expect_no_error(.problem_output_sensitivity(f$p, f$theta, f$p$state$init, "flow"))
  f$p$bind <- function(theta) {
    step <- original(theta); step$state_independent_outputs <- NULL
    step$jac_output_state <- function(...) stop("legacy state derivative")
    step
  }
  expect_error(.problem_output_sensitivity(f$p, f$theta, f$p$state$init, "flow"),
                "legacy state derivative")
})

test_that("legacy custom flow retains the complete implicit derivative", {
  # The near-miss entry names below would be returned by `step$...` partial
  # matching; the protocol must read only exact `outputs_requested` and
  # `state_independent_outputs` entries.
  p <- .new_problem("custom", substrate = list(facility_ids = "f"),
    state = list(init = 1, lower = 0, upper = Inf, name = "x"),
    theta = list(names = "a", lower = 0, upper = Inf),
    bind = function(theta) list(
      map = function(x) .5 * x + theta[[1]],
      outputs = function(x) list(target = .5 * x + theta[[1]], utilization = x,
                                 flow = matrix(x^2 + theta[[1]], 1, 1)),
      outputs_requested_cached = function(...) stop("unexpected partial callback"),
      state_independent_outputs_note = "flow"))
  out <- .solve_problem(p, c(a = 2), requested_outputs = "flow", tol = 1e-10)
  # x* = 4, so flow = x*^2 + a = 18 and d flow / d a = 2 x* dx*/da + 1 = 17.
  expect_equal(unname(out$outputs$flow), matrix(18, 1, 1), tolerance = 1e-8)
  expect_equal(unname(.problem_output_sensitivity(p, c(a = 2), 4, "flow")),
                matrix(17, 1, 1), tolerance = 1e-6)
  expect_identical(.problem_outputs_at(p, c(a = 2), 4, "flow"),
                   p$bind(c(a = 2))$outputs(4))
  step <- .bind_theta(p, c(a = 2))
  expect_null(.problem_output_from_step(step, 4, "absent", required = FALSE))
  expect_error(.problem_output_from_step(step, 4, "absent"), "requested output")
  step$outputs_requested <- function(x, requested_outputs) stop("provider failure")
  expect_error(.problem_output_from_step(step, 4, "absent", required = FALSE),
                "provider failure")
})

test_that("invalid capabilities and unavailable requests fail explicitly", {
  f <- .flow_fixture()
  for (request in list(character(), NA_character_, c("flow", "flow"), 1, "")) {
    expect_error(.solve_problem(f$p, f$theta, requested_outputs = request), "requested_outputs")
  }
  original <- f$p$bind
  for (bad in list(1, c("flow", "flow"), "missing", NA_character_)) {
    f$p$bind <- local({ value <- bad; function(theta) {
      step <- original(theta); step$state_independent_outputs <- value; step
    } })
    expect_error(.bind_theta(f$p, f$theta), "state_independent_outputs")
  }
  f$p$bind <- function(theta) { step <- original(theta); step$outputs_requested <- 1; step }
  expect_error(.bind_theta(f$p, f$theta), "outputs_requested")
  f$p$bind <- function(theta) { step <- original(theta); step$outputs <- NULL; step }
  expect_error(.bind_theta(f$p, f$theta), "must provide an `outputs`")
  legacy <- .flow_fixture(allocation = "huff_decay")
  expect_false("flow" %in% names(legacy$p$metadata$spec$output_axes))
  expect_null(legacy$p$metadata$spec$output_meanings)
  expect_error(.solve_problem(legacy$p, legacy$theta, requested_outputs = "flow"),
                "do not include requested output")
  expect_no_error(.solve_problem(legacy$p, legacy$theta))
})

# Poisson floor (SPAX-054) ------------------------------------------------------
# Eleven origins on a line and three facilities; two residents are registered at
# the far facility, where the Gaussian kernel puts almost no expected flow.
far_flow_case <- function(absent_edge = FALSE) {
  x <- seq(0, 50, by = 5)
  site <- c(a = 0, b = 25, c = 50)
  travel <- abs(outer(x, site, "-")) + 1
  dimnames(travel) <- list(paste0("o", seq_along(x)), names(site))
  if (absent_edge) travel["o1", "c"] <- Inf
  demand <- setNames(rep(1000, length(x)), rownames(travel))
  supply <- c(a = 4, b = 6, c = 5)
  log_mu <- function(sigma, v0) {    # independent log-scale CLM flows
    la <- sweep(-(travel^2) / (2 * sigma^2), 2, log(supply), "+")
    m <- pmax(apply(la, 1, max), log(v0))
    log_den <- m + log(rowSums(exp(la - m)) + exp(log(v0) - m))
    sweep(la, 1, log_den, "-") + log(demand)
  }
  observed <- round(exp(log_mu(4, 0.2)))
  observed["o1", "c"] <- 1
  observed["o11", "a"] <- 1
  nll <- function(sigma, v0) {
    lm <- log_mu(sigma, v0)
    sum(exp(lm)) - sum((observed * lm)[observed > 0])
  }
  inputs <- prepare_allocation(demand, supply, travel)
  list(model = clm_allocation(inputs, family = "gaussian", fit_v0 = TRUE),
       observed = observed, nll = nll, log_mu = log_mu,
       starts = rbind(s1 = c(sigma = 3, v0 = 0.1), s2 = c(sigma = 8, v0 = 1)),
       lower = c(sigma = 0.5, v0 = 1e-4), upper = c(sigma = 30, v0 = 10))
}
far_flow_fit <- function(f, ...) fit_allocation(f$model, f$observed, f$starts, f$lower,
  f$upper, output = "flow", loss = "poisson", gradient = TRUE,
  control = list(maxit = 500, factr = 10, pgtol = 1e-10), ...)

test_that("Poisson flow fits are maximum likelihood below the old floor", {
  f <- far_flow_case()
  # Reference optimum of the exact log-scale likelihood.
  ref <- stats::optim(log(c(4, 0.2)), function(p) f$nll(exp(p[1]), exp(p[2])),
                      method = "BFGS", control = list(reltol = 1e-15, maxit = 1000))
  ref <- stats::nlminb(ref$par, function(p) f$nll(exp(p[1]), exp(p[2])),
                       control = list(rel.tol = 1e-15, x.tol = 1e-12,
                                      eval.max = 2000, iter.max = 1000))
  # The two far cells are modelled near 1e-27 there: far below the old floor.
  far <- f$log_mu(4.328, 0.3346)[cbind(c("o1", "o11"), c("c", "a"))] / log(10)
  expect_true(all(far < -27))

  fit <- expect_no_warning(far_flow_fit(f))
  expect_identical(fit$best$loss_args, list(eps = 1e-250))
  expect_equal(coef(fit), setNames(exp(ref$par), c("sigma", "v0")), tolerance = 1e-5)
  expect_equal(fit$best$loss, f$nll(coef(fit)[["sigma"]], coef(fit)[["v0"]]),
               tolerance = 1e-12)
  expect_identical(.poisson_floor(fit$best), c(eps = 1e-250, positive_below = 0))
  # Without the analytic gradient the default fit lands at the same optimum.
  numerical <- fit_allocation(f$model, f$observed, f$starts, f$lower, f$upper,
    output = "flow", loss = "poisson", gradient = FALSE,
    control = list(maxit = 500, factr = 10, pgtol = 1e-10))
  expect_equal(coef(numerical), coef(fit), tolerance = 1e-4)

  # The old floor caps both far cells and is reported.
  expect_warning(old <- far_flow_fit(f, loss_args = list(eps = 1e-9)),
    "2 cells with positive observed counts are modelled below the Poisson floor \\(eps = 1e-09")
  expect_equal(coef(old)[["sigma"]], 3.99759, tolerance = 1e-4)
  expect_gt(exp(ref$par[1]) - coef(old)[["sigma"]], 0.3)
  expect_identical(.poisson_floor(old$best), c(eps = 1e-9, positive_below = 2))

  # One capped cell is reported in the singular. At the default floor nothing is
  # floored, so this names the old floor.
  one <- f
  one$observed["o11", "a"] <- 0
  expect_warning(far_flow_fit(one, loss_args = list(eps = 1e-9)),
                 "1 cell with positive observed counts is modelled below")

  # Other losses have no Poisson floor.
  sse <- expect_no_warning(fit_allocation(f$model, f$observed, f$starts, f$lower, f$upper,
    output = "flow", loss = "weighted_sse", gradient = TRUE,
    control = list(maxit = 500, factr = 10, pgtol = 1e-10)))
  expect_null(.poisson_floor(sse$best))

  # check_allocation() reports the capped fit and not the default one.
  checked_old <- check_allocation(f$model, old)
  expect_true(checked_old$flags[["positive_counts_below_poisson_floor"]])
  expect_identical(checked_old$poisson_floor, c(eps = 1e-9, positive_below = 2))
  expect_output(print(checked_old), "2 cells with positive observed counts are modelled below")
  checked_new <- check_allocation(f$model, fit)
  expect_false(checked_new$flags[["positive_counts_below_poisson_floor"]])
  expect_identical(checked_new$poisson_floor, c(eps = 1e-250, positive_below = 0))

  # A capped fit is not comparable with the default-floor fit: the floors differ
  # and the far cells lie below the larger one. A fit that stored no floor was
  # made with 1e-9, so it is comparable with the capped fit.
  models <- list(new = f$model, old = f$model)
  compared <- as.data.frame(compare_allocations(list(new = fit, old = old), models = models))
  expect_false(compared$comparable[compared$specification == "old"])
  legacy <- old
  legacy$best$loss_args <- list()
  models <- list(old = f$model, legacy = f$model)
  compared <- as.data.frame(compare_allocations(list(old = old, legacy = legacy),
                                                models = models))
  expect_true(all(compared$comparable))
  expect_equal(compared$loss_difference, c(0, 0))

  # Facility totals sit far above any floor, so differing floors do not matter.
  s <- alloc_small_case()
  fits <- lapply(list(old = list(eps = 1e-9), new = list()), function(args) {
    fit_allocation(s$model, s$observed, s$starts, s$lower, s$upper, loss = "poisson",
                   loss_args = args, gradient = TRUE, control = list(maxit = 100))
  })
  totals <- as.data.frame(compare_allocations(fits, models = list(old = s$model, new = s$model)))
  expect_true(all(totals$comparable))
  expect_equal(totals$loss_difference, c(0, 0), tolerance = 1e-8)

  # An observed count on an absent edge cannot be modelled at all.
  edge <- far_flow_case(absent_edge = TRUE)
  expect_warning(absent <- far_flow_fit(edge), "1 cell with positive observed counts")
  expect_equal(.poisson_floor(absent$best)[["positive_below"]], 1)
})
