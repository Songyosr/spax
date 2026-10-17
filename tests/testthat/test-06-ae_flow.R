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

  starts <- rbind(a = c(sigma = 1, v0 = 1), b = c(sigma = 3, v0 = 5))
  fits <- lapply(c(analytic = TRUE, numerical = FALSE), function(gradient) {
    fit_allocation(model, observed, starts,
      lower = c(sigma = 0.3, v0 = 0.01), upper = c(sigma = 10, v0 = 50),
      output = "flow", loss = "poisson", gradient = gradient,
      control = list(maxit = 200, factr = 10, pgtol = 1e-10))
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
