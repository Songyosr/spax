.flow_fixture <- function(family = "gaussian", disconnected = FALSE,
                          v0 = 4, fit_v0 = TRUE, fit_beta = FALSE,
                          allocation = "clm") {
  d <- terra::rast(nrows = 1, ncols = 6)
  terra::values(d) <- c(2.5, 0, 3.7, NA, 5.2, 1.3)
  distance <- c(d, d, d)
  names(distance) <- c("b", "a", "c")
  travel <- cbind(c(1, 2, 4, 2, 5, 3), c(4, 2, 1, 2, 3, 2),
                  c(2, 3, 5, 2, 1, 4))
  if (disconnected) { travel[6, ] <- Inf; travel[, 3] <- Inf }
  terra::values(distance) <- travel
  p <- .huff_problem(d, c(b = 3, a = 2, c = 2.5), distance,
    family = family, v0 = v0, fit_v0 = fit_v0, fit_beta = fit_beta,
    allocation = allocation)
  theta <- c(sigma = 2, if (fit_v0) c(v0 = v0), if (fit_beta) c(beta = 1.2))
  list(p = p, theta = theta)
}

# Independent scalar equations: no provider kernel, normalization or contraction.
.flow_oracle <- function(p, theta) {
  s <- p$substrate; spec <- p$metadata$spec
  d <- s$distance_active
  k <- switch(spec$family,
    gaussian = exp(-d^2 / (2 * theta[["sigma"]]^2)),
    exponential = exp(-theta[["sigma"]] * d), power = d^(-theta[["sigma"]]))
  beta <- if (spec$fit_beta) theta[["beta"]] else spec$beta
  v0 <- if (spec$fit_v0) theta[["v0"]] else spec$v0
  score <- sweep(k, 2, (spec$kappa * as.numeric(s$S))^beta, "*")
  den <- rowSums(score) + v0
  pr <- score * ifelse(den > 0, 1 / den, 0)
  list(probability = pr, flow = pr * as.numeric(s$D_active))
}

test_that("CLM expected flow obeys independent count algebra for every family", {
  for (family in c("gaussian", "exponential", "power")) {
    for (outside in c(0, 4)) {
      f <- .flow_fixture(family, disconnected = TRUE, v0 = outside,
                         fit_v0 = outside > 0, fit_beta = TRUE)
      p <- f$p; th <- f$theta
      out <- .solve_problem(p, th, requested_outputs = "flow")$outputs
      ref <- .flow_oracle(p, th)
      expect_equal(out$allocation, ref$probability, tolerance = 1e-12)
      expect_equal(out$flow, ref$flow, tolerance = 1e-12)
      expect_equal(as.numeric(colSums(out$flow)), out$utilization, tolerance = 1e-12)
      expect_equal(as.numeric(rowSums(out$flow)), p$substrate$D_active * out$access,
                   tolerance = 1e-12)
      expect_equal(unname(out$flow[4, ]), c(0, 0, 0))
      expect_equal(unname(out$flow[, 3]), rep(0, 4))
      # With v0=0 the disconnected row retains legacy zero mass, not a
      # normalized conditional distribution or an inferred outside category.
      expect_equal(out$access[4] + out$outside_share[4], as.numeric(outside > 0))
      expect_identical(p$metadata$spec$output_axes[["flow"]], "origin_facility")
      expect_identical(p$metadata$spec$output_meanings[["flow"]], "expected_count")
      sens <- .problem_output_sensitivity(p, th, p$state$init, "flow")
      fd <- vapply(seq_along(th), function(k) {
        hi <- lo <- th; h <- 1e-4
        hi[k] <- hi[k] + h; lo[k] <- lo[k] - h
        as.numeric(.flow_oracle(p, hi)$flow - .flow_oracle(p, lo)$flow) / (2 * h)
      }, numeric(length(out$flow)))
      expect_equal(unname(sens), unname(fd), tolerance = 1e-5)
      expect_equal(.problem_outputs(p, p$state$init, th, "flow")$flow, out$flow)
      expect_equal(.problem_outputs_at(p, th, p$state$init, "flow")$flow, out$flow)
    }
  }
})

.flow_fit <- function(f, observed, ...) {
  .fit_problem_nfxp(f$p, observed, init = c(sigma = 2.2, v0 = 5),
    lower = c(sigma = .5, v0 = .1), upper = c(sigma = 5, v0 = 20),
    output = "flow", loss = .poisson_loss, loss_grad = .poisson_gradient,
    loss_args = list(), gradient = TRUE, control = list(maxit = 75), ...)
}

test_that("flow fitting preserves matrix identities masks and fractional counts", {
  f <- .flow_fixture()
  y <- .flow_oracle(f$p, f$theta)$flow
  dimnames(y) <- list(f$p$substrate$origin_ids, f$p$substrate$facility_ids)
  y[1, 2] <- y[3, 1] <- NA_real_
  perm <- y[c(4, 1, 3, 2), c(2, 3, 1)]
  a <- .flow_fit(f, y); b <- .flow_fit(f, perm)
  expect_identical(a$convergence, 0L)
  expect_equal(a$theta, b$theta, tolerance = 1e-12)
  expect_equal(a$theta, f$theta, tolerance = 1e-3)
  expect_identical(a$observed, b$observed)
  expect_equal(a$predicted, b$predicted, tolerance = 1e-12)
  expect_identical(a$target_mask, as.numeric(!is.na(y)) == 1)
  expect_equal(a$loss, .poisson_loss(as.numeric(a$predicted)[a$target_mask],
                                   as.numeric(y)[a$target_mask]))
  expect_identical(a$output_meanings[["allocation"]], "inside_choice_probability")
  expect_identical(a$output_axes[["flow"]], "origin_facility")
  args <- list(problem = f$p, lower = c(sigma = .5, v0 = .1),
    upper = c(sigma = 5, v0 = 20), output = "flow", loss = .poisson_loss,
    loss_grad = .poisson_gradient, loss_args = list(), gradient = TRUE,
    control = list(maxit = 75))
  starts <- rbind(one = c(sigma = 2.2, v0 = 5), two = c(sigma = 2.5, v0 = 7))
  m1 <- do.call(.fit_problem_multistart, c(args, list(observed = y, starts = starts)))
  m2 <- do.call(.fit_problem_multistart, c(args, list(observed = perm, starts = starts)))
  expect_true(all(m1$table$eligible))
  expect_identical(m1$best_start, m2$best_start)
  expect_equal(m1$best$predicted, m2$best$predicted, tolerance = 1e-12)
  expect_identical(m2$best$target_mask, a$target_mask)
  profile <- list(parameter = "sigma", values = c(1.8, 2), init = starts[1, ])
  p1 <- do.call(.profile_problem, c(args, profile, list(observed = y)))
  p2 <- do.call(.profile_problem, c(args, profile, list(observed = perm)))
  expect_true(all(p1$table$eligible))
  expect_equal(p1$table$loss, p2$table$loss, tolerance = 1e-12)
  expect_equal(p1$best$predicted, p2$best$predicted, tolerance = 1e-12)
  expect_identical(p2$best$target_mask, a$target_mask)
  path <- tempfile(fileext = ".rds"); on.exit(unlink(path))
  saveRDS(a, path); restored <- readRDS(path)
  testthat::local_mocked_bindings(.clm_flow = function(...) stop("unexpected materialization"),
    .bind_theta = function(...) stop("unexpected bind"))
  expect_identical(.fit_output(restored, "flow"), a$outputs$flow)
  expect_identical(restored$output_meanings, a$output_meanings)
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
  expect_equal(unname(out$outputs$flow), matrix(18, 1, 1), tolerance = 1e-8)
  expect_equal(unname(.problem_output_sensitivity(p, c(a = 2), 4, "flow")),
                matrix(17, 1, 1), tolerance = 1e-6)
  expect_identical(.problem_outputs(p, 4, c(a = 2)), p$bind(c(a = 2))$outputs(4))
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
