# SPAX-029: static Huff / aggregate-CLM problem spec -------------------------

.mk_huff_data <- function() {
  demand <- terra::rast(nrows = 4, ncols = 4)
  terra::values(demand) <- c(
    10, 20, 30, 40,
    15, 25, 35, 45,
    12, 22, 32, 42,
    18, 28, 38, 48
  )

  d1 <- terra::rast(nrows = 4, ncols = 4)
  d2 <- terra::rast(nrows = 4, ncols = 4)
  d3 <- terra::rast(nrows = 4, ncols = 4)
  terra::values(d1) <- c(1, 2, 3, 4, 2, 3, 4, 5, 3, 4, 5, 6, 4, 5, 6, 7)
  terra::values(d2) <- c(7, 6, 5, 4, 6, 5, 4, 3, 5, 4, 3, 2, 4, 3, 2, 1)
  terra::values(d3) <- c(4, 3, 2, 3, 3, 2, 3, 4, 2, 3, 4, 5, 3, 4, 5, 6)
  distance <- c(d1, d2, d3)
  names(distance) <- c("facility1", "facility2", "facility3")

  supply <- c(facility1 = 30, facility2 = 45, facility3 = 20)
  list(demand = demand, distance = distance, supply = supply)
}

# Build the comparison plan the same way the HAAE oracle does, so we can check
# the static problem against a direct one-pass Huff evaluation.
.mk_huff_plan <- function(td, theta, family = "gaussian", kappa = 1) {
  kernel <- calc_decay(td$distance, method = family, sigma = theta, snap = TRUE)
  .haae_compact_plan(td$demand, td$supply, kernel, kappa = kappa)
}

test_that(".huff_problem solves in one pass with a zero state Jacobian", {
  td <- .mk_huff_data()
  theta <- c(sigma = 2)
  p <- .huff_problem(td$demand, td$supply, td$distance,
                     family = "gaussian", kappa = 1 / 3)

  expect_s3_class(p, "ae_problem")
  expect_s3_class(p, "huff_problem")

  fit <- .solve_problem(p, theta = theta, lambda = 1)
  expect_true(fit$converged)
  expect_identical(fit$iters, 1L)
  # state fixed point is the exogenous attractiveness
  expect_equal(unname(fit$x_star), p$state$init, tolerance = 1e-12)
  # dT/dstate is exactly zero => contractive, spectral radius 0
  expect_equal(fit$spectral_radius, 0)
  expect_true(fit$contraction)
})

test_that(".huff_problem map is state-independent (constant T(x) = a)", {
  td <- .mk_huff_data()
  p <- .huff_problem(td$demand, td$supply, td$distance,
                     family = "gaussian", kappa = 1 / 3)
  step <- .bind_theta(p, c(sigma = 2))

  a <- p$state$init
  x1 <- a * 0.3
  x2 <- a * 5
  expect_equal(step$map(x1), a, tolerance = 1e-12)
  expect_equal(step$map(x2), a, tolerance = 1e-12)
  # analytic Jacobians are zero, matching the constant map under FD
  expect_equal(step$jac_state(x1)$jac_state,
               matrix(0, length(a), length(a)))
  expect_equal(step$jac_state(x1)$jac_utilization,
               matrix(0, length(a), length(a)))
})

test_that("explicit huff_decay is the evaluate-once case of HAAE", {
  td <- .mk_huff_data()
  theta <- c(sigma = 2)
  # classic Huff (beta = 1): attractiveness proportional to supply
  p <- .huff_problem(td$demand, td$supply, td$distance,
                     family = "gaussian", kappa = 1, beta = 1,
                     allocation = "huff_decay")
  step <- .bind_theta(p, theta)

  plan <- .mk_huff_plan(td, theta, family = "gaussian", kappa = 1)
  a <- as.vector(plan$S)  # classic Huff a_j = supply (kappa scale cancels)

  # the static problem output equals one HAAE-kernel pass at the same a
  expect_equal(step$outputs(c(1, 1, 1))$utilization,
               .haae_state(a, plan = plan)$utilization,
               tolerance = 1e-10)
  expect_equal(step$outputs(c(1, 1, 1))$access,
               .haae_state(a, plan = plan)$access,
               tolerance = 1e-10)

  # kappa is a global scale on attractiveness => cancels in the choice share
  p2 <- .huff_problem(td$demand, td$supply, td$distance,
                      family = "gaussian", kappa = 1 / 3, beta = 1,
                      allocation = "huff_decay")
  expect_equal(.bind_theta(p2, theta)$outputs(0)$utilization,
               step$outputs(0)$utilization, tolerance = 1e-10)
})

test_that("explicit huff_decay attenuates demand and access stays in [0, 1]", {
  td <- .mk_huff_data()
  p <- .huff_problem(td$demand, td$supply, td$distance,
                     family = "gaussian", kappa = 1 / 3,
                     allocation = "huff_decay")
  out <- .bind_theta(p, c(sigma = 2))$outputs(0)

  total_demand <- sum(p$substrate$D_active)
  # Huff-then-attenuate (f^2 a / sum f a) loses mass to distance attenuation
  expect_lt(sum(out$utilization), total_demand)
  expect_true(all(out$access >= 0 & out$access <= 1 + 1e-9))
})

test_that("an outside option reduces utilization below the pure-Huff case", {
  td <- .mk_huff_data()
  theta <- c(sigma = 2)
  pure <- .huff_problem(td$demand, td$supply, td$distance,
                        family = "gaussian", kappa = 1 / 3, v0 = 0)
  outside <- .huff_problem(td$demand, td$supply, td$distance,
                           family = "gaussian", kappa = 1 / 3, v0 = 0.5)

  out_pure <- .bind_theta(pure, theta)$outputs(0)
  out_outside <- .bind_theta(outside, theta)$outputs(0)
  expect_lt(sum(out_outside$utilization), sum(out_pure$utilization))
  # outside_share is origin-side (one per active demand cell)
  n_active <- length(pure$substrate$D_active)
  expect_length(out_outside$outside_share, n_active)
  expect_true(all(out_outside$outside_share > 0))
  expect_equal(out_pure$outside_share, rep(0, n_active))
})

test_that(".fit_problem_nfxp recovers the decay parameter for static Huff", {
  td <- .mk_huff_data()
  sigma0 <- 2.5
  p <- .huff_problem(td$demand, td$supply, td$distance,
                     family = "gaussian", kappa = 1 / 3)
  observed <- .solve_problem(p, theta = c(sigma = sigma0))$outputs$utilization

  fit <- .fit_problem_nfxp(
    p, observed = observed,
    init = c(sigma = 6), lower = c(sigma = 0.2), upper = c(sigma = 30),
    output = "utilization", loss_args = list(eta = 1)
  )
  expect_s3_class(fit, "ae_problem_nfxp_fit")
  expect_equal(fit$model, "huff")
  expect_equal(unname(fit$theta_hat["sigma"]), sigma0, tolerance = 1e-2)
  expect_lt(fit$loss, 1e-6)
})

test_that("gradient-mode static Huff fit agrees with the black-box fit", {
  td <- .mk_huff_data()
  sigma0 <- 2.5
  p <- .huff_problem(td$demand, td$supply, td$distance,
                     family = "gaussian", kappa = 1 / 3)
  observed <- .solve_problem(p, theta = c(sigma = sigma0))$outputs$utilization

  args <- list(
    p, observed = observed,
    init = c(sigma = 6), lower = c(sigma = 0.2), upper = c(sigma = 30),
    output = "utilization", loss_args = list(eta = 1)
  )
  bb <- do.call(.fit_problem_nfxp, args)
  gd <- do.call(.fit_problem_nfxp, c(args, list(gradient = TRUE)))

  expect_true(gd$gradient)
  expect_equal(unname(gd$theta_hat["sigma"]), unname(bb$theta_hat["sigma"]),
               tolerance = 1e-2)
})

# SPAX-032: static CLM — joint (sigma, v0) fit by Poisson likelihood -----------

test_that(".huff_spec promotes v0 into theta when fit_v0 = TRUE", {
  s1 <- .huff_spec(fit_v0 = FALSE)
  s2 <- .huff_spec(fit_v0 = TRUE)
  expect_equal(s1$theta$names, "sigma")
  expect_equal(s2$theta$names, c("sigma", "v0"))
  expect_equal(s2$theta$lower, c(0, 0))
  expect_true(s2$fit_v0)
})

test_that("fit_v0 problem reads v0 from theta (not the spec constant)", {
  td <- .mk_huff_data()
  p <- .huff_problem(td$demand, td$supply, td$distance,
                     family = "gaussian", kappa = 1 / 3, fit_v0 = TRUE)
  expect_equal(p$theta$names, c("sigma", "v0"))
  # outputs at two different v0 differ; map stays state-independent + one-pass
  o_lo <- .bind_theta(p, c(sigma = 2, v0 = 0.01))$outputs(0)
  o_hi <- .bind_theta(p, c(sigma = 2, v0 = 5))$outputs(0)
  expect_lt(sum(o_hi$utilization), sum(o_lo$utilization))   # more outside -> less load
  fit <- .solve_problem(p, theta = c(sigma = 2, v0 = 1))
  expect_identical(fit$iters, 1L)
  expect_equal(fit$spectral_radius, 0)
})

test_that(".poisson_loss/gradient behave and match FD", {
  pr <- c(2, 5, 3); ob <- c(2, 4, 3)
  expect_equal(.poisson_loss(pr, ob), sum(pr - ob * log(pr)), tolerance = 1e-12)
  # gradient vs finite difference through a toy sensitivity
  sens <- matrix(c(1, 0.5, -0.2, 0.3, 1, 0.7), nrow = 3)
  g <- .poisson_gradient(pr, ob, sens)
  fd <- sapply(1:2, function(k) {
    h <- 1e-6
    (.poisson_loss(pr + h * sens[, k], ob) - .poisson_loss(pr - h * sens[, k], ob)) / (2 * h)
  })
  expect_equal(g, fd, tolerance = 1e-5)
})

test_that("joint (sigma, v0) Poisson fit recovers generating parameters", {
  td <- .mk_huff_data()
  p <- .huff_problem(td$demand, td$supply, td$distance,
                     family = "gaussian", kappa = 1 / 3, fit_v0 = TRUE)
  truth <- c(sigma = 2.5, v0 = 1.2)
  observed <- .solve_problem(p, theta = truth)$outputs$utilization

  fit <- .fit_problem_nfxp(
    p, observed = observed,
    init = c(sigma = 6, v0 = 0.3),
    lower = c(sigma = 0.2, v0 = 1e-3),
    upper = c(sigma = 30, v0 = 50),
    output = "utilization",
    loss = .poisson_loss, loss_grad = .poisson_gradient,
    loss_args = list(eps = 1e-9), control = list(maxit = 200)
  )
  expect_equal(fit$model, "huff")
  expect_equal(unname(fit$theta_hat["sigma"]), unname(truth["sigma"]), tolerance = 1e-2)
  expect_equal(unname(fit$theta_hat["v0"]), unname(truth["v0"]), tolerance = 1e-2)
  # Exact recovery of the generated mean also recovers its total.
  expect_equal(sum(fit$predicted), sum(observed), tolerance = 1e-4)
})

# SPAX-032 (fit_beta): free supply elasticity in the static CLM ----------------

test_that(".huff_theta_contract appends v0 then beta in order", {
  expect_equal(.huff_theta_contract()$names, "sigma")
  expect_equal(.huff_theta_contract(fit_v0 = TRUE)$names, c("sigma", "v0"))
  expect_equal(.huff_theta_contract(fit_beta = TRUE)$names, c("sigma", "beta"))
  full <- .huff_theta_contract(fit_v0 = TRUE, fit_beta = TRUE)
  expect_equal(full$names, c("sigma", "v0", "beta"))
  expect_equal(full$lower, c(0, 0, 0))
})

test_that("fit_beta problem reads beta from theta and stays one-pass at init", {
  td <- .mk_huff_data()
  p <- .huff_problem(td$demand, td$supply, td$distance,
                     family = "gaussian", kappa = 1 / 3,
                     fit_v0 = TRUE, fit_beta = TRUE)
  expect_equal(p$theta$names, c("sigma", "v0", "beta"))
  # beta reshapes attractiveness: larger beta concentrates load on big facilities
  u1 <- .bind_theta(p, c(sigma = 2, v0 = 1, beta = 0.5))$outputs(0)$utilization
  u2 <- .bind_theta(p, c(sigma = 2, v0 = 1, beta = 2.0))$outputs(0)$utilization
  big <- which.max(td$supply)
  expect_gt(u2[big] / sum(u2), u1[big] / sum(u1))   # share of the largest facility rises
  # at beta = spec default the state init equals the bound attractiveness (one pass)
  p1 <- .huff_problem(td$demand, td$supply, td$distance,
                      family = "gaussian", kappa = 1 / 3, beta = 1, fit_beta = TRUE)
  fit <- .solve_problem(p1, theta = c(sigma = 2, beta = 1))
  expect_identical(fit$iters, 1L)
  expect_equal(fit$spectral_radius, 0)
})

test_that("joint (sigma, v0, beta) Poisson fit recovers a non-unit beta", {
  td <- .mk_huff_data()
  p <- .huff_problem(td$demand, td$supply, td$distance,
                     family = "gaussian", kappa = 1 / 3,
                     fit_v0 = TRUE, fit_beta = TRUE)
  truth <- c(sigma = 2.5, v0 = 1.0, beta = 1.4)
  observed <- .solve_problem(p, theta = truth)$outputs$utilization

  fit <- .fit_problem_nfxp(
    p, observed = observed,
    init = c(sigma = 5, v0 = 0.5, beta = 0.8),
    lower = c(sigma = 0.2, v0 = 1e-3, beta = 0.2),
    upper = c(sigma = 30, v0 = 50, beta = 4),
    output = "utilization",
    loss = .poisson_loss, loss_grad = .poisson_gradient,
    loss_args = list(eps = 1e-9), control = list(maxit = 400)
  )
  expect_equal(unname(fit$theta_hat["beta"]), unname(truth["beta"]), tolerance = 2e-2)
  expect_equal(unname(fit$theta_hat["sigma"]), unname(truth["sigma"]), tolerance = 5e-2)
  expect_equal(sum(fit$predicted), sum(observed), tolerance = 1e-3)
})

test_that("fit_beta defaults preserve single-sigma behaviour", {
  expect_equal(.huff_spec()$theta$names, "sigma")
  expect_false(isTRUE(.huff_spec()$fit_beta))
})

# SPAX-033: explicit allocation forms, with an independent algebra oracle -----

.huff_choice_oracle <- function(distance, demand, supply, theta, family,
                                allocation = "clm", kappa = 1) {
  sigma <- theta[["sigma"]]
  K <- switch(family,
    gaussian = exp(-distance^2 / (2 * sigma^2)),
    exponential = exp(-sigma * distance),
    power = distance^(-sigma)
  )
  K[!is.finite(K)] <- 0
  a <- (kappa * supply)^theta[["beta"]]
  score <- sweep(K, 2, a, "*")
  denom <- rowSums(score) + theta[["v0"]]
  P <- score / denom
  P[!is.finite(P)] <- 0
  W <- if (allocation == "clm") P else P * K
  list(allocation = W, utilization = as.vector(crossprod(W, demand)),
       share = P, kernel = K, denom = denom)
}

test_that("default CLM matches choice algebra and accounts for outside mass", {
  td <- .mk_huff_data()
  expect_identical(.huff_spec()$allocation, "clm")
  expect_error(.huff_spec(allocation = "unknown"), "arg")
  theta <- c(sigma = 2.5, v0 = 4, beta = 1.3)
  p <- .huff_problem(td$demand, td$supply, td$distance,
                     fit_v0 = TRUE, fit_beta = TRUE)
  out <- .solve_problem(p, theta)$outputs
  oracle <- .huff_choice_oracle(p$substrate$distance_active,
                               p$substrate$D_active, td$supply, theta, "gaussian")
  expect_equal(out$allocation, oracle$allocation, tolerance = 1e-12)
  expect_equal(out$utilization, oracle$utilization, tolerance = 1e-12)
  expect_equal(out$allocation, out$huff_share, tolerance = 1e-12)
  expect_equal(out$access + out$outside_share, rep(1, 16), tolerance = 1e-12)
  G <- as.vector(oracle$kernel %*% (td$supply^theta[["beta"]]))
  expect_equal(out$access, G / (G + theta[["v0"]]), tolerance = 1e-12)

  no_outside <- .bind_theta(p, c(sigma = 2.5, v0 = 0, beta = 1.3))$outputs(0)
  expect_equal(sum(no_outside$utilization), sum(p$substrate$D_active),
               tolerance = 1e-12)
})

test_that("both allocation forms conserve supported supply and preserve support", {
  td <- .mk_huff_data()
  d <- terra::values(td$distance)
  d[1, ] <- NA_real_  # positive demand, no reachable facility
  d[, 3] <- NA_real_ # supplied facility with no contact
  terra::values(td$distance) <- d
  for (form in c("clm", "huff_decay")) {
    p <- .huff_problem(td$demand, td$supply, td$distance, v0 = 4,
                       allocation = form)
    out <- .solve_problem(p, c(sigma = 2.5))$outputs
    expect_length(p$substrate$D_active, 16)
    expect_equal(out$access[1], 0)
    expect_equal(out$outside_share[1], 1)
    expect_equal(out$utilization[3], 0)
    ratio <- ifelse(out$utilization > 0, td$supply / out$utilization, 0)
    intensity <- as.vector(out$allocation %*% ratio)
    expect_equal(sum(p$substrate$D_active * intensity),
                 sum(td$supply[out$utilization > 0]), tolerance = 1e-12)
    oracle <- .huff_choice_oracle(p$substrate$distance_active,
                                 p$substrate$D_active, td$supply,
                                 c(sigma = 2.5, v0 = 4, beta = 1), "gaussian", form)
    expect_equal(out$allocation, oracle$allocation, tolerance = 1e-12)
  }

  # Undefined no-opportunity/no-outside choice retains the documented zero guard.
  p <- .huff_problem(td$demand, td$supply, td$distance)
  out <- .solve_problem(p, c(sigma = 2.5))$outputs
  expect_equal(out$allocation[1, ], c(0, 0, 0), ignore_attr = TRUE)
  expect_equal(out$outside_share[1], 0)
})

test_that("both forms differentiate sigma, v0 and beta against analytic choice algebra", {
  td <- .mk_huff_data()
  for (family in c("gaussian", "exponential", "power")) {
    theta <- c(sigma = switch(family, gaussian = 2.5, exponential = 0.3, power = 1.2),
               v0 = 4, beta = 1.3)
    for (form in c("clm", "huff_decay")) {
      p <- .huff_problem(td$demand, td$supply, td$distance, family = family,
                         kappa = 1 / 3, fit_v0 = TRUE, fit_beta = TRUE,
                         allocation = form)
      dist <- p$substrate$distance_active
      D <- p$substrate$D_active
      oracle <- .huff_choice_oracle(dist, D, td$supply, theta, family, form, 1 / 3)
      P <- oracle$share
      dlogK <- switch(family,
        gaussian = dist^2 / theta[["sigma"]]^3,
        exponential = -dist,
        power = -log(dist)
      )
      logS <- matrix(log(td$supply / 3), nrow(dist), ncol(dist), byrow = TRUE)
      deriv <- list(
        P * (dlogK - rowSums(P * dlogK)),
        -P / oracle$denom,
        P * (logS - rowSums(P * logS))
      )
      if (form == "huff_decay") {
        deriv <- lapply(deriv, function(x) x * oracle$kernel)
        deriv[[1]] <- deriv[[1]] + P * oracle$kernel * dlogK
      }
      expected <- vapply(deriv, function(x) as.vector(crossprod(x, D)), numeric(3))
      got <- .problem_output_sensitivity(p, theta, p$state$init)
      expect_equal(unname(got), unname(expected), tolerance = 1e-5,
                   info = paste(family, form))
      allocation_sens <- .problem_output_sensitivity(p, theta, p$state$init,
                                                     output = "allocation")
      expect_equal(unname(allocation_sens), vapply(deriv, as.vector, numeric(length(P))),
                   tolerance = 1e-5, info = paste(family, form, "allocation"))

      observed <- oracle$utilization * c(0.8, 1.1, 1.2)
      grad <- .poisson_gradient(oracle$utilization, observed, got)
      oracle_loss <- function(th) {
        U <- .huff_choice_oracle(dist, D, td$supply, th, family, form, 1 / 3)$utilization
        sum(U - observed * log(U))
      }
      fd <- vapply(seq_along(theta), function(k) {
        hi <- lo <- theta
        hi[k] <- hi[k] + 1e-5
        lo[k] <- lo[k] - 1e-5
        (oracle_loss(hi) - oracle_loss(lo)) / 2e-5
      }, numeric(1))
      expect_equal(grad, fd, tolerance = 1e-5, info = paste(family, form, "loss"))
    }
  }
})

test_that("allocation form survives fitting and inspection", {
  td <- .mk_huff_data()
  fits <- lapply(c("clm", "huff_decay"), function(form) {
    p <- .huff_problem(td$demand, td$supply, td$distance, v0 = 4, allocation = form)
    truth <- .solve_problem(p, c(sigma = 2.5))$outputs$utilization
    fit <- .fit_problem_nfxp(p, observed = truth, init = c(sigma = 2),
                             lower = c(sigma = 0.5), upper = c(sigma = 4))
    expect_identical(fit$allocation_form, form)
    expect_identical(summary(fit)$allocation_form, form)
    expect_output(print(fit), paste0("allocation_form: ", form))
    expect_output(print(summary(fit)), paste0("allocation_form: ", form))
    fit
  })
  # One observed target is necessary for a legitimate model comparison.
  p <- .huff_problem(td$demand, td$supply, td$distance, v0 = 4, allocation = "huff_decay")
  fits[[2]] <- .fit_problem_nfxp(p, observed = fits[[1]]$observed,
                                init = c(sigma = 2), lower = c(sigma = 0.5),
                                upper = c(sigma = 30))
  expect_equal(.compare_problem_fits(setNames(fits, c("clm", "huff_decay")))$allocation_form,
               c("clm", "huff_decay"))
})
