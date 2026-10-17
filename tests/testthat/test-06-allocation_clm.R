# Corrected static CLM provider (SPAX-029/032/033): the no-state member of the
# AE ladder, declared through clm_allocation() over checked numeric inputs.
# The legacy huff_decay form is tested in test-06-legacy_providers.R. The last
# section tests declared decay kernels (SPAX-052): decay_kernel() declarations
# passed as `family`.

test_that("static CLM reaches its constant-map equilibrium in one pass", {
  f <- alloc_huff_case()
  p <- clm_allocation(f$inputs, kappa = 1 / 3)
  expect_s3_class(p, "huff_problem")
  fit <- .solve_problem(p, c(sigma = 2), lambda = 1)
  expect_true(fit$converged)
  expect_identical(fit$iters, 1L)
  # The fixed point is the exogenous attractiveness and dT/dstate is zero.
  expect_equal(unname(fit$x_star), p$state$init, tolerance = 1e-12)
  expect_equal(fit$spectral_radius, 0)
  expect_true(fit$contraction)
  step <- .bind_theta(p, c(sigma = 2))
  a <- p$state$init
  for (x in list(a * 0.3, a * 5)) expect_equal(step$map(x), a, tolerance = 1e-12)
  expect_equal(step$jac_state(a * 0.3)$jac_state, matrix(0, 3, 3))
  expect_equal(step$jac_state(a * 0.3)$jac_utilization, matrix(0, 3, 3))
  # Fitted v0 and beta keep one pass when beta equals the declared constant,
  # so the initial state already is the bound attractiveness.
  q <- clm_allocation(f$inputs, kappa = 1 / 3, fit_v0 = TRUE, fit_beta = TRUE)
  fit <- .solve_problem(q, c(sigma = 2, v0 = 1, beta = 1))
  expect_identical(fit$iters, 1L)
  expect_equal(fit$spectral_radius, 0)
})

test_that("fitted v0 and beta follow sigma in order and are read from theta", {
  f <- alloc_huff_case()
  parameters <- function(...) clm_allocation(f$inputs, ...)$theta$names
  expect_identical(parameters(), "sigma")
  expect_identical(parameters(fit_v0 = TRUE), c("sigma", "v0"))
  expect_identical(parameters(fit_beta = TRUE), c("sigma", "beta"))
  p <- clm_allocation(f$inputs, kappa = 1 / 3, fit_v0 = TRUE, fit_beta = TRUE)
  expect_identical(p$theta$names, c("sigma", "v0", "beta"))
  expect_equal(p$theta$lower, c(0, 0, 0))
  expect_identical(.huff_spec()$allocation, "clm")
  expect_error(.huff_spec(allocation = "unknown"), "arg")
  load <- function(theta) .bind_theta(p, theta)$outputs(0)$utilization
  # More outside weight leaves less inside load.
  expect_lt(sum(load(c(sigma = 2, v0 = 5, beta = 1))),
            sum(load(c(sigma = 2, v0 = 0.01, beta = 1))))
  # A larger supply elasticity concentrates load on the largest facility.
  low <- load(c(sigma = 2, v0 = 1, beta = 0.5))
  high <- load(c(sigma = 2, v0 = 1, beta = 2))
  big <- which.max(f$supply)
  expect_gt(high[big] / sum(high), low[big] / sum(low))
})

test_that("static CLM matches choice algebra and accounts for outside mass", {
  f <- alloc_huff_case()
  theta <- c(sigma = 2.5, v0 = 4, beta = 1.3)
  p <- clm_allocation(f$inputs, fit_v0 = TRUE, fit_beta = TRUE)
  out <- .solve_problem(p, theta)$outputs
  oracle <- clm_oracle(f$distance, f$demand, f$supply, theta[["sigma"]],
                       theta[["v0"]], beta = theta[["beta"]])
  expect_equal(unname(out$allocation), unname(oracle$allocation), tolerance = 1e-12)
  expect_equal(out$huff_share, out$allocation, tolerance = 1e-12)
  expect_equal(out$utilization, oracle$utilization, tolerance = 1e-12)
  expect_equal(unname(out$access), unname(oracle$rho), tolerance = 1e-12)
  expect_equal(unname(out$outside_share), unname(oracle$outside_share), tolerance = 1e-12)
  expect_equal(unname(out$access + out$outside_share), rep(1, 16), tolerance = 1e-12)
  expect_lt(sum(out$utilization), sum(f$demand))
  # Without an outside option all demand is allocated inside.
  inside <- .bind_theta(p, replace(theta, "v0", 0))$outputs(0)
  expect_equal(sum(inside$utilization), sum(f$demand), tolerance = 1e-12)
  expect_equal(inside$outside_share, rep(0, 16))
})

test_that("static CLM conserves supported supply and preserves absent support", {
  f <- alloc_huff_case()
  f$distance[1, ] <- Inf # positive demand with no reachable facility
  f$distance[, 3] <- Inf # supplied facility with no contact
  inputs <- prepare_allocation(f$demand, f$supply, f$distance)
  out <- .solve_problem(clm_allocation(inputs, v0 = 4), c(sigma = 2.5))$outputs
  expect_length(out$access, 16)
  expect_equal(unname(out$access[1]), 0)
  expect_equal(unname(out$outside_share[1]), 1)
  expect_equal(out$utilization[3], 0)
  ratio <- ifelse(out$utilization > 0, f$supply / out$utilization, 0)
  expect_equal(sum(f$demand * as.vector(out$allocation %*% ratio)),
               sum(f$supply[out$utilization > 0]), tolerance = 1e-12)
  oracle <- clm_oracle(f$distance, f$demand, f$supply, 2.5, v0 = 4)
  expect_equal(unname(out$allocation), unname(oracle$allocation), tolerance = 1e-12)
  # No opportunity and no outside option keep the documented zero guard.
  out <- .solve_problem(clm_allocation(inputs), c(sigma = 2.5))$outputs
  expect_equal(unname(out$allocation[1, ]), c(0, 0, 0))
  expect_equal(unname(out$outside_share[1]), 0)
})

test_that("static CLM sensitivities match analytic choice algebra and finite differences", {
  f <- alloc_huff_case()
  for (family in c("gaussian", "exponential", "power")) {
    theta <- c(sigma = switch(family, gaussian = 2.5, exponential = 0.3, power = 1.2),
               v0 = 4, beta = 1.3)
    p <- clm_allocation(f$inputs, family = family, kappa = 1 / 3,
                        fit_v0 = TRUE, fit_beta = TRUE)
    choice <- function(th) clm_oracle(f$distance, f$demand, f$supply, th[["sigma"]],
      th[["v0"]], family, beta = th[["beta"]], kappa = 1 / 3)
    oracle <- choice(theta)
    P <- oracle$probability
    dlogK <- switch(family,
      gaussian = f$distance^2 / theta[["sigma"]]^3,
      exponential = -f$distance,
      power = -log(f$distance))
    logS <- matrix(log(f$supply / 3), nrow(P), ncol(P), byrow = TRUE)
    deriv <- list(P * (dlogK - rowSums(P * dlogK)), -P / oracle$denom,
                  P * (logS - rowSums(P * logS)))
    expected <- vapply(deriv, function(x) as.vector(crossprod(x, f$demand)), numeric(3))
    got <- .problem_output_sensitivity(p, theta, p$state$init)
    expect_equal(unname(got), unname(expected), tolerance = 1e-5, info = family)
    allocation <- .problem_output_sensitivity(p, theta, p$state$init, output = "allocation")
    expect_equal(unname(allocation), unname(vapply(deriv, as.vector, numeric(length(P)))),
                 tolerance = 1e-5, info = family)
    observed <- oracle$utilization * c(0.8, 1.1, 1.2)
    loss <- function(th) {
      U <- choice(th)$utilization
      sum(U - observed * log(U))
    }
    fd <- vapply(seq_along(theta), function(k) {
      h <- replace(numeric(3), k, 1e-5)
      (loss(theta + h) - loss(theta - h)) / 2e-5
    }, numeric(1))
    expect_equal(.poisson_gradient(oracle$utilization, observed, got), fd,
                 tolerance = 1e-5, info = family)
  }
})

test_that("Poisson loss and its gradient match their formula and finite differences", {
  pr <- c(2, 5, 3); ob <- c(2, 4, 3)
  expect_equal(.poisson_loss(pr, ob), sum(pr - ob * log(pr)), tolerance = 1e-12)
  sens <- matrix(c(1, 0.5, -0.2, 0.3, 1, 0.7), nrow = 3)
  fd <- vapply(1:2, function(k) {
    h <- 1e-6
    (.poisson_loss(pr + h * sens[, k], ob) - .poisson_loss(pr - h * sens[, k], ob)) / (2 * h)
  }, numeric(1))
  expect_equal(.poisson_gradient(pr, ob, sens), fd, tolerance = 1e-5)
})

test_that("black-box and gradient fits recover the decay parameter", {
  f <- alloc_huff_case()
  p <- clm_allocation(f$inputs, kappa = 1 / 3)
  observed <- .solve_problem(p, c(sigma = 2.5))$outputs$utilization
  fit <- function(gradient) fit_allocation(p, observed, rbind(start = c(sigma = 6)),
    c(sigma = 0.2), c(sigma = 30), loss_args = list(eta = 1), gradient = gradient)
  bb <- fit(FALSE)
  gd <- fit(TRUE)
  expect_s3_class(bb$best, "ae_problem_nfxp_fit")
  expect_identical(bb$best$model, "huff")
  expect_identical(summary(bb$best)$allocation_form, "clm")
  expect_equal(unname(coef(bb)), 2.5, tolerance = 1e-2)
  expect_lt(bb$best$loss, 1e-6)
  expect_true(gd$best$gradient)
  expect_equal(unname(coef(gd)), unname(coef(bb)), tolerance = 1e-2)
})

test_that("a joint Poisson fit recovers sigma, v0 and a non-unit beta", {
  f <- alloc_huff_case()
  p <- clm_allocation(f$inputs, kappa = 1 / 3, fit_v0 = TRUE, fit_beta = TRUE)
  truth <- c(sigma = 2.5, v0 = 1.0, beta = 1.4)
  observed <- .solve_problem(p, truth)$outputs$utilization
  fit <- fit_allocation(p, observed, rbind(start = c(sigma = 5, v0 = 0.5, beta = 0.8)),
    lower = c(sigma = 0.2, v0 = 1e-3, beta = 0.2), upper = c(sigma = 30, v0 = 50, beta = 4),
    loss = "poisson", loss_args = list(eps = 1e-9), gradient = TRUE,
    control = list(maxit = 400))
  expect_equal(coef(fit)[["beta"]], truth[["beta"]], tolerance = 2e-2)
  expect_equal(coef(fit)[["sigma"]], truth[["sigma"]], tolerance = 1e-2)
  expect_equal(coef(fit)[["v0"]], truth[["v0"]], tolerance = 1e-2)
  # Exact recovery of the generated mean also recovers its total.
  expect_equal(sum(fitted(fit)), sum(observed), tolerance = 1e-4)
})

# Declared decay kernels (SPAX-052) --------------------------------------------
# A kernel declared with decay_kernel() is passed as `family`. Expected values
# come from clm_oracle() with the kernel written out again as a plain function
# (its `kernel` argument), never from the package's own kernel code.

# A custom copy of the Gaussian kernel, written exactly as the built-in formula.
gaussian_copy_kernel <- function() {
  decay_kernel(function(distance, sigma) exp(-(distance^2) / (2 * sigma^2)),
               name = "gaussian-copy", parameter = "travel scale")
}

# Poisson multistart fit of the small case with the custom Gaussian copy, with
# the controls of alloc_small_fit().
gaussian_copy_fit <- function() {
  alloc_memo("gaussian_copy_fit", {
    f <- alloc_small_case()
    model <- clm_allocation(f$inputs, family = gaussian_copy_kernel(), fit_v0 = TRUE)
    fit_allocation(model, rev(f$observed), f$starts, f$lower, f$upper,
      loss = "poisson", gradient = TRUE, control = list(maxit = 100))
  })
}

# The small case with an unknown (NA) and an absent (Inf) travel time on active
# rows, prepared with the explicit legacy policy that omits unknown edges.
small_case_with_gaps <- function() {
  f <- alloc_small_case()
  f$travel["o3", "f1"] <- NA
  f$travel["o1", "f3"] <- Inf
  f$inputs <- prepare_allocation(f$demand, f$supply, f$travel,
    missing_demand = "exclude", missing_travel = "legacy_zero")
  f
}

test_that("a built-in declaration gives the same model, specification and outputs as its string", {
  f <- alloc_huff_case()
  sigma <- c(gaussian = 2.5, exponential = 0.3, power = 1.2)
  for (family in names(sigma)) {
    theta <- c(sigma = sigma[[family]], v0 = 4, beta = 1.3)
    declare <- function(family) clm_allocation(f$inputs, family = family, kappa = 1 / 3,
                                               fit_v0 = TRUE, fit_beta = TRUE)
    by_string <- declare(family)
    by_declaration <- declare(decay_kernel(family))
    # The whole model differs in its closures; its declaration data does not.
    expect_identical(by_declaration$metadata$spec, by_string$metadata$spec, info = family)
    expect_false("kernel" %in% names(by_declaration$metadata$spec), info = family)
    expect_identical(by_declaration$theta, by_string$theta, info = family)
    expect_identical(by_declaration$theta, list(names = c("sigma", "v0", "beta"),
                                                lower = c(0, 0, 0), upper = rep(Inf, 3)))
    a <- evaluate_allocation(by_string, theta, requested_outputs = "flow")
    b <- evaluate_allocation(by_declaration, theta, requested_outputs = "flow")
    expect_identical(b$outputs, a$outputs, info = family)
    expect_identical(b$prediction_snapshot, a$prediction_snapshot, info = family)
    expect_false(is.null(b$prediction_snapshot), info = family)
    oracle <- clm_oracle(f$distance, f$demand, f$supply, theta[["sigma"]], theta[["v0"]],
      family, beta = theta[["beta"]], kappa = 1 / 3)
    expect_equal(unname(b$outputs$allocation), unname(oracle$allocation),
                 tolerance = 1e-12, info = family)
    expect_equal(unname(b$outputs$flow), unname(oracle$flow), tolerance = 1e-12, info = family)
  }
  # The default is still Gaussian, and strings keep their partial matching.
  expect_identical(clm_allocation(f$inputs)$metadata$spec$family, "gaussian")
  expect_identical(clm_allocation(f$inputs, family = "exp")$metadata$spec$family, "exponential")
  expect_error(clm_allocation(f$inputs, family = "cauchy"), "arg")
  expect_error(clm_allocation(f$inputs, family = c("gaussian", "power")), "length")
  expect_error(clm_allocation(f$inputs, family = list(name = "gaussian")), "arg")
})

test_that("a declaration gives the model its name and carries its declaration data", {
  f <- alloc_small_case()
  k <- decay_kernel(function(distance, sigma) exp(-distance / sigma), name = "scaled-exp",
                    parameter = "scale", lower = 0.5, upper = 20)
  model <- clm_allocation(f$inputs, family = k, fit_v0 = TRUE)
  spec <- model$metadata$spec
  expect_identical(spec$family, "scaled-exp")
  expect_identical(spec$kernel, k)
  # Appended after the contract, so the built-in field order is unchanged.
  expect_identical(names(spec), c(names(clm_allocation(f$inputs, fit_v0 = TRUE)$metadata$spec),
                                  "kernel"))
  expect_identical(spec$allocation, "clm")
  expect_s3_class(model, "huff_problem")
  # A declaration that was altered after creation is not accepted.
  broken <- k
  broken$code <- "function(distance, sigma) {"
  expect_error(clm_allocation(f$inputs, family = broken), "invalid decay kernel declaration")
  expect_error(clm_allocation(f$inputs, family = unclass(k)), "arg")
})

test_that("a custom copy of the Gaussian kernel reproduces the Gaussian fit", {
  f <- alloc_small_case()
  reference <- alloc_small_fit()
  fit <- gaussian_copy_fit()
  expect_identical(fit$status, "selected")
  expect_identical(fit$best$family, "gaussian-copy")
  expect_equal(coef(fit)[["sigma"]], coef(reference)[["sigma"]], tolerance = 1e-6)
  expect_equal(coef(fit)[["v0"]], coef(reference)[["v0"]], tolerance = 1e-6)
  expect_equal(fit$best$loss, reference$best$loss, tolerance = 1e-8)
  expect_identical(fit$table$eligible, reference$table$eligible)
  # Fitted counts are the choice algebra at the fitted parameters.
  oracle <- clm_oracle(f$travel[f$active, , drop = FALSE], f$demand[f$active], f$supply,
                       coef(fit)[["sigma"]], coef(fit)[["v0"]])
  expect_equal(fitted(fit), colSums(oracle$flow), tolerance = 1e-8)
  expect_equal(coef(fit), f$truth, tolerance = 1e-3)
})

test_that("a plateau kernel matches the choice algebra at fixed parameters", {
  f <- alloc_small_case()
  plateau <- decay_kernel(
    function(distance, sigma, plateau = 5) {
      # Full weight within the plateau, Gaussian decay beyond it.
      ifelse(distance <= plateau, 1, exp(-(distance - plateau)^2 / (2 * sigma^2)))
    },
    name = "plateau", parameter = "scale", args = list(plateau = 2.5)
  )
  model <- clm_allocation(f$inputs, family = plateau, fit_v0 = TRUE)
  # The same kernel written another way: exp(-0) is 1 within the plateau.
  by_algebra <- function(distance, sigma) exp(-pmax(distance - 2.5, 0)^2 / (2 * sigma^2))
  travel <- f$travel[f$active, , drop = FALSE]
  for (theta in list(c(sigma = 3, v0 = 10), c(sigma = 1.5, v0 = 0), c(sigma = 7, v0 = 25))) {
    out <- evaluate_allocation(model, theta, requested_outputs = "flow")$outputs
    oracle <- clm_oracle(travel, f$demand[f$active], f$supply, theta[["sigma"]],
                         theta[["v0"]], kernel = by_algebra)
    expect_equal(unname(out$allocation), unname(oracle$allocation), tolerance = 1e-12)
    expect_equal(unname(out$flow), unname(oracle$flow), tolerance = 1e-12)
    expect_equal(unname(out$utilization), unname(oracle$utilization), tolerance = 1e-12)
    expect_equal(unname(out$access), unname(oracle$rho), tolerance = 1e-12)
    expect_equal(unname(out$outside_share), unname(oracle$outside_share), tolerance = 1e-12)
  }
  # The declaration kept by the model is plain data.
  expect_identical(model$metadata$spec$kernel$args, list(plateau = 2.5))
  expect_identical(unserialize(serialize(model$metadata$spec, NULL)), model$metadata$spec)
})

test_that("kernels written with a scalar if or returning a plain vector keep the matrix shape", {
  f <- small_case_with_gaps()
  # Unknown (NA) and absent (Inf) travel never reach the kernel: the scalar `if`
  # below would fail on NA. An absent edge has weight 0 and an unknown one is
  # omitted, as for the built-in kernels.
  stepwise <- decay_kernel(
    function(distance, sigma) {
      vapply(distance, function(d) if (d <= 3) 1 else exp(-(d - 3) / sigma), numeric(1))
    },
    name = "stepwise", parameter = "scale"
  )
  rational <- decay_kernel(function(distance, sigma) as.vector(1 / (1 + (distance / sigma)^2)),
                           name = "rational", parameter = "scale")
  written <- list(
    stepwise = list(kernel = stepwise,
      oracle = function(distance, sigma) ifelse(distance <= 3, 1, exp(-(distance - 3) / sigma))),
    rational = list(kernel = rational,
      oracle = function(distance, sigma) 1 / (1 + (distance / sigma)^2))
  )
  travel <- f$travel[f$active, , drop = FALSE]
  expect_true(anyNA(travel) && any(travel == Inf, na.rm = TRUE))
  for (nm in names(written)) {
    k <- written[[nm]]
    weights <- .allocation_kernel(f$inputs, k$kernel$name, 2, kernel = k$kernel)
    expect_identical(dim(weights), dim(f$inputs$distance_active), info = nm)
    expect_identical(dimnames(weights), dimnames(f$inputs$distance_active), info = nm)
    expect_equal(weights, ifelse(is.finite(travel), k$oracle(travel, 2), 0),
                 tolerance = 1e-12, info = nm)
    expect_identical(weights[is.na(travel)], 0, info = nm)
    expect_identical(weights[travel == Inf & !is.na(travel)], 0, info = nm)
    theta <- c(sigma = 2, v0 = 10)
    out <- evaluate_allocation(clm_allocation(f$inputs, family = k$kernel, fit_v0 = TRUE),
                               theta)$outputs
    oracle <- clm_oracle(travel, f$demand[f$active], f$supply, 2, 10, kernel = k$oracle)
    expect_equal(unname(out$allocation), unname(oracle$allocation), tolerance = 1e-12, info = nm)
    expect_equal(unname(out$utilization), unname(oracle$utilization), tolerance = 1e-12, info = nm)
    # The omitted unknown edge and the absent edge attract no load.
    expect_identical(unname(out$allocation["o3", "f1"]), 0, info = nm)
    expect_identical(unname(out$allocation["o1", "f3"]), 0, info = nm)
  }
})

test_that("declared sigma bounds become the parameter bounds", {
  f <- alloc_small_case()
  bounded <- decay_kernel(function(distance, sigma) exp(-distance / sigma), name = "scaled-exp",
                          parameter = "scale", lower = 0.5, upper = 20)
  model <- clm_allocation(f$inputs, family = bounded, fit_v0 = TRUE, fit_beta = TRUE)
  expect_identical(model$theta, list(names = c("sigma", "v0", "beta"),
                                     lower = c(0.5, 0, 0), upper = c(20, Inf, Inf)))
  expect_identical(model$metadata$spec$theta, model$theta)
  # The bounds are inclusive; v0 and beta keep [0, Inf).
  for (sigma in c(0.5, 20)) {
    expect_no_error(evaluate_allocation(model, c(sigma = sigma, v0 = 5, beta = 1)))
  }
  for (sigma in c(0.4999, 20.0001)) {
    expect_error(evaluate_allocation(model, c(sigma = sigma, v0 = 5, beta = 1)),
                 "outside the theta bounds", info = sigma)
  }
  # Starts and search bounds outside the declared bounds are refused.
  sigma_only <- clm_allocation(f$inputs, family = bounded, fit_v0 = TRUE)
  expect_error(fit_allocation(sigma_only, f$observed, f$starts,
    lower = c(sigma = 0.2, v0 = 1), upper = f$upper), "outside the theta bounds")
  expect_error(fit_allocation(sigma_only, f$observed, f$starts,
    lower = f$lower, upper = c(sigma = 40, v0 = 40)), "outside the theta bounds")
  fit <- fit_allocation(sigma_only, f$observed, f$starts, lower = f$lower,
                        upper = c(sigma = 20, v0 = 40), loss = "poisson")
  expect_identical(fit$status, "selected")
  expect_true(coef(fit)[["sigma"]] >= 0.5 && coef(fit)[["sigma"]] <= 20)
  # The built-in kernels keep [0, Inf).
  expect_identical(clm_allocation(f$inputs)$theta, list(names = "sigma", lower = 0, upper = Inf))
  # Domain checks of the kernel itself, also when called without a model.
  expect_error(.allocation_kernel(f$inputs, "scaled-exp", 25, kernel = bounded),
               "scaled-exp decay requires sigma from 0.5 to 20")
  expect_error(.allocation_kernel(f$inputs, "scaled-exp", 0.1, kernel = bounded),
               "scaled-exp decay requires sigma from 0.5 to 20")
})

test_that("a declared kernel needs a positive sigma even when its lower bound is zero", {
  f <- alloc_small_case()
  free <- decay_kernel(function(distance, sigma) exp(-distance / sigma), name = "free-exp",
                       parameter = "scale")
  expect_identical(free$lower, 0)
  message <- "free-exp decay requires positive sigma"
  # Zero is inside the inclusive bounds, but sigma must also be positive; the
  # check holds with and without the finite entries located once.
  expect_error(.allocation_kernel(f$inputs, "free-exp", 0, kernel = free), message)
  expect_error(.allocation_kernel(f$inputs, "free-exp", 0, kernel = free,
                                  plan = .decay_kernel_plan(f$inputs$distance_active)), message)
  model <- clm_allocation(f$inputs, family = free, fit_v0 = TRUE)
  expect_error(evaluate_allocation(model, c(sigma = 0, v0 = 5)), message)
  expect_error(.bind_theta(model, c(sigma = 0, v0 = 5)), message)
  expect_no_error(evaluate_allocation(model, c(sigma = 1e-3, v0 = 5)))
  # The bounds keep their own message, and the built-in kernels are unchanged.
  bounded <- decay_kernel(function(distance, sigma) exp(-distance / sigma), name = "bounded-exp",
                          parameter = "scale", lower = 0.5, upper = 20)
  expect_error(.allocation_kernel(f$inputs, "bounded-exp", 0, kernel = bounded),
               "bounded-exp decay requires sigma from 0.5 to 20")
  expect_error(.bind_theta(clm_allocation(f$inputs), c(sigma = 0)), "positive sigma")
})

test_that("sensitivities at a declared bound are one-sided and match the central ones", {
  f <- alloc_generated_case()
  copy <- decay_kernel(function(distance, sigma) exp(-(distance^2) / (2 * sigma^2)),
                       name = "gcopy", parameter = "travel scale", lower = 10, upper = 40)
  declared <- clm_allocation(f$inputs, family = copy, fit_v0 = TRUE)
  expect_identical(declared$theta$lower, c(10, 0))
  # sigma on the declared lower bound, and within one finite-difference step
  # (about 1e-5) of it: a central step would leave the bounds.
  for (sigma in c(10, 10 + 2e-6, 10 + 5e-6, 10 + 1e-3)) {
    theta <- c(sigma = sigma, v0 = 25)
    P <- clm_oracle(f$distance, f$demand, f$supply, sigma, 25)$probability
    denominator <- rowSums(sweep(exp(-f$distance^2 / (2 * sigma^2)), 2, f$supply, "*")) + 25
    dlogK <- f$distance^2 / sigma^3
    expected <- cbind(as.vector(crossprod(P * (dlogK - rowSums(P * dlogK)), f$demand)),
                      as.vector(crossprod(-P / denominator, f$demand)))
    state <- declared$state$init
    got <- .problem_output_sensitivity(declared, theta, state)
    expect_equal(unname(got), unname(expected), tolerance = 1e-5, info = sigma)
    # The built-in model takes central steps and agrees.
    expect_equal(got, .problem_output_sensitivity(f$model, theta, state), tolerance = 1e-5,
                 info = sigma)
    # The map's own parameter derivative (zero for the static CLM) and the output
    # derivative for a state-independent output are computed without error too.
    expect_no_error(.problem_output_jac_param(declared, theta, state, "utilization"))
    expect_no_error(.problem_output_sensitivity(declared, theta, state, output = "flow"))
  }
  # Inside the bounds nothing changes: the central steps of the unbounded rule.
  theta <- c(sigma = 12, v0 = 25)
  state <- declared$state$init
  fn <- function(th) .problem_output_from_step(.bind_theta(declared, th), state, "utilization")
  expect_identical(.problem_output_jac_param(declared, theta, state, "utilization"),
                   fd_jacobian(fn, theta, check = FALSE))
})

test_that("a kernel declared smooth = FALSE evaluates but cannot be fitted", {
  f <- alloc_small_case()
  # The fitting engine is replaced: a model that passes the check reaches it.
  testthat::local_mocked_bindings(.fit_problem_multistart = function(...) "reached the fitter")
  cutoff <- decay_kernel(function(distance, sigma) 0 + (distance <= sigma),
                         name = "cutoff", parameter = "cutoff", smooth = FALSE)
  model <- clm_allocation(f$inputs, family = cutoff, fit_v0 = TRUE)
  expect_false(model$metadata$spec$kernel$smooth)
  message <- paste("a kernel declared smooth = FALSE cannot be fitted; evaluate it at fixed",
                   "parameters, for example over a grid of sigma and v0")
  expect_error(fit_allocation(model, f$observed, f$starts, f$lower, f$upper),
               message, fixed = TRUE)
  expect_error(fit_allocation(model, f$observed, f$starts, f$lower, f$upper,
                              loss = "poisson", gradient = TRUE), message, fixed = TRUE)
  # Evaluation at fixed parameters is the choice algebra with a step kernel.
  theta <- c(sigma = 4, v0 = 10)
  out <- evaluate_allocation(model, theta)$outputs
  oracle <- clm_oracle(f$travel[f$active, , drop = FALSE], f$demand[f$active], f$supply,
                       4, 10, kernel = function(distance, sigma) 0 + (distance <= sigma))
  expect_equal(unname(out$allocation), unname(oracle$allocation), tolerance = 1e-12)
  expect_equal(unname(out$utilization), unname(oracle$utilization), tolerance = 1e-12)
  # The same kernel declared smooth (the default), and the built-in kernels, are
  # not refused.
  smooth <- decay_kernel(function(distance, sigma) 0 + (distance <= sigma),
                         name = "cutoff", parameter = "cutoff")
  for (family in list(smooth, "gaussian", decay_kernel("power"))) {
    expect_identical(fit_allocation(clm_allocation(f$inputs, family = family, fit_v0 = TRUE),
      f$observed, f$starts, f$lower, f$upper), "reached the fitter")
  }
})

test_that("zero_distance = FALSE refuses a prepared finite zero travel time", {
  f <- alloc_small_case()
  hyperbolic <- decay_kernel(function(distance, sigma) 1 / (distance * sigma),
                             name = "hyperbolic", parameter = "scale", zero_distance = FALSE)
  zero_at <- function(origin, facility = "f1", value = 0) {
    travel <- f$travel
    travel[origin, facility] <- value
    prepare_allocation(f$demand, f$supply, travel, missing_demand = "exclude")
  }
  message <- "hyperbolic decay requires strictly positive finite travel distances"
  # o3 has positive demand: a finite zero travel time on an active row is refused
  # when the model is bound, as for the power kernel.
  with_zero <- zero_at("o3")
  expect_error(evaluate_allocation(clm_allocation(with_zero, family = hyperbolic),
                                   c(sigma = 2)), message)
  expect_error(.bind_theta(clm_allocation(with_zero, family = hyperbolic), c(sigma = 2)),
               message)
  expect_error(.bind_theta(clm_allocation(with_zero, family = "power"), c(sigma = 2)),
               "strictly positive")
  # An absent edge is not a zero, and a zero on an origin without positive demand
  # is not retained in the active travel.
  expect_no_error(evaluate_allocation(
    clm_allocation(zero_at("o3", value = Inf), family = hyperbolic), c(sigma = 2)))
  expect_no_error(evaluate_allocation(
    clm_allocation(zero_at("o2"), family = hyperbolic), c(sigma = 2)))
  # Positive travel evaluates to the kernel formula.
  out <- evaluate_allocation(clm_allocation(f$inputs, family = hyperbolic, fit_v0 = TRUE),
                             c(sigma = 2, v0 = 10))$outputs
  oracle <- clm_oracle(f$travel[f$active, , drop = FALSE], f$demand[f$active], f$supply,
                       2, 10, kernel = function(distance, sigma) 1 / (distance * sigma))
  expect_equal(unname(out$allocation), unname(oracle$allocation), tolerance = 1e-12)
  # A kernel defined at zero evaluates the same input.
  rational <- decay_kernel(function(distance, sigma) 1 / (1 + distance / sigma),
                           name = "rational", parameter = "scale")
  out <- evaluate_allocation(clm_allocation(with_zero, family = rational, fit_v0 = TRUE),
                             c(sigma = 2, v0 = 10))$outputs
  travel <- f$travel[f$active, , drop = FALSE]
  travel["o3", "f1"] <- 0
  oracle <- clm_oracle(travel, f$demand[f$active], f$supply, 2, 10,
                       kernel = function(distance, sigma) 1 / (1 + distance / sigma))
  expect_equal(unname(out$allocation), unname(oracle$allocation), tolerance = 1e-12)
  # Only strict inputs are checked; the kernel is then not defined at zero and
  # the legacy rule zeroes its non-finite weight, as for the built-in kernels.
  legacy <- with_zero
  legacy$input_policy$kernel_domain <- "legacy_zero"
  weights <- .allocation_kernel(legacy, "hyperbolic", 2, kernel = hyperbolic)
  expect_identical(unname(weights["o3", "f1"]), 0)
  expect_error(.allocation_kernel(with_zero, "hyperbolic", 2, kernel = hyperbolic), message)
})

test_that("a declared kernel gives the same weights when its finite travel is located once", {
  f <- alloc_small_case()
  damped <- decay_kernel(function(distance, sigma) {
    exp(-distance / sigma) / (1 + distance)
  }, name = "damped", parameter = "scale")
  fun <- .decay_kernel_function(damped)
  # Checked inputs with an absent edge, and legacy inputs with an unknown edge too.
  strict <- f$inputs
  strict$distance_active[1, 2] <- Inf
  legacy <- strict
  legacy$distance_active[2, 3] <- NA
  legacy$input_policy$kernel_domain <- "legacy_zero"
  for (inputs in list(strict, legacy)) {
    plan <- .decay_kernel_plan(inputs$distance_active)
    planned <- .allocation_kernel(inputs, "damped", 2, kernel = damped, fun = fun, plan = plan)
    unplanned <- .allocation_kernel(inputs, "damped", 2, kernel = damped, fun = fun)
    expect_identical(planned, unplanned)
    d <- inputs$distance_active
    expect_equal(unname(planned), unname(ifelse(is.finite(d), exp(-d / 2) / (1 + d), 0)))
  }
  # With every travel entry finite there is nothing to select: the plan keeps no
  # index and no copy of the values, the kernel runs on the matrix itself, and the
  # weights (with their dimensions and names) are the unplanned ones.
  plan <- .decay_kernel_plan(f$inputs$distance_active)
  expect_identical(plan, list(all_finite = TRUE, any_zero = FALSE))
  planned <- .allocation_kernel(f$inputs, "damped", 2, kernel = damped, fun = fun, plan = plan)
  expect_identical(planned, .allocation_kernel(f$inputs, "damped", 2, kernel = damped, fun = fun))
  d <- f$inputs$distance_active
  expect_identical(dimnames(planned), dimnames(d))
  expect_type(planned, "double")
  expect_equal(unname(planned), unname(exp(-d / 2) / (1 + d)))
  # a kernel returning integers or plain vectors still gives a double matrix
  stepwise <- decay_kernel(function(distance, sigma) 1L * (distance <= 3) + 0L,
                           name = "stepwise", parameter = "scale", smooth = FALSE)
  weights <- .allocation_kernel(f$inputs, "stepwise", 2, kernel = stepwise,
                                plan = .decay_kernel_plan(d))
  expect_identical(weights, 0 + (d <= 3))
  expect_identical(weights, .allocation_kernel(f$inputs, "stepwise", 2, kernel = stepwise))
  # With a gap the plan keeps the index of the finite entries only, in the order of
  # the matrix, and whether one of them is zero.
  gap <- .decay_kernel_plan(strict$distance_active)
  expect_identical(names(gap), c("all_finite", "index", "any_zero"))
  expect_false(gap$all_finite)
  expect_identical(gap$index, which(is.finite(strict$distance_active)))
  expect_false(gap$any_zero)
  zero <- strict
  zero$distance_active[1, 1] <- 0
  expect_true(.decay_kernel_plan(zero$distance_active)$any_zero)
  expect_true(.decay_kernel_plan(f$inputs$distance_active * 0)$any_zero)
  # no finite entry at all, and no entry at all
  none <- matrix(c(Inf, NA, Inf, Inf), 2, dimnames = list(c("a", "b"), c("x", "y")))
  expect_identical(.decay_kernel_plan(none), list(all_finite = FALSE, index = integer(0),
                                                  any_zero = FALSE))
  expect_identical(.decay_kernel_plan(matrix(numeric(0), 0, 3))$index, integer(0))
  # The planned path keeps the domain checks.
  hyper <- decay_kernel(function(distance, sigma) 1 / (distance * sigma),
                        name = "hyper", parameter = "scale", zero_distance = FALSE)
  with_zero <- f$inputs
  with_zero$distance_active[1, 1] <- 0
  expect_error(.allocation_kernel(with_zero, "hyper", 2, kernel = hyper,
                                  plan = .decay_kernel_plan(with_zero$distance_active)),
               "hyper decay requires strictly positive finite travel distances")
  # Weights that turn negative beyond the declaration's test grid are refused.
  linear <- decay_kernel(function(distance, sigma) 1 - distance / 2000,
                         name = "linear", parameter = "unused")
  far <- f$inputs
  far$distance_active[1, 1] <- 3000
  expect_error(.allocation_kernel(far, "linear", 1, kernel = linear,
                                  plan = .decay_kernel_plan(far$distance_active)),
               "decay kernel is invalid on finite travel distances")
})

test_that("the no-decay family weights finite distances by one and has no prediction snapshot", {
  distance <- matrix(c(0, 1.5, Inf, NA, 4, 10), 2, 3,
                     dimnames = list(c("a", "b"), c("x", "y", "z")))
  w <- .decay_weights("none", NULL, distance, sigma = 3)
  expect_identical(dim(w), dim(distance))
  expect_identical(dimnames(w), dimnames(distance))
  expect_identical(w[is.finite(distance)], rep(1, 4))
  # Non-finite distances keep a non-finite value for the caller's zeroing.
  expect_false(any(is.finite(w[!is.finite(distance)])))
  expect_identical(.decay_weights("none", NULL, matrix(1:4, 2), 3), matrix(1, 2, 2))
  # Built-ins are exactly calc_decay() on prepared travel; a custom kernel is
  # evaluated on the finite distances, with weight 0 for an absent edge.
  expect_identical(.decay_weights("gaussian", NULL, distance, 3),
                   calc_decay(distance, "gaussian", 3, snap = TRUE))
  k <- decay_kernel(function(distance, sigma) exp(-distance / sigma), name = "scaled-exp",
                    parameter = "scale")
  custom <- .decay_weights("scaled-exp", k, distance, 3)
  expect_equal(custom[is.finite(distance)], exp(-distance[is.finite(distance)] / 3))
  expect_identical(custom[!is.na(distance) & distance == Inf], 0)
  expect_identical(is.na(custom), is.na(distance))
  expect_identical(dimnames(custom), dimnames(distance))
  # A model with the internal no-decay family spreads demand by attractiveness
  # alone and never declares a prediction snapshot.
  f <- alloc_small_case()
  none <- f$model
  none$metadata$spec$family <- "none"
  none$bind <- .compile_huff_map(none$metadata$spec, none$substrate)$bind
  out <- evaluate_allocation(none, c(sigma = 3, v0 = 10), requested_outputs = "flow")
  expect_null(out$prediction_snapshot)
  expect_null(.bind_theta(none, c(sigma = 3, v0 = 10))$clm_prediction)
  expected <- matrix(f$supply / (sum(f$supply) + 10), 3, 3, byrow = TRUE)
  expect_equal(unname(out$outputs$allocation), expected, tolerance = 1e-12)
  expect_equal(unname(out$outputs$flow), expected * f$demand[f$active], tolerance = 1e-12)
  # Absent edges stay absent: the weight of an unreachable facility is zero.
  gaps <- small_case_with_gaps()
  weights <- .allocation_kernel(gaps$inputs, "none", 3)
  expect_equal(weights, ifelse(is.finite(gaps$inputs$distance_active), 1, 0))
})

test_that("a declared kernel attaches a v2 prediction snapshot that predicts; built-ins keep v1", {
  f <- alloc_small_case()
  k <- decay_kernel(function(distance, sigma) exp(-distance / sigma), name = "scaled-exp",
                    parameter = "scale")
  model <- clm_allocation(f$inputs, family = k, fit_v0 = TRUE)
  theta <- c(sigma = 3, v0 = 10)
  # The bound step declares the snapshot, with the declaration in its specification.
  declaration <- .bind_theta(model, theta)$clm_prediction
  expect_identical(declaration$spec$kernel, k)
  expect_identical(declaration$theta, theta)
  out <- evaluate_allocation(model, theta)
  snapshot <- out$prediction_snapshot
  expect_identical(snapshot$schema, "spax_clm_prediction_v2")
  expect_identical(snapshot$kind, "declared_kernel_static_clm")
  expect_identical(snapshot$spec$family, "scaled-exp")
  expect_identical(snapshot$spec$kernel, k)
  expect_identical(out$evaluated_clm, snapshot[c("spec", "theta", "facility_ids", "supply")])
  # It predicts requested origins: the choice algebra with the same kernel.
  request <- f$travel[c("o3", "o1"), , drop = FALSE]
  rownames(request) <- c("new1", "new2")
  oracle <- clm_oracle(request, c(0, 0), f$supply, 3, 10,
                       kernel = function(distance, sigma) exp(-distance / sigma))
  predicted <- predict_allocation(out, request, outputs = c("rho", "allocation"))
  expect_equal(unname(predicted$outputs$rho), unname(oracle$rho), tolerance = 1e-12)
  expect_equal(unname(predicted$outputs$allocation), unname(oracle$allocation),
               tolerance = 1e-12)
  # Built-in kernels, and their declarations, keep the v1 snapshot.
  for (family in list("gaussian", decay_kernel("gaussian"))) {
    v1 <- evaluate_allocation(clm_allocation(f$inputs, family = family, fit_v0 = TRUE),
                              theta)$prediction_snapshot
    expect_identical(v1$schema, "spax_clm_prediction_v1")
    expect_identical(v1$kind, "builtin_static_clm")
    expect_false("kernel" %in% names(v1$spec))
  }
})

test_that("print and summary of a fit label sigma by the meaning of the kernel", {
  f <- alloc_huff_case()
  meaning <- c(gaussian = "travel scale", exponential = "rate", power = "exponent")
  truth <- c(gaussian = 2.5, exponential = 0.3, power = 1.2)
  start <- c(gaussian = 6, exponential = 0.6, power = 2.5)
  reports <- list()
  for (family in names(meaning)) {
    p <- clm_allocation(f$inputs, family = family, kappa = 1 / 3)
    observed <- .solve_problem(p, c(sigma = truth[[family]]))$outputs$utilization
    fit <- fit_allocation(p, observed, rbind(start = c(sigma = start[[family]])),
                          c(sigma = 0.05), c(sigma = 30), loss_args = list(eta = 1),
                          control = list(maxit = 100))
    line <- paste0("family: ", family, " (sigma = ", meaning[[family]], ")")
    printed <- utils::capture.output(print(fit$best))
    expect_true(line %in% printed, info = family)
    expect_false(paste0("family: ", family) %in% printed, info = family)
    reports[[family]] <- summary(fit$best)
    expect_identical(reports[[family]]$family, family)
    expect_identical(reports[[family]]$family_label, sub("^family: ", "", line))
    expect_true(line %in% utils::capture.output(print(reports[[family]])), info = family)
  }
  # A summary saved before the label existed prints the family alone.
  old <- unclass(reports$power)
  old$family_label <- NULL
  expect_true("family: power" %in%
    utils::capture.output(print(structure(old, class = "summary.ae_problem_nfxp_fit"))))
  # A fit of a custom kernel prints its own name, and the meaning of sigma from
  # the declaration kept in its prediction snapshot.
  copy <- gaussian_copy_fit()$best
  label <- "gaussian-copy (sigma = travel scale)"
  expect_true(paste("family:", label) %in% utils::capture.output(print(copy)))
  expect_identical(summary(copy)$family_label, label)
  expect_true(paste("family:", label) %in% utils::capture.output(print(summary(copy))))
})

test_that("the family label takes the meaning of a custom kernel from its saved declaration", {
  declared <- list(family = "plateau", prediction_snapshot = list(spec = list(
    kernel = list(parameter = "scale"))))
  expect_identical(.fit_family_label(declared), "plateau (sigma = scale)")
  expect_identical(.fit_family_label(list(family = "plateau")), "plateau")
  expect_identical(.fit_family_label(list(family = "plateau", prediction_snapshot = list(
    spec = list(family = "plateau")))), "plateau")
  for (family in c("gaussian", "exponential", "power")) {
    # The built-in meaning does not depend on any saved declaration.
    expect_identical(.fit_family_label(list(family = family)),
      paste0(family, " (sigma = ", c(gaussian = "travel scale", exponential = "rate",
                                     power = "exponent")[[family]], ")"))
  }
  expect_null(.fit_family_label(list()))
})
