# AE problem declaration: the provider contract, theta binding, solving and
# output sensitivities. Generic checks use the static Huff provider, the
# ratio-feedback model (a genuine fixed point with analytic Jacobians) or
# the identity toy provider; SAE/HAAE checks live in their own files.

test_that("providers and bound steps enforce the problem contract", {
  expect_error(.new_problem("toy", list(), list(), list(), bind = 1), "function")
  substrate <- ae_toy_problem()$substrate
  expect_error(.compile_ae_map(list(), substrate), "model spec")
  expect_error(.compile_ae_map(list(model = "other"), substrate), "unsupported")
  f <- ae_toy_inputs()
  expect_error(.huff_problem(f$demand, cbind(f$supply, f$supply), f$distance),
               "one supply measure")

  theta <- c(alpha = 1, beta = 1)
  expect_error(.bind_theta(ae_identity_problem(step = list(map = NULL)), theta),
               "`map` function")
  partial <- ae_identity_problem(step = list(outputs = function(x) list(target = x)))
  expect_error(.solve_problem(partial, theta), "utilization")
  expect_error(.validate_ae_outputs(1), "list")
})

test_that("theta and initial state are checked against their contracts", {
  p <- ae_toy_problem()

  expect_error(.bind_theta(p, c(alpha = 2)), "missing required")
  expect_error(.bind_theta(p, c(1, 2)), "contract length")
  expect_error(.bind_theta(p, c(sigma = -1)), "theta bounds")
  expect_error(.solve_problem(p, theta = 2, x0 = c(-0.1, 1), check = FALSE),
               "outside the problem state bounds")
  expect_error(.solve_problem(p, theta = 2, x0 = 1), "state length")
  expect_no_error(.bind_theta(p, 2))
})

test_that("feedback solves reach a contraction fixed point with diagnostics", {
  p <- ae_ratio_problem()
  theta <- ae_ratio_truth()
  step <- .bind_theta(p, theta)
  fit <- .solve_problem(p, theta, lambda = 0.7, tol = 1e-10,
                        keep_state_history = TRUE)

  expect_true(fit$converged)
  expect_lt(max(abs(step$map(fit$x_star) - fit$x_star)), 1e-9)
  # The default diagnostic is the spectral radius of the undamped map at the
  # fixed point; check it against a finite-difference Jacobian.
  fd <- fd_jacobian_state(step$map, fit$x_star, check = FALSE)
  expect_equal(fit$spectral_radius, max(Mod(eigen(fd)$values)), tolerance = 1e-5)
  expect_gt(fit$spectral_radius, 0)
  expect_identical(fit$contraction, TRUE)
  expect_equal(ncol(fit$state_history), length(p$state$init))
  expect_equal(fit$state_history[nrow(fit$state_history), ], fit$x_star)
  expect_output(print(fit), "state_history")

  bare <- .solve_problem(p, theta, lambda = 0.7, tol = 1e-10, diagnostics = FALSE)
  expect_null(bare$spectral_radius)
  # The step size is measured in the requested norm.
  l2 <- .solve_problem(p, theta, lambda = 0.7, tol = 1e-10, norm = "l2")
  first_step <- 0.7 * (step$map(p$state$init) - p$state$init)
  expect_equal(fit$history$error[1], max(abs(first_step)))
  expect_equal(l2$history$error[1], sqrt(sum(first_step^2)))
  expect_equal(l2$x_star, fit$x_star, tolerance = 1e-8)
})

test_that("output sensitivities use provided Jacobians or finite differences", {
  problem <- ae_identity_problem()
  theta <- c(alpha = 2, beta = 3)
  # x* = theta and utilization = x, so d utilization / d theta = I.
  sensitivity <- .problem_output_sensitivity(problem, theta, theta, "utilization")
  expect_equal(unname(sensitivity), diag(2), tolerance = 1e-5)

  step <- list(
    map = function(x) x,
    outputs = function(x) list(target = x, utilization = x),
    jac_state = function(x) {
      list(jac_state = matrix(0, 1, 1), jac_utilization = matrix(9, 1, 1))
    }
  )
  expect_equal(.problem_output_jac_state(step, x = 1, output = "utilization"),
               matrix(9, 1, 1))
  step$jac_output_state <- function(x, output) matrix(7, 1, 1)
  expect_equal(.problem_output_jac_state(step, x = 1, output = "utilization"),
               matrix(7, 1, 1))
})

# Test-only evaluators in R/ (.problem_map, .problem_outputs,
# .problem_jac_state) have no package caller. Kept pending a maintainer
# decision (test audit 2026-10-14); retiring them means deleting this block.

test_that("test-only problem evaluators match the bound step", {
  p <- ae_ratio_problem()
  theta <- ae_ratio_truth()
  step <- .bind_theta(p, theta)
  x <- c(20, 40)

  expect_identical(.problem_map(p, x, theta), step$map(x))
  expect_identical(.problem_outputs(p, x, theta), step$outputs(x))
  expect_identical(.problem_outputs(p, x, theta, "flow")$flow, step$outputs(x)$flow)
  expect_identical(.problem_jac_state(p, x, theta), step$jac_state(x))
  expect_error(
    .problem_jac_state(ae_identity_problem(), c(1, 1), c(alpha = 1, beta = 1)),
    "analytic state Jacobian"
  )
})
