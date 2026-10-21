# SPAX-061 T3: lean model evaluations inside the optimizer and the scan.

.lean_case <- function(n = 40L, m = 6L, groups = FALSE) {
  set.seed(61)
  ids <- paste0("o", seq_len(n))
  travel <- matrix(runif(n * m, 1, 20), n, m,
                   dimnames = list(ids, paste0("f", seq_len(m))))
  supply <- stats::setNames(runif(m, 10, 40), colnames(travel))
  demand <- stats::setNames(runif(n, 50, 100), ids)
  if (groups) demand <- list(a = demand, b = rev(demand) * 0.5)
  list(inputs = prepare_allocation(demand, supply, travel), travel = travel)
}

.lean_models <- function() {
  c1 <- .lean_case()
  c2 <- .lean_case(groups = TRUE)
  list(
    totals = list(model = clm_allocation(c1$inputs, fit_v0 = TRUE),
                  theta = c(sigma = 3, v0 = 10)),
    huff = list(model = huff_allocation(c1$inputs), theta = c(sigma = 3, beta = 1)),
    grouped = list(model = clm_allocation(c2$inputs, by_group = "sigma", fit_v0 = TRUE),
                   theta = c(sigma.a = 3, sigma.b = 5, v0 = 10))
  )
}

.lean_all <- c("flow", "flow_outside", "utilization_outside")

test_that("a lean bind gives identical vectors and counts to the full bind", {
  for (case in .lean_models()) {
    full <- .bind_theta(case$model, case$theta)
    wanted <- c(.lean_all, if (!is.null(case$model$metadata$spec$groups))
      c("utilization_by_group", "access_by_group", "outside_share_by_group",
        "utilization_outside_by_group"))
    reference <- full$outputs_requested(NULL, wanted)
    for (keep in list(character(), "flow", "flow_outside")) {
      lean <- .bind_theta(case$model, case$theta, keep = keep)
      expect_identical(lean$kept, keep)
      asked <- setdiff(wanted, setdiff(c("flow", "flow_outside"), keep))
      got <- lean$outputs_requested(NULL, asked)
      for (name in names(got)) expect_identical(got[[name]], reference[[name]])
      for (name in c("target", "utilization", "attractiveness", "access", "outside_share")) {
        expect_identical(got[[name]], reference[[name]])
      }
      expect_identical(lean$map(NULL), full$map(NULL))
      expect_identical(lean$jac_state(NULL)$utilization, full$jac_state(NULL)$utilization)
      expect_false(any(c("allocation", "huff_share", "opportunity") %in% names(got)))
    }
  }
})

test_that("a lean step keeps no matrix-sized object in its frame", {
  case <- .lean_case(n = 3000L, m = 60L)
  model <- clm_allocation(case$inputs, fit_v0 = TRUE)
  theta <- c(sigma = 3, v0 = 10)
  M <- as.numeric(object.size(case$inputs$distance_active)) / 1e6
  used <- function() { invisible(gc()); sum(gc()[2L, 2L]) }
  base <- used()
  full <- .bind_theta(model, theta)
  full_mb <- used() - base
  rm(full)
  base <- used()
  lean <- .bind_theta(model, theta, keep = character())
  lean_mb <- used() - base
  expect_gt(full_mb, 2 * M)
  expect_lt(lean_mb, 0.2 * M)
  env <- environment(lean$map)
  expect_false("K" %in% ls(env, all.names = TRUE))
  expect_null(lean$plan$Kd_active)
  rm(lean)
  base <- used()
  flows <- .bind_theta(model, theta, keep = "flow")
  expect_lt(used() - base, 1.3 * M)
  expect_identical(dim(flows$outputs_requested(NULL, "flow")$flow), dim(case$inputs$distance_active))
})

test_that("a lean step signals a classed condition for an output it did not keep", {
  m <- .lean_models()
  for (case in m[c("totals", "grouped")]) {
    lean <- .bind_theta(case$model, case$theta, keep = character())
    for (out in c("allocation", "flow", "opportunity", "flow_outside")) {
      cond <- tryCatch(.problem_output_from_step(lean, NULL, out), condition = identity)
      expect_s3_class(cond, "spax_lean_missing")
      expect_match(conditionMessage(cond), out, fixed = TRUE)
    }
    expect_s3_class(tryCatch(lean$outputs(NULL), condition = identity), "spax_lean_missing")
    # a kept count and the vectors stay readable
    flows <- .bind_theta(case$model, case$theta, keep = "flow")
    expect_true(is.numeric(.problem_output_from_step(flows, NULL, "flow")))
    expect_s3_class(tryCatch(.problem_output_from_step(flows, NULL, "flow_outside"),
                             condition = identity), "spax_lean_missing")
    expect_true(is.numeric(.problem_output_from_step(lean, NULL, "access")))
  }
  # a lean fit checks the kept outputs before returning NULL
  p <- m$totals$model
  fit <- .solve_problem(p, m$totals$theta, requested_outputs = "utilization",
                        keep = character())
  expect_identical(fit$kept, character())
  expect_error(.problem_fit_output(fit, "allocation", required = FALSE),
               class = "spax_lean_missing")
  expect_error(.problem_fit_output(fit, "allocation"), class = "spax_lean_missing")
  expect_null(.solve_problem(p, m$totals$theta)$kept)
})

test_that("the condition is not swallowed inside a fit, the scan or the multistart", {
  case <- .lean_case()
  model <- clm_allocation(case$inputs, fit_v0 = TRUE)
  theta <- c(sigma = 3, v0 = 10)
  observed <- .solve_problem(model, theta, requested_outputs = "allocation")$outputs$allocation
  # a consumer that asks for a matrix the lean step dropped
  local_mocked_bindings(.lean_keep = function(problem, output) character())
  args <- list(model, observed, init = c(sigma = 4, v0 = 8), lower = c(sigma = 1, v0 = 1),
               upper = c(sigma = 9, v0 = 40), output = "allocation")
  for (gradient in c(FALSE, TRUE)) {
    expect_error(do.call(.fit_problem_nfxp, c(args, list(gradient = gradient))),
                 class = "spax_lean_missing")
  }
  expect_error(.fit_problem_multistart(model, observed, rbind(c(sigma = 4, v0 = 8)),
                 lower = c(sigma = 1, v0 = 1), upper = c(sigma = 9, v0 = 40),
                 output = "allocation"), class = "spax_lean_missing")
  # the scan: the same condition from loss_at
  # (outside) output asked, only the inside flow kept: the scan's loss_at reads it
  local_mocked_bindings(.lean_keep = function(problem, output) "flow")
  fo <- .solve_problem(model, theta, requested_outputs = "flow_outside")$outputs$flow_outside
  target <- .bind_problem_target(model, fo, "flow_outside")
  expect_error(
    .allocation_default_search(model, target, "flow_outside", .poisson_loss, list()),
    class = "spax_lean_missing")
})

test_that("only the allow-listed outputs of the corrected CLM run lean", {
  m <- .lean_models()
  p <- m$totals$model
  for (o in c("utilization", "utilization_by_group", "utilization_outside",
              "utilization_outside_by_group")) expect_identical(.lean_keep(p, o), character())
  expect_identical(.lean_keep(p, "flow"), "flow")
  expect_identical(.lean_keep(p, "flow_outside"), "flow_outside")
  for (o in c("access", "outside_share", "allocation", "huff_share", "opportunity",
              "access_by_group")) expect_null(.lean_keep(p, o))
  expect_identical(.lean_keep(m$huff$model, "utilization"), character())
  legacy <- .huff_problem(.lean_case()$inputs, v0 = 5, allocation = "huff_decay")
  expect_null(.lean_keep(legacy, "utilization"))
  # providers whose bind has no `keep`
  expect_identical(.lean_keep(ae_toy_problem(), "utilization"), character())
  expect_null(.lean_keep(ae_toy_problem(model = "sae"), "utilization"))
})

test_that("a bind without `keep` (saved models, other providers) is bound as before", {
  p <- .lean_models()$totals$model
  old <- p
  inner <- p$bind
  old$bind <- function(theta) inner(theta)       # a model saved before `keep`
  step <- .bind_theta(old, c(sigma = 3, v0 = 10), keep = character())
  expect_null(step$kept)
  expect_true(is.function(step$outputs_requested))
  expect_identical(.bind_theta(old, c(sigma = 3, v0 = 10))$map(NULL),
                   .bind_theta(p, c(sigma = 3, v0 = 10))$map(NULL))
  expect_null(.solve_problem(old, c(sigma = 3, v0 = 10), keep = character())$kept)
})

test_that("a lean fit gives the fit of the full path", {
  case <- .lean_case()
  model <- clm_allocation(case$inputs, fit_v0 = TRUE)
  truth <- c(sigma = 3, v0 = 10)
  ev <- .solve_problem(model, truth, requested_outputs = "flow")$outputs
  for (output in c("utilization", "flow")) {
    observed <- if (output == "flow") ev$flow else ev$utilization
    run <- function() suppressWarnings(.fit_problem_nfxp(
      model, observed, init = c(sigma = 4, v0 = 6), lower = c(sigma = 1, v0 = 1),
      upper = c(sigma = 9, v0 = 40), output = output, loss = .poisson_loss,
      loss_grad = .poisson_gradient, loss_args = list(), gradient = TRUE))
    lean <- run()
    # the full path for this output only: the mock ends with the call
    full <- with_mocked_bindings(run(), .lean_keep = function(problem, output) NULL)
    expect_identical(lean$theta, full$theta)
    expect_identical(lean$loss, full$loss)
    expect_identical(lean$optim$counts, full$optim$counts)
    expect_null(lean$equilibrium$kept)
    expect_true(!is.null(lean$outputs$allocation))
  }
})

test_that("the bootstrap does not record the condition as a failed draw", {
  case <- alloc_generated_case()
  fit <- alloc_generated_fit()
  expect_error(with_mocked_bindings(
    .run_allocation_bootstrap(case$model, fit, alloc_bootstrap_observation(),
                              seed = 31L, attempts = 2L),
    .allocation_case_fit = function(...) .lean_missing("allocation", character())),
    class = "spax_lean_missing")
})
