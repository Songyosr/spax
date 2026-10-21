# The first printed line of the objects of the public workflow (SPAX-060 E9, V5), and
# the legacy objects that keep their old headers. A header is a plain description, not
# the name of a class; it applies to the corrected conditional logit models (internal
# model "huff", allocation form "clm") and to the FCA results.

.first <- function(x, n = 1L, ...) utils::capture.output(print(x, ...))[seq_len(n)]

test_that("a fit of the public workflow prints a plain header and its model", {
  f <- alloc_small_case()
  fit <- alloc_small_fit()
  # a multistart fit: the number of starts and the selected one
  expect_identical(.first(fit), paste0("<allocation fit> 2 starts, selected ", fit$best_start))
  expect_identical(.first(summary(fit)), "<allocation fit summary>")
  # the selected fit: a fitted outside weight is a conditional logit
  expect_identical(.first(fit$best, 2L), c("<allocation fit result>",
                                           "model: conditional logit"))
  # one start is "1 start"
  one <- fit_allocation(f$model, f$observed, f$starts[1, , drop = FALSE], f$lower, f$upper,
                        loss = "poisson", control = list(maxit = 100))
  expect_identical(.first(one), paste0("<allocation fit> 1 start, selected ", one$best_start))
  # no fitted v0 and a snapshot with v0 = 0 is a Huff model
  huff <- huff_allocation(f$inputs, family = "gaussian", fit_beta = FALSE)
  hfit <- fit_allocation(huff, f$observed, matrix(c(1, 3), 2, 1, dimnames = list(NULL, "sigma")),
                         c(sigma = 0.3), c(sigma = 20), loss = "poisson",
                         control = list(maxit = 100))
  expect_false("v0" %in% names(hfit$best$theta))
  expect_identical(hfit$best$prediction_snapshot$spec$v0, 0)
  expect_identical(.first(hfit$best, 2L), c("<allocation fit result>", "model: Huff"))
  # a held outside weight above zero is a conditional logit, not a Huff model
  held <- clm_allocation(f$inputs, fit_v0 = FALSE, v0 = 5)
  heldfit <- fit_allocation(held, f$observed, matrix(c(1, 3), 2, 1,
                              dimnames = list(NULL, "sigma")),
                            c(sigma = 0.3), c(sigma = 20), loss = "poisson",
                            control = list(maxit = 100))
  expect_false("v0" %in% names(heldfit$best$theta))
  expect_identical(.first(heldfit$best, 2L), c("<allocation fit result>",
                                               "model: conditional logit"))
  # a fit without a snapshot: a fitted v0 is still a conditional logit, otherwise the model
  # cannot be named
  nosnap <- fit$best
  nosnap$prediction_snapshot <- NULL
  expect_identical(.first(nosnap, 2L)[2L], "model: conditional logit")
  nosnap_huff <- hfit$best
  nosnap_huff$prediction_snapshot <- NULL
  expect_identical(.first(nosnap_huff, 2L)[2L], "model: allocation model")
  # the summary of the selected fit has the plain header and the same model line
  expect_identical(.first(summary(fit$best), 2L), c("<allocation fit result summary>",
                                                    "model: conditional logit"))
  expect_identical(.first(summary(hfit$best), 2L)[2L], "model: Huff")
  # a summary saved before the label was stored keeps the internal label
  old_summary <- summary(fit$best)
  old_summary$model_label <- NULL
  expect_identical(.first(old_summary, 2L), c("<allocation fit result summary>", "model: clm"))
})

test_that("a search with no selected start prints its header and summary header", {
  f <- alloc_small_case()
  failed <- alloc_failed_search(f)
  expect_identical(failed$status, "no_valid_fit")
  expect_identical(failed$model, "huff")
  expect_identical(failed$allocation_form, "clm")
  expect_identical(.first(failed), "<allocation fit> 2 starts, none selected")
  expect_identical(.first(summary(failed)), "<allocation fit summary>")
  # a search saved before the two fields were kept, and with no selected fit, has no
  # way to be told apart and prints the old header
  old <- failed
  old$model <- old$allocation_form <- NULL
  expect_identical(.first(old), "<ae_multistart_fit>")
  expect_identical(.first(summary(old)), "<summary.ae_multistart_fit>")
  # one saved before the two fields were kept prints the new header from its selected fit
  fit <- alloc_small_fit()
  older <- fit
  older$model <- older$allocation_form <- NULL
  expect_identical(.first(older), paste0("<allocation fit> 2 starts, selected ", fit$best_start))
  expect_identical(.first(summary(older)), "<allocation fit summary>")
})

test_that("evaluations and coverage of the public workflow print plain headers", {
  f <- alloc_small_case()
  e <- evaluate_allocation(f$model, f$truth)
  expect_identical(.first(e), "<allocation evaluation>")
  expect_match(.first(e$coverage), "^<allocation coverage> gaussian \\(sigma=3, v0=10\\): ")
  expect_match(.first(summary(e$coverage)), "^<allocation coverage> gaussian ")
  huff <- huff_allocation(f$inputs, family = "gaussian", fit_beta = FALSE)
  eh <- evaluate_allocation(huff, c(sigma = 3))
  expect_identical(.first(eh), "<allocation evaluation>")
  expect_match(.first(eh$coverage), "^<allocation coverage> gaussian \\(sigma=3\\): ")
  # the evaluation of a fit
  expect_identical(.first(alloc_small_fit()$best$equilibrium), "<allocation evaluation>")
})

test_that("checks, comparisons and bootstraps print plain headers", {
  f <- alloc_small_case()
  fit <- alloc_small_fit()
  expect_identical(.first(check_allocation(f$model, fit)), "<allocation check>")
  expo_model <- clm_allocation(f$inputs, family = "exponential", fit_v0 = TRUE)
  expo <- fit_allocation(expo_model, f$observed, rbind(a = c(sigma = 0.5, v0 = 10)),
                         c(sigma = 0.01, v0 = 0.1), c(sigma = 5, v0 = 200),
                         loss = "poisson", control = list(maxit = 100))
  cmp <- compare_allocations(list(gauss = fit, expo = expo),
                             models = list(gauss = f$model, expo = expo_model))
  expect_identical(.first(cmp), "<allocation comparison> reference: gauss")
  boot <- saved_bootstrap_stub()
  expect_identical(.first(boot), "<allocation bootstrap>")
  expect_identical(.first(summary(boot)), "<allocation bootstrap>")
  expect_identical(.first(saved_bootstrap_stub(refused = TRUE)), "<allocation bootstrap>")
})

test_that("the legacy objects keep their headers", {
  # SAE and HAAE: solutions, fits, searches and their summaries
  for (m in c("sae", "haae")) {
    p <- ae_toy_problem(m)
    eq <- .solve_problem(p, c(sigma = 2), lambda = 0.7, tol = 1e-10, max_iter = 500)
    expect_identical(.first(eq), "<ae_equilibrium>", info = m)
    fit <- ae_recovery_fit(p)
    expect_identical(.first(fit, 2L), c("<ae_problem_nfxp_fit>", paste0("model: ", m)),
                     info = m)
    expect_identical(.first(summary(fit), 2L),
                     c("<summary.ae_problem_nfxp_fit>", paste0("model: ", m)), info = m)
    starts <- matrix(c(1.5, 3), 2, 1, dimnames = list(NULL, "sigma"))
    ms <- .fit_problem_multistart(p, eq$outputs$utilization, starts, 0.5, 4, lambda = 0.7,
                                  tol = 1e-10, max_iter = 500, control = list(maxit = 20))
    expect_identical(.first(ms), "<ae_multistart_fit>", info = m)
    expect_identical(.first(summary(ms)), "<summary.ae_multistart_fit>", info = m)
    # also a search with no selected start
    none <- .fit_problem_multistart(p, eq$outputs$utilization, starts, 0.5, 4,
                                    loss = function(predicted, observed) stop("failed"))
    expect_null(none$best)
    expect_identical(.first(none), "<ae_multistart_fit>", info = m)
    expect_identical(.first(summary(none)), "<summary.ae_multistart_fit>", info = m)
  }
  # the historical huff_decay allocation
  f <- alloc_huff_case()
  p <- .huff_problem(f$inputs, v0 = 4, allocation = "huff_decay")
  eq <- .solve_problem(p, c(sigma = 2))
  expect_identical(.first(eq), "<ae_equilibrium>")
  fit <- .fit_problem_nfxp(p, observed = eq$outputs$utilization, init = c(sigma = 2.5),
                           lower = c(sigma = 0.5), upper = c(sigma = 30))
  expect_identical(.first(fit, 2L), c("<ae_problem_nfxp_fit>", "model: huff"))
  # the ratio-response model
  r <- ae_ratio_problem(fix = c("v0", "feedback_scale"))
  expect_identical(.first(.solve_problem(r, c(sigma = 2))), "<ae_equilibrium>")
  expect_identical(.first(ae_recovery_fit(r), 2L), c("<ae_problem_nfxp_fit>", "model: ratio"))
})

test_that("an FCA result prints its type, layers and grid", {
  td <- fca_toy()
  decay <- list(method = "gaussian", sigma = 2)
  # one layer, and two
  one <- spax_e2sfca(td$demand, td$supply_df, td$distance, decay_params = decay,
                     id_col = "id", supply_cols = "doctors")
  expect_identical(.first(one), "<spax access> E2SFCA, 1 layer on 3 x 3 cells")
  expect_identical(.first(summary(one)), "<spax access summary> E2SFCA")
  two <- spax_3sfca(td$demand, td$supply_df, td$distance, decay_params = decay,
                    id_col = "id", supply_cols = c("doctors", "nurses"))
  expect_identical(.first(two), "<spax access> 3SFCA, 2 layers on 3 x 3 cells")
  expect_identical(.first(summary(two)), "<spax access summary> 3SFCA")
  # the result of spax() on the same data
  d <- prep_data(td$demand, td$supply_df, td$distance, id_col = "id", supply_cols = "doctors")
  expect_identical(.first(spax(d, spec_e2sfca(decay_gaussian(2)))),
                   "<spax access> E2SFCA, 1 layer on 3 x 3 cells")
  # the grid is shown as it is: no separator and no scientific notation
  big <- terra::rast(nrows = 100000, ncols = 2, vals = 1, nlyrs = 1)
  fake <- one
  fake$accessibility <- big
  expect_identical(.first(fake), "<spax access> E2SFCA, 1 layer on 100000 x 2 cells")
})
