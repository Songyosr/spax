# Static CLM on the b, a, c identity inputs with a fitted outside option, and
# its allocation with two suppressed cells. `rows`/`cols` permute the axes.
.identity_fixture <- function() {
  f <- ae_identity_inputs(5)
  p <- .huff_problem(f$demand, f$supply, f$distance, fit_v0 = TRUE)
  out <- .solve_problem(p, c(sigma = 2, v0 = 6))$outputs
  y <- out$allocation
  dimnames(y) <- list(c("1", "3", "5"), c("b", "a", "c"))
  y[1, 2] <- y[3, 1] <- NA_real_
  list(p = p, y = y, utilization = stats::setNames(out$utilization, c("b", "a", "c")),
       init = c(sigma = 2.2, v0 = 4), lower = c(sigma = .5, v0 = .1),
       upper = c(sigma = 5, v0 = 30), rows = c(3L, 1L, 2L), cols = c(2L, 3L, 1L))
}

# The saved alignment records which input axes were named and the two
# permutations: canonical_to_input reads the input in canonical order and
# input_to_canonical maps it back.
expect_alignment <- function(bound, input, named) {
  a <- bound$alignment
  expect_identical(a$named, named)
  expect_identical(
    unname(input[a$canonical_to_input[[1]], a$canonical_to_input[[2]], drop = FALSE]),
    unname(bound$observed)
  )
  expect_identical(
    unname(bound$observed[a$input_to_canonical[[1]], a$input_to_canonical[[2]],
                          drop = FALSE]),
    unname(input)
  )
  for (k in 1:2) {
    expect_identical(a$input_to_canonical[[k]][a$canonical_to_input[[k]]],
                     seq_len(dim(input)[k]))
  }
}

.identity_fit <- function(f, observed, output = "allocation", ...) {
  .fit_problem_nfxp(f$p, observed, f$init, f$lower, f$upper,
    output = output, gradient = TRUE, control = list(maxit = 50), ...)
}

test_that("named axes align independently and retain nonconsecutive origin keys", {
  f <- .identity_fixture()
  expect_identical(f$p$substrate$origin_ids, c("1", "3", "5"))
  ref <- .bind_problem_target(f$p, f$y, "allocation")
  input <- f$y[f$rows, f$cols]
  perm <- .bind_problem_target(f$p, input, "allocation")
  expect_identical(perm$observed, ref$observed)
  expect_identical(perm$mask, ref$mask)
  expect_identical(perm$values, ref$values)
  expect_alignment(perm, input, c(TRUE, TRUE))
  for (axis in 1:2) {
    partial <- if (axis == 1) f$y[f$rows, , drop = FALSE] else f$y[, f$cols, drop = FALSE]
    dn <- dimnames(partial); dn[3 - axis] <- list(NULL); dimnames(partial) <- dn
    bound <- .bind_problem_target(f$p, partial, "allocation")
    expect_equal(unname(bound$observed), unname(f$y))
    expect_alignment(bound, partial, seq_len(2) == axis)
  }
  unnamed <- unname(f$y)
  positional <- .bind_problem_target(f$p, unnamed, "allocation")
  expect_identical(positional$observed, unnamed)
  expect_identical(positional$mask, as.numeric(!is.na(unnamed)) == 1)
  expect_alignment(positional, unnamed, c(FALSE, FALSE))
})

test_that("single fits canonicalize matrix and facility targets without changing raw outputs", {
  f <- .identity_fixture()
  a <- .identity_fit(f, f$y)
  b <- .identity_fit(f, f$y[f$rows, f$cols])
  expect_equal(a$theta, b$theta, tolerance = 1e-12)
  expect_equal(a$loss, b$loss, tolerance = 1e-12)
  expect_identical(a$observed, b$observed)
  expect_equal(a$predicted, b$predicted, tolerance = 1e-12)
  expect_identical(a$target_mask, b$target_mask)
  expect_equal(a$outputs, b$outputs, tolerance = 1e-12)
  # Fits save the binding's alignment record and, from permuted inputs,
  # share one canonical identity and compare.
  expect_identical(b$target_alignment,
                   .bind_problem_target(f$p, f$y[f$rows, f$cols], "allocation")$alignment)
  expect_no_error(.compare_problem_fits(list(a = a, b = b)))
  y <- f$utilization; y[2] <- NA_real_
  av <- .identity_fit(f, y, "utilization")
  bv <- .identity_fit(f, y[f$cols], "utilization")
  expect_equal(av$theta, bv$theta, tolerance = 1e-12)
  expect_identical(av$observed, bv$observed)
  expect_identical(names(bv$predicted), c("b", "a", "c"))
  expect_identical(bv$target_mask, c(TRUE, FALSE, TRUE))
})

test_that("objective and gradient callbacks receive canonical masks and unchanged loss arguments", {
  f <- .identity_fixture()
  run <- function(y) {
    answers <- list(); seen <- list()
    weights <- seq_len(sum(!is.na(f$y)))
    testthat::local_mocked_bindings(optim = function(par, fn, gr, ...) {
      answers <<- list(loss = fn(par), gradient = gr(par))
      list(par = par, convergence = 0L)
    }, .package = "stats")
    fit <- .fit_problem_nfxp(f$p, y, f$init, f$lower, f$upper, output = "allocation",
      gradient = TRUE, loss_args = list(weights = weights, offset = 4),
      loss = function(predicted, observed, weights, offset) {
        seen[[length(seen) + 1L]] <<- list(observed = observed, weights = weights, offset = offset)
        sum(weights * (predicted - observed)^2) + offset
      }, loss_grad = function(predicted, observed, sensitivity, weights, offset) {
        as.numeric(crossprod(sensitivity, 2 * weights * (predicted - observed)))
      })
    list(answers = answers, seen = seen, fit = fit)
  }
  a <- run(f$y); b <- run(f$y[f$rows, f$cols])
  expect_equal(a$answers, b$answers, tolerance = 1e-12)
  expect_identical(a$seen, b$seen)
  expect_identical(b$seen[[1]]$observed, as.numeric(f$y)[!is.na(as.numeric(f$y))])
  expect_identical(b$seen[[1]]$weights, seq_len(sum(!is.na(f$y))))
  expect_identical(b$fit$evaluation_reuse, a$fit$evaluation_reuse)
})

test_that("multistart and profile results do not depend on observation order", {
  f <- .identity_fixture()
  starts <- rbind(near = f$init, far = c(sigma = 2.7, v0 = 9))
  args <- list(problem = f$p, starts = starts, lower = f$lower, upper = f$upper,
               output = "allocation", gradient = TRUE, control = list(maxit = 50))
  a <- do.call(.fit_problem_multistart, c(args, list(observed = f$y)))
  b <- do.call(.fit_problem_multistart, c(args, list(observed = f$y[f$rows, f$cols])))
  expect_identical(a$table$eligible, b$table$eligible)
  expect_identical(a$best_start, b$best_start)
  expect_equal(a$table$loss, b$table$loss, tolerance = 1e-12)
  expect_equal(a$best$predicted, b$best$predicted, tolerance = 1e-12)
  expect_identical(b$best$target_mask, as.numeric(!is.na(f$y)) == 1)
  observed <- !is.na(f$y)
  expect_equal(as.numeric(a$best$predicted)[observed], as.numeric(f$y)[observed],
               tolerance = 1e-4)
  args <- list(problem = f$p, parameter = "sigma", values = c(low = 1.8, truth = 2),
               init = f$init, lower = f$lower, upper = f$upper,
               output = "allocation", gradient = TRUE, control = list(maxit = 50))
  a <- do.call(.profile_problem, c(args, list(observed = f$y)))
  b <- do.call(.profile_problem, c(args, list(observed = f$y[f$rows, f$cols])))
  expect_identical(a$table$eligible, b$table$eligible)
  expect_identical(a$best_point, b$best_point)
  expect_equal(a$table$loss, b$table$loss, tolerance = 1e-12)
  expect_equal(a$best$predicted, b$best$predicted, tolerance = 1e-12)
  expect_identical(b$best$target_mask, as.numeric(!is.na(f$y)) == 1)
  expect_equal(as.numeric(a$best$predicted)[observed], as.numeric(f$y)[observed],
               tolerance = 1e-4)
})

test_that("wrapper eligibility uses the canonical mask even for nonfinite unobserved predictions", {
  f <- .identity_fixture()
  template <- .identity_fit(f, f$y)
  testthat::local_mocked_bindings(.fit_problem_nfxp = function(problem, observed, init,
                                                            output = "utilization", ...) {
    expect_s3_class(observed, "ae_bound_target")
    result <- template
    result$theta <- init
    result$convergence <- 0L
    result$equilibrium$converged <- TRUE
    result$predicted[!observed$mask] <- NA_real_
    result
  })
  y <- f$y[f$rows, f$cols]
  search <- .fit_problem_multistart(f$p, y, rbind(one = f$init), f$lower, f$upper,
                                   output = "allocation")
  expect_true(search$table$eligible)
  profile <- .profile_problem(f$p, y, "sigma", c(1.8, 2), f$init, f$lower, f$upper,
                              output = "allocation")
  expect_true(all(profile$table$eligible))
})

test_that("wrappers match IDs only before their loops and reject changed bound identity", {
  f <- .identity_fixture()
  original <- .bind_problem_target
  raw_calls <- 0L
  testthat::local_mocked_bindings(.bind_problem_target = function(problem, observed, output) {
    if (!inherits(observed, "ae_bound_target")) raw_calls <<- raw_calls + 1L
    original(problem, observed, output)
  })
  .fit_problem_multistart(f$p, f$y, rbind(one = f$init, two = c(sigma = 2.5, v0 = 8)),
                         f$lower, f$upper, output = "allocation", control = list(maxit = 5))
  expect_equal(raw_calls, 1L)
  raw_calls <- 0L
  .profile_problem(f$p, f$y, "sigma", c(1.8, 2), f$init, f$lower, f$upper,
                    output = "allocation", control = list(maxit = 5))
  expect_equal(raw_calls, 1L)
  bound <- original(f$p, f$y, "allocation")
  fixed <- .fix_problem_parameter(f$p, "sigma", 2)
  expect_identical(original(fixed, bound, "allocation"), bound)
  expect_error(original(f$p, bound, "access"), "identity")
  changed <- f$p; changed$substrate$facility_ids <- rev(changed$substrate$facility_ids)
  expect_error(original(changed, bound, "allocation"), "identity")
})

test_that("invalid named identities fail before any fit or solve", {
  f <- .identity_fixture()
  testthat::local_mocked_bindings(.solve_problem = function(...) stop("unexpected solve"))
  for (bad in list(c("1", "1", "5"), c("1", " ", "5"), c("1", NA, "5"),
                  c("1", "3", "unknown"))) {
    y <- f$y; rownames(y) <- bad
    expect_error(.identity_fit(f, y), "IDs")
    expect_error(.fit_problem_multistart(f$p, y, rbind(one = f$init), f$lower, f$upper,
                                       output = "allocation"), "IDs")
    expect_error(.profile_problem(f$p, y, "sigma", 2, f$init, f$lower, f$upper,
                                  output = "allocation"), "IDs")
  }
  expect_error(.identity_fit(f, f$y[-1, , drop = FALSE]), "dimension")
  expect_error(.identity_fit(f, as.numeric(f$y)), "shape")
  expect_error(.identity_fit(f, f$y, NULL), "output")
  expect_error(.fit_problem_multistart(f$p, f$y, rbind(one = f$init), f$lower, f$upper,
                                     output = NULL), "output")
  expect_error(.profile_problem(f$p, f$y, "sigma", 2, f$init, f$lower, f$upper,
                                output = NULL), "output")
})

test_that("custom providers need explicit identities only for named observations", {
  p <- ae_identity_problem(c("a", "b"), facility_ids = c("f2", "f1"),
    substrate = list(origin_ids = c("r2", "r1")),
    outputs = function(theta) list(custom = theta))
  fit <- function(problem, y) .fit_problem_nfxp(problem, y, c(a = 1.5, b = 2.5),
    c(a = .1, b = .1), c(a = 5, b = 5), output = "custom")
  legacy <- fit(p, c(2, 3))
  expect_lt(legacy$loss, 1e-8)
  expect_null(legacy$target_alignment$axes)
  expect_false(identical(names(legacy$predicted), p$substrate$facility_ids))
  expect_error(fit(p, c(r1 = 3, r2 = 2)), "requires declared")
  p$metadata$spec$output_axes <- c(custom = "origin", utilization = "facility", target = "facility")
  declared <- fit(p, c(r1 = 3, r2 = 2))
  expect_named(declared$predicted, c("r2", "r1"))
  expect_named(declared$observed, c("r2", "r1"))
  expect_lt(declared$loss, 1e-8)
  expect_error(fit(p, c(f1 = 3, f2 = 2)), "exactly match")
})

test_that("singleton matrix axes keep dimensions and old raster keys remain resolvable", {
  f <- .identity_fixture()
  old <- f$p; old$substrate$origin_ids <- NULL
  expect_identical(.bind_problem_target(old, f$y, "allocation")$identity,
                   .bind_problem_target(f$p, f$y, "allocation")$identity)
  for (dims in list(c(1L, 3L), c(3L, 1L))) {
    p <- f$p
    p$substrate$origin_ids <- as.character(seq_len(dims[1]))
    p$substrate$facility_ids <- letters[seq_len(dims[2])]
    y <- matrix(seq_len(prod(dims)), dims[1], dims[2],
      dimnames = list(p$substrate$origin_ids, p$substrate$facility_ids))
    bound <- .bind_problem_target(p, y[rev(seq_len(dims[1])), rev(seq_len(dims[2])), drop = FALSE],
                                  "allocation")
    expect_identical(dim(bound$observed), dims)
    expect_identical(bound$observed, y)
  }
})

test_that("declared output identities are validated", {
  p <- .identity_fixture()$p
  expect_error(.problem_output_identity(p, NA_character_), "length-one")
  # Outputs without a declared axis keep positional identity.
  expect_null(.problem_output_identity(p, "custom")$axes)

  bad <- p
  bad$metadata$spec$output_axes <- c(p$metadata$spec$output_axes, utilization = "facility")
  expect_error(.problem_output_identity(bad, "allocation"), "unique output names")
  bad <- p
  bad$metadata$spec$output_axes[["allocation"]] <- "edge"
  expect_error(.problem_output_identity(bad, "allocation"), "unsupported declared output axis")
  bad <- p
  bad$substrate$facility_ids <- c("b", NA, "c")
  expect_error(.problem_output_identity(bad, "utilization"), "nonmissing")
  bad$substrate$facility_ids <- c("b", "b", "c")
  expect_error(.problem_output_identity(bad, "utilization"), "unique and nonblank")
})

test_that(".fit_target_meta describes vector and matrix targets with NA masks", {
  meta <- .fit_target_meta(c(1, NA, 3))
  expect_null(meta$dim)
  expect_equal(meta$mask, c(TRUE, FALSE, TRUE))
  expect_equal(meta$values, c(1, 3))
  expect_equal(meta$n_observed, 2L)

  m <- matrix(c(1, 2, NA, 4), 2, 2)
  meta_m <- .fit_target_meta(m)
  expect_equal(meta_m$dim, c(2L, 2L))
  expect_equal(meta_m$length, 4L)
  expect_equal(meta_m$values, c(1, 2, 4))

  expect_error(.fit_target_meta("a"), "must be numeric")
  expect_error(.fit_target_meta(array(1, c(2, 2, 2))), "vector or matrix")
  expect_error(.fit_target_meta(c(NA_real_, NA_real_)), "non-missing")
  expect_error(.fit_target_meta(c(1, Inf)), "finite")
  expect_error(.fit_target_meta(numeric(0)), "empty")
})
