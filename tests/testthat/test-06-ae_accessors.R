.accessor_fixture <- function(n = 2, family = "gaussian") {
  d <- terra::rast(nrows = 1, ncols = n)
  terra::values(d) <- seq_len(n) * 10
  dist <- c(d, d)
  names(dist) <- c("a", "b")
  terra::values(dist) <- cbind(seq_len(n), rev(seq_len(n)) + 0.5)
  list(d = d, dist = dist, S = c(a = 30, b = 20), family = family)
}

test_that("declared axes disambiguate coincident origin and facility lengths", {
  f <- .accessor_fixture()
  for (model in c("sae", "haae", "huff")) {
    ctor <- get(paste0(".", model, "_problem"))
    p <- ctor(f$d, f$S, f$dist)
    state <- p$state$init
    out <- .problem_outputs_at(p, c(sigma = 2), state)
    expect_setequal(names(p$metadata$spec$output_axes), names(out))
    surfaces <- .problem_available_surfaces(p, out)
    expected <- switch(model, sae = "access", haae = c("access", "pooled"),
                       huff = c("access", "outside_share"))
    expect_setequal(surfaces, expected)
    expect_error(.problem_output_surface(p, c(sigma = 2), state, "utilization"),
                  "not an origin-side")
    expect_error(.problem_output_surface(p, c(sigma = 2), state, "target"),
                  "not an origin-side")
    expect_s4_class(.problem_output_surface(p, c(sigma = 2), state, "access"), "SpatRaster")
  }
})

test_that("saved accessors preserve values and shapes without solving or mapping", {
  f <- .accessor_fixture()
  p <- .huff_problem(f$d, f$S, f$dist, v0 = 10)
  truth <- .solve_problem(p, c(sigma = 2))$utilization
  fit <- .fit_problem_nfxp(p, truth, init = c(sigma = 1.8),
                           lower = c(sigma = 1), upper = c(sigma = 3))
  expect_identical(fit$output_axes, p$metadata$spec$output_axes)
  expect_error(.fit_output_surface(fit, "utilization"), "not an origin-side")
  testthat::local_mocked_bindings(
    .solve_problem = function(...) stop("unexpected solve"),
    .bind_theta = function(...) stop("unexpected bind"),
    .rewrap_cells = function(...) stop("unexpected map"))
  expect_identical(.fit_output(fit, "allocation"), fit$outputs$allocation)
  expect_identical(.fit_output_vector(fit, "utilization"), fit$outputs$utilization)
  expect_setequal(.fit_available_surfaces(fit), c("access", "outside_share"))
  expect_error(.fit_output_vector(fit, "allocation"), "not a numeric vector")
  expect_error(.fit_output(fit, "missing"), "requested output")
  expect_error(.fit_output(fit, NA_character_), "nonmissing")
  expect_error(.fit_output_surface(fit, NA_character_), "character")
})

test_that("facility table does not relabel origin targets as facility loads", {
  f <- .accessor_fixture(3)
  p <- .huff_problem(f$d, f$S, f$dist, v0 = 10)
  truth <- .solve_problem(p, c(sigma = 2))$outputs$access
  fit <- .fit_problem_nfxp(p, truth, init = c(sigma = 1.8),
                           lower = c(sigma = 1), upper = c(sigma = 3), output = "access")
  tab <- .ae_fit_facility_table(fit)
  expect_equal(nrow(tab), 2)
  expect_equal(tab$facility_id, c("a", "b"))
  expect_equal(tab$predicted, unname(fit$outputs$utilization))
  expect_false(any(c("observed", "residual") %in% names(tab)))
  expect_length(.fit_output_vector(fit, "access"), 3)
})

test_that("generic by-name surfaces honor custom declarations and preserve legacy fallback", {
  f <- .accessor_fixture()
  out <- list(custom = c(0.2, 0.5), facility_diagnostic = c(100, 200))
  axes <- c(custom = "origin", facility_diagnostic = "facility")
  expect_equal(.surface_output_names(out, 2, axes), "custom")
  expect_s4_class(.rewrap_problem_surface(out, "custom", f$d, 1:2, axes), "SpatRaster")
  expect_error(.rewrap_problem_surface(out, "facility_diagnostic", f$d, 1:2, axes),
                "not an origin-side")
  expect_equal(.surface_output_names(out, 2), names(out))
})

test_that("output semantics reflect actual bounded and unbounded model cases", {
  f <- .accessor_fixture()
  for (model in c("sae", "haae", "huff")) {
    p <- get(paste0(".", model, "_problem"))(f$d, f$S, f$dist)
    out <- .problem_outputs_at(p, c(sigma = 2), p$state$init)
    expect_true(all(out$access >= 0 & out$access <= 1 + 1e-9))
    expect_equal(out$access, rowSums(out$allocation), tolerance = 1e-10)
  }
  # HAAE/Huff-decay inherit weights >1 for power distances below one unit.
  terra::values(f$dist) <- 0.5
  h <- .haae_problem(f$d, f$S, f$dist, family = "power")
  ho <- .problem_outputs_at(h, c(sigma = 1), h$state$init)
  expect_true(all(ho$access > 1))
  expect_true(all(ho$pooled > ho$access))
  clm <- .huff_problem(f$d, f$S, f$dist, family = "power", v0 = 10)
  co <- .problem_outputs_at(clm, c(sigma = 1), clm$state$init)
  expect_equal(co$access + co$outside_share, rep(1, 2))
  # SAE's smooth cap tends to 1 + log1p(exp(-beta))/beta, not exactly 1.
  expect_gt(.sae_soft_cap(100, beta = 1), 1)
  expect_equal(.sae_soft_cap(100, beta = 1), 1 + log1p(exp(-1)))
})
