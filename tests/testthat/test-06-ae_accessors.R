# Two origins and two facilities, so origin and facility vectors have the same
# length and only declared axes can tell them apart.
.accessor_fixture <- function(n = 2, family = "gaussian") {
  d <- terra::rast(nrows = 1, ncols = n)
  terra::values(d) <- seq_len(n) * 10
  dist <- c(d, d)
  names(dist) <- c("a", "b")
  terra::values(dist) <- cbind(seq_len(n), rev(seq_len(n)) + 0.5)
  list(d = d, dist = dist, S = c(a = 30, b = 20), family = family)
}

# The static CLM and the ratio-feedback model on the accessor fixture.
.accessor_cases <- function(f = .accessor_fixture()) {
  huff <- .huff_problem(f$d, f$S, f$dist)
  list(
    huff = list(problem = huff, theta = c(sigma = 2)),
    ratio = list(problem = .ratio_problem(huff$substrate),
                 theta = c(sigma = 2, v0 = 10, feedback_scale = 1.5))
  )
}

test_that("declared axes disambiguate coincident origin and facility lengths", {
  for (case in .accessor_cases()) {
    p <- case$problem
    theta <- case$theta
    state <- p$state$init
    out <- .problem_outputs_at(p, theta, state)
    # Declared axes include optional outputs even when the default omits them.
    expect_setequal(names(p$metadata$spec$output_axes), union(names(out), "flow"))
    expect_setequal(.problem_available_surfaces(p, out), c("access", "outside_share"))
    expect_error(.problem_output_surface(p, theta, state, "utilization"),
                 "not an origin-side")
    expect_error(.problem_output_surface(p, theta, state, "target"),
                 "not an origin-side")
    expect_s4_class(.problem_output_surface(p, theta, state, "access"), "SpatRaster")
  }
})

test_that("origin surfaces cover retained cells and leave the rest missing", {
  p <- ae_toy_problem(demand = c(10, 0, 30, 40))
  theta <- c(sigma = 2)
  solved <- .solve_problem(p, theta)
  outputs <- .problem_outputs_at(p, theta, solved$x_star)

  expect_equal(outputs$utilization, solved$outputs$utilization)
  values <- terra::values(.problem_output_surface(p, theta, solved$x_star, "access"))[, 1]
  expect_true(is.na(values[2])) # valid zero demand, not a prediction
  expect_equal(values[p$substrate$demand_kept_index], outputs$access)
  expect_error(.problem_output_surface(p, theta, solved$x_star, "missing"),
               "requested output")
  expect_error(.problem_output_surface(p, theta, solved$x_star, NA_character_),
               "length-one")
  expect_error(.problem_available_surfaces(p, 1), "list")
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
  expect_no_error(summary(fit))
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

test_that("CLM access is a bounded inside probability mass", {
  f <- .accessor_fixture()
  cases <- .accessor_cases(f)
  # Power decay over distances below one unit gives kernel weights above one;
  # the CLM denominator still bounds access (unlike HAAE, see its tests).
  terra::values(f$dist) <- 0.5
  cases$power <- list(
    problem = .huff_problem(f$d, f$S, f$dist, family = "power", v0 = 10),
    theta = c(sigma = 1)
  )
  for (case in cases) {
    p <- case$problem
    out <- .problem_outputs_at(p, case$theta, p$state$init)
    expect_true(all(out$access >= 0 & out$access <= 1 + 1e-9))
    expect_equal(out$access, rowSums(out$allocation), tolerance = 1e-10)
  }
  expect_equal(out$access + out$outside_share, rep(1, 2))
})
