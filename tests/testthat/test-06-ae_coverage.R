.coverage_fixture <- function() {
  demand <- terra::rast(nrows = 2, ncols = 3)
  terra::values(demand) <- c(10, 30, 5, 0, NA, 20)
  distance <- c(demand, demand, demand)
  names(distance) <- c("a", "b", "c")
  terra::values(distance) <- cbind(c(1, 4, NA, 2, 3, 2),
                                   c(3, 1, NA, 3, 2, 5), rep(NA_real_, 6))
  p <- .huff_problem(demand, c(a = 100, b = 20, c = 50), distance, v0 = 20)
  list(problem = p, theta = c(sigma = 2), demand = demand, distance = distance)
}

test_that("coverage matches independent choice, ratio, caps and conservation", {
  f <- .coverage_fixture()
  x <- .problem_coverage(f$problem, f$theta, norm = 3, units = "sessions/week per patient")
  D <- c(10, 30, 5, 20)
  S <- c(100, 20, 50)
  dist <- terra::values(f$distance)[c(1, 2, 3, 6), ]
  score <- sweep(exp(-dist^2 / 8), 2, S, "*")
  score[is.na(score)] <- 0
  P <- score / (rowSums(score) + 20)
  U <- as.vector(crossprod(P, D))
  r <- ifelse(U > 0, S / U, 0)
  A <- as.vector(P %*% r)
  expect_equal(x$facilities$r, r)
  expect_equal(x$cells$A, A)
  expect_equal(x$cells$rho, rowSums(P))
  expect_equal(x$cells$E_post, pmin(A / 3, 1))
  expect_equal(x$cells$E_fac, as.vector(P %*% pmin(r / 3, 1)))
  expect_true(all(x$cells$E_post >= x$cells$E_fac - 1e-14))
  expect_equal(sum(D * A), sum(S[U > 0]), tolerance = 1e-12)
  expect_true(x$conservation$passed)
  expect_equal(x$conservation$excluded_facilities, 1)
  expect_equal(x$cells$rho[3], 0)
  expect_true(is.na(x$cells$abar[3]))
  # A genuinely inconsistent spread operator violates conservation.
  mismatch_A <- as.vector((P^2) %*% r)
  expect_gt(abs(sum(D * mismatch_A) - sum(S[U > 0])), 1)
  expect_equal(x$regional$abar_raw, sum(D * A) / sum(D * rowSums(P)))
  expect_false("allocation" %in% names(x))
})

test_that("fit extraction reuses outputs without solving or extracting raster data", {
  f <- .coverage_fixture()
  truth <- .solve_problem(f$problem, f$theta)$outputs$utilization
  fit <- .fit_problem_nfxp(f$problem, truth, init = c(sigma = 1.8),
                           lower = c(sigma = 1), upper = c(sigma = 3))
  reference <- .problem_coverage(f$problem, fit$theta, norm = 3)
  testthat::local_mocked_bindings(
    .solve_problem = function(...) stop("unexpected solve"),
    .bind_theta = function(...) stop("unexpected bind"),
    .rewrap_cells = function(...) stop("unexpected raster construction"),
    .interaction_substrate = function(...) stop("unexpected extraction"))
  x <- .ae_coverage(fit, norm = 3)
  expect_equal(x$cells, reference$cells)
  expect_equal(.coverage_aggregate(x), reference$regional)
  fit$outputs$utilization[1] <- fit$outputs$utilization[1] * 2
  expect_error(.ae_coverage(fit), "inconsistent")
  fit$coverage_meta <- NULL
  expect_error(.ae_coverage(fit), "regenerate")
})

test_that("zone aggregation aligns IDs and applies floors after raw aggregation", {
  f <- .coverage_fixture()
  x <- .problem_coverage(f$problem, f$theta, norm = 3, rho_floor = 0.05,
                         contact_need_floor = 5)
  zones <- data.frame(cell = c(6, 3, 2, 1, 4, 5), zone = c("b", "b", "a", "a", "z", "z"))
  z <- .coverage_aggregate(x, zones)
  expect_equal(z$A, z$rho * z$abar_raw, tolerance = 1e-12)
  expect_equal(sum(z$demand * z$A), sum(x$cells$demand * x$cells$A))
  expected <- with(x$cells[1:2, ], sum(demand * A) / sum(demand * rho))
  expect_equal(z$abar_raw[z$zone == "a"], expected)
  wrong <- with(x$cells[1:2, ], weighted.mean(abar_raw, demand))
  expect_gt(abs(wrong - expected), 0.001)
  expect_equal(.coverage_aggregate(x, zones[6:1, ]), z)
  expect_error(.coverage_aggregate(x, zones[-4, ]), "every retained")
  zones$cell[1] <- zones$cell[2]
  expect_error(.coverage_aggregate(x, zones), "unique cell")
  floored <- .problem_coverage(f$problem, f$theta, norm = 3,
                               rho_floor = 1, contact_need_floor = 1000)
  expect_true(all(floored$cells$masked))
  expect_true(all(is.na(floored$cells$abar)))
  expect_equal(floored$cells$A, x$cells$A)
  expect_equal(floored$regional$A, x$regional$A)
  expect_true(floored$regional$masked)
  expect_equal(sum(summary(floored)$quadrant_shares$share), 1)
  expect_equal(summary(floored)$quadrant_shares$quadrant, "unclassified")
  at_floor <- .problem_coverage(f$problem, f$theta, norm = 3,
    rho_floor = x$regional$rho, contact_need_floor = x$regional$contact_need)
  expect_false(at_floor$regional$masked)
})

test_that("zero supply, zero load and unknown observations remain distinct", {
  f <- .coverage_fixture()
  p <- .huff_problem(f$demand, c(a = 100, b = 0, c = 50), f$distance, v0 = 20)
  x <- .problem_coverage(p, f$theta, norm = 3, observed = c(c = NA, b = 0, a = 50))
  expect_equal(x$facilities$r[2:3], c(0, 0))
  expect_equal(x$facilities$r_plugin, c(2, NA, NA))
  expect_equal(x$facilities$observed, c(50, 0, NA))
  expect_equal(x$conservation$supported_supply, 100)
  expect_error(.problem_coverage(p, f$theta, observed = c(a = 1, b = 2, z = 3)), "facility IDs")
  expect_error(.problem_coverage(p, f$theta, observed = 1:3), "named vector")
  expect_error(.problem_coverage(p, f$theta, norm = 0), "norm")
  expect_error(.problem_coverage(p, f$theta, rho_floor = 2), "rho_floor")
  legacy <- .huff_problem(f$demand, c(a = 100, b = 20, c = 50), f$distance,
                          allocation = "huff_decay")
  expect_error(.problem_coverage(legacy, f$theta), "static CLM")
})

test_that("methods and lazy maps preserve fit support, including unreachable demand", {
  f <- .coverage_fixture()
  x <- .problem_coverage(f$problem, f$theta, norm = 3, units = "sessions/week")
  expect_match(format(x), "sessions/week")
  expect_output(print(x), "conservation: PASS")
  expect_s3_class(summary(x), "summary.ae_coverage")
  expect_output(print(summary(x)), "Quadrant shares")
  map <- .coverage_surface(x, "rho")
  expect_true(terra::compareGeom(map, f$demand))
  expect_equal(which(is.na(terra::values(map)[, 1])), c(4L, 5L))
  expect_equal(unname(terra::values(map)[3, 1]), 0)
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({grDevices::dev.off(); unlink(path)}, add = TRUE)
  for (type in c("coverage", "contact", "adequacy", "quadrant", "all")) {
    expect_invisible(plot(x, type = type))
  }
  empty <- .problem_coverage(f$problem, f$theta, rho_floor = 1)
  expect_invisible(plot(empty, type = "all"))
})

test_that("all-zero opportunity has finite zero intensity and no conditional report", {
  f <- .coverage_fixture()
  p <- .huff_problem(f$demand, c(a = 0, b = 0, c = 0), f$distance, v0 = 20)
  x <- .problem_coverage(p, f$theta, norm = 3, observed = c(a = NA, b = 0, c = NA))
  expect_equal(x$cells$A, rep(0, 4))
  expect_equal(x$cells$rho, rep(0, 4))
  expect_true(all(is.na(x$cells$abar)))
  expect_true(x$conservation$passed)
  expect_true(all(is.na(summary(x)$ratio)))
  expect_true(all(is.na(summary(x)$plugin_gap)))
  expect_equal(summary(x)$quadrant_shares$share, 1)
})

test_that("classification cuts use explicit inclusive thresholds", {
  settings <- list(rho_floor = 0.05, contact_need_floor = 5,
                   contact_cut = 0.7, adequacy_cut = 3)
  tab <- data.frame(demand = rep(100, 4), rho = c(0.7, 0.6, 0.7, 0.6),
                     A = c(2.1, 1.8, 1.4, 1.2))
  out <- .coverage_reporting(tab, settings, aggregate = TRUE)
  expect_equal(out$quadrant, c("adequate", "contact-limited", "capacity-limited", "both-low"))
  expect_equal(out$A, out$rho * out$abar_raw)
})
