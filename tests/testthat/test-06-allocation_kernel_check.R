# The finite plan (SPAX-061 T1) makes the strict validity check cheap without
# changing any weight or refusal message.

kc_sub <- function(d, policy = "strict") {
  list(distance_active = d, input_policy = list(kernel_domain = policy))
}
kc_run <- function(sub, family, sigma, planned) {
  fp <- if (planned) spax:::.finite_plan(sub$distance_active)
  tryCatch(
    spax:::.allocation_kernel(sub, family, sigma, finite_plan = fp),
    error = function(e) paste("ERR:", conditionMessage(e))
  )
}

test_that("a finite plan returns identical weights for the built-in families", {
  withabsent <- matrix(c(1, 2, NA, Inf, 5, 6), 2)
  allfinite <- matrix(c(1, 2, 3, 4, 5, 6), 2)
  empty <- matrix(numeric(), 0, 3)
  for (d in list(withabsent, allfinite, empty)) {
    for (fam in c("gaussian", "exponential", "power", "none")) {
      sub <- kc_sub(d)
      expect_identical(kc_run(sub, fam, 3, TRUE), kc_run(sub, fam, 3, FALSE))
    }
  }
  expect_identical(kc_run(kc_sub(withabsent), "gaussian", 3, TRUE)[1, 2], 0)
})

test_that("the refusals are the same with and without the plan", {
  d <- matrix(c(1, 2, NA, Inf, 5, 6), 2)
  dz <- d
  dz[1] <- 0
  dt <- d
  dt[1] <- 1e-320
  cases <- list(
    list(kc_sub(dz), "power", 1.5),
    list(kc_sub(d), "gaussian", Inf),
    list(kc_sub(d), "gaussian", -1),
    list(kc_sub(dt), "power", 2),
    list(kc_sub(d), "exponential", NaN)
  )
  for (cs in cases) {
    a <- kc_run(cs[[1]], cs[[2]], cs[[3]], TRUE)
    b <- kc_run(cs[[1]], cs[[2]], cs[[3]], FALSE)
    expect_identical(a, b)
  }
  expect_match(kc_run(kc_sub(dz), "power", 1.5, TRUE), "strictly positive finite")
  expect_match(kc_run(kc_sub(dt), "power", 2, TRUE), "invalid on finite travel")
})

test_that("the non-strict path gives the same weights", {
  mixed <- matrix(c(1, 2, NA, Inf, 5, 6), 2)
  allfinite <- matrix(c(1, 2, 3, 4, 5, 6), 2)
  ref <- function(sub, fam) {
    K <- spax:::.decay_weights(fam, NULL, sub$distance_active, 3)
    K[!is.finite(K)] <- 0
    K
  }
  for (d in list(mixed, allfinite)) {
    for (fam in c("gaussian", "exponential", "power", "none")) {
      sub <- kc_sub(d, "lenient")
      expect_identical(spax:::.allocation_kernel(sub, fam, 3), ref(sub, fam))
    }
  }
})
