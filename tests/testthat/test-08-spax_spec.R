# Decay objects (decay_*()) and model specifications (spec_clm(), spec_huff()):
# validation messages and their order, result structures and printed lines. These
# objects hold no data, so nothing here needs prepared inputs. Printed output is
# compared as exact lines rather than with snapshots.

decay_text <- c(
  decay = paste0("decay must be decay_gaussian(), decay_exponential(), decay_power(), ",
                 "decay_custom(), a decay_kernel() or one of \"gaussian\", ",
                 "\"exponential\", \"power\""),
  sigma = "sigma must be a positive number or \"fit\"",
  list = "decay by group must be a named list with one decay per group",
  mixed = paste0("to mix fitted and held v0 by group, give a named list: ",
                 "outside = list(young = \"fit\", old = 0)"),
  outside = "outside must be \"fit\", a number, or a named list or vector by group",
  beta = "beta must be a positive number or \"fit\"",
  kappa = "kappa must be a positive number",
  clm_by = "by_group can contain \"sigma\" and \"v0\"",
  huff_by = "by_group can contain \"sigma\"; the Huff model has no outside option",
  v0_needs = "by_group = \"v0\" needs outside = \"fit\"",
  held = paste0("a held sigma cannot differ by group; give the decay by group, for ",
                "example list(young = decay_gaussian(5), old = decay_gaussian(8))")
)

expect_message_exact <- function(object, message) {
  expect_error(object, message, fixed = TRUE)
}

# A declared kernel with bounds on sigma.
bounded_kernel <- function(lower = 1, upper = 10) {
  decay_kernel(function(distance, sigma) exp(-distance / sigma),
               name = "simple", parameter = "scale", lower = lower, upper = upper)
}

# decay objects ------------------------------------------------------------------

test_that("the built-in constructors return a spax_decay with the built-in kernel", {
  for (family in c("gaussian", "exponential", "power")) {
    constructor <- get(paste0("decay_", family))
    fit <- constructor()
    expect_s3_class(fit, "spax_decay")
    expect_identical(names(fit), c("kernel", "sigma"))
    expect_identical(fit$kernel, decay_kernel(family))
    expect_identical(fit$sigma, "fit")
    held <- constructor(2.5)
    expect_identical(held$kernel, decay_kernel(family))
    expect_identical(held$sigma, 2.5)
    expect_identical(constructor(sigma = 2.5), held)
  }
})

test_that("an integer sigma is stored as a double without names", {
  expect_identical(decay_gaussian(5L)$sigma, 5)
  expect_identical(decay_gaussian(c(a = 5))$sigma, 5)
})

test_that("sigma is exactly \"fit\" or a single finite number above 0", {
  bad <- list("Fit", "fits", c("fit", "fit"), NA, NA_real_, NULL, TRUE, 0, -1,
              Inf, -Inf, NaN, c(1, 2), numeric(), "5", factor("fit"), list(1),
              character())
  for (constructor in list(decay_gaussian, decay_exponential, decay_power)) {
    for (value in bad) {
      expect_message_exact(constructor(value), decay_text[["sigma"]])
    }
  }
  expect_message_exact(decay_custom(bounded_kernel(), 0), decay_text[["sigma"]])
  expect_message_exact(decay_custom(bounded_kernel(), "5"), decay_text[["sigma"]])
})

test_that("a held sigma lies within the bounds of its kernel, inclusive", {
  kernel <- bounded_kernel(1, 10)
  expect_identical(decay_custom(kernel, 1)$sigma, 1)
  expect_identical(decay_custom(kernel, 10)$sigma, 10)
  expect_identical(decay_custom(kernel, 5.5)$sigma, 5.5)
  expect_message_exact(decay_custom(kernel, 0.5),
                       "sigma must be from 1 to 10 for the simple kernel")
  expect_message_exact(decay_custom(kernel, 10.5),
                       "sigma must be from 1 to 10 for the simple kernel")
  expect_message_exact(decay_custom(bounded_kernel(2, Inf), 1),
                       "sigma must be from 2 to Inf for the simple kernel")
  expect_message_exact(decay_custom(bounded_kernel(0.25, 0.75), 1),
                       "sigma must be from 0.25 to 0.75 for the simple kernel")
  # The format check comes before the bounds check.
  expect_message_exact(decay_custom(kernel, -1), decay_text[["sigma"]])
  expect_identical(decay_custom(kernel)$sigma, "fit")
  expect_identical(decay_custom(kernel)$kernel, kernel)
  # The built-in kernels allow any positive number.
  expect_identical(decay_gaussian(1e-9)$sigma, 1e-9)
  expect_identical(decay_exponential(1e9)$sigma, 1e9)
})

test_that("decay_custom() needs a declared kernel", {
  msg <- "kernel must be a decay_kernel()"
  expect_message_exact(decay_custom("gaussian"), msg)
  expect_message_exact(decay_custom(function(distance, sigma) 1), msg)
  expect_message_exact(decay_custom(NULL), msg)
  expect_message_exact(decay_custom(decay_gaussian()), msg)
  expect_message_exact(decay_custom(list(name = "x")), msg)
  expect_error(decay_custom(), "kernel")
  # The kernel is checked before sigma.
  expect_message_exact(decay_custom("gaussian", sigma = -1), msg)
})

test_that("decay_custom() keeps the declaration unchanged", {
  kernel <- bounded_kernel()
  fit <- decay_custom(kernel, sigma = 4)
  expect_s3_class(fit, "spax_decay")
  expect_identical(names(fit), c("kernel", "sigma"))
  expect_identical(fit$kernel, kernel)
  expect_s3_class(fit$kernel, "spax_decay_kernel")
  # A built-in declaration made through decay_kernel() is the same as decay_*().
  expect_identical(decay_custom(decay_kernel("power")), decay_power())
  expect_identical(decay_custom(decay_kernel("gaussian"), 3), decay_gaussian(3))
})

test_that(".as_spax_decay() accepts a decay, a kernel and an exact built-in name", {
  held <- decay_power(2)
  expect_identical(.as_spax_decay(held), held)
  expect_identical(.as_spax_decay(decay_kernel("exponential")), decay_exponential())
  plain <- bounded_kernel()
  expect_identical(.as_spax_decay(plain), decay_custom(plain))
  for (family in c("gaussian", "exponential", "power")) {
    expect_identical(.as_spax_decay(family), get(paste0("decay_", family))())
  }
  expect_identical(.as_spax_decay(c(a = "power")), decay_power())
})

test_that(".as_spax_decay() refuses everything else, without partial matching", {
  for (value in list("gauss", "Gaussian", "GAUSSIAN", "exp", "pow", "inverse", "binary",
                     "none", "", NA_character_, c("gaussian", "power"), character(),
                     5, NULL, TRUE, list(decay_gaussian()), function(d, s) 1,
                     quote(gaussian))) {
    expect_message_exact(.as_spax_decay(value), decay_text[["decay"]])
  }
  expect_message_exact(
    .as_spax_decay(5, arg = "family"),
    sub("^decay", "family", decay_text[["decay"]])
  )
})

test_that("decay objects print and format as one line", {
  expect_identical(format(decay_gaussian()),
                   "<spax decay> gaussian (sigma = travel scale): fit")
  expect_identical(format(decay_gaussian(4.6)),
                   "<spax decay> gaussian (sigma = travel scale): 4.6")
  expect_identical(format(decay_exponential()),
                   "<spax decay> exponential (sigma = rate): fit")
  expect_identical(format(decay_exponential(0.25)),
                   "<spax decay> exponential (sigma = rate): 0.25")
  expect_identical(format(decay_power(2)), "<spax decay> power (sigma = exponent): 2")
  expect_identical(format(decay_power(1 / 3)),
                   "<spax decay> power (sigma = exponent): 0.3333333")
  expect_identical(format(decay_custom(bounded_kernel())),
                   "<spax decay> simple (sigma = scale): fit")
  expect_identical(format(decay_custom(bounded_kernel(), 7)),
                   "<spax decay> simple (sigma = scale): 7")
  expect_identical(capture.output(print(decay_gaussian())),
                   "<spax decay> gaussian (sigma = travel scale): fit")
  expect_identical(capture.output(print(decay_power(4.6))),
                   "<spax decay> power (sigma = exponent): 4.6")
  x <- decay_gaussian()
  shown <- capture.output(result <- withVisible(print(x)))
  expect_false(result$visible)
  expect_identical(result$value, x)
})

# spec_clm(): defaults and structure -----------------------------------------------

test_that("spec_clm() and spec_huff() return the documented structure", {
  clm <- spec_clm()
  expect_s3_class(clm, "spax_spec")
  expect_identical(names(clm), c("kind", "decay", "outside", "beta", "kappa", "by_group"))
  expect_identical(clm$kind, "clm")
  expect_identical(clm$decay, decay_gaussian())
  expect_identical(clm$outside, "fit")
  expect_identical(clm$beta, 1)
  expect_identical(clm$kappa, 1)
  expect_identical(clm$by_group, character())

  huff <- spec_huff()
  expect_s3_class(huff, "spax_spec")
  expect_identical(names(huff), names(clm))
  expect_identical(huff$kind, "huff")
  expect_identical(huff$decay, decay_power())
  expect_identical(huff$outside, 0)
  expect_identical(huff$beta, "fit")
  expect_identical(huff$kappa, 1)
  expect_identical(huff$by_group, character())
})

test_that("a decay given as a name or a kernel becomes a spax_decay", {
  expect_identical(spec_clm(decay = "exponential")$decay, decay_exponential())
  expect_identical(spec_huff(decay = "gaussian")$decay, decay_gaussian())
  plain <- bounded_kernel()
  expect_identical(spec_clm(decay = plain)$decay, decay_custom(plain))
  expect_identical(spec_clm(decay = decay_kernel("power"))$decay, decay_power())
  expect_identical(spec_huff(decay = decay_power(2))$decay, decay_power(2))
  expect_identical(spec_clm(decay = c(a = "gaussian"))$decay, decay_gaussian())
})

test_that("a decay list gives one decay per group in the given order", {
  groups <- list(young = decay_gaussian(5), old = "power", mid = bounded_kernel())
  spec <- spec_clm(decay = groups)
  expect_identical(names(spec$decay), c("young", "old", "mid"))
  expect_identical(spec$decay$young, decay_gaussian(5))
  expect_identical(spec$decay$old, decay_power())
  expect_identical(spec$decay$mid, decay_custom(bounded_kernel()))
  expect_false(inherits(spec$decay, "spax_decay"))
  expect_identical(spec_huff(decay = groups)$decay, spec$decay)
  expect_identical(names(spec_clm(decay = list(only = "gaussian"))$decay), "only")
})

test_that("numeric arguments are stored as doubles", {
  spec <- spec_clm(outside = 2L, beta = 2L, kappa = 3L)
  expect_identical(spec$outside, 2)
  expect_identical(spec$beta, 2)
  expect_identical(spec$kappa, 3)
  expect_identical(spec_clm(outside = c(a = 1L, b = 0L))$outside, c(a = 1, b = 0))
  expect_identical(spec_clm(outside = list(a = 1L, b = "fit"))$outside,
                   list(a = 1, b = "fit"))
  expect_identical(spec_huff(beta = 1L, kappa = 2L)$beta, 1)
})

# decay validation -----------------------------------------------------------------

test_that("classes are checked before list-ness", {
  # A spax_decay and a spax_decay_kernel are both named lists, but they are one decay.
  expect_identical(spec_clm(decay = decay_exponential(0.5))$decay, decay_exponential(0.5))
  expect_identical(spec_clm(decay = decay_kernel("exponential"))$decay,
                   decay_exponential())
  # A list of decays is by group.
  expect_identical(names(spec_clm(decay = list(a = decay_power(), b = decay_power()))$decay),
                   c("a", "b"))
})

test_that("a decay list needs unique nonempty names and at least one element", {
  msg <- decay_text[["list"]]
  expect_message_exact(spec_clm(decay = list()), msg)
  expect_message_exact(spec_clm(decay = list(decay_gaussian())), msg)
  expect_message_exact(spec_clm(decay = list(a = decay_gaussian(), decay_power())), msg)
  expect_message_exact(spec_clm(decay = list(a = decay_gaussian(), a = decay_power())), msg)
  expect_message_exact(spec_clm(decay = list(a = decay_gaussian(), decay_power())), msg)
  expect_message_exact(spec_clm(decay = stats::setNames(list(decay_gaussian()), "")), msg)
  expect_message_exact(spec_clm(decay = stats::setNames(list(decay_gaussian()), NA)), msg)
  expect_message_exact(spec_huff(decay = list(decay_gaussian(), decay_power())), msg)
})

test_that("a bad element of a decay list gets the decay message", {
  expect_message_exact(spec_clm(decay = list(a = decay_gaussian(), b = "gauss")),
                       decay_text[["decay"]])
  expect_message_exact(spec_clm(decay = list(a = 5)), decay_text[["decay"]])
  expect_message_exact(spec_clm(decay = list(a = NULL)), decay_text[["decay"]])
  expect_message_exact(spec_clm(decay = list(a = list(decay_gaussian()))),
                       decay_text[["decay"]])
})

test_that("anything else is not a decay", {
  for (value in list(5, NULL, TRUE, NA, function(d, s) 1, quote(x),
                     data.frame(a = 1), structure(list(a = decay_gaussian()), class = "x"),
                     c("gaussian", "power"), "gauss")) {
    expect_message_exact(spec_clm(decay = value), decay_text[["decay"]])
    expect_message_exact(spec_huff(decay = value), decay_text[["decay"]])
  }
})

# outside --------------------------------------------------------------------------

test_that("outside accepts each documented form", {
  expect_identical(spec_clm(outside = "fit")$outside, "fit")
  expect_identical(spec_clm(outside = 0)$outside, 0)
  expect_identical(spec_clm(outside = 0.3)$outside, 0.3)
  expect_identical(spec_clm(outside = 12)$outside, 12)
  expect_identical(spec_clm(outside = c(young = 0.1, old = 0.2))$outside,
                   c(young = 0.1, old = 0.2))
  expect_identical(spec_clm(outside = c(young = 0, old = 0))$outside,
                   c(young = 0, old = 0))
  expect_identical(spec_clm(outside = list(young = "fit", old = 0))$outside,
                   list(young = "fit", old = 0))
  expect_identical(spec_clm(outside = list(young = 0.1, old = 0.2))$outside,
                   list(young = 0.1, old = 0.2))
  expect_identical(spec_clm(outside = list(young = "fit", old = "fit"))$outside,
                   list(young = "fit", old = "fit"))
})

test_that("a named vector of length 1 is by group, an unnamed number is shared", {
  one <- spec_clm(outside = c(young = 0.1))
  expect_identical(one$outside, c(young = 0.1))
  expect_true(is.numeric(one$outside) && !is.null(names(one$outside)))
  expect_identical(spec_clm(outside = 0.1)$outside, 0.1)
  expect_null(names(spec_clm(outside = 0.1)$outside))
  expect_identical(spec_clm(outside = list(young = "fit"))$outside, list(young = "fit"))
})

test_that("a character vector with \"fit\" and other entries asks for a named list", {
  expect_message_exact(spec_clm(outside = c("fit", "fit")), decay_text[["mixed"]])
  expect_message_exact(spec_clm(outside = c("fit", "0")), decay_text[["mixed"]])
  expect_message_exact(spec_clm(outside = c(young = "fit", old = "0")),
                       decay_text[["mixed"]])
  expect_message_exact(spec_clm(outside = c(young = "fit", old = "fit", mid = "fit")),
                       decay_text[["mixed"]])
  # A character vector longer than 1 without "fit" is not the mixed case.
  expect_message_exact(spec_clm(outside = c("a", "b")), decay_text[["outside"]])
})

test_that("any other outside value gets the general message", {
  bad <- list("Fit", "fits", "none", "0", NA, NA_real_, NULL, TRUE, -1, -0.001, Inf,
              NaN, c(0.1, 0.2), numeric(), character(),
              c(a = NA_real_), c(a = -1), c(a = Inf), c(a = 0.1, b = NA),
              c(a = 0.1, 0.2), c(a = 0.1, a = 0.2),
              stats::setNames(c(0.1, 0.2), c("a", "")),
              stats::setNames(c(0.1, 0.2), c("a", NA)),
              stats::setNames(0.1, ""),
              c(young = "fit"),
              list(), list("fit"), list(0.1), list(a = "fit", "fit"),
              list(a = "fit", a = 0), list(a = "fit", b = -1), list(a = "fit", b = NA),
              list(a = "fit", b = "Fit"), list(a = "fit", b = c(0.1, 0.2)),
              list(a = "fit", b = NULL), list(a = "fit", b = list(0)),
              list(a = "fit", b = Inf), list(a = c("fit", "fit")),
              stats::setNames(list("fit"), ""), data.frame(a = 0.1))
  for (value in bad) {
    expect_message_exact(spec_clm(outside = value), decay_text[["outside"]])
  }
})

test_that("spec_huff() has no outside argument", {
  expect_error(spec_huff(outside = 0), "unused argument")
  expect_identical(spec_huff()$outside, 0)
})

# beta and kappa -------------------------------------------------------------------

test_that("beta is \"fit\" or a positive number", {
  expect_identical(spec_clm(beta = "fit")$beta, "fit")
  expect_identical(spec_clm(beta = 0.5)$beta, 0.5)
  expect_identical(spec_huff(beta = 2)$beta, 2)
  for (value in list("Fit", "fits", "1", NA, NA_real_, NULL, TRUE, 0, -1, Inf, NaN,
                     c(1, 2), numeric(), c("fit", "fit"), list(1))) {
    expect_message_exact(spec_clm(beta = value), decay_text[["beta"]])
    expect_message_exact(spec_huff(beta = value), decay_text[["beta"]])
  }
})

test_that("kappa is a positive number", {
  expect_identical(spec_clm(kappa = 2.5)$kappa, 2.5)
  expect_identical(spec_huff(kappa = 0.1)$kappa, 0.1)
  for (value in list("fit", "1", NA, NA_real_, NULL, TRUE, 0, -1, Inf, NaN,
                     c(1, 2), numeric(), list(1))) {
    expect_message_exact(spec_clm(kappa = value), decay_text[["kappa"]])
    expect_message_exact(spec_huff(kappa = value), decay_text[["kappa"]])
  }
})

# by_group -------------------------------------------------------------------------

test_that("by_group is a duplicate-free subset of sigma and v0 (clm)", {
  expect_identical(spec_clm(by_group = "sigma")$by_group, "sigma")
  expect_identical(spec_clm(by_group = "v0")$by_group, "v0")
  expect_identical(spec_clm(by_group = c("sigma", "v0"))$by_group, c("sigma", "v0"))
  expect_identical(spec_clm(by_group = c("v0", "sigma"))$by_group, c("v0", "sigma"))
  expect_identical(spec_clm(by_group = character())$by_group, character())
  for (value in list("beta", "Sigma", c("sigma", "beta"), c("sigma", "sigma"), NA,
                     NA_character_, c("sigma", NA), NULL, 1, TRUE, list("sigma"),
                     factor("sigma"))) {
    expect_message_exact(spec_clm(by_group = value), decay_text[["clm_by"]])
  }
})

test_that("a Huff model can differ by group only in sigma", {
  expect_identical(spec_huff(by_group = "sigma")$by_group, "sigma")
  expect_identical(spec_huff(by_group = character())$by_group, character())
  for (value in list("v0", c("sigma", "v0"), "beta", c("sigma", "sigma"), NA, NULL, 1)) {
    expect_message_exact(spec_huff(by_group = value), decay_text[["huff_by"]])
  }
})

test_that("by_group = \"v0\" needs outside = \"fit\" or a list by group", {
  expect_message_exact(spec_clm(outside = 0, by_group = "v0"), decay_text[["v0_needs"]])
  expect_message_exact(spec_clm(outside = 0.2, by_group = "v0"), decay_text[["v0_needs"]])
  expect_message_exact(spec_clm(outside = c(a = 0.1, b = 0.2), by_group = "v0"),
                       decay_text[["v0_needs"]])
  expect_message_exact(spec_clm(outside = c(a = 0.1), by_group = c("sigma", "v0")),
                       decay_text[["v0_needs"]])
  expect_message_exact(spec_clm(outside = list(a = 0.1, b = 0.2), by_group = "v0"),
                       decay_text[["v0_needs"]])
  expect_identical(spec_clm(outside = "fit", by_group = "v0")$by_group, "v0")
  expect_identical(spec_clm(outside = list(a = "fit", b = 0), by_group = "v0")$by_group,
                   "v0")
})

test_that("a held single decay cannot differ by group in sigma", {
  expect_message_exact(spec_clm(decay = decay_gaussian(5), by_group = "sigma"),
                       decay_text[["held"]])
  expect_message_exact(spec_clm(decay = decay_exponential(0.5), by_group = c("sigma", "v0")),
                       decay_text[["held"]])
  expect_message_exact(spec_huff(decay = decay_power(2), by_group = "sigma"),
                       decay_text[["held"]])
  expect_message_exact(spec_clm(decay = decay_custom(bounded_kernel(), 5),
                                by_group = "sigma"), decay_text[["held"]])
  # A fitted decay, or a decay by group, is fine.
  expect_identical(spec_clm(decay = decay_gaussian(), by_group = "sigma")$by_group, "sigma")
  expect_identical(spec_clm(decay = "power", by_group = "sigma")$by_group, "sigma")
  groups <- list(young = decay_gaussian(5), old = decay_gaussian(8))
  expect_identical(spec_clm(decay = groups, by_group = "sigma")$by_group, "sigma")
  expect_identical(spec_huff(decay = groups, by_group = "sigma")$by_group, "sigma")
  # Without by_group a held single decay is fine.
  expect_identical(spec_clm(decay = decay_gaussian(5))$decay$sigma, 5)
})

# order of the checks ----------------------------------------------------------------

test_that("arguments are checked in order: decay, outside, beta, kappa, by_group", {
  # Each call has an error in every argument that follows the one named.
  expect_message_exact(spec_clm(decay = 5, outside = "bad", beta = -1, kappa = -1,
                                by_group = "bad"), decay_text[["decay"]])
  expect_message_exact(spec_clm(outside = "bad", beta = -1, kappa = -1, by_group = "bad"),
                       decay_text[["outside"]])
  expect_message_exact(spec_clm(beta = -1, kappa = -1, by_group = "bad"),
                       decay_text[["beta"]])
  expect_message_exact(spec_clm(kappa = -1, by_group = "bad"), decay_text[["kappa"]])
  expect_message_exact(spec_clm(by_group = "bad"), decay_text[["clm_by"]])
  expect_message_exact(spec_huff(decay = 5, beta = -1, kappa = -1, by_group = "v0"),
                       decay_text[["decay"]])
  expect_message_exact(spec_huff(beta = -1, kappa = -1, by_group = "v0"),
                       decay_text[["beta"]])
  expect_message_exact(spec_huff(kappa = -1, by_group = "v0"), decay_text[["kappa"]])
  expect_message_exact(spec_huff(by_group = "v0"), decay_text[["huff_by"]])
  # Within by_group: the subset rule, then v0, then sigma.
  expect_message_exact(spec_clm(decay = decay_gaussian(5), outside = 0.1,
                                by_group = c("v0", "sigma", "bad")), decay_text[["clm_by"]])
  expect_message_exact(spec_clm(decay = decay_gaussian(5), outside = 0.1,
                                by_group = c("v0", "sigma")), decay_text[["v0_needs"]])
  expect_message_exact(spec_clm(decay = decay_gaussian(5), by_group = c("v0", "sigma")),
                       decay_text[["held"]])
  # The decay list message comes before an outside error.
  expect_message_exact(spec_clm(decay = list(decay_gaussian()), outside = "bad"),
                       decay_text[["list"]])
})

test_that("validation does not need data", {
  expect_no_error(spec_clm(decay = list(young = decay_gaussian(5), old = decay_power(2)),
                           outside = c(young = 0.1, old = 0.2), by_group = "sigma"))
  expect_no_error(spec_huff(decay = list(a = "gaussian", b = "exponential"),
                            by_group = "sigma"))
})

# printing -------------------------------------------------------------------------

test_that("the default conditional logit prints as documented", {
  expected <- c(
    "<spax spec> conditional logit with an outside option",
    "  decay       gaussian (sigma = travel scale): fit",
    "  attraction  (kappa * supply)^beta: kappa = 1, beta = 1",
    "  outside     v0: fit"
  )
  expect_identical(capture.output(print(spec_clm())), expected)
  expect_identical(format(spec_clm()), expected)
  x <- spec_clm()
  shown <- capture.output(result <- withVisible(print(x)))
  expect_identical(shown, expected)
  expect_false(result$visible)
  expect_identical(result$value, x)
})

test_that("the default Huff model prints without an outside option", {
  expect_identical(capture.output(print(spec_huff())), c(
    "<spax spec> Huff model (no outside option)",
    "  decay       power (sigma = exponent): fit",
    "  attraction  (kappa * supply)^beta: kappa = 1, beta = fit",
    "  outside     none"
  ))
})

test_that("a conditional logit with outside = 0 and a Huff model have the same model line", {
  clm0 <- capture.output(print(spec_clm(decay = decay_power(), outside = 0, beta = "fit")))
  huff <- capture.output(print(spec_huff()))
  expect_identical(clm0, huff)
  expect_identical(clm0[1], "<spax spec> Huff model (no outside option)")
  expect_identical(capture.output(print(spec_clm(outside = 0)))[1],
                   "<spax spec> Huff model (no outside option)")
  expect_identical(capture.output(print(spec_clm(outside = 1e-6)))[1],
                   "<spax spec> conditional logit with an outside option")
})

test_that("held values print as numbers", {
  expect_identical(capture.output(print(
    spec_clm(decay = decay_exponential(0.25), outside = 0.05, beta = 0.5, kappa = 2)
  )), c(
    "<spax spec> conditional logit with an outside option",
    "  decay       exponential (sigma = rate): 0.25",
    "  attraction  (kappa * supply)^beta: kappa = 2, beta = 0.5",
    "  outside     v0: 0.05"
  ))
  expect_identical(capture.output(print(
    spec_huff(decay = decay_power(sigma = 1.5), beta = 1)
  )), c(
    "<spax spec> Huff model (no outside option)",
    "  decay       power (sigma = exponent): 1.5",
    "  attraction  (kappa * supply)^beta: kappa = 1, beta = 1",
    "  outside     none"
  ))
})

test_that("a declared kernel prints by its name and parameter label", {
  expect_identical(capture.output(print(spec_clm(decay = bounded_kernel()))), c(
    "<spax spec> conditional logit with an outside option",
    "  decay       simple (sigma = scale): fit",
    "  attraction  (kappa * supply)^beta: kappa = 1, beta = 1",
    "  outside     v0: fit"
  ))
  expect_identical(capture.output(print(
    spec_clm(decay = decay_custom(bounded_kernel(), 4), outside = 0.1)
  ))[2], "  decay       simple (sigma = scale): 4")
})

test_that("a decay by group prints one entry per group", {
  expect_identical(capture.output(print(spec_clm(
    decay = list(young = decay_gaussian(), old = decay_power(2)),
    outside = list(young = "fit", old = 0)
  ))), c(
    "<spax spec> conditional logit with an outside option",
    "  decay       by group: young gaussian (fit), old power (2)",
    "  attraction  (kappa * supply)^beta: kappa = 1, beta = 1",
    "  outside     v0 by group: young fit, old 0"
  ))
  expect_identical(capture.output(print(spec_huff(
    decay = list(a = decay_power(2), b = decay_power(), c = bounded_kernel())
  ))), c(
    "<spax spec> Huff model (no outside option)",
    "  decay       by group: a power (2), b power (fit), c simple (fit)",
    "  attraction  (kappa * supply)^beta: kappa = 1, beta = fit",
    "  outside     none"
  ))
})

test_that("a decay by group stays a list when by_group names sigma too", {
  expect_identical(capture.output(print(spec_clm(
    decay = list(young = decay_gaussian(), old = decay_gaussian()), by_group = "sigma"
  )))[2], "  decay       by group: young gaussian (fit), old gaussian (fit)")
})

test_that("a single decay with by_group = \"sigma\" prints \"fit by group\"", {
  expect_identical(capture.output(print(spec_clm(by_group = "sigma"))), c(
    "<spax spec> conditional logit with an outside option",
    "  decay       gaussian (sigma = travel scale): fit by group",
    "  attraction  (kappa * supply)^beta: kappa = 1, beta = 1",
    "  outside     v0: fit"
  ))
  expect_identical(capture.output(print(spec_huff(by_group = "sigma"))), c(
    "<spax spec> Huff model (no outside option)",
    "  decay       power (sigma = exponent): fit by group",
    "  attraction  (kappa * supply)^beta: kappa = 1, beta = fit",
    "  outside     none"
  ))
})

test_that("outside by group prints each group", {
  expect_identical(capture.output(print(
    spec_clm(outside = c(young = 0.1, old = 0.25))
  ))[c(1, 4)], c(
    "<spax spec> conditional logit with an outside option",
    "  outside     v0 by group: young 0.1, old 0.25"
  ))
  expect_identical(capture.output(print(
    spec_clm(outside = c(young = 0.1))
  ))[4], "  outside     v0 by group: young 0.1")
  expect_identical(capture.output(print(
    spec_clm(outside = list(young = "fit", old = "fit"))
  ))[4], "  outside     v0 by group: young fit, old fit")
  expect_identical(capture.output(print(
    spec_clm(outside = list(young = 0.1, old = "fit", mid = 0))
  ))[4], "  outside     v0 by group: young 0.1, old fit, mid 0")
})

test_that("v0 in by_group prints \"fit by group\"", {
  expect_identical(capture.output(print(spec_clm(by_group = c("sigma", "v0")))), c(
    "<spax spec> conditional logit with an outside option",
    "  decay       gaussian (sigma = travel scale): fit by group",
    "  attraction  (kappa * supply)^beta: kappa = 1, beta = 1",
    "  outside     v0: fit by group"
  ))
  expect_identical(capture.output(print(spec_clm(by_group = "v0")))[c(2, 4)], c(
    "  decay       gaussian (sigma = travel scale): fit",
    "  outside     v0: fit by group"
  ))
})

test_that("every outside weight 0 by group is the Huff model", {
  expect_identical(capture.output(print(spec_clm(outside = c(a = 0, b = 0))))[c(1, 4)], c(
    "<spax spec> Huff model (no outside option)",
    "  outside     none"
  ))
  expect_identical(capture.output(print(spec_clm(outside = list(a = 0, b = 0))))[c(1, 4)], c(
    "<spax spec> Huff model (no outside option)",
    "  outside     none"
  ))
  # One group with an outside option, or a fitted one, is a conditional logit.
  expect_identical(capture.output(print(spec_clm(outside = c(a = 0, b = 0.1))))[1],
                   "<spax spec> conditional logit with an outside option")
  expect_identical(capture.output(print(spec_clm(outside = list(a = 0, b = "fit"))))[1],
                   "<spax spec> conditional logit with an outside option")
})

test_that("kappa and beta print without trailing zeros", {
  expect_identical(capture.output(print(spec_clm(beta = 0.75, kappa = 1.5)))[3],
                   "  attraction  (kappa * supply)^beta: kappa = 1.5, beta = 0.75")
  expect_identical(capture.output(print(spec_huff(beta = 2, kappa = 10)))[3],
                   "  attraction  (kappa * supply)^beta: kappa = 10, beta = 2")
})
