# FCA specifications (spec_2sfca() and the others): structure, validation messages
# and their order, printed lines, and the mapping to the arguments of the FCA runner
# (.fca_spec_args()), which must reproduce what the wrappers build. These objects
# hold no data; the mapping is also run on the toy data against the wrappers.

expect_message_exact <- function(object, message) {
  expect_error(object, message, fixed = TRUE)
}

fca_text <- c(
  threshold = paste0("threshold must be a positive number or a named vector of ",
                     "positive numbers by group"),
  sigma = "an FCA specification needs a sigma value",
  decay = paste0("decay must be decay_gaussian(), decay_exponential(), decay_power(), ",
                 "decay_step(), decay_inverse(), decay_custom() or a named list of ",
                 "these by group"),
  decay_list = "decay by group must be a named list with one decay per group",
  selection = paste0("selection must be decay_gaussian(), decay_exponential(), ",
                     "decay_power(), decay_step(), decay_inverse(), decay_custom() or a ",
                     "named list of these by group"),
  selection_list = "selection by group must be a named list with one decay per group",
  attractiveness = paste0("attractiveness must be \"supply\" or a numeric vector named ",
                          "by facility with finite values of 0 or more"),
  alpha = "alpha must be a nonnegative number"
)

# match.arg() words this message; compare with its own wording.
normalize_text <- "'arg' should be one of"

# A declared kernel for the declared-kernel cases.
pexp_kernel <- function() {
  decay_kernel(function(distance, sigma) exp(-distance / sigma),
               name = "pexp", parameter = "scale")
}

# structure ---------------------------------------------------------------------------

test_that("every specification has the same seven fields, with NULL for the absent ones", {
  fields <- c("kind", "decay", "normalize", "selection", "attractiveness", "alpha",
              "threshold")
  specs <- list(
    spec_2sfca(15), spec_e2sfca(), spec_m2sfca(), spec_3sfca(), spec_huff2sfca()
  )
  for (spec in specs) {
    expect_identical(class(spec), c("spax_fca_spec", "spax_spec"))
    expect_identical(names(spec), fields)
  }

  two <- spec_2sfca(15)
  expect_identical(two$kind, "2sfca")
  expect_identical(two$threshold, 15)
  expect_null(two$decay)
  expect_null(two$normalize)
  expect_null(two$selection)
  expect_null(two$attractiveness)
  expect_null(two$alpha)

  e2 <- spec_e2sfca()
  expect_identical(e2$kind, "e2sfca")
  expect_identical(e2$decay, decay_gaussian(30))
  expect_identical(e2$normalize, "identity")
  expect_null(e2$selection)
  expect_null(e2$attractiveness)
  expect_null(e2$alpha)
  expect_null(e2$threshold)

  m2 <- spec_m2sfca(decay_exponential(0.2), "semi")
  expect_identical(m2$kind, "m2sfca")
  expect_identical(m2$decay, decay_exponential(0.2))
  expect_identical(m2$normalize, "semi")

  three <- spec_3sfca()
  expect_identical(three$kind, "3sfca")
  expect_identical(three$decay, decay_gaussian(30))
  expect_null(three$selection)
  expect_null(three$normalize)
  expect_null(three$alpha)

  huff <- spec_huff2sfca()
  expect_identical(huff$kind, "huff2sfca")
  expect_identical(huff$decay, decay_gaussian(30))
  expect_null(huff$selection)
  expect_identical(huff$attractiveness, "supply")
  expect_identical(huff$alpha, 1)
  expect_null(huff$normalize)
})

test_that("a specification is also a spax_spec but not an allocation specification", {
  expect_s3_class(spec_e2sfca(), "spax_spec")
  expect_false(inherits(spec_e2sfca(), "spax_allocation_spec"))
  expect_false(inherits(spec_clm(), "spax_fca_spec"))
  expect_identical(class(spec_clm()), c("spax_allocation_spec", "spax_spec"))
})

test_that("numeric arguments are stored as doubles", {
  expect_identical(spec_2sfca(15L)$threshold, 15)
  expect_identical(spec_2sfca(c(a = 15L, b = 20L))$threshold, c(a = 15, b = 20))
  expect_identical(spec_huff2sfca(alpha = 2L)$alpha, 2)
  expect_identical(spec_huff2sfca(attractiveness = c(a = 1L, b = 2L))$attractiveness,
                   c(a = 1, b = 2))
})

# threshold ---------------------------------------------------------------------------

test_that("the threshold is a positive number or a named vector by group", {
  expect_identical(spec_2sfca(0.5)$threshold, 0.5)
  expect_identical(spec_2sfca(threshold = 60)$threshold, 60)
  expect_identical(spec_2sfca(c(young = 10, old = 20))$threshold,
                   c(young = 10, old = 20))
  # A named vector of length 1 is by group, an unnamed number is shared.
  expect_identical(spec_2sfca(c(young = 10))$threshold, c(young = 10))
  expect_null(names(spec_2sfca(10)$threshold))
  bad <- list(0, -1, NA, NA_real_, NULL, TRUE, Inf, NaN, c(1, 2), numeric(), "15",
              factor("15"), list(15), c(a = 0), c(a = -1), c(a = NA_real_), c(a = Inf),
              c(a = 1, b = NA), c(a = 1, 2), c(a = 1, a = 2),
              stats::setNames(c(1, 2), c("a", "")), stats::setNames(c(1, 2), c("a", NA)),
              stats::setNames(1, ""), list(a = 1, b = 2))
  for (value in bad) {
    expect_message_exact(spec_2sfca(value), fca_text[["threshold"]])
  }
  expect_error(spec_2sfca(), "threshold")
})

# decay -------------------------------------------------------------------------------

test_that("a decay is a decay object with a value for sigma, a step or an inverse decay", {
  expect_identical(spec_e2sfca(decay_gaussian(10))$decay, decay_gaussian(10))
  expect_identical(spec_e2sfca(decay = decay_exponential(0.3))$decay, decay_exponential(0.3))
  expect_identical(spec_e2sfca(decay_power(2))$decay, decay_power(2))
  expect_identical(spec_m2sfca(decay_step(15))$decay, decay_step(15))
  expect_identical(spec_3sfca(decay_inverse())$decay, decay_inverse())
  expect_identical(spec_huff2sfca(decay_inverse())$decay, decay_inverse())
  kernel <- pexp_kernel()
  expect_identical(spec_e2sfca(decay_custom(kernel, 4))$decay, decay_custom(kernel, 4))
  # Each of the four functions takes the same forms.
  for (constructor in list(spec_e2sfca, spec_m2sfca, spec_3sfca, spec_huff2sfca)) {
    expect_identical(constructor(decay_gaussian(10))$decay, decay_gaussian(10))
    expect_message_exact(constructor(decay_gaussian()), fca_text[["sigma"]])
    expect_message_exact(constructor(5), fca_text[["decay"]])
  }
})

test_that("a raster sigma is accepted in an FCA specification", {
  one <- toy_rast(c(1, 2, 3))
  several <- toy_rast(c(1, 2, 3), nlyr = 2)
  for (raster in list(one, several)) {
    spec <- spec_e2sfca(decay_gaussian(raster))
    expect_s4_class(spec$decay$sigma, "SpatRaster")
    expect_s4_class(spec_m2sfca(decay_exponential(raster))$decay$sigma, "SpatRaster")
    expect_s4_class(spec_3sfca(decay_power(raster))$decay$sigma, "SpatRaster")
    expect_s4_class(spec_huff2sfca(decay_gaussian(1), selection = decay_gaussian(raster))$selection$sigma,
                    "SpatRaster")
  }
})

test_that("sigma = \"fit\", a bare kernel and a built-in name need a value", {
  kernel <- pexp_kernel()
  for (value in list(decay_gaussian(), decay_exponential(), decay_power(),
                     decay_custom(kernel), kernel, decay_kernel("gaussian"),
                     "gaussian", "exponential", "power", c(a = "power"))) {
    expect_message_exact(spec_e2sfca(value), fca_text[["sigma"]])
    expect_message_exact(spec_3sfca(decay_gaussian(1), selection = value),
                         fca_text[["sigma"]])
  }
})

test_that("anything else is not a decay", {
  for (value in list(5, NULL, TRUE, NA, function(d, s) 1, quote(x), "gauss", "inverse",
                     "binary", "Gaussian", c("gaussian", "power"), character(),
                     data.frame(a = 1), structure(list(a = decay_gaussian(1)), class = "x"))) {
    expect_message_exact(spec_e2sfca(value), fca_text[["decay"]])
  }
  expect_message_exact(spec_3sfca(decay_gaussian(1), selection = 5), fca_text[["selection"]])
  expect_message_exact(spec_3sfca(decay_gaussian(1), selection = "inverse"),
                       fca_text[["selection"]])
})

test_that("a decay list gives one decay per group in the given order", {
  groups <- list(young = decay_gaussian(5), old = decay_step(15), mid = decay_inverse(),
                 raster = decay_power(toy_rast(2)))
  spec <- spec_e2sfca(groups)
  expect_identical(names(spec$decay), c("young", "old", "mid", "raster"))
  expect_identical(spec$decay$young, decay_gaussian(5))
  expect_identical(spec$decay$old, decay_step(15))
  expect_identical(spec$decay$mid, decay_inverse())
  expect_false(inherits(spec$decay, c("spax_decay", "spax_fca_decay")))
  expect_identical(names(spec_3sfca(groups)$decay), names(groups))
  expect_identical(names(spec_e2sfca(list(only = decay_step(3)))$decay), "only")
  expect_identical(spec_3sfca(decay_gaussian(1), selection = groups)$selection, spec$decay)
})

test_that("a decay list needs unique nonempty names and a valid decay in each element", {
  msg <- fca_text[["decay_list"]]
  expect_message_exact(spec_e2sfca(list()), msg)
  expect_message_exact(spec_e2sfca(list(decay_gaussian(1))), msg)
  expect_message_exact(spec_e2sfca(list(a = decay_gaussian(1), decay_step(2))), msg)
  expect_message_exact(spec_e2sfca(list(a = decay_gaussian(1), a = decay_step(2))), msg)
  expect_message_exact(spec_e2sfca(stats::setNames(list(decay_gaussian(1)), "")), msg)
  expect_message_exact(spec_e2sfca(stats::setNames(list(decay_gaussian(1)), NA)), msg)
  expect_message_exact(spec_3sfca(decay_gaussian(1), selection = list(decay_step(2))),
                       fca_text[["selection_list"]])
  # An element is checked like a single decay.
  expect_message_exact(spec_e2sfca(list(a = decay_gaussian(1), b = decay_gaussian())),
                       fca_text[["sigma"]])
  expect_message_exact(spec_e2sfca(list(a = decay_gaussian(1), b = "power")),
                       fca_text[["sigma"]])
  expect_message_exact(spec_e2sfca(list(a = decay_gaussian(1), b = 5)), fca_text[["decay"]])
  expect_message_exact(spec_e2sfca(list(a = NULL)), fca_text[["decay"]])
  expect_message_exact(spec_e2sfca(list(a = list(decay_gaussian(1)))), fca_text[["decay"]])
})

# selection ---------------------------------------------------------------------------

test_that("the selection is NULL (the decay) or a decay in any form of decay", {
  expect_null(spec_3sfca(decay_gaussian(10))$selection)
  expect_null(spec_huff2sfca(decay_gaussian(10), selection = NULL)$selection)
  expect_identical(spec_3sfca(decay_gaussian(10), decay_inverse())$selection, decay_inverse())
  expect_identical(spec_3sfca(decay_gaussian(10), selection = decay_step(5))$selection,
                   decay_step(5))
  expect_identical(spec_huff2sfca(decay_gaussian(10), decay_exponential(0.5))$selection,
                   decay_exponential(0.5))
  expect_error(spec_e2sfca(decay_gaussian(10), selection = decay_inverse()), "unused argument")
  expect_error(spec_m2sfca(decay_gaussian(10), selection = decay_inverse()), "unused argument")
})

# normalize and alpha -----------------------------------------------------------------

test_that("normalize is identity, standard or semi", {
  for (value in c("identity", "standard", "semi")) {
    expect_identical(spec_e2sfca(normalize = value)$normalize, value)
    expect_identical(spec_m2sfca(normalize = value)$normalize, value)
  }
  # match.arg() allows a partial name.
  expect_identical(spec_e2sfca(normalize = "stand")$normalize, "standard")
  for (value in list("none", "Standard", 1, NA, c("identity", "semi"), character())) {
    expect_error(spec_e2sfca(normalize = value))
  }
  expect_error(spec_e2sfca(normalize = "none"), normalize_text, fixed = TRUE)
  expect_error(spec_m2sfca(normalize = "none"), normalize_text, fixed = TRUE)
  expect_error(spec_3sfca(normalize = "semi"), "unused argument")
  expect_error(spec_2sfca(15, normalize = "semi"), "unused argument")
})

test_that("alpha is a single finite number of 0 or more", {
  expect_identical(spec_huff2sfca(alpha = 0)$alpha, 0)
  expect_identical(spec_huff2sfca(alpha = 2.5)$alpha, 2.5)
  for (value in list(-1, -0.001, NA, NA_real_, NULL, TRUE, Inf, -Inf, NaN, c(1, 2), numeric(),
                     "1", list(1))) {
    expect_message_exact(spec_huff2sfca(alpha = value), fca_text[["alpha"]])
  }
})

# attractiveness ----------------------------------------------------------------------

test_that("attractiveness is \"supply\" or a numeric vector named by facility", {
  expect_identical(spec_huff2sfca(attractiveness = "supply")$attractiveness, "supply")
  expect_identical(spec_huff2sfca(attractiveness = c(a = 0, b = 2.5))$attractiveness,
                   c(a = 0, b = 2.5))
  expect_identical(spec_huff2sfca(attractiveness = c(a = 3))$attractiveness, c(a = 3))
  bad <- list("Supply", "gp_fte", NULL, NA, TRUE, 3, c(1, 2), c(a = -1), c(a = NA_real_),
              c(a = Inf), c(a = 1, b = NA), c(a = 1, 2), c(a = 1, a = 2),
              stats::setNames(c(1, 2), c("a", "")), stats::setNames(c(1, 2), c("a", NA)),
              numeric(), list(a = 1), c(a = "1"), c("supply", "supply"))
  for (value in bad) {
    expect_message_exact(spec_huff2sfca(attractiveness = value), fca_text[["attractiveness"]])
  }
  expect_error(spec_3sfca(attractiveness = "supply"), "unused argument")
})

# order of the checks -----------------------------------------------------------------

test_that("arguments are checked in the order of the arguments", {
  expect_message_exact(spec_e2sfca(decay = 5, normalize = "bad"), fca_text[["decay"]])
  expect_error(spec_e2sfca(normalize = "bad"), normalize_text, fixed = TRUE)
  expect_message_exact(spec_3sfca(decay = 5, selection = 5), fca_text[["decay"]])
  expect_message_exact(spec_3sfca(selection = 5), fca_text[["selection"]])
  expect_message_exact(
    spec_huff2sfca(decay = 5, selection = 5, attractiveness = "bad", alpha = -1),
    fca_text[["decay"]]
  )
  expect_message_exact(
    spec_huff2sfca(selection = 5, attractiveness = "bad", alpha = -1),
    fca_text[["selection"]]
  )
  expect_message_exact(spec_huff2sfca(attractiveness = "bad", alpha = -1),
                       fca_text[["attractiveness"]])
  expect_message_exact(spec_huff2sfca(alpha = -1), fca_text[["alpha"]])
  # The decay message about sigma comes before the selection message.
  expect_message_exact(spec_3sfca(decay_gaussian(), selection = 5), fca_text[["sigma"]])
  # The decay list message comes before a later error.
  expect_message_exact(spec_e2sfca(list(decay_gaussian(1)), normalize = "bad"),
                       fca_text[["decay_list"]])
})

test_that("validation does not need data", {
  expect_no_error(spec_e2sfca(list(young = decay_gaussian(5), old = decay_step(2)),
                              normalize = "standard"))
  expect_no_error(spec_huff2sfca(list(a = decay_gaussian(5), b = decay_inverse()),
                                 selection = list(a = decay_step(3), b = decay_step(4)),
                                 attractiveness = c(f1 = 1, f2 = 2), alpha = 0))
})

# printing ----------------------------------------------------------------------------

test_that("each specification prints as documented", {
  expect_identical(capture.output(print(spec_2sfca(15))), c(
    "<spax spec> 2SFCA",
    "  decay  step (threshold): 15"
  ))
  expect_identical(capture.output(print(spec_e2sfca(decay_gaussian(10)))), c(
    "<spax spec> E2SFCA",
    "  decay      gaussian (sigma = travel scale): 10",
    "  normalize  identity"
  ))
  expect_identical(capture.output(print(spec_m2sfca(decay_exponential(0.25), "standard"))), c(
    "<spax spec> M2SFCA",
    "  decay      exponential (sigma = rate): 0.25",
    "  normalize  standard"
  ))
  expect_identical(capture.output(print(spec_3sfca(decay_power(2)))), c(
    "<spax spec> 3SFCA",
    "  decay      power (sigma = exponent): 2",
    "  selection  same as decay"
  ))
  expect_identical(capture.output(print(spec_3sfca(decay_gaussian(10), decay_inverse()))), c(
    "<spax spec> 3SFCA",
    "  decay      gaussian (sigma = travel scale): 10",
    "  selection  inverse: 1/(d + 1)"
  ))
  expect_identical(capture.output(print(spec_huff2sfca(decay_gaussian(10)))), c(
    "<spax spec> Huff-2SFCA",
    "  decay           gaussian (sigma = travel scale): 10",
    "  selection       same as decay",
    "  attractiveness  supply ^ alpha 1"
  ))
  expect_identical(capture.output(print(spec_huff2sfca(
    decay_step(15), decay_gaussian(10), c(a = 1, b = 2, c = 3), alpha = 1.5
  ))), c(
    "<spax spec> Huff-2SFCA",
    "  decay           step (threshold): 15",
    "  selection       gaussian (sigma = travel scale): 10",
    "  attractiveness  3 values ^ alpha 1.5"
  ))
})

test_that("a raster sigma prints as variable (raster)", {
  one <- toy_rast(c(1, 2, 3))
  expect_identical(capture.output(print(spec_e2sfca(decay_gaussian(one))))[2],
                   "  decay      gaussian (sigma = travel scale): variable (raster)")
  expect_identical(capture.output(print(spec_3sfca(decay_gaussian(1), decay_power(one))))[3],
                   "  selection  power (sigma = exponent): variable (raster)")
})

test_that("a decay or threshold by group prints one entry per group", {
  expect_identical(capture.output(print(spec_2sfca(c(young = 10, old = 20)))), c(
    "<spax spec> 2SFCA",
    "  decay  by group: young step (10), old step (20)"
  ))
  expect_identical(capture.output(print(spec_e2sfca(
    list(young = decay_gaussian(8), old = decay_step(20), mid = decay_inverse(),
         far = decay_power(toy_rast(2)))
  ))), c(
    "<spax spec> E2SFCA",
    paste0("  decay      by group: young gaussian (8), old step (20), mid inverse, ",
           "far power (variable (raster))"),
    "  normalize  identity"
  ))
  expect_identical(capture.output(print(spec_3sfca(
    decay_gaussian(10), list(a = decay_step(3), b = decay_inverse())
  )))[3], "  selection  by group: a step (3), b inverse")
})

test_that("a specification formats as its printed lines and prints invisibly", {
  x <- spec_e2sfca(decay_gaussian(10))
  expect_identical(format(x), capture.output(print(x)))
  shown <- capture.output(result <- withVisible(print(x)))
  expect_false(result$visible)
  expect_identical(result$value, x)
  # Allocation specifications print as before through the shared method.
  expect_identical(capture.output(print(spec_huff()))[1],
                   "<spax spec> Huff model (no outside option)")
})

# mapping to the FCA runner -----------------------------------------------------------

test_that(".fca_spec_args() maps each kind to the arguments of the runner", {
  two <- .fca_spec_args(spec_2sfca(15))
  expect_identical(two$kind, "2sfca")
  expect_identical(two$type, "e2sfca")
  expect_identical(two$label, "2SFCA")
  expect_identical(two$decay_params, list(method = "binary", sigma = 15))
  expect_null(two$selection_params)
  expect_identical(two$demand_normalize, "identity")
  expect_null(two$attractiveness)
  expect_identical(two$alpha, 1)
  expect_identical(two$parameter_names, c("decay_params", "demand_normalize"))

  e2 <- .fca_spec_args(spec_e2sfca(decay_gaussian(10), "standard"))
  expect_identical(e2$type, "e2sfca")
  expect_identical(e2$label, "E2SFCA")
  expect_identical(e2$decay_params, list(method = "gaussian", sigma = 10))
  expect_null(e2$selection_params)
  expect_identical(e2$demand_normalize, "standard")
  expect_identical(e2$parameter_names, c("decay_params", "demand_normalize"))

  m2 <- .fca_spec_args(spec_m2sfca(decay_exponential(0.25), "semi"))
  expect_identical(m2$type, "m2sfca")
  expect_identical(m2$label, "M2SFCA")
  expect_identical(m2$decay_params, list(method = "exponential", sigma = 0.25))
  expect_identical(m2$demand_normalize, "semi")
  expect_identical(m2$parameter_names, c("decay_params", "demand_normalize"))

  three <- .fca_spec_args(spec_3sfca(decay_power(2), decay_inverse()))
  expect_identical(three$type, "3sfca")
  expect_identical(three$label, "3SFCA")
  expect_identical(three$decay_params, list(method = "power", sigma = 2))
  expect_identical(three$selection_params, list(method = "inverse"))
  expect_identical(three$demand_normalize, "identity")
  expect_null(three$attractiveness)
  expect_identical(three$parameter_names, c("decay_params", "selection_params"))

  huff <- .fca_spec_args(spec_huff2sfca(decay_gaussian(10), decay_step(5),
                                        c(a = 1, b = 2), alpha = 2))
  expect_identical(huff$type, "huff")
  expect_identical(huff$label, "Huff-2SFCA")
  expect_identical(huff$decay_params, list(method = "gaussian", sigma = 10))
  expect_identical(huff$selection_params, list(method = "binary", sigma = 5))
  expect_identical(huff$demand_normalize, "identity")
  expect_identical(huff$attractiveness, c(a = 1, b = 2))
  expect_identical(huff$alpha, 2)
  expect_identical(huff$parameter_names,
                   c("decay_params", "selection_params", "attractiveness", "alpha"))
})

test_that("a missing selection maps to the mapped decay, and supply to NULL", {
  for (spec in list(spec_3sfca(decay_gaussian(10)), spec_huff2sfca(decay_gaussian(10)))) {
    args <- .fca_spec_args(spec)
    expect_identical(args$selection_params, args$decay_params)
    expect_identical(args$selection_params, list(method = "gaussian", sigma = 10))
  }
  expect_null(.fca_spec_args(spec_huff2sfca(decay_gaussian(10)))$attractiveness)
  expect_null(.fca_spec_args(spec_huff2sfca(attractiveness = "supply"))$attractiveness)
  # The default sigma is the one of the wrappers.
  expect_identical(.fca_spec_args(spec_e2sfca())$decay_params,
                   eval(formals(spax_e2sfca)$decay_params))
  expect_identical(.fca_spec_args(spec_3sfca())$decay_params,
                   eval(formals(spax_3sfca)$decay_params))
})

test_that("decay objects map as the wrappers store them", {
  # Built-in decays: the method name and a double, never an integer.
  for (family in c("gaussian", "exponential", "power")) {
    constructor <- get(paste0("decay_", family))
    params <- .fca_spec_args(spec_e2sfca(constructor(5L)))$decay_params
    expect_identical(params, list(method = family, sigma = 5))
    expect_type(params$sigma, "double")
  }
  # Step: the binary method and the threshold; inverse: no sigma element.
  expect_identical(.fca_spec_args(spec_e2sfca(decay_step(15L)))$decay_params,
                   list(method = "binary", sigma = 15))
  inverse <- .fca_spec_args(spec_e2sfca(decay_inverse()))$decay_params
  expect_identical(inverse, list(method = "inverse"))
  expect_false("sigma" %in% names(inverse))
  expect_false(is.object(inverse))
  # A declared kernel: the kernel itself and the value of sigma.
  kernel <- pexp_kernel()
  expect_identical(.fca_spec_args(spec_e2sfca(decay_custom(kernel, 4)))$decay_params,
                   list(method = kernel, sigma = 4))
  # A built-in kernel declared with decay_kernel() is the built-in method.
  expect_identical(.fca_spec_args(spec_e2sfca(decay_custom(decay_kernel("power"), 2)))$decay_params,
                   list(method = "power", sigma = 2))
  # A raster sigma stays the raster, as stored.
  sigma <- toy_rast(c(1, 2, 3))
  params <- .fca_spec_args(spec_e2sfca(decay_gaussian(sigma)))$decay_params
  expect_identical(names(params), c("method", "sigma"))
  expect_identical(params$method, "gaussian")
  expect_s4_class(params$sigma, "SpatRaster")
  expect_identical(terra::values(params$sigma), terra::values(sigma))
})

test_that("a threshold by group maps to one binary decay list per group", {
  args <- .fca_spec_args(spec_2sfca(c(young = 10, old = 20)))
  expect_identical(args$decay_params, list(young = list(method = "binary", sigma = 10),
                                           old = list(method = "binary", sigma = 20)))
  expect_identical(.fca_spec_args(spec_2sfca(c(young = 10)))$decay_params,
                   list(young = list(method = "binary", sigma = 10)))
})

test_that("decay and selection lists by group map element by element", {
  kernel <- pexp_kernel()
  decay <- list(a = decay_gaussian(5), b = decay_step(15), c = decay_inverse(),
                d = decay_custom(kernel, 3))
  args <- .fca_spec_args(spec_3sfca(decay))
  expected <- list(a = list(method = "gaussian", sigma = 5),
                   b = list(method = "binary", sigma = 15),
                   c = list(method = "inverse"),
                   d = list(method = kernel, sigma = 3))
  expect_identical(args$decay_params, expected)
  # No selection: the same list.
  expect_identical(args$selection_params, expected)
  # A shared selection with decays by group, and a selection by group with a shared decay.
  shared <- .fca_spec_args(spec_3sfca(decay, decay_inverse()))
  expect_identical(shared$selection_params, list(method = "inverse"))
  by_group <- .fca_spec_args(spec_3sfca(decay_gaussian(5),
                                        list(a = decay_step(2), b = decay_step(3))))
  expect_identical(by_group$decay_params, list(method = "gaussian", sigma = 5))
  expect_identical(by_group$selection_params,
                   list(a = list(method = "binary", sigma = 2),
                        b = list(method = "binary", sigma = 3)))
})

test_that(".fca_spec_args() refuses an unknown kind", {
  bad <- spec_e2sfca()
  bad$kind <- "bogus"
  expect_message_exact(.fca_spec_args(bad), "unknown FCA kind 'bogus'")
})

# The arguments reproduce the wrappers -------------------------------------------------

# Run the engine from the arguments of a specification, as the wrapper does after its
# validation, and return the values, the type and the parameters list of the result.
run_from_spec <- function(spec, td, supply = td$supply_vector, demand = td$demand,
                          distance = td$distance) {
  a <- .fca_spec_args(spec)
  attractiveness <- if (identical(a$kind, "huff2sfca")) {
    .fca_attractiveness(a$attractiveness, supply, distance, NULL, NULL, FALSE)
  }
  result <- .fca_block_run(a$type, demand, supply, distance, a$decay_params,
                           a$selection_params, attractiveness, a$alpha, a$demand_normalize)
  out <- .fca_group_result(result, list(
    decay_params = a$decay_params, demand_normalize = a$demand_normalize,
    selection_params = a$selection_params, attractiveness = attractiveness,
    alpha = a$alpha
  )[a$parameter_names])
  list(values = terra::values(out$accessibility), type = a$label,
       parameters = out$parameters)
}

expect_same_as_wrapper <- function(got, wrapper) {
  expect_identical(got$values, terra::values(wrapper$accessibility))
  expect_identical(got$type, wrapper$type)
  expect_identical(got$parameters, wrapper$parameters)
}

test_that("the arguments of each specification reproduce its wrapper", {
  td <- fca_toy()
  attract <- c(facility1 = 2, facility2 = 1)
  args <- list(td$demand, td$supply_vector, td$distance)

  expect_same_as_wrapper(
    run_from_spec(spec_2sfca(3), td),
    do.call(spax_2sfca, c(args, list(threshold = 3)))
  )
  expect_same_as_wrapper(
    run_from_spec(spec_e2sfca(decay_gaussian(2), "standard"), td),
    do.call(spax_e2sfca, c(args, list(decay_params = list(method = "gaussian", sigma = 2),
                                      demand_normalize = "standard")))
  )
  expect_same_as_wrapper(
    run_from_spec(spec_m2sfca(decay_exponential(0.4), "semi"), td),
    do.call(spax_m2sfca, c(args, list(decay_params = list(method = "exponential", sigma = 0.4),
                                      demand_normalize = "semi")))
  )
  expect_same_as_wrapper(
    run_from_spec(spec_e2sfca(decay_inverse()), td),
    do.call(spax_e2sfca, c(args, list(decay_params = list(method = "inverse"))))
  )
  expect_same_as_wrapper(
    run_from_spec(spec_e2sfca(decay_step(3)), td),
    do.call(spax_e2sfca, c(args, list(decay_params = list(method = "binary", sigma = 3))))
  )
  # No selection is the decay: the wrapper's default.
  expect_same_as_wrapper(
    run_from_spec(spec_3sfca(decay_gaussian(2)), td),
    do.call(spax_3sfca, c(args, list(decay_params = list(method = "gaussian", sigma = 2))))
  )
  expect_same_as_wrapper(
    run_from_spec(spec_3sfca(decay_gaussian(2), decay_inverse()), td),
    do.call(spax_3sfca, c(args, list(decay_params = list(method = "gaussian", sigma = 2),
                                     selection_params = list(method = "inverse"))))
  )
  # The attractiveness defaults to the supply measure.
  expect_same_as_wrapper(
    run_from_spec(spec_huff2sfca(decay_gaussian(2)), td),
    do.call(spax_huff2sfca, c(args, list(decay_params = list(method = "gaussian", sigma = 2))))
  )
  expect_same_as_wrapper(
    run_from_spec(spec_huff2sfca(decay_exponential(0.4), decay_step(4), attract, 1.5), td),
    do.call(spax_huff2sfca, c(args, list(
      decay_params = list(method = "exponential", sigma = 0.4),
      selection_params = list(method = "binary", sigma = 4),
      attractiveness = attract, alpha = 1.5
    )))
  )
})

test_that("a declared kernel and a raster sigma reproduce the wrappers", {
  td <- fca_toy()
  args <- list(td$demand, td$supply_vector, td$distance)
  kernel <- pexp_kernel()
  expect_same_as_wrapper(
    run_from_spec(spec_e2sfca(decay_custom(kernel, 2)), td),
    do.call(spax_e2sfca, c(args, list(decay_params = list(method = kernel, sigma = 2))))
  )
  sigma <- toy_rast(c(1, 2, 3, 4, 5, 6, 7, 8, 9))
  expect_same_as_wrapper(
    run_from_spec(spec_m2sfca(decay_gaussian(sigma)), td),
    do.call(spax_m2sfca, c(args, list(decay_params = list(method = "gaussian", sigma = sigma))))
  )
  expect_same_as_wrapper(
    run_from_spec(spec_3sfca(decay_exponential(sigma)), td),
    do.call(spax_3sfca, c(args, list(decay_params = list(method = "exponential",
                                                          sigma = sigma))))
  )
})

test_that("decays and thresholds by group reproduce the wrappers", {
  f <- fca_block_fixture(3)
  td <- list(demand = list(a = f$demand, b = toy_rast(rev(f$D), nrows = 3, ncols = 4)),
             supply_vector = f$S[, 1], distance = f$distance)
  args <- list(td$demand, td$supply_vector, td$distance)
  expect_same_as_wrapper(
    run_from_spec(spec_2sfca(c(a = 3, b = 5)), td, td$supply_vector, td$demand, td$distance),
    do.call(spax_2sfca, c(args, list(threshold = c(a = 3, b = 5))))
  )
  decay <- list(a = decay_gaussian(2.5), b = decay_step(4))
  params <- list(a = list(method = "gaussian", sigma = 2.5),
                 b = list(method = "binary", sigma = 4))
  expect_same_as_wrapper(
    run_from_spec(spec_e2sfca(decay, "standard"), td, td$supply_vector, td$demand,
                  td$distance),
    do.call(spax_e2sfca, c(args, list(decay_params = params,
                                      demand_normalize = "standard")))
  )
  expect_same_as_wrapper(
    run_from_spec(spec_3sfca(decay, decay_inverse()), td, td$supply_vector, td$demand,
                  td$distance),
    do.call(spax_3sfca, c(args, list(decay_params = params,
                                     selection_params = list(method = "inverse"))))
  )
})

# the runner ---------------------------------------------------------------------------

test_that(".fca_group_kernels() stops on an unknown type and keeps the known ones", {
  td <- fca_toy()
  decay <- list(method = "gaussian", sigma = 2)
  expect_message_exact(.fca_group_kernels("bogus", td$distance, decay),
                       "unknown FCA type 'bogus'")
  # The kind of a specification and the label are not runner types.
  expect_message_exact(.fca_group_kernels("2sfca", td$distance, decay),
                       "unknown FCA type '2sfca'")
  expect_message_exact(.fca_group_kernels("huff2sfca", td$distance, decay),
                       "unknown FCA type 'huff2sfca'")
  expect_error(.fca_block_run("bogus", td$demand, td$supply_vector, td$distance, decay),
               "unknown FCA type 'bogus'", fixed = TRUE)
  for (type in c("e2sfca", "m2sfca", "3sfca", "huff")) {
    spec <- .fca_group_kernels(type, td$distance, decay, decay,
                               attractiveness = c(1, 2), alpha = 1)
    expect_named(spec, c("rasters", "kernels"))
    expect_named(spec$kernels, c("demand", "access"))
  }
})
