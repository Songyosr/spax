# decay_kernel(): declarations of built-in and custom decay kernels, their exact
# text storage and checks, and their use through calc_decay() and the FCA
# wrappers. Expected values are written out here as plain formulas.

kernel_fields <- c("name", "builtin", "parameter", "lower", "upper",
                   "zero_distance", "smooth", "args", "code")

# The plateau kernel as a formula: full weight up to `plateau`, Gaussian beyond.
plateau_weights <- function(distance, sigma, plateau = 5) {
  ifelse(distance <= plateau, 1, exp(-(distance - plateau)^2 / (2 * sigma^2)))
}

# A declaration with a custom code string, for tests of tampered declarations.
with_code <- function(kernel, code) {
  kernel$code <- code
  kernel
}

# Replace fields of a declaration, keeping its class.
with_fields <- function(kernel, ...) {
  fields <- list(...)
  for (nm in names(fields)) kernel[nm] <- list(fields[[nm]])
  kernel
}

simple_kernel <- function() {
  decay_kernel(function(distance, sigma) exp(-distance / sigma),
               name = "simple", parameter = "scale")
}

test_that("built-in declarations carry the built-in meaning", {
  expected <- list(
    gaussian = list(parameter = "travel scale", zero_distance = TRUE),
    exponential = list(parameter = "rate", zero_distance = TRUE),
    power = list(parameter = "exponent", zero_distance = FALSE)
  )
  for (family in names(expected)) {
    k <- decay_kernel(family)
    expect_s3_class(k, "spax_decay_kernel")
    expect_identical(names(k), kernel_fields)
    expect_identical(
      unclass(k),
      list(name = family, builtin = TRUE, parameter = expected[[family]]$parameter,
           lower = 0, upper = Inf, zero_distance = expected[[family]]$zero_distance,
           smooth = TRUE, args = list(), code = NA_character_),
      info = family
    )
    # an alias of the string everywhere
    expect_identical(.resolve_decay_family(k), list(family = family, kernel = NULL))
    expect_no_error(.validate_decay_kernel(k))
  }
})

test_that("a built-in declaration gives the weights of its string, including the string defaults", {
  formulas <- list(
    gaussian = function(d, s) exp(-d^2 / (2 * s^2)),
    exponential = function(d, s) exp(-s * d),
    power = function(d, s) d^(-s)
  )
  sigma <- c(gaussian = 12, exponential = 0.15, power = 1.5)
  d <- c(0, 0.5, 1, 4, 10, 25, NA)
  d_matrix <- matrix(d[1:6], 2, dimnames = list(c("a", "b"), NULL))
  r <- toy_rast(c(1, 4, 9, 16, NA, 30), nrows = 2, ncols = 3, nlyr = 2,
                names = c("x", "y"))
  for (family in names(formulas)) {
    k <- decay_kernel(family)
    s <- sigma[[family]]
    f <- formulas[[family]]
    expect_equal(calc_decay(d, k, sigma = s), f(d, s), info = family)
    expect_equal(calc_decay(d_matrix, k, sigma = s), f(d_matrix, s), info = family)
    expect_equal(terra::values(calc_decay(r, k, sigma = s)), f(terra::values(r), s),
                 info = family)
    # exactly the string path: same defaults, snap and error messages
    expect_identical(calc_decay(d, k, sigma = s), calc_decay(d, family, sigma = s))
    expect_identical(calc_decay(d, k), calc_decay(d, family))
    expect_identical(calc_decay(d, k, sigma = s, snap = TRUE),
                     calc_decay(d, family, sigma = s, snap = TRUE))
  }
  expect_equal(calc_decay(0, decay_kernel("power"), sigma = 2), Inf)
  expect_error(calc_decay(c(1, 2), decay_kernel("gaussian"), sigma = -1), "sigma must be >")
  # a variable catchment works as for the string
  d_rast <- toy_rast(as.numeric(1:16), 4, 4)
  sig <- toy_rast(seq(2, 8, length.out = 16), 4, 4)
  expect_identical(terra::values(calc_decay(d_rast, decay_kernel("gaussian"), sigma = sig)),
                   terra::values(calc_decay(d_rast, "gaussian", sigma = sig)))
})

test_that("a built-in declaration refuses every other argument and unknown names", {
  extra <- list(name = "x", parameter = "scale", lower = 1, upper = 5,
                zero_distance = FALSE, smooth = FALSE, args = list(a = 1))
  for (arg in names(extra)) {
    expect_error(do.call(decay_kernel, c(list("gaussian"), extra[arg])),
                 "a built-in kernel takes no other arguments", info = arg)
  }
  # a value equal to the default still counts as supplied
  expect_error(decay_kernel("power", lower = 0), "takes no other arguments")
  expect_error(decay_kernel("power", name = NULL), "takes no other arguments")
  for (bad in list("inverse", "binary", "Gaussian", "gauss", "", NA_character_,
                   c("gaussian", "power"), character(0), 42, NULL, TRUE)) {
    expect_error(decay_kernel(bad), "fun must be", info = deparse(bad))
  }
})

test_that("a braced custom kernel is stored as exact text and rebuilt identically", {
  # A braced function: with source references, naive storage compares unequal.
  plateau_fun <- function(distance, sigma, plateau = 5, offset = 0.1) {
    # This comment is not stored.
    offset + (1 - offset) * ifelse(distance <= plateau, 1,
                                   exp(-(distance - plateau)^2 / (2 * sigma^2)))
  }
  k <- decay_kernel(plateau_fun, name = "plateau", parameter = "scale",
                    args = list(plateau = 5))
  expect_s3_class(k, "spax_decay_kernel")
  expect_identical(names(k), kernel_fields)
  expect_identical(
    unclass(k)[setdiff(kernel_fields, "code")],
    list(name = "plateau", builtin = FALSE, parameter = "scale", lower = 0,
         upper = Inf, zero_distance = TRUE, smooth = TRUE,
         args = list(plateau = 5))
  )
  expect_type(k$code, "character")
  expect_length(k$code, 1L)
  expect_false(grepl("comment", k$code))

  fn <- .decay_kernel_function(k)
  fn0 <- utils::removeSource(plateau_fun)
  expect_type(fn, "closure")
  expect_identical(environment(fn), baseenv())
  expect_null(attr(fn, "srcref"))
  expect_identical(formals(fn), formals(fn0))
  expect_identical(body(fn), body(fn0))
  d <- c(0, 0.5, 5, 7, 40, 1000)
  expect_identical(fn(d, 4), plateau_fun(d, 4))
  expect_equal(fn(d, 4), 0.1 + 0.9 * ifelse(d <= 5, 1, exp(-(d - 5)^2 / 32)))

  # The declaration is plain data: it survives serialization unchanged.
  expect_identical(unserialize(serialize(k, NULL)), k)
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(k, path)
  restored <- readRDS(path)
  expect_identical(restored, k)
  expect_identical(.decay_kernel_function(restored)(d, 4), plateau_fun(d, 4))
  expect_no_error(.validate_decay_kernel(restored))
})

test_that("source references and nested braced functions do not break exact storage", {
  # Functions parsed with keep.source = TRUE carry srcrefs, as in interactive R.
  src <- paste0("function(distance, sigma, plateau = 5) {\n",
                "  # kept in the source reference only\n",
                "  ifelse(distance <= plateau, 1, exp(-(distance - plateau)^2 / (2 * sigma^2)))\n",
                "}")
  with_src <- eval(parse(text = src, keep.source = TRUE))
  expect_false(is.null(attr(with_src, "srcref")))
  k <- decay_kernel(with_src, name = "plateau", parameter = "scale",
                    args = list(plateau = 5))
  expect_false(grepl("kept in the source", k$code))
  d <- c(0, 3, 5, 12, 80)
  expect_identical(.decay_kernel_function(k)(d, 4), with_src(d, 4))
  expect_equal(.decay_kernel_function(k)(d, 4), plateau_weights(d, 4))

  # A braced function containing another braced function.
  nested_src <- paste0("function(distance, sigma) {\n",
                       "  decay <- function(z) {\n",
                       "    exp(-z^2 / 2)\n",
                       "  }\n",
                       "  decay(distance / sigma)\n",
                       "}")
  nested <- eval(parse(text = nested_src, keep.source = TRUE))
  expect_false(is.null(attr(nested, "srcref")))
  k_nested <- decay_kernel(nested, name = "nested", parameter = "scale")
  fn <- .decay_kernel_function(k_nested)
  expect_identical(body(fn), body(utils::removeSource(nested)))
  expect_identical(fn(d, 4), nested(d, 4))
  expect_equal(fn(d, 4), exp(-(d / 4)^2 / 2))

  # The same two shapes written directly in the test (whatever keep.source is).
  nested_direct <- function(distance, sigma) {
    decay <- function(z) {
      exp(-z^2 / 2)
    }
    decay(distance / sigma)
  }
  k_direct <- decay_kernel(nested_direct, name = "nested_direct", parameter = "scale")
  expect_identical(.decay_kernel_function(k_direct)(d, 4), nested_direct(d, 4))
  expect_identical(unserialize(serialize(k_direct, NULL)), k_direct)

  # A lambda is stored as a function.
  k_lambda <- decay_kernel(\(distance, sigma) {
    exp(-distance / sigma)
  }, name = "lambda", parameter = "scale")
  expect_equal(.decay_kernel_function(k_lambda)(d, 4), exp(-d / 4))

  # Exactly the same declaration comes from the same code with or without srcrefs.
  with_refs <- decay_kernel(with_src, name = "k", parameter = "scale", args = list(plateau = 5))
  without_refs <- decay_kernel(utils::removeSource(with_src), name = "k",
                               parameter = "scale", args = list(plateau = 5))
  expect_identical(with_refs, without_refs)
})

test_that("a custom declaration refuses a function that is not a closure with distance and sigma", {
  ok <- function(distance, sigma) exp(-distance / sigma)
  declare <- function(fun, ...) decay_kernel(fun, name = "k", parameter = "scale", ...)

  expect_error(declare(sum), "fun must be a closure")
  expect_error(declare(`+`), "fun must be a closure")
  expect_error(declare(42), "fun must be")
  expect_error(declare(list(ok)), "fun must be")
  for (bad in list(function(x, sigma) exp(-x / sigma),
                   function(distance, s) exp(-distance / s),
                   function(sigma, distance) exp(-distance / sigma),
                   function(distance) exp(-distance),
                   function(...) 1,
                   function() 1)) {
    expect_error(declare(bad), "named distance and sigma", info = deparse(bad)[1])
  }

  # Other arguments need a default, an entry in args or must be `...`.
  without_default <- function(distance, sigma, p) exp(-p * distance / sigma)
  expect_error(declare(without_default), "needs a default")
  expect_error(declare(without_default), "missing: p")
  k <- declare(without_default, args = list(p = 2))
  expect_identical(do.call(.decay_kernel_function(k), c(list(c(1, 2), 3), k$args)),
                   exp(-2 * c(1, 2) / 3))
  expect_equal(calc_decay(c(1, 2), k, sigma = 3), exp(-2 * c(1, 2) / 3))
  with_dots <- function(distance, sigma, ...) exp(-distance / sigma)
  expect_s3_class(declare(with_dots), "spax_decay_kernel")
  with_default <- function(distance, sigma, p = 2) exp(-p * distance / sigma)
  expect_s3_class(declare(with_default), "spax_decay_kernel")
})

test_that("a custom declaration refuses a missing or invalid name and parameter", {
  ok <- function(distance, sigma) exp(-distance / sigma)
  expect_error(decay_kernel(ok, parameter = "scale"), "name is required")
  expect_error(decay_kernel(ok, name = "k"), "parameter is required")
  expect_error(decay_kernel(ok, name = NULL, parameter = "scale"), "name is required")

  for (reserved in c("gaussian", "Exponential", "POWER", "inverse", "Binary", "NONE")) {
    expect_error(decay_kernel(ok, name = reserved, parameter = "scale"),
                 "name cannot be", info = reserved)
  }
  for (bad in list("1plateau", "has space", "_x", "a/b", "a+b", "", NA_character_,
                   c("a", "b"), character(0), 5, TRUE)) {
    expect_error(decay_kernel(ok, name = bad, parameter = "scale"),
                 "name must be one string", info = deparse(bad))
  }
  for (good in c("plateau", "A", "a1", "a_b.c-d", "gaussian2", "power_law")) {
    expect_s3_class(decay_kernel(ok, name = good, parameter = "scale"),
                    "spax_decay_kernel")
  }
  for (bad in list("", NA_character_, c("a", "b"), character(0), 5, TRUE)) {
    expect_error(decay_kernel(ok, name = "k", parameter = bad),
                 "parameter must be one nonempty string", info = deparse(bad))
  }
})

test_that("a custom declaration refuses invalid bounds and flags", {
  ok <- function(distance, sigma) exp(-distance / sigma)
  declare <- function(...) decay_kernel(ok, name = "k", parameter = "scale", ...)

  for (bounds in list(list(lower = -1), list(lower = 1, upper = 1), list(lower = 2, upper = 1),
                      list(upper = 0), list(lower = Inf), list(upper = -Inf),
                      list(upper = NA_real_), list(lower = NA_real_), list(lower = NaN))) {
    expect_error(do.call(declare, bounds), "0 <= lower < upper", info = deparse(bounds))
  }
  expect_error(declare(lower = "a"), "lower must be numeric")
  expect_error(declare(lower = TRUE), "lower must be numeric")
  expect_error(declare(lower = c(0, 1)), "lower must have length 1")
  expect_error(declare(upper = "a"), "upper must be numeric")
  expect_error(declare(upper = c(1, 2)), "upper must have length 1")

  # Bounds are inclusive and coerced to double.
  k <- declare(lower = 1L, upper = 5L)
  expect_identical(c(k$lower, k$upper), c(1, 5))
  expect_type(k$lower, "double")
  expect_identical(declare(lower = 0.5, upper = Inf)$upper, Inf)

  for (bad in list(NA, "yes", c(TRUE, FALSE), 1, NULL)) {
    expect_error(declare(zero_distance = bad), "zero_distance must be TRUE or FALSE",
                 info = deparse(bad))
    expect_error(declare(smooth = bad), "smooth must be TRUE or FALSE", info = deparse(bad))
  }
  k <- declare(zero_distance = FALSE, smooth = FALSE)
  expect_false(k$zero_distance)
  expect_false(k$smooth)
})

test_that("a custom declaration refuses invalid args", {
  scaled <- function(distance, sigma, p = 1, label = "x") exp(-distance / sigma)
  declare <- function(args) {
    decay_kernel(scaled, name = "k", parameter = "scale", args = args)
  }

  expect_error(declare(5), "args must be a named list")
  expect_error(declare("p"), "args must be a named list")
  expect_error(declare(data.frame(p = 1)), "args must be a named list")
  expect_error(declare(list(1)), "unique, nonempty names")
  expect_error(declare(list(p = 1, p = 2)), "unique, nonempty names")
  expect_error(declare(stats::setNames(list(1, 2), c("p", ""))), "unique, nonempty names")
  expect_error(declare(stats::setNames(list(1, 2), c("p", NA))), "unique, nonempty names")
  # names must be arguments of fun other than distance and sigma
  expect_error(declare(list(q = 1)), "other than distance and sigma: q")
  expect_error(declare(list(distance = 1)), "other than distance and sigma: distance")
  expect_error(declare(list(sigma = 1)), "other than distance and sigma: sigma")
  expect_error(declare(list(p = 1, q = 2, r = 3)), "q, r")
  # values must be plain numeric, logical or character vectors
  for (bad in list(c(a = 1), NULL, sum, list(1), factor("a"), as.Date("2020-01-01"),
                   1 + 2i, matrix(1:4, 2), structure(1, units = "min"))) {
    expect_error(declare(list(p = bad)), "without attributes: p", info = deparse(bad)[1])
  }

  for (good in list(2, 2L, TRUE, "a", c(1, 2), NA_real_)) {
    k <- declare(list(p = good))
    expect_identical(k$args, list(p = good))
  }
  k <- declare(list(label = "a", p = 3))
  expect_identical(names(k$args), c("label", "p"))
  # an empty args is stored as list(), whatever its names
  expect_identical(declare(stats::setNames(list(), character(0)))$args, list())
  expect_identical(declare(list())$args, list())
})

test_that("the kernel may use only its arguments, base R, pkg::fn calls and args", {
  declare <- function(fun, ...) decay_kernel(fun, name = "k", parameter = "scale", ...)
  d <- c(0, 1, 5, 30)

  # An object in the workspace is not visible to the stored code.
  z <- 3
  uses_workspace <- function(distance, sigma) exp(-distance / sigma) * z
  expect_error(declare(uses_workspace), "the kernel failed on a test grid")
  expect_error(declare(uses_workspace), "object 'z' not found")
  helper <- function(x) exp(-x)
  uses_helper <- function(distance, sigma) helper(distance / sigma)
  expect_error(declare(uses_helper), "could not find function")
  # Functions of other base packages need their namespace.
  without_namespace <- function(distance, sigma) 2 * pnorm(-distance / sigma)
  expect_error(declare(without_namespace), "test grid")
  expect_error(declare(without_namespace), "could not find function \"pnorm\"")
  with_namespace <- function(distance, sigma) 2 * stats::pnorm(-distance / sigma)
  k <- declare(with_namespace)
  expect_equal(.decay_kernel_function(k)(d, 3), 2 * stats::pnorm(-d / 3))
  # The message names the allowed ingredients.
  expect_error(declare(uses_workspace), "`pkg::fn` calls and `args`", fixed = TRUE)

  # A free variable with a base name resolves differently in the workspace.
  T <- 0.5
  uses_t <- function(distance, sigma) exp(-distance / sigma) * T
  expect_error(declare(uses_t), "different values when rebuilt")
  pi <- 3
  uses_pi <- function(distance, sigma) exp(-distance / (sigma * pi))
  expect_error(declare(uses_pi), "different values when rebuilt")
  # Base R itself is available, including functions that take functions.
  vapplied <- function(distance, sigma) {
    vapply(distance, function(x) exp(-x / sigma), numeric(1))
  }
  k <- declare(vapplied)
  expect_equal(.decay_kernel_function(k)(d, 3), exp(-d / 3))

  # An error inside the kernel is reported with its message.
  expect_error(declare(function(distance, sigma) stop("boom")),
               "the kernel failed on a test grid.*: boom")
})

test_that("the kernel must return finite, nonnegative, non-increasing values", {
  declare <- function(fun, ...) decay_kernel(fun, name = "k", parameter = "scale", ...)

  expect_error(declare(function(distance, sigma) 1 - exp(-distance / sigma)),
               "must not increase with distance")
  expect_error(declare(function(distance, sigma) exp(-distance / sigma) - 0.5),
               "must return nonnegative values")
  expect_error(declare(function(distance, sigma) ifelse(distance > 100, NaN, exp(-distance / sigma))),
               "must return finite values; it returned NaN at distance 200")
  expect_error(declare(function(distance, sigma) ifelse(distance > 100, NA_real_, exp(-distance / sigma))),
               "must return finite values")
  expect_error(declare(function(distance, sigma) 1 / (distance * sigma)),
               "must return finite values; it returned Inf at distance 0")
  expect_error(declare(function(distance, sigma) ifelse(distance > 100, -Inf, 1)),
               "must return finite values")

  # wrong type or length
  expect_error(declare(function(distance, sigma) 1), "numeric vector with one value per distance")
  expect_error(declare(function(distance, sigma) exp(-distance / sigma)[-1]),
               "numeric vector with one value per distance")
  expect_error(declare(function(distance, sigma) as.character(exp(-distance / sigma))),
               "numeric vector with one value per distance")
  expect_error(declare(function(distance, sigma) NULL), "numeric vector with one value per distance")
  expect_error(declare(function(distance, sigma) as.list(exp(-distance / sigma))),
               "numeric vector with one value per distance")
  expect_error(declare(function(distance, sigma) exp(-distance / sigma) > 0.5),
               "numeric vector with one value per distance")

  # Numerical noise below the tolerance is not an increase; larger noise is.
  expect_s3_class(
    declare(function(distance, sigma) exp(-distance / sigma) + 1e-14 * abs(sin(distance))),
    "spax_decay_kernel"
  )
  expect_error(
    declare(function(distance, sigma) exp(-distance / sigma) + 1e-9 * abs(sin(distance))),
    "must not increase"
  )
  # Integer weights are numeric; equal values are non-increasing.
  steps <- function(distance, sigma) 1L * (distance <= 10)
  expect_s3_class(declare(steps, smooth = FALSE), "spax_decay_kernel")
  expect_s3_class(declare(function(distance, sigma) rep(0.5, length(distance))),
                  "spax_decay_kernel")
})

test_that("the declaration check uses the documented distances and sigma values", {
  declare <- function(fun, ...) decay_kernel(fun, name = "k", parameter = "scale", ...)

  # A zero distance is probed unless the kernel is declared undefined there.
  power_like <- function(distance, sigma) distance^(-sigma)
  expect_error(declare(power_like), "returned Inf at distance 0")
  k <- declare(power_like, zero_distance = FALSE)
  expect_false(k$zero_distance)
  expect_equal(.decay_kernel_function(k)(c(1, 4), 2), c(1, 4)^-2)
  fails_only_at_zero <- function(distance, sigma) ifelse(distance == 0, NaN, exp(-distance / sigma))
  expect_error(declare(fails_only_at_zero), "finite")
  expect_s3_class(declare(fails_only_at_zero, zero_distance = FALSE), "spax_decay_kernel")
  # The grid runs from 0.1 to 1000.
  expect_error(declare(function(distance, sigma) ifelse(distance == 0.1, NaN, exp(-distance / sigma))),
               "finite")
  expect_error(declare(function(distance, sigma) ifelse(distance >= 1000, NaN, exp(-distance / sigma))),
               "finite")
  expect_s3_class(declare(function(distance, sigma) ifelse(distance > 1000, NaN, exp(-distance / sigma))),
                  "spax_decay_kernel")

  # Unbounded sigma is probed at 0.01, 1 and 100.
  fails_above_50 <- function(distance, sigma) exp(-distance / sigma) * (if (sigma > 50) NaN else 1)
  expect_error(declare(fails_above_50), "finite")
  expect_s3_class(declare(fails_above_50, upper = 10), "spax_decay_kernel")
  fails_below_half <- function(distance, sigma) exp(-distance / sigma) * (if (sigma < 0.5) NaN else 1)
  expect_error(declare(fails_below_half), "finite")
  # The first probe value is the lower bound itself, not a rounded copy of it.
  expect_s3_class(declare(fails_below_half, lower = 0.5), "spax_decay_kernel")
  # exp(log(5)) and exp(log(10)) differ from 5 and 10 in the last bits; the probe
  # values stay inside the inclusive bounds.
  fails_outside <- function(distance, sigma) {
    exp(-distance / sigma) * (if (sigma < 5 || sigma > 10) NaN else 1)
  }
  expect_s3_class(declare(fails_outside, lower = 5, upper = 10), "spax_decay_kernel")

  # An interval with nothing between 0.01 and 100 is probed at its midpoint; an
  # unbounded one above 100 at twice its lower bound.
  only_155 <- function(distance, sigma) {
    exp(-distance / sigma) * (if (abs(sigma - 155) < 1e-9) 1 else NaN)
  }
  expect_s3_class(declare(only_155, lower = 150, upper = 160), "spax_decay_kernel")
  expect_error(declare(only_155, lower = 150, upper = 200), "finite")
  only_400 <- function(distance, sigma) {
    exp(-distance / sigma) * (if (abs(sigma - 400) < 1e-9) 1 else NaN)
  }
  expect_s3_class(declare(only_400, lower = 200), "spax_decay_kernel")
  expect_error(declare(only_400, lower = 300), "finite")
  only_tiny <- function(distance, sigma) {
    exp(-distance / sigma) * (if (abs(sigma - 0.0025) < 1e-12) 1 else NaN)
  }
  expect_s3_class(declare(only_tiny, lower = 0, upper = 0.005), "spax_decay_kernel")
})

test_that("the kernel function must be storable exactly", {
  # A vector embedded in the body does not survive deparse and parse.
  embedded <- function(distance, sigma) distance
  body(embedded) <- call("*", quote(exp(-distance / sigma)), c(1, 1))
  expect_error(decay_kernel(embedded, name = "k", parameter = "scale"),
               "the kernel function cannot be stored exactly")
  # An object with attributes on the function itself is not stored.
  classed <- structure(function(distance, sigma) exp(-distance / sigma), class = "special")
  k <- decay_kernel(classed, name = "k", parameter = "scale")
  expect_equal(.decay_kernel_function(k)(c(0, 1), 2), exp(-c(0, 1) / 2))
})

test_that(".decay_kernel_function() evaluates only a single function expression", {
  k <- simple_kernel()
  expect_equal(.decay_kernel_function(k)(c(0, 2), 2), exp(-c(0, 2) / 2))
  flag <- "spax_decay_kernel_tampering_flag"
  expect_false(exists(flag, envir = globalenv(), inherits = FALSE))
  tampered <- c(
    "1 + 1", "x", "42", "", "   ",
    "function(distance, sigma", # syntax error
    "function(distance, sigma) 1\nfunction(distance, sigma) 2",
    "(function(distance, sigma) 1)",
    "f <- function(distance, sigma) 1",
    "{ function(distance, sigma) 1 }",
    "stop('ran the code')",
    sprintf("assign('%s', TRUE, envir = globalenv())", flag),
    sprintf("{ assign('%s', TRUE, envir = globalenv()); function(distance, sigma) 1 }", flag)
  )
  for (code in tampered) {
    expect_error(.decay_kernel_function(with_code(k, code)),
                 "the kernel code must be a single function expression", info = code)
  }
  expect_error(.decay_kernel_function(with_code(k, NA_character_)), "single function expression")
  expect_error(.decay_kernel_function(with_code(k, character(0))), "single function expression")
  expect_false(exists(flag, envir = globalenv(), inherits = FALSE))
  # Evaluating a function expression does not run its body.
  lazy <- with_code(k, "function(distance, sigma) stop('body ran')")
  fn <- .decay_kernel_function(lazy)
  expect_error(fn(1, 1), "body ran")
})

test_that(".validate_decay_kernel() accepts declarations and refuses malformed ones", {
  custom <- decay_kernel(function(distance, sigma, plateau, shape = "a") {
    ifelse(distance <= plateau, 1, exp(-(distance - plateau) / sigma))
  }, name = "plateau", parameter = "scale", lower = 0.5, upper = 50,
  zero_distance = FALSE, smooth = FALSE, args = list(plateau = 5))
  expect_identical(.validate_decay_kernel(custom), custom)
  expect_invisible(.validate_decay_kernel(custom))
  expect_identical(.validate_decay_kernel(decay_kernel("power")), decay_kernel("power"))

  refuse <- function(x, info = NULL) {
    expect_error(.validate_decay_kernel(x), "invalid decay kernel declaration", info = info)
  }
  # class and fields
  refuse(NULL, "NULL")
  refuse("gaussian", "string")
  refuse(unclass(custom), "no class")
  refuse(structure(unclass(custom), class = c("spax_decay_kernel", "list")), "extra class")
  refuse(structure(unclass(custom)[rev(names(custom))], class = "spax_decay_kernel"), "order")
  refuse(structure(unclass(custom)[-9], class = "spax_decay_kernel"), "missing field")
  refuse(structure(c(unclass(custom), list(extra = 1)), class = "spax_decay_kernel"), "extra field")
  # types
  refuse(with_fields(custom, name = 5), "name")
  refuse(with_fields(custom, name = c("a", "b")), "name length")
  refuse(with_fields(custom, builtin = NA), "builtin")
  refuse(with_fields(custom, parameter = ""), "parameter")
  refuse(with_fields(custom, lower = "a"), "lower type")
  refuse(with_fields(custom, lower = 1L), "integer bound")
  refuse(with_fields(custom, upper = NA_real_), "upper")
  refuse(with_fields(custom, zero_distance = NA), "zero_distance")
  refuse(with_fields(custom, smooth = "TRUE"), "smooth")
  refuse(with_fields(custom, args = 1), "args")
  refuse(with_fields(custom, code = c("a", "b")), "code length")
  refuse(with_fields(custom, code = NA_character_), "code NA")
  # bounds
  refuse(with_fields(custom, lower = -1), "negative lower")
  refuse(with_fields(custom, lower = 50), "lower equals upper")
  refuse(with_fields(custom, lower = 60), "lower above upper")
  # names
  refuse(with_fields(custom, name = "Gaussian"), "reserved name")
  refuse(with_fields(custom, name = "1x"), "bad name")
  # built-in declarations must match the built-in kernel exactly
  refuse(with_fields(custom, builtin = TRUE), "custom as built-in")
  refuse(with_fields(decay_kernel("gaussian"), parameter = "scale"), "built-in label")
  refuse(with_fields(decay_kernel("gaussian"), lower = 1), "built-in bound")
  refuse(with_fields(decay_kernel("gaussian"), name = "inverse"), "built-in name")
  refuse(with_fields(decay_kernel("gaussian"), code = "function(distance, sigma) 1"),
         "built-in code")
  # code
  refuse(with_code(custom, "1 + 1"), "not a function")
  refuse(with_code(custom, "function(x, sigma) 1"), "formals")
  refuse(with_code(custom, "function(distance) 1"), "formals one")
  refuse(with_fields(custom, args = list(zzz = 1)), "args name not a formal")
  refuse(with_fields(custom, args = list(plateau = 5, plateau = 6)), "duplicated args")
  refuse(with_fields(custom, args = list(plateau = c(a = 5))), "args value attributes")
  refuse(with_fields(custom, args = list()), "formal without default or args")

  # Text layout is not compared: other whitespace and a different deparse validate.
  expect_no_error(.validate_decay_kernel(with_code(custom, paste0("\n\n  ", gsub("\n", "\n\n", custom$code), "\n"))))
  expect_no_error(.validate_decay_kernel(
    with_code(custom, "function(distance, sigma, plateau, shape = 'a') exp(-distance / sigma)")))
})

test_that("calc_decay() evaluates a declared kernel on finite distances and keeps the input's shape", {
  plateau_fun <- function(distance, sigma, plateau = 5) {
    ifelse(distance <= plateau, 1, exp(-(distance - plateau)^2 / (2 * sigma^2)))
  }
  k <- decay_kernel(plateau_fun, name = "plateau", parameter = "scale",
                    args = list(plateau = 5))

  # vectors keep their names; a missing distance has no weight and an infinite
  # one (an absent edge) the weight 0, as with the built-in kernels
  d <- c(a = 0, b = 2, c = 5, d = NA, e = 10, f = Inf, g = NaN, h = 20)
  w <- calc_decay(d, k, sigma = 4)
  finite <- is.finite(d)
  expect_identical(names(w), names(d))
  expect_type(w, "double")
  expect_true(all(is.na(w[is.na(d)])))
  expect_identical(unname(w["f"]), 0)
  expect_equal(w[finite], calc_decay(d[finite], plateau_fun, sigma = 4))
  expect_equal(unname(w[finite]), plateau_weights(unname(d[finite]), 4))
  expect_equal(calc_decay(1:10, k, sigma = 4), plateau_weights(1:10, 4))
  expect_length(calc_decay(numeric(0), k, sigma = 4), 0)
  expect_identical(calc_decay(c(NA, Inf), k, sigma = 4), c(NA_real_, 0))
  expect_identical(calc_decay(Inf, "gaussian", sigma = 4), 0)
  # the plain function still gets every distance (today's behavior)
  expect_equal(calc_decay(c(1, Inf, NA), plateau_fun, sigma = 4), c(1, 0, NA))

  # matrices keep dim and dimnames
  m <- matrix(c(1, NA, 6, 12, Inf, 3), 2, dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
  wm <- calc_decay(m, k, sigma = 4)
  expect_identical(dim(wm), dim(m))
  expect_identical(dimnames(wm), dimnames(m))
  expected <- matrix(NA_real_, 2, 3, dimnames = dimnames(m))
  expected[is.finite(m)] <- plateau_weights(m[is.finite(m)], 4)
  expected[!is.na(m) & m == Inf] <- 0
  expect_equal(wm, expected)

  # rasters keep geometry, layer names and missing cells
  r <- toy_rast(c(1, 4, 9, NA, 16, 30), nrows = 2, ncols = 3, nlyr = 2, names = c("x", "y"))
  wr <- calc_decay(r, k, sigma = 4)
  expect_s4_class(wr, "SpatRaster")
  expect_true(terra::compareGeom(wr, r))
  expect_identical(names(wr), c("x", "y"))
  v <- terra::values(r)
  vw <- terra::values(wr)
  expect_identical(is.na(vw), is.na(v))
  expect_equal(vw[!is.na(v)], plateau_weights(v[!is.na(v)], 4))
  # a function written with arithmetic only also runs on a raster: same weights
  arithmetic <- function(distance, sigma) exp(-distance / sigma) / (1 + distance)
  k_arithmetic <- decay_kernel(arithmetic, name = "arithmetic", parameter = "scale")
  w_plain <- calc_decay(r, arithmetic, sigma = 4)
  w_declared <- calc_decay(r, k_arithmetic, sigma = 4)
  expect_true(terra::compareGeom(w_declared, w_plain))
  expect_identical(names(w_declared), names(w_plain))
  expect_equal(terra::values(w_declared), terra::values(w_plain))

  # snap skips input validation and changes nothing else
  expect_identical(calc_decay(d, k, sigma = 4, snap = TRUE), w)
  expect_identical(terra::values(calc_decay(r, k, sigma = 4, snap = TRUE)), vw)

  # fixed arguments come from the declaration
  k8 <- decay_kernel(plateau_fun, name = "plateau8", parameter = "scale",
                     args = list(plateau = 8))
  expect_identical(calc_decay(c(6, 7), k8, sigma = 4), c(1, 1))
  expect_true(all(calc_decay(c(6, 7), k, sigma = 4) < 1))
})

test_that("calc_decay() keeps the geometry of a projected raster", {
  skip_if_no_proj()
  k <- simple_kernel()
  r <- toy_rast(c(1, 4, 9, 16), nrows = 2, ncols = 2, nlyr = 1, names = "facility")
  terra::crs(r) <- "EPSG:32630"
  terra::ext(r) <- c(0, 2000, 0, 2000)
  w <- calc_decay(r, k, sigma = 10)
  expect_true(terra::compareGeom(w, r))
  expect_identical(terra::crs(w), terra::crs(r))
  expect_identical(names(w), "facility")
  expect_equal(unname(terra::values(w)[, 1]), exp(-c(1, 4, 9, 16) / 10))
})

test_that("calc_decay() checks sigma, extra arguments and distance for a declared kernel", {
  k <- simple_kernel()
  d <- c(1, 2, 3)
  bounded <- decay_kernel(function(distance, sigma) exp(-distance / sigma),
                          name = "bounded", parameter = "scale", lower = 1, upper = 10)

  expect_error(calc_decay(d, k), "sigma is required for a declared kernel")
  expect_error(calc_decay(d, k, snap = TRUE), "sigma is required")
  sig <- toy_rast(4, 3, 3)
  expect_error(calc_decay(toy_rast(1:9), k, sigma = sig),
               "a variable catchment needs a built-in method or a plain function")
  expect_error(calc_decay(d, k, sigma = sig), "a variable catchment needs")
  expect_error(calc_decay(d, k, sigma = 4, plateau = 3),
               "pass fixed arguments through decay_kernel(args = )", fixed = TRUE)
  expect_error(calc_decay(d, k, 4, FALSE, 3), "pass fixed arguments through")

  for (bad in list(0.5, 11, -1, "a", c(2, 3), NA_real_, NaN, Inf, TRUE)) {
    expect_error(calc_decay(d, bounded, sigma = bad), "sigma must be a single number from 1 to 10",
                 info = deparse(bad))
  }
  expect_equal(calc_decay(d, bounded, sigma = 1), exp(-d / 1))
  expect_equal(calc_decay(d, bounded, sigma = 10L), exp(-d / 10))
  # a lower bound of zero admits sigma = 0 (the kernel then decides the weights)
  expect_no_error(calc_decay(d, k, sigma = 0))

  expect_error(calc_decay("a", k, sigma = 1), "distance must be one of")
  expect_error(calc_decay(matrix("a", 2, 2), k, sigma = 1), "distance must be one of")
  expect_error(calc_decay(structure(c(1, 2), class = "km"), k, sigma = 1),
               "distance must be one of")
  expect_error(calc_decay(list(1, 2), k, sigma = 1), "distance must be one of")

  # a declaration is validated before use
  expect_error(calc_decay(d, with_fields(k, lower = -1), sigma = 1),
               "invalid decay kernel declaration")
  expect_error(calc_decay(d, with_code(k, "1 + 1"), sigma = 1),
               "single function expression")

  # a kernel that returns the wrong number of values at use time is refused
  one_value <- with_code(k, "function(distance, sigma) 1")
  expect_error(calc_decay(c(1, 2), one_value, sigma = 4),
               "one numeric value for each distance")
  as_text <- with_code(k, "function(distance, sigma) 'a'")
  expect_error(calc_decay(1, as_text, sigma = 4), "one numeric value for each distance")
})

test_that("a declared kernel works through spax_e2sfca() and spax_m2sfca()", {
  td <- fca_toy()
  d <- unname(terra::values(td$distance))
  D <- terra::values(td$demand)[, 1]
  S <- unname(td$supply_matrix)
  k <- decay_kernel(function(distance, sigma, plateau = 2) {
    ifelse(distance <= plateau, 1, exp(-(distance - plateau)^2 / (2 * sigma^2)))
  }, name = "plateau", parameter = "scale", args = list(plateau = 2))
  W <- plateau_weights(d, 2, plateau = 2)
  access <- function(result) unname(terra::values(result$accessibility))
  run <- function(fun, ..., distance = td$distance) {
    fun(td$demand, td$supply_df, distance, id_col = "id",
        supply_cols = c("doctors", "nurses"), ...)
  }

  decay <- list(method = k, sigma = 2)
  for (norm in c("identity", "standard", "semi")) {
    e2 <- run(spax_e2sfca, decay_params = decay, demand_normalize = norm)
    expect_equal(access(e2), fca_oracle(D, W, W, S, norm), info = norm)
  }
  expect_identical(e2$parameters$decay_params, decay)
  m2 <- run(spax_m2sfca, decay_params = decay, demand_normalize = "standard")
  expect_equal(access(m2), fca_oracle(D, W, W^2, S, "standard"))
  expect_equal(access(run(spax_e2sfca, decay_params = decay, snap = TRUE)),
               fca_oracle(D, W, W, S))

  # a missing travel time is an unreachable facility, as for a built-in kernel
  td$distance[1] <- NA
  dm <- unname(terra::values(td$distance))
  expect_true(anyNA(dm))
  Wm <- plateau_weights(dm, 2, plateau = 2)
  expect_equal(access(run(spax_e2sfca, decay_params = decay, demand_normalize = "standard",
                          distance = td$distance)),
               fca_oracle(D, Wm, Wm, S, "standard"))

  # a declared built-in kernel gives exactly the string's result
  expect_identical(
    access(run(spax_e2sfca, decay_params = list(method = decay_kernel("gaussian"), sigma = 2))),
    access(run(spax_e2sfca, decay_params = list(method = "gaussian", sigma = 2)))
  )
})

test_that("print() of FCA results names the decay method and sigma", {
  td <- fca_toy()
  run <- function(decay_params, fun = spax_e2sfca) {
    fun(td$demand, td$supply_df, td$distance, decay_params = decay_params,
        id_col = "id", supply_cols = c("doctors", "nurses"))
  }
  shown <- function(result) paste(utils::capture.output(print(result)), collapse = "\n")

  # a string, as before
  expect_match(shown(run(list(method = "gaussian", sigma = 2))),
               "- Decay function: gaussian (sigma = 2)", fixed = TRUE)
  expect_match(shown(spax_2sfca(td$demand, td$supply_df, td$distance, threshold = 3,
                                id_col = "id", supply_cols = "doctors")),
               "- Decay function: binary (sigma = 3)", fixed = TRUE)
  # a plain function (this used to fail)
  plain <- function(distance, sigma) exp(-distance^2 / (2 * sigma^2))
  expect_output(expect_invisible(print(run(list(method = plain, sigma = 2)))),
                "- Decay function: custom function (sigma = 2)", fixed = TRUE)
  expect_match(shown(run(list(method = plain, sigma = 2), spax_m2sfca)),
               "- Decay function: custom function (sigma = 2)", fixed = TRUE)
  # a declaration, by its name
  k <- decay_kernel(function(distance, sigma) exp(-distance / sigma),
                    name = "plain_exponential", parameter = "scale")
  expect_match(shown(run(list(method = k, sigma = 2))),
               "- Decay function: plain_exponential (sigma = 2)", fixed = TRUE)
  expect_match(shown(run(list(method = decay_kernel("power"), sigma = 1.5))),
               "- Decay function: power (sigma = 1.5)", fixed = TRUE)
  # a raster sigma (a variable catchment; this used to fail)
  expect_match(shown(run(list(method = "gaussian", sigma = toy_rast(2)))),
               "- Decay function: gaussian (sigma = variable (raster))", fixed = TRUE)
  # the rest of the report is unchanged
  expect_match(shown(run(list(method = k, sigma = 2))), "Measures: 2", fixed = TRUE)
})

test_that("print() describes a declaration and returns it invisibly", {
  k <- decay_kernel(function(distance, sigma, rate = 0.1, kind = "a") {
    exp(-rate * distance / sigma)
  }, name = "scaled", parameter = "scale", lower = 0.5, upper = 40,
  args = list(rate = 0.1, kind = "a"))
  expect_output(expect_invisible(print(k)), "<spax_decay_kernel> scaled", fixed = TRUE)
  out <- utils::capture.output(print(k))
  expect_true(all(c("sigma: scale", "bounds on sigma: 0.5 to 40",
                    "defined at distance 0: yes", "smooth in sigma: yes",
                    "fixed arguments: rate = 0.1, kind = \"a\"", "function:") %in% out))
  # the function is shown with ordinary numerals, not the exact (hex) ones
  expect_true(any(grepl("function ?\\(distance, sigma, rate = 0.1, kind = \"a\"\\)", out)))
  expect_false(any(grepl("0x", out, fixed = TRUE)))
  expect_match(k$code, "0x", fixed = TRUE)

  flags <- utils::capture.output(print(decay_kernel(
    function(distance, sigma) ifelse(distance <= sigma, 1, 0), name = "cutoff",
    parameter = "radius", zero_distance = FALSE, smooth = FALSE, lower = 1, upper = 120)))
  expect_true(all(c("<spax_decay_kernel> cutoff", "sigma: radius", "bounds on sigma: 1 to 120",
                    "defined at distance 0: no", "smooth in sigma: no",
                    "fixed arguments: none") %in% flags))

  builtin <- utils::capture.output(print(decay_kernel("gaussian")))
  expect_true(all(c("<spax_decay_kernel> gaussian (built-in)", "sigma: travel scale",
                    "bounds on sigma: 0 to Inf", "fixed arguments: none") %in% builtin))
  expect_true(any(grepl("exp\\(-\\(distance\\^2\\)", builtin)))

  # a declaration whose code cannot be rebuilt still prints
  expect_output(print(with_code(k, "1 + 1")), "the stored code cannot be rebuilt", fixed = TRUE)
})

test_that(".resolve_decay_family() and .decay_kernel_label() normalize decay methods", {
  # strings keep match.arg() semantics over the three built-in kernels
  expect_identical(.resolve_decay_family(c("gaussian", "exponential", "power")),
                   list(family = "gaussian", kernel = NULL))
  expect_identical(.resolve_decay_family("power"), list(family = "power", kernel = NULL))
  expect_identical(.resolve_decay_family("exp"), list(family = "exponential", kernel = NULL))
  expect_error(.resolve_decay_family("triangular"), "should be one of")
  expect_error(.resolve_decay_family(c("power", "gaussian")), "must be of length 1")
  expect_error(.resolve_decay_family(3), "must be NULL or a character vector")
  # declarations: built-ins are aliases, custom kernels travel with their family name
  expect_identical(.resolve_decay_family(decay_kernel("exponential")),
                   list(family = "exponential", kernel = NULL))
  k <- simple_kernel()
  expect_identical(.resolve_decay_family(k), list(family = "simple", kernel = k))
  expect_error(.resolve_decay_family(with_fields(k, lower = -1)), "invalid decay kernel declaration")

  expect_identical(.decay_kernel_label("gaussian"), "gaussian")
  expect_identical(.decay_kernel_label(k), "simple")
  expect_identical(.decay_kernel_label(decay_kernel("power")), "power")
  expect_identical(.decay_kernel_label(function(distance, sigma) 1), "custom function")
  expect_identical(.decay_kernel_label(sum), "custom function")
})
