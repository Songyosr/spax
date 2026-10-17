# Declared decay kernels ------------------------------------------------------

.decay_builtin_families <- c("gaussian", "exponential", "power")
.decay_reserved_names <- c(.decay_builtin_families, "inverse", "binary", "none")
.decay_kernel_fields <- c("name", "builtin", "parameter", "lower", "upper",
                          "zero_distance", "smooth", "args", "code")

#' Declare a distance decay kernel
#'
#' Declares the decay weights \eqn{w(d; \sigma)}{w(d; sigma)} that
#' [calc_decay()] and the FCA functions apply to a travel time or distance
#' \eqn{d}. A declaration is plain data: it names a built-in kernel, or it
#' records your own function as exact source text together with what its
#' parameter `sigma` means, the bounds `sigma` may take and any fixed extra
#' arguments. Nothing but data is stored, so a declaration can be compared,
#' saved and reloaded in a fresh session. This experimental function lets one
#' declaration be used wherever a decay `method` is accepted. A declaration can
#' also be passed as `family` to [clm_allocation()].
#'
#' **Built-in kernels.** `decay_kernel("gaussian")`, `decay_kernel("exponential")`
#' and `decay_kernel("power")` declare the kernels of the same names in
#' [calc_decay()]. They take no other argument and behave exactly like the
#' method strings. Their `sigma` is a travel scale, a rate and an exponent; the
#' power kernel is undefined at distance zero.
#'
#' **Your own function.** `fun` must be a closure whose first two arguments are
#' `distance` and `sigma`, and it must return one weight per distance. Every
#' other argument needs a default value, an entry in `args`, or is `...`. The
#' function is stored as text and rebuilt in the base environment whenever it
#' is used, so it may use only its arguments, base R, `pkg::fn` calls (for
#' example `stats::pnorm`) and `args`. It cannot refer to objects in your
#' workspace. Comments and layout are not kept.
#'
#' **Checks at declaration.** The function is run on a grid of distances (0.1
#' to 1000, and 0 unless `zero_distance = FALSE`) at a few values of `sigma`
#' inside the bounds. Every call must return a numeric vector with one value
#' per distance that is finite, nonnegative and non-increasing in distance, and
#' the values must be identical whether the function is run from its stored
#' text or as given. A function that fails a check is refused with the reason.
#'
#' **Saving.** The stored code is parsed, and turned into a function without
#' running it, whenever the kernel is used; running the kernel runs that code.
#' As with any R function, use declarations only from sources you trust.
#'
#' @param fun A built-in kernel name (`"gaussian"`, `"exponential"` or
#'   `"power"`), or a closure `function(distance, sigma, ...)` that returns the
#'   weights.
#' @param name A single string naming a custom kernel. It starts with a letter
#'   and continues with letters, digits, `_`, `.` or `-`, and it is not (in any
#'   capitalization) `gaussian`, `exponential`, `power`, `inverse`, `binary` or
#'   `none`. Required for a function; not allowed for a built-in kernel.
#' @param parameter A single nonempty string saying what `sigma` means for the
#'   kernel, for example `"scale"`. It labels printed output. Required for a
#'   function.
#' @param lower,upper Inclusive bounds on `sigma`, with
#'   `0 <= lower < upper <= Inf`.
#' @param zero_distance `FALSE` declares the kernel undefined at distance zero,
#'   as the power kernel is: the check at declaration then leaves zero out of
#'   its grid, and a model from [clm_allocation()] refuses prepared inputs that
#'   contain a finite zero travel time when it is evaluated or fitted.
#' @param smooth `FALSE` declares a kernel that is not smooth in `sigma` (steps
#'   or thresholds). It can be evaluated and used in FCA, and a model built with
#'   [clm_allocation()] can be evaluated at fixed parameters with
#'   [evaluate_allocation()], but [fit_allocation()] refuses it, because model
#'   fitting and bootstrapping need a kernel that is smooth in `sigma`.
#' @param args A named list of fixed extra arguments for `fun`. Its names must
#'   be arguments of `fun` other than `distance` and `sigma`; its values must be
#'   numeric, logical or character vectors without attributes.
#' @return A `spax_decay_kernel`: a list of plain data with the fields `name`,
#'   `builtin`, `parameter`, `lower`, `upper`, `zero_distance`, `smooth`, `args`
#'   and `code` (the function's exact source text, `NA` for a built-in kernel).
#' @examples
#' # A built-in declaration behaves like its method string
#' decay_kernel("gaussian")
#'
#' # A plateau kernel: full weight within 5 minutes, Gaussian decay beyond
#' plateau <- decay_kernel(
#'   function(distance, sigma, plateau = 5) {
#'     ifelse(distance <= plateau, 1, exp(-(distance - plateau)^2 / (2 * sigma^2)))
#'   },
#'   name = "plateau", parameter = "scale", args = list(plateau = 5)
#' )
#' plateau
#'
#' calc_decay(c(0, 5, 10, 20, NA), plateau, sigma = 8)
#' @family experimental allocation workflow
#' @export
#' @md
decay_kernel <- function(fun, name = NULL, parameter = NULL, lower = 0, upper = Inf,
                         zero_distance = TRUE, smooth = TRUE, args = list()) {
  fun_message <- "fun must be \"gaussian\", \"exponential\", \"power\" or a function"
  if (is.character(fun)) {
    if (length(fun) != 1L || is.na(fun) || !fun %in% .decay_builtin_families) {
      stop(fun_message)
    }
    supplied <- c(name = !missing(name), parameter = !missing(parameter),
                  lower = !missing(lower), upper = !missing(upper),
                  zero_distance = !missing(zero_distance), smooth = !missing(smooth),
                  args = !missing(args))
    if (any(supplied)) {
      stop("a built-in kernel takes no other arguments (remove: ",
           paste(names(supplied)[supplied], collapse = ", "), ")")
    }
    return(.builtin_decay_kernel(fun))
  }
  if (!is.function(fun)) stop(fun_message)
  if (is.primitive(fun)) stop("fun must be a closure, not a primitive function")
  formal_list <- as.list(formals(fun))
  if (length(formal_list) < 2L || !identical(names(formal_list)[1:2], c("distance", "sigma"))) {
    stop("the first two arguments of fun must be named distance and sigma")
  }
  if (is.null(name)) stop("name is required to declare a kernel from a function")
  if (is.null(parameter)) stop("parameter is required to declare a kernel from a function")
  .check_decay_kernel_name(name)
  if (!is.character(parameter) || length(parameter) != 1L || is.na(parameter) ||
      !nzchar(parameter)) {
    stop("parameter must be one nonempty string")
  }
  .chck_is_numeric(lower, "lower")
  .chck_length(length(lower), 1, "lower")
  .chck_is_numeric(upper, "upper")
  .chck_length(length(upper), 1, "upper")
  lower <- as.double(lower)
  upper <- as.double(upper)
  if (is.na(lower) || is.na(upper) || lower < 0 || lower >= upper) {
    stop("lower and upper must be numbers with 0 <= lower < upper <= Inf")
  }
  if (!is.logical(zero_distance) || length(zero_distance) != 1L || is.na(zero_distance)) {
    stop("zero_distance must be TRUE or FALSE")
  }
  if (!is.logical(smooth) || length(smooth) != 1L || is.na(smooth)) {
    stop("smooth must be TRUE or FALSE")
  }
  rest <- formal_list[-(1:2)]
  .check_decay_kernel_args(args, setdiff(names(rest), "..."))
  if (!length(args)) args <- list()
  .check_decay_kernel_formals(rest, names(args))

  # Source references and attributes are removed so that the stored text and
  # the comparison below describe the code only.
  fun0 <- utils::removeSource(fun)
  attributes(fun0) <- NULL
  code <- paste(deparse(fun0, control = c("keepInteger", "keepNA", "niceNames",
                                         "showAttributes", "hexNumeric")),
                collapse = "\n")
  kernel <- structure(
    list(name = name, builtin = FALSE, parameter = parameter, lower = lower,
         upper = upper, zero_distance = zero_distance, smooth = smooth,
         args = args, code = code),
    class = "spax_decay_kernel"
  )
  rebuilt <- tryCatch(.decay_kernel_function(kernel), error = function(e) NULL)
  if (is.null(rebuilt) || !identical(formals(rebuilt), formals(fun0)) ||
      !identical(body(rebuilt), body(fun0))) {
    stop("the kernel function cannot be stored exactly")
  }
  .probe_decay_kernel(kernel, fun, rebuilt)
  kernel
}

#' @rdname decay_kernel
#' @param x A `spax_decay_kernel`.
#' @param ... Unused.
#' @export
#' @md
print.spax_decay_kernel <- function(x, ...) {
  cat("<spax_decay_kernel> ", x$name, if (isTRUE(x$builtin)) " (built-in)", "\n", sep = "")
  cat("sigma: ", x$parameter, "\n", sep = "")
  cat("bounds on sigma: ", format(x$lower), " to ", format(x$upper), "\n", sep = "")
  cat("defined at distance 0: ", if (isTRUE(x$zero_distance)) "yes" else "no", "\n", sep = "")
  cat("smooth in sigma: ", if (isTRUE(x$smooth)) "yes" else "no", "\n", sep = "")
  if (length(x$args)) {
    shown <- vapply(x$args, function(value) paste(deparse(value), collapse = " "), character(1))
    cat("fixed arguments: ", paste(names(x$args), shown, sep = " = ", collapse = ", "),
        "\n", sep = "")
  } else {
    cat("fixed arguments: none\n")
  }
  fun <- tryCatch(.decay_kernel_function(x), error = function(e) NULL)
  if (is.null(fun)) {
    cat("function: the stored code cannot be rebuilt\n")
  } else {
    cat("function:\n")
    cat(deparse(fun), sep = "\n")
  }
  invisible(x)
}

# The declaration of one of the three built-in kernels.
.builtin_decay_kernel <- function(family) {
  parameter <- c(gaussian = "travel scale", exponential = "rate", power = "exponent")
  structure(
    list(name = family, builtin = TRUE, parameter = parameter[[family]], lower = 0,
         upper = Inf, zero_distance = !identical(family, "power"), smooth = TRUE,
         args = list(), code = NA_character_),
    class = "spax_decay_kernel"
  )
}

# The function a declaration stands for. Custom code is parsed, and only a single
# `function` expression is evaluated: that creates a closure in the base
# environment without running its body.
.decay_kernel_function <- function(kernel) {
  if (isTRUE(kernel$builtin)) {
    return(switch(kernel$name,
      gaussian = .help_decay_gaussian,
      exponential = .help_decay_exponential,
      power = .help_decay_power,
      stop("unknown built-in kernel")
    ))
  }
  exprs <- tryCatch(parse(text = kernel$code, keep.source = FALSE), error = function(e) NULL)
  if (length(exprs) != 1L || !is.call(exprs[[1L]]) ||
      !identical(exprs[[1L]][[1L]], as.name("function"))) {
    stop("the kernel code must be a single function expression")
  }
  eval(exprs[[1L]], baseenv())
}

# Evaluate a custom kernel on the finite entries of a numeric vector or matrix and
# return an array of the input's shape. An infinite distance is an absent edge and
# gets the weight 0, as with the built-in kernels; a missing distance keeps NA.
# `fun` is the rebuilt function when the caller already has it.
.eval_decay_kernel <- function(kernel, distance, sigma, fun = NULL) {
  finite <- is.finite(distance)
  out <- rep(NA_real_, length(distance))
  if (any(finite)) {
    if (is.null(fun)) fun <- .decay_kernel_function(kernel)
    weights <- do.call(fun, c(list(distance[finite], sigma), kernel$args))
    if (!is.numeric(weights) || length(weights) != sum(finite)) {
      stop("the kernel must return one numeric value for each distance")
    }
    out[finite] <- weights
  }
  out[!is.na(distance) & distance == Inf] <- 0
  if (is.null(dim(distance))) {
    names(out) <- names(distance)
  } else {
    dim(out) <- dim(distance)
    dimnames(out) <- dimnames(distance)
  }
  out
}

# Check a declaration read from outside this function (for example a saved
# snapshot): structure and types only, never a comparison of deparsed text.
.validate_decay_kernel <- function(x) {
  tryCatch(.validate_decay_kernel_fields(x), error = function(e) {
    stop("invalid decay kernel declaration: ", conditionMessage(e), call. = FALSE)
  })
  invisible(x)
}

.validate_decay_kernel_fields <- function(x) {
  if (!is.list(x) || !identical(class(x), "spax_decay_kernel") ||
      !identical(names(x), .decay_kernel_fields)) {
    stop("the class or the fields are wrong")
  }
  scalar_string <- function(value) is.character(value) && length(value) == 1L && !is.na(value)
  scalar_flag <- function(value) is.logical(value) && length(value) == 1L && !is.na(value)
  scalar_bound <- function(value) is.double(value) && length(value) == 1L && !is.na(value)
  if (!scalar_string(x$name) || !scalar_flag(x$builtin) || !scalar_string(x$parameter) ||
      !nzchar(x$parameter) || !scalar_bound(x$lower) || !scalar_bound(x$upper) ||
      !scalar_flag(x$zero_distance) || !scalar_flag(x$smooth)) {
    stop("a field has the wrong type")
  }
  if (x$lower < 0 || x$lower >= x$upper) {
    stop("the bounds must satisfy 0 <= lower < upper <= Inf")
  }
  if (x$builtin) {
    if (!x$name %in% .decay_builtin_families || !identical(x, .builtin_decay_kernel(x$name))) {
      stop("a built-in declaration must match its built-in kernel")
    }
    return(invisible(TRUE))
  }
  .check_decay_kernel_name(x$name)
  if (!scalar_string(x$code)) stop("code must be one string")
  formal_list <- as.list(formals(.decay_kernel_function(x)))
  if (length(formal_list) < 2L || !identical(names(formal_list)[1:2], c("distance", "sigma"))) {
    stop("the first two arguments of the function must be named distance and sigma")
  }
  rest <- formal_list[-(1:2)]
  .check_decay_kernel_args(x$args, setdiff(names(rest), "..."))
  .check_decay_kernel_formals(rest, names(x$args))
  invisible(TRUE)
}

.check_decay_kernel_name <- function(name) {
  if (!is.character(name) || length(name) != 1L || is.na(name) ||
      !grepl("^[A-Za-z][A-Za-z0-9_.-]*$", name)) {
    stop("name must be one string that starts with a letter and continues with letters, ",
         "digits, _, . or -")
  }
  if (tolower(name) %in% .decay_reserved_names) {
    stop("name cannot be gaussian, exponential, power, inverse, binary or none ",
         "(in any capitalization)")
  }
  invisible(TRUE)
}

# `args` is a plain list with unique nonempty names among the `allowed` argument
# names; its values are plain numeric, logical or character vectors.
.check_decay_kernel_args <- function(args, allowed) {
  if (!is.list(args) || is.object(args)) stop("args must be a named list")
  if (!length(args)) return(invisible(TRUE))
  nms <- names(args)
  if (is.null(nms) || anyNA(nms) || any(!nzchar(nms)) || anyDuplicated(nms)) {
    stop("args must have unique, nonempty names")
  }
  unknown <- setdiff(nms, allowed)
  if (length(unknown)) {
    stop("args names must be arguments of fun other than distance and sigma: ",
         paste(unknown, collapse = ", "))
  }
  plain <- vapply(args, function(value) {
    (is.numeric(value) || is.logical(value) || is.character(value)) &&
      is.null(attributes(value))
  }, logical(1))
  if (!all(plain)) {
    stop("args values must be numeric, logical or character vectors without attributes: ",
         paste(nms[!plain], collapse = ", "))
  }
  invisible(TRUE)
}

# Arguments after `distance` and `sigma` need a default, an entry in `args` or
# must be `...`.
.check_decay_kernel_formals <- function(rest, supplied) {
  empty <- vapply(names(rest), function(arg) identical(rest[[arg]], quote(expr = )),
                  logical(1))
  missing_default <- setdiff(names(rest)[empty], c("...", supplied))
  if (length(missing_default)) {
    stop("every argument of fun after distance and sigma needs a default, an entry in ",
         "args or must be ...; missing: ", paste(missing_default, collapse = ", "))
  }
  invisible(TRUE)
}

# The sigma values of the declaration check: three values spread (on the log
# scale) over the part of [lower, upper] within [0.01, 100], or one value when
# that part is empty. exp(log(x)) can differ from x in the last bits, so the
# values are clamped to the (inclusive) bounds.
.decay_probe_sigma <- function(lower, upper) {
  lo <- max(lower, 1e-2)
  hi <- min(upper, 1e2)
  if (lo >= hi) {
    return(if (is.finite(upper)) (lower + upper) / 2 else 2 * lower)
  }
  pmin(pmax(exp(seq(log(lo), log(hi), length.out = 3)), lower), upper)
}

# Run the rebuilt function and the given one on the test grid; stop unless both
# return the same finite, nonnegative, non-increasing weights.
.probe_decay_kernel <- function(kernel, fun, rebuilt) {
  distance <- c(if (kernel$zero_distance) 0, 0.1, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000)
  evaluate <- function(f, sigma) {
    tryCatch(do.call(f, c(list(distance, sigma), kernel$args)), error = function(e) {
      stop("the kernel failed on a test grid; it may use only its arguments, base R, ",
           "`pkg::fn` calls and `args`: ", conditionMessage(e), call. = FALSE)
    })
  }
  for (sigma in .decay_probe_sigma(kernel$lower, kernel$upper)) {
    at <- function(i) {
      sprintf("at distance %s with sigma = %s", format(distance[i]), format(sigma, digits = 4))
    }
    w <- evaluate(rebuilt, sigma)
    if (!is.numeric(w) || length(w) != length(distance)) {
      stop("the kernel must return a numeric vector with one value per distance")
    }
    if (!all(is.finite(w))) {
      i <- which(!is.finite(w))[1L]
      stop("the kernel must return finite values; it returned ", format(w[i]), " ", at(i))
    }
    if (any(w < 0)) {
      i <- which(w < 0)[1L]
      stop("the kernel must return nonnegative values; it returned ", format(w[i]), " ", at(i))
    }
    rises <- diff(w) > 1e-12 * pmax(1, w[-1L])
    if (any(rises)) {
      i <- which(rises)[1L] + 1L
      stop("the kernel must not increase with distance; it increases ", at(i))
    }
    if (!identical(w, evaluate(fun, sigma))) {
      stop("the kernel gives different values when rebuilt from its stored code; it may use ",
           "only its arguments, base R, `pkg::fn` calls and `args`")
    }
  }
  invisible(TRUE)
}

# A string or a declaration to the family string the allocation workflow keeps
# and, for a custom declaration, the declaration itself. Strings keep match.arg()
# semantics over the three built-in kernels; a built-in declaration is an alias
# of its string.
.resolve_decay_family <- function(family) {
  if (inherits(family, "spax_decay_kernel")) {
    .validate_decay_kernel(family)
    return(list(family = family$name, kernel = if (isTRUE(family$builtin)) NULL else family))
  }
  list(family = match.arg(family, .decay_builtin_families), kernel = NULL)
}

# A printable label for a decay method: a string as is, a declaration by its name
# and a plain function as "custom function".
.decay_kernel_label <- function(x) {
  if (inherits(x, "spax_decay_kernel")) return(x$name)
  if (is.function(x)) return("custom function")
  as.character(x)
}
