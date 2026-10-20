# Decay objects: a kernel declaration together with its sigma --------------------

#' Declare a decay for a model specification
#'
#' Experimental. A decay object names a distance decay kernel and says whether
#' its parameter `sigma` is estimated when the model is fitted or held at a
#' number. It is one slot of a model specification such as [spec_clm()] or
#' [spec_huff()], which holds no data and is used with `spax()`.
#'
#' `decay_gaussian()`, `decay_exponential()` and `decay_power()` use the
#' built-in kernels of [calc_decay()]; `decay_custom()` uses a kernel declared
#' with [decay_kernel()]. What `sigma` means depends on the kernel:
#' for the Gaussian kernel it is a scale in travel units, for the exponential
#' kernel a rate (the weight is `exp(-sigma * d)`) and for the power kernel an
#' exponent. For a declared kernel it is the kernel's `parameter`. The power
#' kernel is undefined at distance zero.
#'
#' `sigma = "fit"` (the default) lets the fit estimate it. A number holds it
#' at that value. A number must be positive and must lie within the bounds the
#' kernel declares for `sigma` (the built-in kernels allow any positive number).
#'
#' A model specification also accepts an exact built-in name (`"gaussian"`,
#' `"exponential"` or `"power"`) or a [decay_kernel()] where it expects a decay;
#' both mean `sigma = "fit"`. There is no partial matching.
#'
#' @param sigma `"fit"`, or a single finite number greater than 0 that holds
#'   `sigma` at that value.
#' @param kernel A kernel declared with [decay_kernel()].
#' @param x A decay object.
#' @param ... Unused.
#' @return A `spax_decay`: a list with the kernel declaration (`kernel`, a
#'   `spax_decay_kernel`) and `sigma` (`"fit"` or a number).
#' @examples
#' decay_gaussian()
#' decay_power(sigma = 1.5)
#'
#' # A declared kernel with bounds on sigma
#' plateau <- decay_kernel(
#'   function(distance, sigma, plateau = 5) {
#'     ifelse(distance <= plateau, 1, exp(-(distance - plateau)^2 / (2 * sigma^2)))
#'   },
#'   name = "plateau", parameter = "scale", lower = 1, upper = 20,
#'   args = list(plateau = 5)
#' )
#' decay_custom(plateau)
#' decay_custom(plateau, sigma = 8)
#' @family allocation interface
#' @export
#' @md
decay_gaussian <- function(sigma = "fit") {
  .decay_object(.builtin_decay_kernel("gaussian"), sigma)
}

#' @rdname decay_gaussian
#' @export
#' @md
decay_exponential <- function(sigma = "fit") {
  .decay_object(.builtin_decay_kernel("exponential"), sigma)
}

#' @rdname decay_gaussian
#' @export
#' @md
decay_power <- function(sigma = "fit") {
  .decay_object(.builtin_decay_kernel("power"), sigma)
}

#' @rdname decay_gaussian
#' @export
#' @md
decay_custom <- function(kernel, sigma = "fit") {
  if (!inherits(kernel, "spax_decay_kernel")) {
    stop("kernel must be a decay_kernel()", call. = FALSE)
  }
  .decay_object(kernel, sigma)
}

#' @rdname decay_gaussian
#' @export
#' @md
format.spax_decay <- function(x, ...) {
  paste0("<spax decay> ", .decay_text(x))
}

#' @rdname decay_gaussian
#' @export
#' @md
print.spax_decay <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

# A decay object: the kernel declaration and a checked sigma.
.decay_object <- function(kernel, sigma) {
  structure(list(kernel = kernel, sigma = .check_decay_sigma(sigma, kernel)),
            class = "spax_decay")
}

# sigma is exactly "fit" or a positive number inside the kernel's bounds; a number
# is returned as a plain double.
.check_decay_sigma <- function(sigma, kernel) {
  if (identical(sigma, "fit")) return(sigma)
  if (!is.numeric(sigma) || length(sigma) != 1L || !is.finite(sigma) || sigma <= 0) {
    stop("sigma must be a positive number or \"fit\"", call. = FALSE)
  }
  if (sigma < kernel$lower || sigma > kernel$upper) {
    stop("sigma must be from ", format(kernel$lower), " to ", format(kernel$upper),
         " for the ", kernel$name, " kernel", call. = FALSE)
  }
  as.double(sigma)
}

# Any accepted form of a decay to a spax_decay: a spax_decay as it is, a kernel
# declaration or an exact built-in name with sigma = "fit". `arg` names the argument
# in the error message.
.as_spax_decay <- function(x, arg = "decay") {
  if (inherits(x, "spax_decay")) return(x)
  if (inherits(x, "spax_decay_kernel")) return(decay_custom(x))
  if (is.character(x) && length(x) == 1L && !is.na(x) && x %in% .decay_builtin_families) {
    return(decay_custom(.builtin_decay_kernel(unname(x))))
  }
  stop(arg, " must be decay_gaussian(), decay_exponential(), decay_power(), ",
       "decay_custom(), a decay_kernel() or one of \"gaussian\", \"exponential\", ",
       "\"power\"", call. = FALSE)
}

# "fit", or a number as printed.
.decay_sigma_text <- function(sigma) {
  if (identical(sigma, "fit")) "fit" else format(sigma, digits = 7L, trim = TRUE)
}

# "gaussian (sigma = travel scale): fit"
.decay_text <- function(x) {
  paste0(x$kernel$name, " (sigma = ", x$kernel$parameter, "): ", .decay_sigma_text(x$sigma))
}
