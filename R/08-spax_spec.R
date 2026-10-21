# Model specifications: an allocation model without data ---------------------------

#' Specify an allocation model without data
#'
#' Experimental. `spec_clm()` and `spec_huff()` declare an allocation model
#' with its decay kernel and its parameters, each either estimated (`"fit"`)
#' or held at a number. A specification holds no data: it is checked as far as
#' that is possible without data, and it is bound to prepared inputs when it is
#' used with `spax()`.
#'
#' **Conditional logit** (`spec_clm()`). Demand at an origin goes to each
#' facility in proportion to its attractiveness times the decay weight of the
#' travel time, and to an outside option with weight `v0`. The attractiveness
#' of a facility is `(kappa * supply)^beta`. `outside = 0` removes the outside
#' option, which is the Huff model.
#'
#' **Huff** (`spec_huff()`). The same model without an outside option. Its
#' default decay is a power kernel and its default `beta` is fitted.
#'
#' **Parameters.** A parameter is a number (held) or `"fit"` (estimated), in
#' its own slot:
#' * the decay's `sigma`, in the decay object ([decay_gaussian()] and its
#'   relatives);
#' * the outside weight `v0`, in `outside` (conditional logit only);
#' * the attractiveness exponent `beta`.
#'
#' `kappa` is a scale that is always held.
#'
#' **Demand groups.** For data with demand groups, a named list of decays
#' gives each group its own kernel; a number in an element holds that group's
#' `sigma`. `outside` can be a named numeric vector (held by group) or a named
#' list whose elements are `"fit"` or a number. `by_group` lets one fitted
#' decay (`"sigma"`) or one fitted outside weight (`"v0"`) differ by group.
#' Group-specific parameters can be fitted only to counts recorded by group;
#' a held `sigma` cannot differ by group unless the decay is given by group.
#'
#' @param decay A decay: [decay_gaussian()], [decay_exponential()],
#'   [decay_power()], [decay_custom()], a [decay_kernel()] or one of
#'   `"gaussian"`, `"exponential"` and `"power"` (the last two forms mean
#'   `sigma = "fit"`). Or a named list of these with one element per demand
#'   group.
#' @param outside The outside weight `v0`: `"fit"` (the default), `0` (no
#'   outside option), a single positive number (held), a named numeric vector
#'   by group (held), or a named list by group whose elements are `"fit"` or
#'   a number.
#' @param beta The attractiveness exponent: `"fit"`, or a single finite number
#'   greater than 0 that holds it.
#' @param kappa A single finite number greater than 0, the scale of the
#'   attractiveness `(kappa * supply)^beta`. It is held.
#' @param by_group For data with demand groups, the parameters that differ by
#'   group: `"sigma"` and, for `spec_clm()`, `"v0"`. A single decay or outside
#'   weight listed here must be fitted (`"fit"`), because a held value is one
#'   number; hold values by group through a list or vector instead.
#' @param x A model specification.
#' @param ... Unused.
#' @return A `spax_spec`: a list with `kind` (`"clm"` or `"huff"`), `decay`,
#'   `outside` (`0` for `spec_huff()`), `beta`, `kappa` and `by_group`.
#' @examples
#' # Huff: power decay and fitted exponent, no outside option
#' spec_huff()
#'
#' # Conditional logit with a fitted outside weight, and with one held at 0.05
#' spec_clm(decay = decay_gaussian(), outside = "fit")
#' spec_clm(decay = decay_exponential(), outside = 0.05)
#'
#' # Evaluate at given values: everything is a number
#' spec_huff(decay = decay_power(sigma = 1.5), beta = 1)
#'
#' # Demand groups: a kernel for each group and outside weights by group
#' spec_clm(
#'   decay = list(young = decay_gaussian(5), old = decay_power(2)),
#'   outside = list(young = "fit", old = 0)
#' )
#' @family allocation interface
#' @export
#' @md
spec_clm <- function(decay = decay_gaussian(), outside = "fit", beta = 1, kappa = 1,
                     by_group = character()) {
  decay <- .spec_decay(decay)
  outside <- .spec_outside(outside)
  beta <- .spec_beta(beta)
  kappa <- .spec_kappa(kappa)
  by_group <- .spec_by_group(by_group, decay, outside, kind = "clm")
  structure(list(kind = "clm", decay = decay, outside = outside, beta = beta,
                 kappa = kappa, by_group = by_group), class = "spax_spec")
}

#' @rdname spec_clm
#' @export
#' @md
spec_huff <- function(decay = decay_power(), beta = "fit", kappa = 1,
                      by_group = character()) {
  decay <- .spec_decay(decay)
  beta <- .spec_beta(beta)
  kappa <- .spec_kappa(kappa)
  by_group <- .spec_by_group(by_group, decay, outside = 0, kind = "huff")
  structure(list(kind = "huff", decay = decay, outside = 0, beta = beta,
                 kappa = kappa, by_group = by_group), class = "spax_spec")
}

#' @rdname spec_clm
#' @export
#' @md
format.spax_spec <- function(x, ...) {
  line <- function(label, text) sprintf("  %-10s  %s", label, text)
  c(paste0("<spax spec> ", .spec_model_line(x)),
    line("decay", .spec_decay_line(x)),
    line("attraction", paste0("(kappa * supply)^beta: kappa = ", .spec_number(x$kappa),
                              ", beta = ", .decay_sigma_text(x$beta))),
    line("outside", .spec_outside_line(x)))
}

#' @rdname spec_clm
#' @export
#' @md
print.spax_spec <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

# Validation, in the order of the arguments of spec_clm() ----------------------

# One decay in any accepted form, or a named list of them by demand group.
.spec_decay <- function(decay) {
  # Both decay classes are named lists, so classes come before list-ness.
  if (inherits(decay, c("spax_decay", "spax_decay_kernel")) || is.character(decay)) {
    return(.as_spax_decay(decay))
  }
  if (is.list(decay) && !is.object(decay)) {
    if (!.has_group_names(names(decay), length(decay))) {
      stop("decay by group must be a named list with one decay per group", call. = FALSE)
    }
    return(lapply(decay, .as_spax_decay))
  }
  .as_spax_decay(decay)
}

# Unique, nonempty names for at least one element.
.has_group_names <- function(nms, n) {
  n >= 1L && !is.null(nms) && !anyNA(nms) && all(nzchar(nms)) && !anyDuplicated(nms)
}

# A single finite number, greater than 0 (`positive`) or from 0.
.spec_is_number <- function(x, positive) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) return(FALSE)
  if (positive) x > 0 else x >= 0
}

# "fit", a number, a named numeric vector (held by group), or a named list by group
# whose elements are "fit" or numbers. Numbers are returned as doubles.
.spec_outside <- function(outside) {
  if (identical(outside, "fit")) return(outside)
  if (is.numeric(outside)) {
    if (is.null(names(outside)) && .spec_is_number(outside, positive = FALSE)) {
      return(as.double(outside))
    }
    if (.has_group_names(names(outside), length(outside)) && all(is.finite(outside)) &&
        all(outside >= 0)) {
      return(stats::setNames(as.double(outside), names(outside)))
    }
  } else if (is.list(outside) && !is.object(outside)) {
    held <- function(x) .spec_is_number(x, positive = FALSE)
    if (.has_group_names(names(outside), length(outside)) &&
        all(vapply(outside, function(x) identical(x, "fit") || held(x), logical(1)))) {
      return(lapply(outside, function(x) if (identical(x, "fit")) x else as.double(x)))
    }
  } else if (is.character(outside) && length(outside) > 1L && "fit" %in% outside) {
    stop("to mix fitted and held v0 by group, give a named list: ",
         "outside = list(young = \"fit\", old = 0)", call. = FALSE)
  }
  stop("outside must be \"fit\", a number, or a named list or vector by group", call. = FALSE)
}

.spec_beta <- function(beta) {
  if (identical(beta, "fit")) return(beta)
  if (!.spec_is_number(beta, positive = TRUE)) {
    stop("beta must be a positive number or \"fit\"", call. = FALSE)
  }
  as.double(beta)
}

.spec_kappa <- function(kappa) {
  if (!.spec_is_number(kappa, positive = TRUE)) {
    stop("kappa must be a positive number", call. = FALSE)
  }
  as.double(kappa)
}

.spec_by_group <- function(by_group, decay, outside, kind) {
  allowed <- if (identical(kind, "clm")) c("sigma", "v0") else "sigma"
  if (!is.character(by_group) || anyNA(by_group) || anyDuplicated(by_group) ||
      !all(by_group %in% allowed)) {
    stop(if (identical(kind, "clm")) {
      "by_group can contain \"sigma\" and \"v0\""
    } else {
      "by_group can contain \"sigma\"; the Huff model has no outside option"
    }, call. = FALSE)
  }
  held_list <- is.list(outside) && !any(vapply(outside, identical, logical(1), "fit"))
  if ("v0" %in% by_group && (is.numeric(outside) || held_list)) {
    stop("by_group = \"v0\" needs outside = \"fit\"", call. = FALSE)
  }
  if ("sigma" %in% by_group && inherits(decay, "spax_decay") &&
      !identical(decay$sigma, "fit")) {
    stop("a held sigma cannot differ by group; give the decay by group, for example ",
         "list(young = decay_gaussian(5), old = decay_gaussian(8))", call. = FALSE)
  }
  as.character(by_group)
}

# Printing ------------------------------------------------------------------------

.spec_number <- function(x) {
  vapply(x, format, character(1), digits = 7L, trim = TRUE, USE.NAMES = FALSE)
}

# The Huff model has no outside option: v0 is 0, or 0 in every group.
.spec_has_outside <- function(x) {
  outside <- x$outside
  if (identical(outside, "fit")) return(TRUE)
  if (is.list(outside)) {
    return(!all(vapply(outside, function(v) is.numeric(v) && v == 0, logical(1))))
  }
  any(outside != 0)
}

.spec_model_line <- function(x) {
  if (.spec_has_outside(x)) {
    "conditional logit with an outside option"
  } else {
    "Huff model (no outside option)"
  }
}

.spec_decay_line <- function(x) {
  if (!inherits(x$decay, "spax_decay")) {
    parts <- vapply(names(x$decay), function(g) {
      d <- x$decay[[g]]
      paste0(g, " ", d$kernel$name, " (", .decay_sigma_text(d$sigma), ")")
    }, character(1))
    return(paste0("by group: ", paste(parts, collapse = ", ")))
  }
  paste0(.decay_text(x$decay), if ("sigma" %in% x$by_group) " by group")
}

.spec_outside_line <- function(x) {
  outside <- x$outside
  # No outside option at all (Huff, or every weight held at 0) prints as none.
  held <- if (is.list(outside)) unlist(outside) else outside
  if (is.numeric(held) && length(held) && all(held == 0)) return("none")
  if (is.list(outside)) {
    shown <- vapply(outside, function(v) {
      if (identical(v, "fit")) "fit" else .spec_number(v)
    }, character(1))
    return(paste0("v0 by group: ", paste(names(outside), shown, collapse = ", ")))
  }
  if (is.numeric(outside) && !is.null(names(outside))) {
    return(paste0("v0 by group: ", paste(names(outside), .spec_number(outside),
                                         collapse = ", ")))
  }
  paste0("v0: ", .decay_sigma_text(outside), if ("v0" %in% x$by_group) " by group")
}
