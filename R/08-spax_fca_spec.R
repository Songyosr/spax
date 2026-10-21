# FCA specifications: a floating catchment area index without data ----------------

#' Specify a floating catchment area index without data
#'
#' Experimental. `spec_2sfca()`, `spec_e2sfca()`, `spec_m2sfca()`,
#' `spec_3sfca()` and `spec_huff2sfca()` declare a floating catchment area (FCA)
#' index with its decay and its options, as the data-free counterparts of
#' [spax_2sfca()], [spax_e2sfca()], [spax_m2sfca()], [spax_3sfca()] and
#' [spax_huff2sfca()]. A specification holds no data and no fitted value: an FCA
#' index is computed, not fitted, so every decay needs its `sigma` (or its
#' threshold) here.
#'
#' **Decays.** A decay is [decay_gaussian()], [decay_exponential()] or
#' [decay_power()] with a number for `sigma`, or with a `SpatRaster` of `sigma`
#' values (a variable catchment); [decay_step()] or [decay_inverse()]; or
#' [decay_custom()] with a kernel declared by [decay_kernel()] and a number for
#' `sigma`. `"fit"` is refused, and so is a bare kernel name, which means
#' `sigma = "fit"`. With demand groups, `decay` and `selection` can be a named
#' list by group, and `threshold` a named numeric vector by group.
#'
#' * `spec_2sfca()`: the original two-step method with a binary catchment of
#'   travel time `threshold`, with no demand normalization.
#' * `spec_e2sfca()` and `spec_m2sfca()`: the enhanced and the modified methods
#'   with distance decay `decay`. `normalize` is the normalization of demand:
#'   `"identity"`, `"standard"` or `"semi"` (see [calc_normalize()]).
#' * `spec_3sfca()`: the three-step method. `selection` is the decay the
#'   selection weights are built from; `NULL` (the default) means the same decay
#'   as `decay`.
#' * `spec_huff2sfca()`: the Huff-model selection. `attractiveness` is `"supply"`
#'   (the supply measure) or a numeric vector named by facility with one finite
#'   value of 0 or more per facility, and `alpha` is its exponent.
#'
#' @param threshold For `spec_2sfca()`: a single finite number greater than 0,
#'   the largest travel time in the catchment, or a named numeric vector of such
#'   numbers, one per demand group.
#' @param decay The decay of the weights: a decay with a value for `sigma`
#'   ([decay_gaussian()], [decay_exponential()], [decay_power()],
#'   [decay_custom()]), [decay_step()] or [decay_inverse()]; or a named list of
#'   these by demand group.
#' @param normalize The normalization of demand: `"identity"` (the default),
#'   `"standard"` or `"semi"`.
#' @param selection The decay of the selection weights, in the form of `decay`;
#'   `NULL` (the default) uses `decay`.
#' @param attractiveness `"supply"` (the default), or a numeric vector named by
#'   facility with finite values of 0 or more.
#' @param alpha A single finite number of 0 or more: the exponent of the
#'   attractiveness in the selection probability.
#' @param x An FCA specification.
#' @param ... Unused.
#' @return A `spax_fca_spec` (also a `spax_spec`): a list with `kind` (`"2sfca"`,
#'   `"e2sfca"`, `"m2sfca"`, `"3sfca"` or `"huff2sfca"`), `decay`, `normalize`,
#'   `selection`, `attractiveness`, `alpha` and `threshold`. A field that the
#'   method does not have is `NULL`.
#' @examples
#' spec_2sfca(threshold = 15)
#' spec_e2sfca(decay = decay_gaussian(sigma = 10))
#' spec_m2sfca(decay = decay_exponential(sigma = 0.2), normalize = "standard")
#'
#' # Selection with another decay than the weights
#' spec_3sfca(decay = decay_gaussian(10), selection = decay_inverse())
#'
#' # Huff selection with an attractiveness named by facility
#' spec_huff2sfca(decay = decay_gaussian(10),
#'                attractiveness = c(a = 2, b = 1), alpha = 2)
#'
#' # Demand groups: a decay for each group
#' spec_e2sfca(decay = list(young = decay_gaussian(8), old = decay_step(20)))
#' @seealso [decay_step()], [decay_gaussian()], [decay_kernel()]
#' @family access interface
#' @rdname spec_e2sfca
#' @export
#' @md
spec_2sfca <- function(threshold) {
  threshold <- .spec_fca_threshold(threshold)
  .new_fca_spec("2sfca", threshold = threshold)
}

#' @rdname spec_e2sfca
#' @export
#' @md
spec_e2sfca <- function(decay = decay_gaussian(sigma = 30),
                        normalize = c("identity", "standard", "semi")) {
  decay <- .spec_fca_decay(decay, "decay")
  normalize <- .spec_fca_normalize(normalize)
  .new_fca_spec("e2sfca", decay = decay, normalize = normalize)
}

#' @rdname spec_e2sfca
#' @export
#' @md
spec_m2sfca <- function(decay = decay_gaussian(sigma = 30),
                        normalize = c("identity", "standard", "semi")) {
  decay <- .spec_fca_decay(decay, "decay")
  normalize <- .spec_fca_normalize(normalize)
  .new_fca_spec("m2sfca", decay = decay, normalize = normalize)
}

#' @rdname spec_e2sfca
#' @export
#' @md
spec_3sfca <- function(decay = decay_gaussian(sigma = 30), selection = NULL) {
  decay <- .spec_fca_decay(decay, "decay")
  selection <- .spec_fca_selection(selection)
  .new_fca_spec("3sfca", decay = decay, selection = selection)
}

#' @rdname spec_e2sfca
#' @export
#' @md
spec_huff2sfca <- function(decay = decay_gaussian(sigma = 30), selection = NULL,
                           attractiveness = "supply", alpha = 1) {
  decay <- .spec_fca_decay(decay, "decay")
  selection <- .spec_fca_selection(selection)
  attractiveness <- .spec_fca_attractiveness(attractiveness)
  alpha <- .spec_fca_alpha(alpha)
  .new_fca_spec("huff2sfca", decay = decay, selection = selection,
                attractiveness = attractiveness, alpha = alpha)
}

#' @rdname spec_e2sfca
#' @export
#' @md
format.spax_fca_spec <- function(x, ...) {
  rows <- list(decay = .fca_decay_line(
    if (identical(x$kind, "2sfca")) .fca_threshold_decay(x$threshold) else x$decay
  ))
  if (!is.null(x$normalize)) rows$normalize <- x$normalize
  if (x$kind %in% c("3sfca", "huff2sfca")) {
    rows$selection <- if (is.null(x$selection)) "same as decay" else .fca_decay_line(x$selection)
  }
  if (identical(x$kind, "huff2sfca")) {
    what <- if (identical(x$attractiveness, "supply")) {
      "supply"
    } else {
      paste0(length(x$attractiveness), " values")
    }
    rows$attractiveness <- paste0(what, " ^ alpha ", .spec_number(x$alpha))
  }
  labels <- formatC(names(rows), width = -max(nchar(names(rows))))
  c(paste0("<spax spec> ", .fca_spec_labels[[x$kind]]),
    paste0("  ", labels, "  ", unlist(rows, use.names = FALSE)))
}

# The fields of every FCA specification; a field the method does not have is NULL.
.new_fca_spec <- function(kind, decay = NULL, normalize = NULL, selection = NULL,
                          attractiveness = NULL, alpha = NULL, threshold = NULL) {
  structure(list(kind = kind, decay = decay, normalize = normalize,
                 selection = selection, attractiveness = attractiveness,
                 alpha = alpha, threshold = threshold),
            class = c("spax_fca_spec", "spax_spec"))
}

# The label of each kind: the `type` of the result of the matching wrapper.
.fca_spec_labels <- c(`2sfca` = "2SFCA", e2sfca = "E2SFCA", m2sfca = "M2SFCA",
                      `3sfca` = "3SFCA", huff2sfca = "Huff-2SFCA")

# Validation, in the order of the arguments -------------------------------------

# A positive number (shared), or a named vector of positive numbers (by group).
.spec_fca_threshold <- function(threshold) {
  if (is.numeric(threshold)) {
    if (is.null(names(threshold)) && .spec_is_number(threshold, positive = TRUE)) {
      return(as.double(threshold))
    }
    if (.has_group_names(names(threshold), length(threshold)) &&
        all(is.finite(threshold)) && all(threshold > 0)) {
      return(stats::setNames(as.double(threshold), names(threshold)))
    }
  }
  stop("threshold must be a positive number or a named vector of positive numbers ",
       "by group", call. = FALSE)
}

# The normalization of demand: one exact name; the default vector gives "identity".
.spec_fca_normalize <- function(normalize) {
  choices <- c("identity", "standard", "semi")
  if (identical(normalize, choices)) return("identity")
  if (!is.character(normalize) || length(normalize) != 1L || !normalize %in% choices) {
    stop("normalize must be \"identity\", \"standard\" or \"semi\"", call. = FALSE)
  }
  normalize
}

# One decay in any accepted form, or a named list of them by demand group. `arg`
# names the argument in the messages ("decay" or "selection").
.spec_fca_decay <- function(x, arg) {
  # The decay classes are lists, so classes come before list-ness.
  if (is.list(x) && !is.object(x)) {
    # A decay_params list of the FCA one-shots is not a list by group.
    if ("method" %in% names(x)) {
      stop(arg, " is given as a list with `method`: write it as decay_gaussian(sigma = ...) ",
           "or another decay_*() function", call. = FALSE)
    }
    if (!.has_group_names(names(x), length(x))) {
      stop(arg, " by group must be a named list with one decay per group", call. = FALSE)
    }
    return(lapply(x, .spec_fca_decay_one, arg = arg))
  }
  .spec_fca_decay_one(x, arg)
}

.spec_fca_decay_one <- function(x, arg) {
  needs_sigma <- "an FCA specification needs a sigma value"
  if (inherits(x, "spax_fca_decay")) return(x)
  # A decay_*() object keeps its number or raster. "fit", a bare kernel and an exact
  # built-in name (each means sigma = "fit") are refused: an FCA index is computed.
  if (inherits(x, "spax_decay")) {
    if (identical(x$sigma, "fit")) stop(needs_sigma, call. = FALSE)
    return(x)
  }
  if (inherits(x, "spax_decay_kernel") ||
      (is.character(x) && length(x) == 1L && !is.na(x) && x %in% .decay_builtin_families)) {
    stop(needs_sigma, call. = FALSE)
  }
  stop(arg, " must be decay_gaussian(), decay_exponential(), decay_power(), ",
       "decay_step(), decay_inverse(), decay_custom() or a named list of these by group",
       call. = FALSE)
}

# NULL means the decay of the specification.
.spec_fca_selection <- function(selection) {
  if (is.null(selection)) return(NULL)
  .spec_fca_decay(selection, "selection")
}

.spec_fca_attractiveness <- function(attractiveness) {
  if (identical(attractiveness, "supply")) return(attractiveness)
  if (is.numeric(attractiveness) &&
      .has_group_names(names(attractiveness), length(attractiveness)) &&
      all(is.finite(attractiveness)) && all(attractiveness >= 0)) {
    return(stats::setNames(as.double(attractiveness), names(attractiveness)))
  }
  stop("attractiveness must be \"supply\" or a numeric vector named by facility with ",
       "finite values of 0 or more", call. = FALSE)
}

.spec_fca_alpha <- function(alpha) {
  if (!.spec_is_number(alpha, positive = FALSE)) {
    stop("alpha must be a nonnegative number", call. = FALSE)
  }
  as.double(alpha)
}

# Printing ------------------------------------------------------------------------

# The decay objects of a threshold: one decay_step(), or one per group.
.fca_threshold_decay <- function(threshold) {
  if (is.null(names(threshold))) decay_step(threshold) else lapply(threshold, decay_step)
}

# One decay as printed after its label: the text of the decay, or by group
# "young gaussian (8), old step (20)".
.fca_decay_line <- function(decay) {
  if (inherits(decay, c("spax_decay", "spax_fca_decay"))) {
    return(if (inherits(decay, "spax_decay")) .decay_text(decay) else .fca_decay_text(decay))
  }
  parts <- vapply(names(decay), function(g) {
    d <- decay[[g]]
    if (inherits(d, "spax_fca_decay")) {
      paste0(g, " ", if (identical(d$method, "binary")) {
        paste0("step (", .decay_sigma_text(d$sigma), ")")
      } else {
        "inverse"
      })
    } else {
      paste0(g, " ", d$kernel$name, " (", .decay_sigma_text(d$sigma), ")")
    }
  }, character(1))
  paste0("by group: ", paste(parts, collapse = ", "))
}

# Mapping to the arguments of the FCA runner ----------------------------------------

# One decay as the `decay_params` list of the FCA functions: a built-in decay is
# its method name and sigma (a double or a raster), a declared kernel the kernel
# and sigma, a step or inverse decay its list. A named list of decays (demand
# groups) maps element by element.
.fca_map_decay <- function(decay) {
  if (inherits(decay, "spax_fca_decay")) return(unclass(decay))
  if (inherits(decay, "spax_decay")) {
    kernel <- decay$kernel
    return(list(method = if (isTRUE(kernel$builtin)) kernel$name else kernel,
                sigma = decay$sigma))
  }
  lapply(decay, .fca_map_decay)
}

# The arguments of .fca_block_run() for an FCA specification, and the pieces of
# the result of the matching wrapper: `type` (the runner's type), `label` (the
# `type` of the result), `decay_params`, `selection_params` (for 3SFCA and Huff, the
# mapped decay itself when the specification has no selection), `demand_normalize`,
# `attractiveness` (`NULL` for "supply", as the wrappers default it; the Huff wrapper
# resolves it with .fca_attractiveness()), `alpha`, and `parameter_names`, the
# elements of the `parameters` list of the result in the wrapper's order. Groups are
# in the order of the specification; they are bound to the data later.
.fca_spec_args <- function(spec) {
  kind <- spec$kind
  type <- switch(kind,
    `2sfca` = , e2sfca = "e2sfca",
    m2sfca = "m2sfca",
    `3sfca` = "3sfca",
    huff2sfca = "huff",
    stop("unknown FCA kind '", kind, "'", call. = FALSE)
  )
  decay_params <- .fca_map_decay(
    if (identical(kind, "2sfca")) .fca_threshold_decay(spec$threshold) else spec$decay
  )
  selection <- kind %in% c("3sfca", "huff2sfca")
  list(
    kind = kind,
    type = type,
    label = unname(.fca_spec_labels[[kind]]),
    decay_params = decay_params,
    selection_params = if (selection) {
      if (is.null(spec$selection)) decay_params else .fca_map_decay(spec$selection)
    },
    demand_normalize = if (kind %in% c("e2sfca", "m2sfca")) spec$normalize else "identity",
    attractiveness = if (identical(kind, "huff2sfca") &&
                         !identical(spec$attractiveness, "supply")) spec$attractiveness,
    alpha = if (identical(kind, "huff2sfca")) spec$alpha else 1,
    parameter_names = switch(type,
      e2sfca = , m2sfca = c("decay_params", "demand_normalize"),
      `3sfca` = c("decay_params", "selection_params"),
      huff = c("decay_params", "selection_params", "attractiveness", "alpha")
    )
  )
}
