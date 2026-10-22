# 0. Errors ----------------------------------------------------------------------
#' Signal an error from the call the user made
#'
#' Like [stop()], but the error names the call that the user made to spax (see
#' `.user_call()`) instead of the internal function that found the problem, and
#' names no call when there is none. A condition object is signalled as it is.
#' Working out the call never replaces the message: if it fails, no call is
#' named.
#' @noRd
.stop <- function(..., call. = TRUE, domain = NULL) {
  if (...length() == 1L && inherits(..1, "condition")) stop(..1)
  call <- if (isTRUE(call.)) tryCatch(.user_call(), error = function(e) NULL)
  stop(simpleError(.makeMessage(..., domain = domain), call))
}

#' The call to spax that the user made
#'
#' The innermost frame in which control passed from code outside spax into a
#' spax function: the user's own call (`prep_data(...)`), a call that base R
#' makes for the user (`FUN(X[[i]], ...)` in `lapply()`), or, for an S3 method,
#' the call to its generic (`predict(fit, ...)` rather than
#' `predict.spax_allocation(fit, ...)`). Calls that spax makes to its own
#' functions, directly or through functions of other packages such as
#' `lapply()` or `optim()`, are never named. A function given as an object, as
#' in `do.call(prep_data, args)`, is named by its spax name. `NULL` when there
#' is no such frame.
#' @noRd
.user_call <- function() {
  ns <- topenv(environment())
  here <- sys.nframe()
  parents <- sys.parents()
  calls <- sys.calls()
  # Where the function of frame k comes from: spax, another package (base R
  # included) or the user's own code
  origin <- function(k) {
    env <- environment(sys.function(k))
    if (is.null(env)) return("package")
    top <- topenv(env)
    if (identical(top, ns)) "spax" else if (isNamespace(top)) "package" else "user"
  }
  # Frame k was entered from outside spax when its callers, followed through
  # frames of other packages, lead to the user's code or to the top level
  entered_from_outside <- function(k) {
    current <- k
    p <- parents[current]
    while (p > 0L && p < current && origin(p) == "package") {
      current <- p
      p <- parents[current]
    }
    !(p > 0L && p < current && origin(p) == "spax")
  }
  for (k in rev(seq_len(here - 1L))) {
    if (origin(k) != "spax" || identical(sys.function(k), .stop) ||
        !entered_from_outside(k)) next
    # A method dispatched by NextMethod(): name the method that dispatched it
    if (k > 1L && identical(sys.function(k - 1L), NextMethod)) next
    call <- calls[[k]]
    head <- call[[1L]]
    # An S3 method: the call to its generic. The frames that dispatched the
    # method come just before it and share its caller; each is called by the
    # generic's name or runs UseMethod() or standardGeneric() for it. Name the
    # outermost of them.
    if (k > 1L && is.name(head)) {
      name <- as.character(head)
      dots <- gregexpr(".", name, fixed = TRUE)[[1L]]
      dots <- dots[dots > 1L]
      generics <- if (length(dots)) substring(name, 1L, dots - 1L) else character()
      # The generic that frame j dispatches, or NA
      dispatches <- function(j) {
        h <- calls[[j]][[1L]]
        if (is.call(h) && length(h) == 3L && is.name(h[[1L]]) &&
            as.character(h[[1L]]) %in% c("::", ":::")) h <- h[[3L]]
        if (is.name(h) && as.character(h) %in% generics) return(as.character(h))
        b <- body(sys.function(j))
        if (is.call(b) && is.name(b[[1L]]) &&
            as.character(b[[1L]]) %in% c("UseMethod", "standardGeneric") &&
            length(b) > 1L && is.character(b[[2L]]) && b[[2L]] %in% generics) {
          return(b[[2L]])
        }
        NA_character_
      }
      first <- 0L
      generic <- NA_character_
      j <- k - 1L
      while (length(generics) && j >= 1L && parents[j] == parents[k] &&
             !is.na(g <- dispatches(j))) {
        first <- j
        generic <- g
        j <- j - 1L
      }
      if (first > 0L) {
        call <- calls[[first]]
        # The generic given as an object, as in do.call(predict, args): its name
        if (is.function(call[[1L]])) call[[1L]] <- as.name(generic)
        return(call)
      }
    }
    # A function given as an object: its name in spax
    if (is.function(head)) {
      found <- Find(function(nm) identical(get(nm, envir = ns), head),
                    c(getNamespaceExports(ns), ls(ns, all.names = TRUE)))
      if (is.null(found)) return(NULL)
      call[[1L]] <- as.name(found)
    }
    return(call)
  }
  NULL
}

# 1. Basic Type Check ----------------------------------------------------------
#' Check if input is of a specific structure* (or range of classes)
#' @noRd
.chck_class <- function(x, classes, name = "Input") {
  if (!inherits(x, classes)) {
    class_list <- paste(classes, collapse = ", ")
    .stop(sprintf("%s must be one of the following classes: %s", name, class_list))
  }
  invisible(TRUE)
}

#' Check if input is numeric
#' @noRd
.chck_is_numeric <- function(x, name = "Input") {
  if (!is.numeric(x)) {
    .stop(sprintf("%s must be numeric", name))
  }
  invisible(TRUE)
}

#' Does a numeric object hold any NA, NaN or infinite value?
#'
#' Same answer as `any(!is.finite(x))`. For a double it scans with `anyNA()`,
#' `min()` and `max()`, which allocate nothing, where `is.finite()` allocates a
#' logical vector as long as `x`. Other types take the original test.
#' @noRd
.any_nonfinite <- function(x) {
  if (is.double(x)) {
    return(length(x) > 0L && (anyNA(x) || min(x) == -Inf || max(x) == Inf))
  }
  any(!is.finite(x))
}

#' Check if input is a numeric vector
#' @noRd
.chck_numeric_vector <- function(x, name = "Input", finite = TRUE) {
  .chck_is_numeric(x, name = name)
  if (is.null(x) || !is.null(dim(x))) {
    .stop(sprintf("%s must be a numeric vector", name))
  }
  if (finite && .any_nonfinite(x)) {
    .stop(sprintf("%s must contain only finite values", name))
  }
  invisible(TRUE)
}

#' Check if input is a numeric matrix
#' @noRd
.chck_numeric_matrix <- function(x, name = "Input", finite = TRUE) {
  .chck_is_numeric(x, name = name)
  if (is.null(dim(x)) || length(dim(x)) != 2L) {
    .stop(sprintf("%s must be a numeric matrix", name))
  }
  if (finite && .any_nonfinite(x)) {
    .stop(sprintf("%s must contain only finite values", name))
  }
  invisible(TRUE)
}

#' Check if input is a positive scalar
#' @noRd
.chck_positive_scalar <- function(x, name = "Input") {
  .chck_is_numeric(x, name = name)
  .chck_length(length(x), 1, name = name)
  if (!is.finite(x) || x <= 0) {
    .stop(sprintf("%s must be a positive scalar", name))
  }
  invisible(TRUE)
}

#' Check if input is a nonnegative scalar
#' @noRd
.chck_nonnegative_scalar <- function(x, name = "Input") {
  .chck_is_numeric(x, name = name)
  .chck_length(length(x), 1, name = name)
  if (!is.finite(x) || x < 0) {
    .stop(sprintf("%s must be a nonnegative scalar", name))
  }
  invisible(TRUE)
}

#' Check if input is a positive integer scalar
#' @noRd
.chck_positive_integer <- function(x, name = "Input") {
  .chck_is_numeric(x, name = name)
  .chck_length(length(x), 1, name = name)
  if (!is.finite(x) || x < 1 || x != as.integer(x)) {
    .stop(sprintf("%s must be a positive integer", name))
  }
  invisible(TRUE)
}

# 2. Basic Value Checks --------------------------------------------------------
#' Check if value is in valid range
#' @noRd
.chck_in_range <- function(x, min = 0, max = 1, inclusive = TRUE, name = "Value") {
  if (inclusive) {
    if (any(x < min | x > max, na.rm = TRUE)) {
      .stop(sprintf("%s must be >= %s and <= %s", name, min, max))
    }
  } else {
    if (any(x <= min | x >= max, na.rm = TRUE)) {
      .stop(sprintf("%s must be > %s and < %s", name, min, max))
    }
  }
  invisible(TRUE)
}

#' Check if value is positive
#' @noRd
.chck_positive <- function(x, allow_zero = FALSE, name = "Input") {
  min_val <- if (allow_zero) 0 else .Machine$double.eps
  inclusive <- if (allow_zero) TRUE else FALSE
  .chck_in_range(x, min = min_val, max = Inf, inclusive = inclusive, name = name)
}

# 3. Length and Dimension Checks -----------------------------------------------
#' Check if length matches expected
#' @noRd
.chck_length <- function(len_x, expected, name = "Input") {
  if (len_x != expected) {
    .stop(sprintf("%s must have length %d", name, expected))
  }
  invisible(TRUE)
}

#' Check if length of two inputs match
#' @noRd
.chck_lengths_match <- function(len_x, len_y, name_x = "First input", name_y = "Second input") {
  if (len_x != len_y) {
    .stop(sprintf("Length of %s (%d) must match length of %s (%d)",
                 name_x, len_x, name_y, len_y))
  }
  invisible(TRUE)
}

# 4. Raster Checks ------------------------------------------------------------
#' Check if the input is a SpatRaster - just a fast-track to .chck_class
#' @noRd
.chck_is_raster <- function(x, name = "Input") {
  .chck_class(x, "SpatRaster", name = name)
}

#' Check if rasters have matching geometry
#' @noRd
.chck_raster_alignment <- function(x, y, name_x = "First raster", name_y = "Second raster") {
  if (!compareGeom(x, y, stopOnError = FALSE)) {
    .stop(sprintf("%s and %s must have the same geometry (resolution, extent, and CRS)",
                 name_x, name_y))
  }
  invisible(TRUE)
}

# 5. Dataframe Checks ---------------------------------------------------------
#' Check if columns exist in dataframe
#' @noRd
.chck_cols_exist <- function(df, cols, df_name = "data.frame") {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    .stop(sprintf("Column(s) not found in %s: %s",
                 df_name,
                 paste(missing, collapse = ", ")))
  }
  invisible(TRUE)
}

#' Check that numeric or raster weights are non-negative (NA is allowed)
#' @noRd
.chck_nonnegative_weights <- function(x, name = "weights") {
  lowest <- if (inherits(x, "SpatRaster")) {
    suppressWarnings(min(terra::global(x, "min", na.rm = TRUE)[[1]], na.rm = TRUE))
  } else if (is.numeric(x)) {
    suppressWarnings(min(x, na.rm = TRUE))
  } else {
    NA_real_
  }
  if (isTRUE(lowest < 0)) {
    .stop(sprintf("%s must be non-negative", name))
  }
  invisible(TRUE)
}

# 6. Function Checks ----------------------------------------------------------
# Same function, ignoring source references, which a saved copy read back can differ in.
.same_function <- function(a, b) {
  is.function(a) && is.function(b) &&
    identical(utils::removeSource(a), utils::removeSource(b))
}
