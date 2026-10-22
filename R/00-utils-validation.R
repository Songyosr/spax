# 0. Errors ----------------------------------------------------------------------
#' Signal an error from the call the user made
#'
#' Like [stop()], but the error names the call that the user made to spax (see
#' `.user_call()`) instead of the internal function that found the problem, and
#' names no call when there is none. A condition object is signalled as it is.
#' @keywords internal
.stop <- function(..., call. = TRUE, domain = NULL) {
  if (...length() == 1L && inherits(..1, "condition")) stop(..1)
  stop(simpleError(.makeMessage(..., domain = domain),
                   if (isTRUE(call.)) .user_call()))
}

#' The call to spax that the user made
#'
#' The outermost call on the stack to an exported spax function or, for an S3
#' method of spax, the call to its generic (`predict(fit, ...)` rather than
#' `predict.spax_allocation(fit, ...)`). The outermost call is the one the user
#' typed; functions it calls in turn, exported or not, are not named. `NULL`
#' when there is none, for example when an internal function is called directly.
#' @keywords internal
.user_call <- function() {
  ns <- topenv(environment(.user_call))
  # Dot-named functions are internal even when a development load exports them
  exported <- grep("^[.]", getNamespaceExports(ns), value = TRUE, invert = TRUE)
  methods <- getNamespaceInfo(ns, "S3methods")
  method_names <- ifelse(is.na(methods[, 3L]), paste(methods[, 1L], methods[, 2L], sep = "."),
                         methods[, 3L])
  calls <- sys.calls()
  # The name of the function of each call: `f(...)` or `pkg::f(...)`
  called <- vapply(calls, function(cl) {
    fn <- cl[[1L]]
    if (is.name(fn)) return(as.character(fn))
    if (is.call(fn) && length(fn) == 3L && is.name(fn[[1L]]) &&
        as.character(fn[[1L]]) %in% c("::", ":::") && is.name(fn[[3L]])) {
      return(as.character(fn[[3L]]))
    }
    NA_character_
  }, character(1))
  for (i in seq_along(calls)) {
    if (is.na(called[i])) next
    if (called[i] %in% exported) return(calls[[i]])
    method <- match(called[i], method_names)
    if (!is.na(method)) {
      generic <- methods[method, 1L]
      if (i > 1L && identical(called[i - 1L], generic)) return(calls[[i - 1L]])
      return(calls[[i]])
    }
  }
  NULL
}

# 1. Basic Type Check ----------------------------------------------------------
#' Check if input is of a specific structure* (or range of classes)
#' @keywords internal
.chck_class <- function(x, classes, name = "Input") {
  if (!inherits(x, classes)) {
    class_list <- paste(classes, collapse = ", ")
    .stop(sprintf("%s must be one of the following classes: %s", name, class_list))
  }
  invisible(TRUE)
}

#' Check if input is numeric
#' @keywords internal
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
#' @keywords internal
.any_nonfinite <- function(x) {
  if (is.double(x)) {
    return(length(x) > 0L && (anyNA(x) || min(x) == -Inf || max(x) == Inf))
  }
  any(!is.finite(x))
}

#' Check if input is a numeric vector
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
.chck_positive_scalar <- function(x, name = "Input") {
  .chck_is_numeric(x, name = name)
  .chck_length(length(x), 1, name = name)
  if (!is.finite(x) || x <= 0) {
    .stop(sprintf("%s must be a positive scalar", name))
  }
  invisible(TRUE)
}

#' Check if input is a nonnegative scalar
#' @keywords internal
.chck_nonnegative_scalar <- function(x, name = "Input") {
  .chck_is_numeric(x, name = name)
  .chck_length(length(x), 1, name = name)
  if (!is.finite(x) || x < 0) {
    .stop(sprintf("%s must be a nonnegative scalar", name))
  }
  invisible(TRUE)
}

#' Check if input is a positive integer scalar
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
.chck_positive <- function(x, allow_zero = FALSE, name = "Input") {
  min_val <- if (allow_zero) 0 else .Machine$double.eps
  inclusive <- if (allow_zero) TRUE else FALSE
  .chck_in_range(x, min = min_val, max = Inf, inclusive = inclusive, name = name)
}

# 3. Length and Dimension Checks -----------------------------------------------
#' Check if length matches expected
#' @keywords internal
.chck_length <- function(len_x, expected, name = "Input") {
  if (len_x != expected) {
    .stop(sprintf("%s must have length %d", name, expected))
  }
  invisible(TRUE)
}

#' Check if length of two inputs match
#' @keywords internal
.chck_lengths_match <- function(len_x, len_y, name_x = "First input", name_y = "Second input") {
  if (len_x != len_y) {
    .stop(sprintf("Length of %s (%d) must match length of %s (%d)",
                 name_x, len_x, name_y, len_y))
  }
  invisible(TRUE)
}

# 4. Raster Checks ------------------------------------------------------------
#' Check if the input is a SpatRaster - just a fast-track to .chck_class
#' @keywords internal
.chck_is_raster <- function(x, name = "Input") {
  .chck_class(x, "SpatRaster", name = name)
}

#' Check if rasters have matching geometry
#' @keywords internal
.chck_raster_alignment <- function(x, y, name_x = "First raster", name_y = "Second raster") {
  if (!compareGeom(x, y, stopOnError = FALSE)) {
    .stop(sprintf("%s and %s must have the same geometry (resolution, extent, and CRS)",
                 name_x, name_y))
  }
  invisible(TRUE)
}

# 5. Dataframe Checks ---------------------------------------------------------
#' Check if columns exist in dataframe
#' @keywords internal
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
#' @keywords internal
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
