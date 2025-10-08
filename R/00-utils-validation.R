# 1. Basic Type Check ----------------------------------------------------------
#' Check if input is of a specific structure* (or range of classes)
#' @keywords internal
.chck_class <- function(x, classes, name = "Input") {
  if (!inherits(x, classes)) {
    class_list <- paste(classes, collapse = ", ")
    stop(sprintf("%s must be one of the following classes: %s", name, class_list))
  }
  invisible(TRUE)
}

#' Check if input is numeric
#' @keywords internal
.chck_is_numeric <- function(x, name = "Input") {
  if (!is.numeric(x)) {
    stop(sprintf("%s must be numeric", name))
  }
  invisible(TRUE)
}

#' Check if value is integer
#' @keywords internal
.chck_is_integer <- function(x, name = "Input") {
  if (!is.numeric(x) || any(x != as.integer(x), na.rm = TRUE)) {
    stop(sprintf("%s must be integer value(s)", name))
  }
  invisible(TRUE)
}

#' Validate IFCA-specific spax object
#' @keywords internal
.chck_ifca <- function(x, check_history = TRUE, name = "Input") {
  .chck_class(x, "spax", name)

  if (!identical(x$type, "iFCA")) {
    stop(name, " must be a result from spax_ifca()")
  }

  if (check_history && (is.null(x$iterations) || is.null(x$iterations$history))) {
    stop("No iteration history found in ", name)
  }

  invisible(TRUE)
}

# 2. Basic Value Checks --------------------------------------------------------
#' Check if value is in valid range
#' @keywords internal
.chck_in_range <- function(x, min = 0, max = 1, inclusive = TRUE, name = "Value") {
  if (inclusive) {
    if (any(x < min | x > max, na.rm = TRUE)) {
      stop(sprintf("%s must be >= %s and <= %s", name, min, max))
    }
  } else {
    if (any(x <= min | x >= max, na.rm = TRUE)) {
      stop(sprintf("%s must be > %s and < %s", name, min, max))
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
    stop(sprintf("%s must have length %d", name, expected))
  }
  invisible(TRUE)
}

#' Check if length of two inputs match
#' @keywords internal
.chck_lengths_match <- function(len_x, len_y, name_x = "First input", name_y = "Second input") {
  if (len_x != len_y) {
    stop(sprintf("Length of %s (%d) must match length of %s (%d)",
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
    stop(sprintf("%s and %s must have the same geometry (resolution, extent, and CRS)",
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
    stop(sprintf("Column(s) not found in %s: %s",
                 df_name,
                 paste(missing, collapse = ", ")))
  }
  invisible(TRUE)
}

# 6. NA Handling --------------------------------------------------------------
#' Check if values contain NAs when not allowed
#' @keywords internal
.chck_no_na <- function(x, name = "Input") {
  if (any(is.na(x))) {
    stop(sprintf("%s contains NA values", name))
  }
  invisible(TRUE)
}
