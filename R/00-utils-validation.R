# 1. Basic Type Assertions ----------------------------------------------------
#' Assert input is of specific class(es)
#' @param x Object to check
#' @param classes Character vector of valid classes
#' @param name Name of input for error messages
#' @keywords internal
.assert_class <- function(x, classes, name = "Input") {
  if (!inherits(x, classes)) {
    class_list <- paste(classes, collapse = ", ")
    stop(sprintf("%s must be one of the following classes: %s", name, class_list))
  }
  invisible(TRUE)
}

#' Assert input is numeric
#' @param x Object to check
#' @param name Name of input for error messages
#' @keywords internal
.assert_numeric <- function(x, name = "Input") {
  if (!is.numeric(x)) {
    stop(sprintf("%s must be numeric", name))
  }
  invisible(TRUE)
}

#' Assert value is integer
#' @param x Numeric vector to check
#' @param name Name of input for error messages
#' @keywords internal
.assert_integer <- function(x, name = "Input") {
  if (!is.numeric(x) || any(x != as.integer(x), na.rm = TRUE)) {
    stop(sprintf("%s must be integer value(s)", name))
  }
  invisible(TRUE)
}

# 2. Basic Value Assertions --------------------------------------------------
#' Assert values are strictly greater than minimum
#' @param x Numeric vector to check
#' @param min Minimum value to exceed
#' @param name Name of input for error messages
#' @keywords internal
.assert_greater <- function(x, min, name = "Value") {
  if (any(x <= min, na.rm = TRUE)) {
    stop(sprintf("%s must be > %s", name, min))
  }
  invisible(TRUE)
}

#' Assert values are greater than or equal to minimum
#' @param x Numeric vector to check
#' @param min Minimum allowed value
#' @param name Name of input for error messages
#' @keywords internal
.assert_greater_equal <- function(x, min, name = "Value") {
  if (any(x < min, na.rm = TRUE)) {
    stop(sprintf("%s must be >= %s", name, min))
  }
  invisible(TRUE)
}

#' Assert values are strictly less than maximum
#' @param x Numeric vector to check
#' @param max Maximum value to not exceed
#' @param name Name of input for error messages
#' @keywords internal
.assert_less <- function(x, max, name = "Value") {
  if (any(x >= max, na.rm = TRUE)) {
    stop(sprintf("%s must be < %s", name, max))
  }
  invisible(TRUE)
}

#' Assert values are less than or equal to maximum
#' @param x Numeric vector to check
#' @param max Maximum allowed value
#' @param name Name of input for error messages
#' @keywords internal
.assert_less_equal <- function(x, max, name = "Value") {
  if (any(x > max, na.rm = TRUE)) {
    stop(sprintf("%s must be <= %s", name, max))
  }
  invisible(TRUE)
}

#' Assert values are within range with explicit boundary conditions
#' @param x Numeric vector to check
#' @param min Minimum allowed value
#' @param max Maximum allowed value
#' @param name Name of input for error messages
#' @keywords internal
.assert_range <- function(x, min, max, name = "Value") {
  if (any(x < min | x > max, na.rm = TRUE)) {
    stop(sprintf("%s must be >= %s and <= %s", name, min, max))
  }
  invisible(TRUE)
}

#' Assert values are positive
#' @param x Numeric vector to check
#' @param allow_zero Whether to include zero as valid value
#' @param name Name of input for error messages
#' @keywords internal
.assert_positive <- function(x, allow_zero = FALSE, name = "Input") {
  if (allow_zero) {
    .assert_greater_equal(x, 0, name)
  } else {
    .assert_greater(x, 0, name)
  }
  invisible(TRUE)
}

# 3. Length and Dimension Assertions ------------------------------------------
#' Assert length matches expected value
#' @param len_x Length to check
#' @param expected Expected length
#' @param name Name of input for error messages
#' @keywords internal
.assert_length <- function(len_x, expected, name = "Input") {
  if (len_x != expected) {
    stop(sprintf("%s must have length %d", name, expected))
  }
  invisible(TRUE)
}

#' Assert two lengths match
#' @param len_x First length to check
#' @param len_y Second length to check
#' @param name_x Name of first input for error messages
#' @param name_y Name of second input for error messages
#' @keywords internal
.assert_lengths_match <- function(len_x, len_y, name_x = "First input", name_y = "Second input") {
  if (len_x != len_y) {
    stop(sprintf("Length of %s (%d) must match length of %s (%d)",
                 name_x, len_x, name_y, len_y))
  }
  invisible(TRUE)
}

# 4. Raster Assertions ------------------------------------------------------
#' Assert input is a SpatRaster
#' @param x Object to check
#' @param name Name of input for error messages
#' @keywords internal
.assert_raster <- function(x, name = "Input") {
  .assert_class(x, "SpatRaster", name = name)
}

#' Assert two rasters have matching geometry
#' @param x First raster to check
#' @param y Second raster to check
#' @param name_x Name of first raster for error messages
#' @param name_y Name of second raster for error messages
#' @keywords internal
.assert_raster_alignment <- function(x, y, name_x = "First raster", name_y = "Second raster") {
  # # Verify both inputs are rasters first
  # .assert_raster(x, name_x)
  # .assert_raster(y, name_y)

  # Commpare
  if (!compareGeom(x, y, stopOnError = FALSE)) {
    stop(sprintf("%s and %s must have the same geometry (resolution, extent, and CRS)",
                 name_x, name_y))
  }
  invisible(TRUE)
}


# 5. Data Frame Assertions -------------------------------------------------
#' Assert columns exist in data frame
#' @param df Data frame to check
#' @param cols Character vector of required column names
#' @param df_name Name of data frame for error messages
#' @keywords internal
.assert_cols_exist <- function(df, cols, df_name = "data.frame") {
  # Verify input is a data frame first
  # .assert_class(df, "data.frame", df_name)

  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(sprintf("Column(s) not found in %s: %s",
                 df_name,
                 paste(missing, collapse = ", ")))
  }
  invisible(TRUE)
}

# 6. NA Assertions --------------------------------------------------------
#' Assert no NA values present
#' @param x Object to check for NAs
#' @param name Name of input for error messages
#' @keywords internal
.assert_no_na <- function(x, name = "Input") {
  if (any(is.na(x))) {
    stop(sprintf("%s contains NA values", name))
  }
  invisible(TRUE)
}
