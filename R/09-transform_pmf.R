#' Validate inputs for PMF transformation
#' @keywords internal
.chck_transform_pmf_raster <- function(x, snap = FALSE) {
  if (snap) {
    return(invisible(TRUE))
  }

  .chck_class(x, "SpatRaster", "x")

  vals <- terra::values(x)

  if (all(is.na(vals))) {
    stop("Input raster 'x' contains only NA values")
  }

  if (any(vals < 0, na.rm = TRUE)) {
    stop("values in x must be >= 0")
  }

  total <- sum(vals, na.rm = TRUE)
  if (total <= 0) {
    stop("Sum of all values in 'x' is zero - cannot create PMF")
  }

  invisible(TRUE)
}

#' Transform input into a probability mass function (PMF)
#'
#' Converts raster or vector inputs into a probability mass function where all
#' values sum to 1. Raster inputs are normalised directly, while vector inputs
#' are first rasterised using a supplied template.
#'
#' @param x A `SpatRaster` containing values to normalise, or a `SpatVector`
#'   whose attribute values will be rasterised.
#' @param value_col Character name of the attribute column to use when `x` is a
#'   `SpatVector`. Ignored for raster inputs.
#' @param template `SpatRaster` providing geometry for rasterising vector
#'   inputs. Required when `x` is a `SpatVector`.
#' @param return_total Logical; when `TRUE`, return both the PMF raster and the
#'   total sum of the input values. Default is `FALSE`.
#' @param snap Logical; skip validation checks when `TRUE`. Default is `FALSE`.
#'
#' @return If `return_total = FALSE`, returns a `SpatRaster` whose cells sum to
#'   1. If `return_total = TRUE`, returns a list with components:
#'   \describe{
#'     \item{pmf}{`SpatRaster` of probabilities}
#'     \item{total}{Numeric total of the original values}
#'   }
#'
#' @examples
#' \dontrun{
#' pop <- terra::rast(u5pd)
#'
#' # Basic PMF
#' pmf <- transform_pmf(pop)
#'
#' # Include total for later reuse
#' result <- transform_pmf(pop, return_total = TRUE)
#' result$total
#' }
#'
#' @export
transform_pmf <- function(x, value_col = NULL, template = NULL,
                          return_total = FALSE, snap = FALSE) {
  if (!snap) {
    if (inherits(x, "SpatRaster")) {
      .chck_transform_pmf_raster(x)
    } else if (inherits(x, "SpatVector")) {
      .chck_vector_input(x, value_col, template)
    } else {
      .chck_class(x, "SpatRaster", "x")
    }
  }

  if (inherits(x, "SpatVector")) {
    x <- terra::rasterize(x, template, field = value_col)
    if (!snap) {
      .chck_transform_pmf_raster(x)
    }
  }

  total <- terra::global(x, "sum", na.rm = TRUE)$sum
  if (total <= 0) {
    stop("Sum of all values in 'x' is zero - cannot create PMF")
  }

  pmf <- x / total

  if (return_total) {
    list(
      pmf = pmf,
      total = total
    )
  } else {
    pmf
  }
}
