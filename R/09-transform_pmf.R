#' #' Validate inputs for PMF transformation
#' #' @param x SpatRaster to be validated
#' #' @param snap Logical; if TRUE skip validation
#' #' @return Invisible TRUE if valid, error otherwise
#' #' @keywords internal
#' .chck_transform_pmf <- function(x, snap = FALSE) {
#'   if (snap) {
#'     return(invisible(TRUE))
#'   }
#'
#'   # Input type validation
#'   if (!inherits(x, "SpatRaster")) {
#'     stop("Input must be a SpatRaster object")
#'   }
#'
#'   # Check for all NA values
#'   if (all(is.na(terra::values(x)))) {
#'     stop("Input raster contains only NA values")
#'   }
#'
#'   # Check for negative values
#'   min_val <- terra::global(x, "min", na.rm = TRUE)$min
#'   if (min_val < 0) {
#'     stop("Input values cannot be negative")
#'   }
#'
#'   # Check for zero sum
#'   total <- terra::global(x, "sum", na.rm = TRUE)$sum
#'   if (total == 0) {
#'     stop("Total sum is zero - cannot create PMF")
#'   }
#'
#'   invisible(TRUE)
#' }
#'
#' #' Helper function to verify PMF properties
#' #' @param pmf_raster Computed PMF raster
#' #' @return Invisible TRUE, with warning if precision issues detected
#' #' @keywords internal
#' .help_transform_pmf_verify <- function(pmf_raster) {
#'   sum_check <- terra::global(pmf_raster, "sum", na.rm = TRUE)$sum
#'   if (abs(sum_check - 1) > 1e-10) {
#'     warning("PMF sum deviates from 1 by ", abs(sum_check - 1))
#'   }
#'   invisible(TRUE)
#' }
#'
#' #' Compute Probability Mass Function from Population Density
#' #'
#' #' @description
#' #' Converts a population density raster into a probability mass function (PMF)
#' #' where all cell values sum to 1. Useful for spatial sampling where probability
#' #' of selection should be proportional to population density.
#' #'
#' #' @param x SpatRaster of density values (e.g., population density)
#' #' @param return_total logical; if TRUE, returns a list containing both PMF raster
#' #'        and total population. If FALSE (default), returns only PMF raster
#' #' @param snap Logical; if TRUE skip validation and verification
#' #' @return If return_total = FALSE, returns SpatRaster containing probability values
#' #'         that sum to 1. If return_total = TRUE, returns a list with components:
#' #'         \itemize{
#' #'           \item pmf: SpatRaster of probability values
#' #'           \item total: numeric value of total density
#' #'         }
#' #' @examples
#' #' # Basic usage
#' #' pop_density <- terra::rast(matrix(1:100, 10, 10))
#' #' pmf <- transform_pmf(pop_density)
#' #'
#' #' # Get PMF and total population
#' #' result <- transform_pmf(pop_density, return_total = TRUE)
#' #' pmf <- result$pmf
#' #' total_pop <- result$total
#' #' @export
#' transform_pmf <- function(x, return_total = FALSE, snap = FALSE) {
#'   # Validation
#'   if (!snap) {
#'     .chck_transform_pmf(x)
#'   }
#'
#'   # Core computation
#'   total <- terra::global(x, "sum", na.rm = TRUE)$sum
#'   pmf_raster <- x / total
#'
#'   # Verification
#'   if (!snap) {
#'     .help_transform_pmf_verify(pmf_raster)
#'   }
#'
#'   # Return results
#'   if (return_total) {
#'     return(list(pmf = pmf_raster, total = total))
#'   } else {
#'     return(pmf_raster)
#'   }
#' }
#' Transform Input to Probability Mass Function
#'
#' @description
#' Converts input data into a probability mass function (PMF) where all values sum
#' to 1. Supports both raster and vector inputs, making it suitable for various
#' spatial data types.
#'
#' When used with:
#' - Raster input: Normalizes values to create PMF surface
#' - Vector input: Creates PMF surface through rasterization
#'
#' @param x SpatRaster or SpatVector containing values to transform
#' @param value_col Character; name of column if x is SpatVector
#' @param template SpatRaster; template for rasterization if x is SpatVector
#' @param return_total Logical; if TRUE return both PMF and total value
#' @param snap Logical; if TRUE skip validation
#' @return If return_total = FALSE, returns SpatRaster PMF.
#'         If return_total = TRUE, returns list with:
#'         - pmf: SpatRaster PMF
#'         - total: numeric total of input values
#' @examples
#' \dontrun{
#' # From raster input
#' pop <- rast(u5pd)
#' pmf1 <- transform_pmf(pop)
#'
#' # From vector input
#' pmf2 <- transform_pmf(
#'   case_spatial,
#'   value_col = "cases",
#'   template = pop
#' )
#'
#' # With total value
#' result <- transform_pmf(pop, return_total = TRUE)
#' pmf <- result$pmf
#' total <- result$total
#' }
#' @export
transform_pmf <- function(x, value_col = NULL, template = NULL,
                          return_total = FALSE, snap = FALSE) {
  # Input validation
  if (!snap) {
    if (inherits(x, "SpatRaster")) {
      .chck_raster_input(x)
      # Additional checks from original transform_pmf
      if (all(is.na(terra::values(x)))) {
        stop("Input raster contains only NA values")
      }
    } else if (inherits(x, "SpatVector")) {
      .chck_vector_input(x, value_col, template)
    } else {
      stop("Input must be either SpatRaster or SpatVector")
    }
  }

  # Convert vector to raster if needed
  if (inherits(x, "SpatVector")) {
    x <- terra::rasterize(x, template, field = value_col)
  }

  # Get total for both return and validation
  total <- terra::global(x, "sum", na.rm = TRUE)$sum

  # Validate total
  if (total <= 0) {
    stop("Total sum must be positive to create PMF")
  }

  # Create PMF
  pmf <- x / total

  # Return based on return_total
  if (return_total) {
    return(list(
      pmf = pmf,
      total = total
    ))
  } else {
    return(pmf)
  }
}
