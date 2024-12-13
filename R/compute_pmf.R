#' Compute Probability Mass Function from Population Density
#'
#' @description
#' Converts a population density raster into a probability mass function (PMF)
#' where all cell values sum to 1. Useful for spatial sampling where probability
#' of selection should be proportional to population density.
#'
#' @param x SpatRaster of density values (e.g., population density)
#' @param return_total logical; if TRUE, returns a list containing both PMF raster
#'        and total population. If FALSE (default), returns only PMF raster
#' @return If return_total = FALSE, returns SpatRaster containing probability values
#'         that sum to 1. If return_total = TRUE, returns a list with components:
#'         \itemize{
#'           \item pmf: SpatRaster of probability values
#'           \item total: numeric value of total density
#'         }
#' @examples
#' # Basic usage
#' pop_density <- terra::rast(matrix(1:100, 10, 10))
#' pmf <- compute_pmf(pop_density)
#'
#' # Get PMF and total population
#' result <- compute_pmf(pop_density, return_total = TRUE)
#' pmf <- result$pmf
#' total_pop <- result$total
#' @export
compute_pmf <- function(x, return_total = FALSE) {
  # Input validation
  if (!inherits(x, "SpatRaster")) {
    stop("Input must be a SpatRaster object")
  }

  # Check for all NA values
  if (all(is.na(values(x)))) {
    stop("Input raster contains only NA values")
  }

  # Check for negative values
  min_val <- global(x, "min", na.rm = TRUE)$min
  if (min_val < 0) {
    stop("Input values cannot be negative")
  }

  # Calculate total
  total <- global(x, "sum", na.rm = TRUE)$sum

  # Check for zero total
  if (total == 0) {
    stop("Total sum is zero - cannot create PMF")
  }

  # Create PMF by dividing each cell by total
  pmf_raster <- x / total

  # Verify the sum is 1 (within floating point precision)
  sum_check <- global(pmf_raster, "sum", na.rm = TRUE)$sum
  if (abs(sum_check - 1) > 1e-10) {
    warning("PMF sum deviates from 1 by ", abs(sum_check - 1))
  }

  # Return based on return_total parameter
  if (return_total) {
    return(list(
      pmf = pmf_raster,
      total = total
    ))
  } else {
    return(pmf_raster)
  }
}
