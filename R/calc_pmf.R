#' Create Probability Mass Function from Population Density
#'
#' @description
#' Converts a population density raster into a probability mass function (PMF) raster
#' where all cell values sum to 1. This is useful for spatial sampling where the
#' probability of selection should be proportional to population density.
#'
#' @param pop_density SpatRaster of population density values
#' @param return_total logical; if TRUE, returns a list containing both PMF raster
#'        and total population. If FALSE (default), returns only PMF raster
#' @return If return_total = FALSE, returns SpatRaster containing probability values
#'         that sum to 1. If return_total = TRUE, returns a list with components:
#'         \itemize{
#'           \item pmf: SpatRaster of probability values
#'           \item total_pop: numeric value of total population
#'         }
#' @export
calc_pmf <- function(pop_density, return_total = FALSE) {
  # Input validation
  if (!inherits(pop_density, "SpatRaster")) {
    stop("Input must be a SpatRaster object")
  }

  # Check for all NA values
  if (all(is.na(values(pop_density)))) {
    stop("Input raster contains only NA values")
  }

  # Check for negative values
  min_val <- global(pop_density, "min", na.rm = TRUE)$min
  if (min_val < 0) {
    stop("Population density cannot contain negative values")
  }

  # Calculate total population (sum of all cells)
  total_population <- global(pop_density, "sum", na.rm = TRUE)$sum

  # Check for zero total population
  if (total_population == 0) {
    stop("Total population is zero - cannot create PMF")
  }

  # Create PMF by dividing each cell by total population
  pmf_raster <- pop_density / total_population

  # Verify the sum is 1 (within floating point precision)
  sum_check <- global(pmf_raster, "sum", na.rm = TRUE)$sum
  if (abs(sum_check - 1) > 1e-10) {
    warning("PMF sum deviates from 1 by ", abs(sum_check - 1))
  }

  # Return based on return_total parameter
  if (return_total) {
    return(list(
      pmf = pmf_raster,
      total_pop = total_population
    ))
  } else {
    return(pmf_raster)
  }
}
