#' Helper function to attach names to raster if available
#' @param rast SpatRaster object
#' @param base_path Path to the raster file
#' @return SpatRaster with names attached if available
#' @keywords internal
.attach_raster_names <- function(rast, base_path) {
  # Construct names file path by adding _names.rds to base path
  names_path <- sub("\\.tif$", "_names.rds", base_path)

  # If names file exists, load and attach names
  if (file.exists(names_path)) {
    layer_names <- readRDS(names_path)
    if (length(layer_names) == terra::nlyr(rast)) {
      names(rast) <- layer_names
    }
  }

  return(rast)
}

#' Read Example Datasets from spax Package
#'
#' @description
#' Lists and loads example datasets included with the spax package.
#' When called without arguments, lists available datasets. When given
#' a filename, loads that specific dataset.
#'
#' @param dataset Character. Name of dataset to load. If NULL (default),
#'        lists available files.
#'
#' @return If dataset is NULL, returns character vector of available files.
#'         If dataset is specified, returns the loaded dataset as a SpatRaster.
#'
#' @examples
#' # List available datasets
#' read_spax_example()
#'
#' # Load a specific dataset
#' hos_iscr <- read_spax_example("hos_iscr.tif")
#'
#' @export
#'
read_spax_example <- function(dataset = NULL) {
  # Get all files in extdata directory
  ex_dir <- system.file("extdata", package = "spax")
  if (ex_dir == "") {
    stop("Example data directory not found. Is spax installed correctly?")
  }

  available <- dir(ex_dir, pattern = "\\.tif$")

  # If no dataset specified, return list of available files
  if (is.null(dataset)) {
    return(available)
  }

  # Check if requested dataset exists
  if (!dataset %in% available) {
    stop(sprintf(
      "Dataset '%s' not found. Available datasets:\n%s",
      dataset,
      paste("  -", available, collapse = "\n")
    ))
  }

  # Full path to the dataset
  full_path <- system.file("extdata", dataset, package = "spax")

  # Load raster and attach names if available
  r <- terra::rast(full_path)
  r <- .attach_raster_names(r, full_path)

  return(r)
}

# Data -----------------------------------------------------------------------

## hos_iscr.tif --------------------------------------------------------------
#' Example Isochrone Data for Thailand Health Region 12
#'
#' @description
#' A RasterStack containing travel time isochrones for 77 hospitals in Thailand's Region 12.
#' Each layer represents the travel time (in minutes) to reach one hospital, computed using
#' the OSRM (Open Source Routing Machine) routing engine.
#'
#' **Note**: As of version 0.2.4, this dataset has been moved to external data storage
#' to improve package performance. The data is no longer lazy-loaded, so direct usage
#' like `rast(hos_iscr)` is not supported. Please use `read_spax_example("hos_iscr.tif")`
#' to access this dataset.
#'
#' @format A GeoTIFF file containing a multi-layer raster with:
#' \describe{
#'   \item{Dimensions}{509 rows × 647 columns × 77 layers}
#'   \item{Resolution}{520.4038 × 520.4038 meters}
#'   \item{Extent}{505646.5, 842347.8, 620843.7, 885729.2 (xmin, xmax, ymin, ymax)}
#'   \item{CRS}{WGS 84 / UTM zone 47N (EPSG:32647)}
#'   \item{Values}{Travel time in minutes}
#'   \item{Layer Names}{Match facility IDs in hc12_hos (e.g., "c172", "c173")}
#' }
#'
#' @details
#' Travel times were calculated using the following time-break structure:
#' \itemize{
#'   \item 1-minute intervals for first 10 minutes
#'   \item 2-minute intervals from 12 to 30 minutes
#'   \item 5-minute intervals from 35 to 60 minutes
#'   \item 15-minute intervals from 75 to 180 minutes
#' }
#'
#' @source
#' Computed using OSRM with OpenStreetMap data.
#' Hospital locations from \code{\link{hc12_hos}}.
#'
#' @references
#' Luxen, D., & Vetter, C. (2011). Real-time routing with OpenStreetMap data.
#' In Proceedings of the 19th ACM SIGSPATIAL International Conference on
#' Advances in Geographic Information Systems (pp. 513-516).
#'
#' @examples
#' \dontrun{
#' # Old method (no longer supported):
#' # rast(hos_iscr)  # Will not work after v0.2.4
#'
#' # New method:
#' hos_iscr <- read_spax_example("hos_iscr.tif")
#'
#' # Plot travel time to first hospital
#' plot(hos_iscr[[1]], main = "Travel Time to First Hospital (minutes)")
#' }
#' @name hos_iscr
NULL


## u5pd.tif ------------------------------------------------------------------
#' Under-5 Population Density in Thailand's Health Region 12
#'
#' @description
#' A raster containing population density estimates for children under five years old
#' in Thailand's Health Region 12. The data represents the number of children per
#' grid cell, derived from Meta's High Resolution Population Density Maps project.
#' Available as external data through read_spax_example("u5pd.tif").
#'
#' Note: As of version 0.2.4, this dataset has been moved to external data storage
#' to improve package performance. Please use read_spax_example("u5pd.tif") to
#' access this dataset.
#'
#' @format A GeoTIFF file containing a raster with:
#' \describe{
#'   \item{Dimensions}{865 rows × 1100 columns}
#'   \item{Resolution}{520.4038 × 520.4038 meters}
#'   \item{Extent}{505677.7, 842408.5, 620846.3, 885639.2 (xmin, xmax, ymin, ymax)}
#'   \item{CRS}{WGS 84 / UTM zone 47N (EPSG:32647)}
#'   \item{Values}{Population density (children per grid cell)}
#'   \item{Range}{Minimum: 0, Maximum: 33.19188}
#' }
#'
#' @source Meta. (2024). High Resolution Population Density Maps. Data for Good at Meta.
#' Retrieved from \url{https://dataforgood.facebook.com/dfg/tools/high-resolution-population-density-maps}
#'
#' @details
#' This dataset was processed from Meta's High Resolution Population Density Maps
#' for Thailand (2020). The original data was aggregated to a 520.4038-meter
#' resolution to balance spatial detail with computational efficiency. Zero values
#' indicate areas with no estimated child population, while NA values represent
#' areas outside the region boundary or without data coverage.
#'
#' @examples
#' \dontrun{
#' # Load the population density data
#' pop <- read_spax_example("u5pd.tif")
#'
#' # Basic visualization
#' plot(pop, main = "Under-5 Population Density")
#'
#' # Get basic statistics
#' terra::global(pop, "sum", na.rm = TRUE) # Total estimated children
#' terra::global(pop, c("min", "max"), na.rm = TRUE) # Range of density values
#'
#' # Convert to probability mass function for spatial sampling
#' pmf_result <- transform_pmf(pop, return_total = TRUE)
#' pop_pmf <- pmf_result$pmf
#' total_pop <- pmf_result$total
#'
#' # Generate sample points based on density
#' samples <- sample_pmf(
#'   pop_pmf,
#'   method = "poisson",
#'   size = total_pop,
#'   prob = 0.1, # Example sampling rate
#'   iterations = 1
#' )
#' }
#'
#' @name u5pd
NULL
