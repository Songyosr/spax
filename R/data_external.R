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
#' # Load a specific dataset: travel time to each Cornwall GP practice
#' travel <- read_spax_example("cornwall_gp_travel.tif")
#' names(travel)[1:3]
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
