#' Save a spax accessibility analysis
#'
#' Saves the result of a floating catchment analysis (an object of class `spax`)
#' with its rasters. A result of [spax()] (a `spax_allocation`) is saved with
#' [saveRDS()].
#'
#' @param x A spax object
#' @param file Character; path to save the object (without extension)
#' @param dir Logical; whether to create a directory with same name for files (default FALSE)
#' @param overwrite Logical; whether to overwrite existing files/directory (default FALSE)
#' @param datatype Character; the data type of the raster file, passed to
#'   [terra::writeRaster()]. The default `"FLT8S"` (double precision) keeps the
#'   values: [read_spax()] returns them as they were. `"FLT4S"` (single
#'   precision, terra's own default) writes files about half the size, with
#'   values rounded to about seven significant digits (a relative change of up
#'   to 6e-8).
#' @param ... Additional arguments passed to [terra::writeRaster()]
#' @return Invisibly returns the input object
#' @examples
#' # E2SFCA on the Cornwall population grid, with a Gaussian decay of 10 minutes
#' pop <- read_spax_example("cornwall_pop.tif")
#' travel <- read_spax_example("cornwall_gp_travel.tif")
#' result <- spax_e2sfca(pop, sf::st_drop_geometry(cornwall_gp), travel,
#'                       decay_params = list(method = "gaussian", sigma = 10),
#'                       id_col = "practice", supply_cols = "gp_fte_extg")
#'
#' # Save it to temporary files: the object, and its accessibility raster
#' path <- tempfile("cornwall_e2sfca")
#' save_spax(result, path)
#' basename(list.files(dirname(path), pattern = basename(path)))
#'
#' # Read it back, then remove the files
#' read_spax(path)
#' unlink(paste0(path, c("_spax.rds", "_accessibility.tif")))
#' @export
#' @md
save_spax <- function(x, file, dir = FALSE, overwrite = FALSE, datatype = "FLT8S",
                      ...) {
  # Input validation
  if (inherits(x, "spax_allocation")) {
    stop("save_spax() saves FCA results; save an allocation result with saveRDS()",
         call. = FALSE)
  }
  .chck_class(x, "spax", "x")
  .chck_class(file, "character", "file")

  # Clean file path and handle directory mode
  base_path <- tools::file_path_sans_ext(file)
  if (dir) {
    dir_path <- base_path
    if (!overwrite && dir.exists(dir_path)) {
      .stop("Directory already exists: ", dir_path, "\nUse overwrite = TRUE to replace")
    }
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    base_path <- file.path(dir_path, basename(base_path))
  }

  # Construct file paths
  spax_file <- paste0(base_path, "_spax.rds")
  rast_file <- paste0(base_path, "_accessibility.tif")
  names_file <- paste0(base_path, "_names.rds")

  # Check for existing files if not overwriting
  if (!overwrite) {
    existing <- c(spax_file, rast_file, names_file)[
      file.exists(c(spax_file, rast_file, names_file))
    ]
    if (length(existing) > 0) {
      .stop(
        "Files exist. Use overwrite = TRUE to replace:\n",
        paste("  -", existing, collapse = "\n")
      )
    }
  }

  # Save components
  terra::writeRaster(x$accessibility, rast_file, overwrite = overwrite,
                     datatype = datatype, ...)

  if (terra::nlyr(x$accessibility) > 1) {
    saveRDS(names(x$accessibility), names_file)
  }

  x_save <- x
  x_save$accessibility <- base_path
  saveRDS(x_save, spax_file)

  invisible(x)
}

#' Read a spax object from disk
#'
#' @param file Character; path/name of saved spax files
#' @return A spax object
#' @examples
#' # E2SFCA on the Cornwall population grid for two supply measures
#' pop <- read_spax_example("cornwall_pop.tif")
#' travel <- read_spax_example("cornwall_gp_travel.tif")
#' result <- spax_e2sfca(pop, sf::st_drop_geometry(cornwall_gp), travel,
#'                       decay_params = list(method = "gaussian", sigma = 10),
#'                       id_col = "practice",
#'                       supply_cols = c("gp_fte_extg", "gp_fte"))
#'
#' # Save it to temporary files
#' path <- tempfile("cornwall_e2sfca")
#' save_spax(result, path)
#'
#' # Read it back: the layers keep their names and values
#' restored <- read_spax(path)
#' names(restored$accessibility)
#' all.equal(terra::values(restored$accessibility),
#'           terra::values(result$accessibility))
#'
#' # Remove the files
#' unlink(paste0(path, c("_spax.rds", "_accessibility.tif", "_names.rds")))
#' @export
read_spax <- function(file) {
  .chck_class(file, "character", "file")

  file <- tools::file_path_sans_ext(file)

  spax_file <- paste0(file, "_spax.rds")
  rast_file <- paste0(file, "_accessibility.tif")

  if (!file.exists(spax_file)) .stop("Spax object file not found: ", spax_file)
  if (!file.exists(rast_file)) .stop("Accessibility raster file not found: ", rast_file)

  x <- readRDS(spax_file)
  x$accessibility <- .attach_raster_names(terra::rast(rast_file), file)

  .chck_class(x, "spax", "reconstructed object")
  return(x)
}
