#' Save a spax accessibility analysis
#'
#' @param x A spax object
#' @param file Character; path to save the object (without extension)
#' @param dir Logical; whether to create a directory with same name for files (default FALSE)
#' @param overwrite Logical; whether to overwrite existing files/directory (default FALSE)
#' @param ... Additional arguments passed to terra::writeRaster()
#' @return Invisibly returns the input object
#' @export
save_spax <- function(x, file, dir = FALSE, overwrite = FALSE, ...) {
  # Input validation
  .assert_class(x, "spax", "x")
  .assert_class(file, "character", "file")

  # Clean file path and handle directory mode
  base_path <- tools::file_path_sans_ext(file)
  if (dir) {
    dir_path <- base_path
    if (!overwrite && dir.exists(dir_path)) {
      stop("Directory already exists: ", dir_path, "\nUse overwrite = TRUE to replace")
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
      stop(
        "Files exist. Use overwrite = TRUE to replace:\n",
        paste("  -", existing, collapse = "\n")
      )
    }
  }

  # Save components
  terra::writeRaster(x$accessibility, rast_file, overwrite = overwrite, ...)

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
#' @export
read_spax <- function(file) {
  .assert_class(file, "character", "file")

  file <- tools::file_path_sans_ext(file)

  spax_file <- paste0(file, "_spax.rds")
  rast_file <- paste0(file, "_accessibility.tif")

  if (!file.exists(spax_file)) stop("Spax object file not found: ", spax_file)
  if (!file.exists(rast_file)) stop("Accessibility raster file not found: ", rast_file)

  x <- readRDS(spax_file)
  x$accessibility <- .attach_raster_names(terra::rast(rast_file), file)

  .assert_class(x, "spax", "reconstructed object")
  return(x)
}
