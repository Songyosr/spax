#' Create a uniform PMF from a template and vector
#' @param x SpatVector containing areas
#' @param template SpatRaster template
#' @return SpatRaster PMF with uniform weights within polygons
#' @keywords internal
.help_create_uniform_pmf <- function(x, template) {
  # Create binary raster of polygon coverage
  area_rast <- terra::rasterize(x, template, field = 1)
  # Convert to PMF using existing function
  return(transform_pmf(area_rast, snap = TRUE))
}

#' Validate inputs for demand transformation
#' @keywords internal
.chck_transform_demand <- function(x, value_col, pmf = NULL, template = NULL) {
  # Input type validation
  if (!inherits(x, "SpatVector")) {
    stop("Input 'x' must be a SpatVector object")
  }

  # Check value column
  if (!value_col %in% names(x)) {
    stop(sprintf("Column '%s' not found in input vector", value_col))
  }

  # Check case counts; `[[` on a SpatVector returns a one-column data frame
  cases <- x[[value_col]][[1]]
  if (!is.numeric(cases)) {
    stop("Case counts must be numeric")
  }
  if (any(cases < 0, na.rm = TRUE)) {
    stop("Case counts cannot be negative")
  }

  # PMF validation if provided
  if (!is.null(pmf)) {
    if (!inherits(pmf, "SpatRaster")) {
      stop("PMF must be a SpatRaster object")
    }
    # Check PMF sums to 1
    sum_check <- global(pmf, "sum", na.rm = TRUE)$sum
    if (abs(sum_check - 1) > 1e-10) {
      stop("PMF values must sum to 1")
    }
  }

  # Template validation if provided
  if (!is.null(template)) {
    if (!inherits(template, "SpatRaster")) {
      stop("Template must be a SpatRaster object")
    }
  }

  # Check that either PMF or template is provided
  if (is.null(pmf) && is.null(template)) {
    stop("Either PMF or template raster must be provided")
  }

  invisible(TRUE)
}

#' Core computation for demand transformation
#' @keywords internal
.help_transform_demand_core <- function(x, value_col, pmf = NULL, template = NULL,
                                        density = FALSE) {
  # Get total cases
  total_cases <- sum(x[[value_col]][[1]], na.rm = TRUE)

  # If no PMF provided, create uniform PMF
  if (is.null(pmf)) {
    message("No PMF provided. Assuming uniform distribution within each area.")
    pmf <- .help_create_uniform_pmf(x, template)
  }

  # Extract original PMF values for each polygon
  poly_weights <- terra::extract(pmf, x, fun = sum, na.rm = TRUE)
  # Counts are placed with rasterize() (cell centres); an area that receives no
  # cell there loses its count even if extract() gives it a partial weight.
  placed <- terra::rasterize(x, pmf, field = seq_len(nrow(x)))
  placed <- unique(terra::values(placed, mat = FALSE))
  empty <- !(seq_len(nrow(x)) %in% placed) |
    !is.finite(poly_weights[, 2]) | poly_weights[, 2] <= 0
  if (any(empty)) {
    warning(sprintf(paste0("%d area(s) cover no grid cell or have zero weight; ",
      "their counts are dropped and the surface is rescaled. ",
      "Use larger areas or a finer template."), sum(empty)), call. = FALSE)
  }

  # Calculate scaling factors for each polygon
  cases <- x[[value_col]][[1]]
  scale_factors <- cases / poly_weights[,2]

  # Create raster of scaling factors
  scale_rast <- terra::rasterize(x, pmf, field = scale_factors)

  # Apply scaling factors to PMF
  result <- transform_pmf(pmf * scale_rast, snap = TRUE)

  # Return density or PMF
  if (density) {
    return(result * total_cases)
  } else {
    return(transform_pmf(result, snap = TRUE))
  }
}

#' Transform Case Count Data into Spatial Distribution
#'
#' @description
#' Transforms case count data from vector format into either a probability mass
#' function (PMF) or density surface, optionally using an existing PMF as a
#' weighting surface.
#'
#' @param x SpatVector containing case counts
#' @param value_col Character; name of column containing case counts
#' @param pmf Optional SpatRaster PMF surface for weighting
#' @param template Optional SpatRaster template (used if PMF not provided)
#' @param density Logical; if TRUE returns density surface, if FALSE returns PMF
#' @param full_output Logical; if TRUE returns list with additional information
#' @param snap Logical; if TRUE skip validation
#' @return If full_output = FALSE, returns SpatRaster (PMF or density).
#'         If full_output = TRUE, returns list with:
#'         - surface: SpatRaster (PMF or density)
#'         - totals: data.frame with original and redistributed totals by area
#' @examples
#' pop <- read_spax_example("cornwall_pop.tif")
#'
#' # Group small areas into larger units (LSOA names end in a letter; dropping it
#' # gives the containing MSOA) so every unit covers several grid cells
#' areas <- terra::vect(sf::st_transform(cornwall_lsoa, 4326))
#' areas$msoa <- sub("[A-Z]$", "", areas$name)
#' msoa <- terra::aggregate(areas, by = "msoa", fun = "sum")
#'
#' # Area totals: residents registered with a GP outside Cornwall
#' # Basic PMF creation (uniform within each area)
#' result1 <- transform_demand(
#'   msoa,
#'   value_col = "sum_registered_out",
#'   template = pop
#' )
#'
#' # Create density surface with existing PMF
#' pop_pmf <- transform_pmf(pop)
#' result2 <- transform_demand(
#'   msoa,
#'   value_col = "sum_registered_out",
#'   pmf = pop_pmf,
#'   density = TRUE,
#'   full_output = TRUE
#' )
#' @export
transform_demand <- function(x, value_col, pmf = NULL, template = NULL,
                             density = FALSE, full_output = FALSE, snap = FALSE) {
  # Validation
  if (!snap) {
    .chck_transform_demand(x, value_col, pmf, template)
  }

  # Core computation
  result <- .help_transform_demand_core(x, value_col, pmf, template, density)

  # Return based on output type
  if (!full_output) {
    return(result)
  } else {
    # Calculate area totals
    orig_totals <- x[[value_col]][[1]]
    new_totals <- terra::extract(result, x, fun = sum, na.rm = TRUE)

    return(list(
      surface = result,
      totals = data.frame(
        id = if ("id" %in% names(x)) x$id else seq_len(nrow(x)),
        original = orig_totals,
        redistributed = new_totals[,2]
      )
    ))
  }
}
