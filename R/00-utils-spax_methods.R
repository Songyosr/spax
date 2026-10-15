#------------------------------------------------------------------------------#
# Print Methods
#------------------------------------------------------------------------------#

#' Print method for spax objects
#'
#' @description
#' Prints a concise summary of a spax object, showing key information about the
#' accessibility analysis without detailed statistics.
#'
#' @param x A spax object created by spax_2sfca() or spax_e2sfca()
#' @param ... Currently ignored, for extensibility
#'
#' @return Invisibly returns the input object
#' @export
print.spax <- function(x, ...) {
  # Title with analysis type
  cat("Spatial Accessibility Analysis (", x$type, ")\n\n", sep = "")

  # Core spatial information
  cat("Spatial Properties:\n")
  cat(sprintf("- Resolution: %g x %g\n",
              res(x$accessibility)[1],
              res(x$accessibility)[2]))
  cat(sprintf("- Dimensions: %d x %d\n",
              dim(x$accessibility)[1],
              dim(x$accessibility)[2]))

  # Accessibility measures
  cat(sprintf("\nMeasures: %d\n", nlyr(x$accessibility)))
  cat("- ", paste(names(x$accessibility), collapse = "\n- "), "\n", sep = "")

  # Facility information if available
  if (!is.null(x$facilities)) {
    cat(sprintf("\nFacilities: %d\n", nrow(x$facilities)))
  }

  # Core parameters
  cat("\nParameters:\n")
  if (!is.null(x$parameters$decay_params)) {
    cat("- Decay function:", x$parameters$decay_params$method)
    if (!is.null(x$parameters$decay_params$sigma)) {
      cat(sprintf(" (sigma = %g)", x$parameters$decay_params$sigma))
    }
    cat("\n")
  }
  if (!is.null(x$parameters$demand_normalize)) {
    cat("- Demand normalization:", x$parameters$demand_normalize, "\n")
  }

  invisible(x)
}



#------------------------------------------------------------------------------#
# Summary Methods
#------------------------------------------------------------------------------#

#' Summary method for spax objects
#'
#' @description
#' Provides detailed statistical summaries of accessibility analysis results,
#' including accessibility scores, facility statistics, and model-specific metrics.
#'
#' @param x A spax object created by spax_2sfca() or spax_e2sfca()
#' @param quantiles Numeric vector of probabilities for quantile computation
#' @param ... Currently ignored, for extensibility
#'
#' @return A summary.spax object
#' @exportS3Method base::summary
summary.spax <- function(x, quantiles = c(0, 0.25, 0.5, 0.75, 1), ...) {
  # Calculate accessibility statistics
  acc_stats <- lapply(1:nlyr(x$accessibility), function(i) {
    layer <- x$accessibility[[i]]
    values <- terra::values(layer)

    # Basic statistics
    stats <- terra::global(layer, c("min", "max", "mean", "sd"), na.rm = TRUE)

    # Coverage statistics
    n_cells <- length(values)
    n_covered <- sum(!is.na(values))

    # Quantiles
    quants <- terra::global(layer,
                            function(x) stats::quantile(x, probs = quantiles, na.rm = TRUE)
    )

    # Combine into named list
    list(
      name = names(x$accessibility)[i],
      min = stats$min,
      max = stats$max,
      mean = stats$mean,
      sd = stats$sd,
      quantiles = stats::setNames(as.numeric(quants[1, ]),
                                  names(stats::quantile(0, quantiles))),
      coverage = list(
        cells_total = n_cells,
        cells_covered = n_covered,
        proportion = n_covered / n_cells
      )
    )
  })
  names(acc_stats) <- names(x$accessibility)

  # Calculate facility statistics
  fac_stats <- NULL
  if (!is.null(x$facilities)) {
    fac_stats <- list(
      count = nrow(x$facilities)
    )

    # Supply statistics if present
    supply_cols <- setdiff(names(x$facilities), "id")
    if (length(supply_cols) > 0) {
      fac_stats$supply <- lapply(supply_cols, function(col) {
        values <- x$facilities[[col]]
        list(
          name = col,
          min = min(values, na.rm = TRUE),
          max = max(values, na.rm = TRUE),
          mean = mean(values, na.rm = TRUE),
          sd = sd(values, na.rm = TRUE),
          total = sum(values, na.rm = TRUE)
        )
      })
      names(fac_stats$supply) <- supply_cols
    }
  }

  # Create summary object using constructor
  .create_spax_summary(
    accessibility = acc_stats,
    facilities = fac_stats,
    model_specific = NULL,
    type = x$type,
    parameters = x$parameters
  )
}


# Print method for summary.spax
#' Print function for spax summary objects
#'
#' @description
#' Formats and displays summary statistics for spax objects in a readable format.
#' This is a placeholder until S3 method export issues are resolved.
#'
#' @param x A summary.spax object returned by summary.spax()
#' @param digits Number of digits to round numeric values (default = 4)
#' @param ... Currently ignored, for extensibility
#'
#' @return Invisibly returns the input object
#' @export
print.summary.spax <- function(x, digits = 4, ...) {
  # Format numbers with consistent precision
  fmt <- paste0("%.", digits, "g")

  # Title and type
  cat("\nSummary of Spatial Accessibility Analysis (", x$type, ")\n", sep = "")
  cat(paste(rep("-", 45), collapse = ""), "\n\n")

  # Accessibility Statistics
  cat("Accessibility Measures:\n")
  for (measure in x$accessibility) {
    cat("\n", measure$name, ":\n", sep = "")
    cat(sprintf("  Range: %s to %s\n",
                sprintf(fmt, measure$min),
                sprintf(fmt, measure$max)))
    cat(sprintf("  Mean (SD): %s (%s)\n",
                sprintf(fmt, measure$mean),
                sprintf(fmt, measure$sd)))

    # Quantiles with labels
    cat("  Quantiles:\n")
    quant_labels <- names(measure$quantiles)
    if (identical(quant_labels, c("0%", "25%", "50%", "75%", "100%"))) {
      quant_labels <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")
    }
    for (i in seq_along(measure$quantiles)) {
      cat(sprintf("    %-8s %s\n",
                  paste0(quant_labels[i], ":"),
                  sprintf(fmt, measure$quantiles[i])))
    }

    # Coverage statistics
    cat(sprintf("\n  Coverage: %.1f%% (%d of %d cells)\n",
                100 * measure$coverage$proportion,
                measure$coverage$cells_covered,
                measure$coverage$cells_total))
  }

  # Facility Statistics (if present)
  if (!is.null(x$facilities)) {
    cat("\nFacility Statistics:\n")
    cat("-------------------\n")
    cat(sprintf("Total Facilities: %d\n", x$facilities$count))

    if (!is.null(x$facilities$supply)) {
      for (supply in x$facilities$supply) {
        cat(sprintf("\n%s:\n", supply$name))
        cat(sprintf("  Total Supply: %s\n", sprintf(fmt, supply$total)))
        cat(sprintf("  Mean (SD): %s (%s)\n",
                    sprintf(fmt, supply$mean),
                    sprintf(fmt, supply$sd)))
        cat(sprintf("  Range: %s to %s\n",
                    sprintf(fmt, supply$min),
                    sprintf(fmt, supply$max)))
      }
    }
  }

  invisible(x)
}

#------------------------------------------------------------------------------#
# Plot Methods
#------------------------------------------------------------------------------#

#' Plot Spatial Accessibility Results
#'
#' @description
#' Plot method for spax objects. This function visualizes the accessibility surface(s)
#' contained in a spax object using the terra plotting engine.
#'
#' @param x A spax object containing accessibility results
#' @param ... Additional arguments passed to [terra::plot()]. Common options include:
#'        \itemize{
#'          \item y: which layers to plot (number or name)
#'          \item main: title(s) for the plot(s)
#'          \item col: color scheme
#'          \item breaks: break points for the legend
#'          \item legend: logical; whether to show the legend
#'        }
#'
#' @return The plot is drawn on the current graphics device.
#'
#' @details
#' This is a simple wrapper around [terra::plot()] for SpatRaster objects.
#' For full control over plot appearance, including layer selection, color schemes,
#' legends, and additional map elements, see the documentation of the terra
#' package (?terra::plot).
#'
#' @seealso
#' [terra::plot()] for the complete set of plotting parameters and options
#'
#' @examples
#' # Load example data
#' library(terra)
#' library(sf)
#'
#' # Load data: Cornwall population grid and travel time to each GP practice
#' pop <- read_spax_example("cornwall_pop.tif")
#' travel <- read_spax_example("cornwall_gp_travel.tif")
#'
#' # Calculate accessibility for two GP supply measures
#' result <- spax_e2sfca(
#'   demand = pop,
#'   supply = st_drop_geometry(cornwall_gp),
#'   distance = travel,
#'   decay_params = list(method = "gaussian", sigma = 10),
#'   demand_normalize = "standard",
#'   id_col = "practice",
#'   supply_cols = c("gp_fte_extg", "gp_fte")
#' )
#'
#' # Basic plot of all layers
#' plot(result)
#'
#' # Plot with custom title and color scheme
#' plot(result, main = c("GPs excluding trainees", "All GPs"),
#'      col = hcl.colors(100, "YlOrRd"))
#'
#' # Plot single layer by name
#' plot(result, y = "gp_fte_extg", main = "Access to GPs")
#' @exportS3Method graphics::plot
plot.spax <- function(x, ...) {
  plot(x$accessibility, ...)
}
