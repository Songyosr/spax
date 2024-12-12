#' Calculate accessibility for a single ratio vector
#'
#' Internal function to calculate accessibility using one ratio vector.
#' Not exported - used by calc_accessibility().
#'
#' @param ratios Numeric vector of supply-to-demand ratios
#' @param weights Multi-layer SpatRaster of pre-computed weights
#' @param full_output Logical; whether to return full results or just total accessibility
#' @return If full_output = TRUE, returns list with:
#'         - access_total: Single-layer SpatRaster of total accessibility
#'         - access_by_site: Multi-layer SpatRaster of site-specific accessibility
#'         If full_output = FALSE, returns only access_total SpatRaster
#' @keywords internal
calc_single_accessibility <- function(ratios, weights, full_output = TRUE) {
  # Input validation
  if (nlyr(weights) != length(ratios)) {
    stop("Number of weights layers must match length of ratio vector")
  }

  # Calculate site-specific accessibility
  access_by_site <- weights * ratios

  # Sum for total accessibility
  access_total <- sum(access_by_site, na.rm = TRUE)

  if (full_output) {
    return(list(
      access_total = access_total,
      access_by_site = access_by_site
    ))
  } else {
    return(access_total)
  }
}

#' Calculate spatial accessibility index
#'
#' Calculates accessibility scores using supply-to-demand ratios and pre-computed
#' spatial weights. Handles multiple ratio measures efficiently.
#'
#' @param ratios Numeric vector, matrix, or data.frame of supply-to-demand ratios:
#'        - Vector: Must match number of weight layers
#'        - Matrix: Rows match weight layers, columns are different measures
#'        - Data.frame: Same as matrix but requires ratio_cols parameter
#' @param weights Multi-layer SpatRaster where each layer represents one site's
#'        probability-based catchment area
#' @param ratio_cols Character vector of column names if ratios is a data.frame
#' @param full_output Logical; whether to return full results or just total accessibility (default FALSE)
#' @param parallel Logical; whether to use parallel processing (default FALSE)
#' @param n_cores Integer; number of cores for parallel processing (default NULL)
#' @return If full_output = TRUE, returns list containing:
#'         - access_totals: SpatRaster stack of total accessibility per ratio measure
#'         - access_by_site: List of site-specific accessibility SpatRasters
#'         If full_output = FALSE, returns only access_totals SpatRaster
#' @note For parallel processing, the parallel package must be installed.
#' @export
calc_accessibility <- function(ratios, weights, ratio_cols = NULL,
                               full_output = FALSE,
                               parallel = FALSE, n_cores = NULL) {
  # Handle vector input directly
  if (is.vector(ratios)) {
    return(calc_single_accessibility(ratios, weights, full_output))
  }

  # Process data.frame or matrix input
  if (is.data.frame(ratios)) {
    if (is.null(ratio_cols)) {
      stop("ratio_cols must be specified when ratios is a data.frame")
    }
    ratios_mat <- as.matrix(ratios[ratio_cols])
    col_names <- ratio_cols
  } else if (is.matrix(ratios)) {
    ratios_mat <- ratios
    col_names <- colnames(ratios_mat)
    if (is.null(col_names)) {
      col_names <- paste0("ratio", 1:ncol(ratios_mat))
    }
  } else {
    stop("ratios must be a vector, matrix, or data.frame")
  }

  # Validate dimensions
  if (nrow(ratios_mat) != nlyr(weights)) {
    stop("Number of ratios must match number of weight layers")
  }

  # Setup parallel processing if requested
  if (parallel) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      stop("Package 'parallel' needed for parallel processing. Please install it.",
           call. = FALSE)
    }
    if (is.null(n_cores)) {
      n_cores <- parallel::detectCores() - 1
    }
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl))

    # Export required objects to cluster
    parallel::clusterExport(cl, c("weights", "calc_single_accessibility", "full_output"))

    # Calculate accessibility for each ratio measure in parallel
    results <- parallel::parApply(cl, ratios_mat, 2, function(r) {
      calc_single_accessibility(r, weights, full_output)
    })
  } else {
    # Calculate accessibility for each ratio measure sequentially
    results <- apply(ratios_mat, 2, function(r) {
      calc_single_accessibility(r, weights, full_output)
    }, simplify = FALSE)
  }

  # Name the results list with column names
  names(results) <- col_names

  if (full_output) {
    # Extract and combine total accessibility rasters using pluck
    access_totals <- rast(rlist::pluck(results, "access_total"))
    names(access_totals) <- col_names

    return(list(
      access_totals = access_totals,
      access_by_site = lapply(results, function(x) x$access_by_site)
    ))
  } else {
    # For simple output, just combine the total accessibility rasters
    access_totals <- rast(results)
    names(access_totals) <- col_names
    return(access_totals)
  }
}
