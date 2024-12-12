#' Distribute values across weighted surfaces
#'
#' @description
#' General-purpose function to distribute values across space using a stack of weight
#' rasters. Each input value is spread according to its corresponding weight layer.
#'
#' @param values Numeric vector, matrix, or data.frame of values to distribute:
#'        - Vector: Must match number of weight layers
#'        - Matrix: Rows match weight layers, columns are different measures
#'        - Data.frame: Same as matrix but requires value_cols parameter
#' @param weights Multi-layer SpatRaster where each layer represents one unit's
#'        weighted distribution surface
#' @param value_cols Character vector of column names if values is a data.frame
#' @param full_output Logical; whether to return intermediate calculations (default FALSE)
#' @param parallel Logical; whether to use parallel processing (default FALSE)
#' @param n_cores Integer; number of cores for parallel processing (default NULL)
#' @return If full_output = TRUE, returns list containing:
#'         - total_distribution: SpatRaster stack of total distribution per measure
#'         - unit_distribution: List of unit-specific distribution SpatRasters
#'         If full_output = FALSE, returns only total_distribution SpatRaster
#' @export
weighted_spread <- function(values, weights, value_cols = NULL,
                            full_output = FALSE,
                            parallel = FALSE, n_cores = NULL) {
  # Handle vector input directly
  if (is.vector(values)) {
    return(.spread_single(values, weights, full_output))
  }

  # Process data.frame or matrix input
  if (is.data.frame(values)) {
    if (is.null(value_cols)) {
      stop("value_cols must be specified when values is a data.frame")
    }
    values_mat <- as.matrix(values[value_cols])
    col_names <- value_cols
  } else if (is.matrix(values)) {
    values_mat <- values
    col_names <- colnames(values_mat)
    if (is.null(col_names)) {
      col_names <- paste0("value", 1:ncol(values_mat))
    }
  } else {
    stop("values must be a vector, matrix, or data.frame")
  }

  # Validate dimensions
  if (nrow(values_mat) != nlyr(weights)) {
    stop("Number of values must match number of weight layers")
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

    parallel::clusterExport(cl, c("weights", ".spread_single", "full_output"))
    results <- parallel::parApply(cl, values_mat, 2, function(v) {
      .spread_single(v, weights, full_output)
    })
  } else {
    results <- apply(values_mat, 2, function(v) {
      .spread_single(v, weights, full_output)
    }, simplify = FALSE)
  }

  names(results) <- col_names

  if (full_output) {
    total_distribution <- rast(rlist::pluck(results, "total_distribution"))
    names(total_distribution) <- col_names

    return(list(
      total_distribution = total_distribution,
      unit_distribution = lapply(results, function(x) x$unit_distribution)
    ))
  } else {
    total_distribution <- rast(results)
    names(total_distribution) <- col_names
    return(total_distribution)
  }
}

#' Internal function for single value distribution
#' @keywords internal
.spread_single <- function(values, weights, full_output = TRUE) {
  if (nlyr(weights) != length(values)) {
    stop("Number of weights layers must match length of values vector")
  }

  unit_distribution <- weights * values
  total_distribution <- sum(unit_distribution, na.rm = TRUE)

  if (full_output) {
    return(list(
      total_distribution = total_distribution,
      unit_distribution = unit_distribution
    ))
  } else {
    return(total_distribution)
  }
}

#' Calculate spatial accessibility scores
#'
#' @description
#' Specialized version of weighted_spread() for accessibility analysis.
#' Distributes supply-to-demand ratios across space using probability-based
#' catchment areas to calculate accessibility scores.
#'
#' @param ratios Numeric vector, matrix, or data.frame of supply-to-demand ratios:
#'        - Vector: Must match number of weight layers
#'        - Matrix: Rows match weight layers, columns are different measures
#'        - Data.frame: Same as matrix but requires ratio_cols parameter
#' @param weights Multi-layer SpatRaster where each layer represents one site's
#'        probability-based catchment area
#' @param ratio_cols Character vector of column names if ratios is a data.frame
#' @param full_output Logical; whether to return intermediate calculations (default FALSE)
#' @param plot_prefix Character string to prepend to output names (default NULL)
#' @param parallel Logical; whether to use parallel processing (default FALSE)
#' @param n_cores Integer; number of cores for parallel processing (default NULL)
#' @return If full_output = TRUE, returns list containing:
#'         - access_scores: SpatRaster stack of accessibility scores per measure
#'         - site_specific: List of site-specific accessibility SpatRasters
#'         If full_output = FALSE, returns only access_scores SpatRaster
#' @export
spread_access <- function(ratios, weights, ratio_cols = NULL,
                          full_output = FALSE, plot_prefix = NULL,
                          parallel = FALSE, n_cores = NULL) {
  result <- weighted_spread(
    values = ratios,
    weights = weights,
    value_cols = ratio_cols,
    full_output = full_output,
    parallel = parallel,
    n_cores = n_cores
  )

  if (full_output) {
    names(result$total_distribution) <- paste0(plot_prefix, names(result$total_distribution))
    return(list(
      access_scores = result$total_distribution,
      site_specific = result$unit_distribution
    ))
  } else {
    names(result) <- paste0("accessibility_", names(result))
    return(result)
  }
}
