#' Validate inputs for compute_access
#' @param demand SpatRaster representing spatial distribution of demand
#' @param supply vector, matrix, or data.frame containing supply capacity values
#' @param demand_weights Multi-layer SpatRaster of demand-side weights
#' @param access_weights Multi-layer SpatRaster of accessibility-side weights
#' @param id_col Character; column name for facility IDs if supply is a data.frame
#' @param supply_cols Character vector; names of supply columns if supply is a data.frame
#' @param indicator_names Character vector; custom names for output accessibility layers
#' @keywords internal
.chck_compute_access <- function(demand, supply, demand_weights, access_weights,
                                 id_col = NULL, supply_cols = NULL,
                                 indicator_names = NULL) {
  # Input type validation
  if (!inherits(demand, "SpatRaster")) {
    stop("demand must be a SpatRaster object")
  }
  if (!inherits(demand_weights, "SpatRaster")) {
    stop("demand_weights must be a SpatRaster object")
  }
  if (!inherits(access_weights, "SpatRaster")) {
    stop("access_weights must be a SpatRaster object")
  }

  # Check for sf object
  if (inherits(supply, "sf")) {
    stop("Supply data is an sf object. Please use st_drop_geometry() first")
  }

  # Check raster compatibility
  if (!all(res(demand) == res(demand_weights)) ||
      !all(res(demand) == res(access_weights))) {
    stop("All raster inputs must have the same resolution")
  }
  if (!all(ext(demand) == ext(demand_weights)) ||
      !all(ext(demand) == ext(access_weights))) {
    stop("All raster inputs must have the same extent")
  }

  # Validate data.frame inputs
  if (is.data.frame(supply)) {
    if (is.null(id_col)) {
      stop("id_col must be specified when supply is a data.frame")
    }
    if (is.null(supply_cols)) {
      stop("supply_cols must be specified when supply is a data.frame")
    }
    if (!id_col %in% names(supply)) {
      stop(sprintf("id_col '%s' not found in supply data.frame", id_col))
    }
    if (!all(supply_cols %in% names(supply))) {
      missing_cols <- setdiff(supply_cols, names(supply))
      stop(sprintf("supply_cols not found in supply data.frame: %s",
                   paste(missing_cols, collapse = ", ")))
    }
  }

  # Validate weights layers match supply
  n_facilities <- if (is.data.frame(supply) || is.matrix(supply)) nrow(supply) else length(supply)
  if (nlyr(demand_weights) != n_facilities) {
    stop("Number of demand_weights layers must match number of facilities")
  }
  if (nlyr(access_weights) != n_facilities) {
    stop("Number of access_weights layers must match number of facilities")
  }

  # Validate indicator names if provided
  if (!is.null(indicator_names)) {
    n_indicators <- if (is.data.frame(supply)) {
      length(supply_cols)
    } else if (is.matrix(supply)) {
      ncol(supply)
    } else {
      1
    }
    if (length(indicator_names) != n_indicators) {
      stop("Length of indicator_names must match number of supply measures")
    }
  }

  invisible(TRUE)
}

#' Validate inputs for spax_e2sfca
#' @param demand SpatRaster representing spatial distribution of demand
#' @param supply vector, matrix, or data.frame containing supply capacity values
#' @param distance SpatRaster stack of travel times/distances to facilities
#' @param decay_params List of parameters for decay function
#' @param demand_normalize Character specifying normalization method
#' @param id_col Character; column name for facility IDs if supply is a data.frame
#' @param supply_cols Character vector; names of supply columns if supply is a data.frame
#' @keywords internal
.chck_e2sfca <- function(demand, supply, distance, decay_params,
                         demand_normalize, id_col = NULL, supply_cols = NULL) {
  # Input type validation
  if (!inherits(demand, "SpatRaster")) {
    stop("demand must be a SpatRaster object")
  }
  if (!inherits(distance, "SpatRaster")) {
    stop("distance must be a SpatRaster object")
  }

  # Check for sf object
  if (inherits(supply, "sf")) {
    stop("Supply data is an sf object. Please use st_drop_geometry() first")
  }

  # Validate decay_params
  if (!is.list(decay_params)) {
    stop("decay_params must be a list")
  }
  if (is.null(decay_params$method)) {
    stop("decay_params must include 'method'")
  }

  # Validate demand_normalize
  valid_normalize <- c("identity", "standard", "semi")
  if (!demand_normalize %in% valid_normalize) {
    stop(sprintf("demand_normalize must be one of: %s",
                 paste(valid_normalize, collapse = ", ")))
  }

  # Validate raster compatibility
  if (!all(res(demand) == res(distance))) {
    stop("demand and distance must have the same resolution")
  }
  if (!all(ext(demand) == ext(distance))) {
    stop("demand and distance must have the same extent")
  }

  # Validate facility counts match
  n_facilities <- if (is.data.frame(supply)) nrow(supply) else length(supply)
  if (nlyr(distance) != n_facilities) {
    stop("Number of distance layers must match number of facilities")
  }

  # Validate data.frame inputs
  if (is.data.frame(supply)) {
    if (is.null(id_col)) {
      stop("id_col must be specified when supply is a data.frame")
    }
    if (is.null(supply_cols)) {
      stop("supply_cols must be specified when supply is a data.frame")
    }
    if (!id_col %in% names(supply)) {
      stop(sprintf("id_col '%s' not found in supply data.frame", id_col))
    }
    if (!all(supply_cols %in% names(supply))) {
      missing_cols <- setdiff(supply_cols, names(supply))
      stop(sprintf("supply_cols not found in supply data.frame: %s",
                   paste(missing_cols, collapse = ", ")))
    }

    # Check if id_col values are unique
    if (any(duplicated(supply[[id_col]]))) {
      stop("Values in id_col must be unique")
    }
  }

  invisible(TRUE)
}

#' Process and validate supply data for accessibility calculations
#'
#' @param supply vector, matrix, or data.frame containing supply capacity values
#' @param id_col Character; column name for facility IDs if supply is a data.frame
#' @param supply_cols Character vector; names of supply columns if supply is a data.frame
#' @param weight_ids Character vector of IDs from weight layers for matching
#' @return List containing:
#'   \item{values}{Matrix or vector of supply values}
#'   \item{ids}{Vector of facility IDs}
#'   \item{cols}{Names of supply measures}
#' @keywords internal
.help_process_supply <- function(supply, id_col = NULL, supply_cols = NULL,
                                 weight_ids = NULL) {
  # Handle data.frame input
  if (is.data.frame(supply)) {
    if (is.null(weight_ids)) {
      # If no weight_ids provided, use order from data.frame
      values <- as.matrix(supply[, supply_cols, drop = FALSE])
      ids <- supply[[id_col]]
    } else {
      # Match and reorder to weight layers
      matched_idx <- match(weight_ids, supply[[id_col]])
      if (any(is.na(matched_idx))) {
        stop("Some weight layer IDs not found in supply data")
      }
      values <- as.matrix(supply[matched_idx, supply_cols, drop = FALSE])
      ids <- weight_ids
    }
    cols <- supply_cols
  }
  # Handle matrix input
  else if (is.matrix(supply)) {
    values <- supply
    ids <- if (!is.null(weight_ids)) {
      weight_ids
    } else {
      paste0("facility_", seq_len(nrow(supply)))
    }
    cols <- colnames(supply)
    if (is.null(cols)) {
      cols <- paste0("measure_", seq_len(ncol(supply)))
    }
  }
  # Handle vector input
  else if (is.vector(supply)) {
    values <- as.matrix(supply)
    ids <- if (!is.null(weight_ids)) {
      weight_ids
    } else {
      paste0("facility_", seq_along(supply))
    }
    cols <- "supply"
  }
  else {
    stop("Unsupported supply data type")
  }

  # Return processed results
  list(
    values = values,
    ids = ids,
    cols = cols
  )
}

#' Core computation for accessibility calculation
#'
#' @param demand SpatRaster of demand
#' @param supply_values Matrix of supply values
#' @param demand_weights SpatRaster of demand-side weights
#' @param access_weights SpatRaster of accessibility-side weights
#' @param indicator_names Character vector of names for output layers
#' @return SpatRaster of accessibility scores
#' @keywords internal
.compute_access_core <- function(demand, supply_values, demand_weights,
                                 access_weights, indicator_names = NULL) {
  # Calculate demand by site
  demand_by_site <- gather_demand(demand, demand_weights)

  # Calculate supply-to-demand ratios
  ratios <- sweep(supply_values, 1, demand_by_site$potential_demand, "/")

  # Calculate accessibility scores
  result <- spread_weighted(ratios, access_weights)

  # Apply names if provided
  if (!is.null(indicator_names)) {
    names(result) <- indicator_names
  } else {
    n_measures <- if (is.matrix(ratios)) ncol(ratios) else 1
    names(result) <- paste0("accessibility_", seq_len(n_measures))
  }

  return(result)
}


#' Calculate spatial accessibility using weighted surfaces
#'
#' @description
#' General-purpose function to calculate spatial accessibility by combining
#' demand-side and supply-side weighted distributions. Supports multiple
#' supply measures and flexible weight specifications.
#'
#' @param demand SpatRaster representing spatial distribution of demand
#' @param supply vector, matrix, or data.frame containing supply capacity values.
#'        If using an sf object, please use st_drop_geometry() first.
#' @param demand_weights Multi-layer SpatRaster of demand-side weights
#' @param access_weights Multi-layer SpatRaster of accessibility-side weights
#' @param id_col Character; column name for facility IDs if supply is a data.frame
#' @param supply_cols Character vector; names of supply columns if supply is a data.frame
#' @param indicator_names Character vector; custom names for output accessibility layers
#' @param snap Logical; if TRUE enable fast computation mode (default = FALSE)
#' @return SpatRaster of accessibility scores
#' @export
compute_access <- function(demand, supply, demand_weights, access_weights,
                           id_col = NULL, supply_cols = NULL,
                           indicator_names = NULL,
                           snap = FALSE) {
  # Validation (skip if snap = TRUE)
  if (!snap) {
    .chck_compute_access(
      demand, supply, demand_weights, access_weights,
      id_col, supply_cols, indicator_names
    )
  }

  # Process supply data
  weight_ids <- names(demand_weights)
  processed_supply <- .help_process_supply(
    supply = supply,
    id_col = id_col,
    supply_cols = supply_cols,
    weight_ids = weight_ids
  )

  # Use helper for core computation
  result <- .compute_access_core(
    demand = demand,
    supply_values = processed_supply$values,
    demand_weights = demand_weights,
    access_weights = access_weights,
    indicator_names = indicator_names %||% paste0("A_", processed_supply$cols)
  )

  return(result)
}

#' Calculate Enhanced Two-Step Floating Catchment Area (E2SFCA) accessibility scores
#'
#' @description
#' Implements the Enhanced Two-Step Floating Catchment Area (E2SFCA) method as proposed
#' by Luo & Qi (2009). This method improves upon the original 2SFCA by incorporating
#' distance decay effects and allowing for variable catchment sizes, providing more
#' realistic measures of spatial accessibility to services.
#'
#' @param demand SpatRaster representing spatial distribution of demand
#' @param supply vector, matrix, or data.frame containing supply capacity values
#' @param distance SpatRaster stack of travel times/distances to facilities
#' @param decay_params List of parameters for decay function:
#'        \itemize{
#'          \item method: "gaussian", "exponential", "power", or "binary"
#'          \item sigma: decay parameter controlling the rate of distance decay
#'          \item Additional parameters passed to custom decay functions
#'        }
#' @param demand_normalize Character specifying normalization method:
#'        \itemize{
#'          \item "identity": No normalization (original weights)
#'          \item "standard": Weights sum to 1 (prevents demand inflation)
#'          \item "semi": Normalize only when sum > 1 (prevents deflation)
#'        }
#' @param id_col Character; column name for facility IDs if supply is a data.frame
#' @param supply_cols Character vector; names of supply columns if supply is a data.frame
#' @param indicator_names Character vector; custom names for output accessibility layers
#' @param snap Logical; if TRUE enable fast computation mode (default = FALSE)
#' @return SpatRaster of accessibility scores
#'
#' @details
#' The E2SFCA method enhances the original 2SFCA by introducing:
#'
#' Step 1: For each facility j:
#' * Weight demand points by distance decay: Wd(dij)
#' * Calculate supply-to-demand ratio Rj = Sj/sum(Pi * Wd(dij))
#'
#' Step 2: For each demand location i:
#' * Weight facility ratios by distance decay: Wr(dij)
#' * Calculate accessibility score Ai = sum(Rj * Wr(dij))
#'
#' Key improvements over original 2SFCA:
#' 1. Distance decay within catchments
#' 2. Differentiated travel behavior in demand vs. access phases
#' 3. Smoother accessibility surfaces
#' 4. More realistic representation of access barriers
#'
#' The method supports various distance decay functions and normalization approaches
#' to handle different accessibility scenarios and prevent demand overestimation
#' in overlapping service areas.
#'
#' @references
#' Luo, W., & Qi, Y. (2009). An enhanced two-step floating catchment area (E2SFCA)
#' method for measuring spatial accessibility to primary care physicians.
#' *Health & Place*, *15*(4), 1100-1107.
#' https://doi.org/10.1016/j.healthplace.2009.06.002
#'
#' @examples
#' # Load example data
#' library(terra)
#'
#' # Convert under-5 population density to proper format
#' pop_rast <- rast(u5pd)
#'
#' # Calculate accessibility with Gaussian decay
#' result <- spax_e2sfca(
#'   demand = pop_rast,
#'   supply = hc12_hos,
#'   distance = rast(hos_iscr),
#'   decay_params = list(
#'     method = "gaussian",
#'     sigma = 30  # 30-minute characteristic distance
#'   ),
#'   demand_normalize = "semi",  # Prevent demand inflation
#'   id_col = "id",
#'   supply_cols = "s_doc"
#' )
#'
#' # Plot the results
#' plot(result, main = "Doctor Accessibility (E2SFCA)")
#'
#' # Compare different decay functions
#' result_exp <- spax_e2sfca(
#'   demand = pop_rast,
#'   supply = hc12_hos,
#'   distance = rast(hos_iscr),
#'   decay_params = list(
#'     method = "exponential",
#'     sigma = 0.1
#'   ),
#'   demand_normalize = "semi",
#'   id_col = "id",
#'   supply_cols = "s_doc"
#' )
#'
#' # Plot both for comparison
#' plot(c(result, result_exp),
#'      main = c("Gaussian Decay", "Exponential Decay"))
#'
#' @seealso
#' * [spax_2sfca()] for the original method without distance decay
#' * [compute_access()] for more flexible accessibility calculations
#' * [calc_decay()] for available decay functions
#'
#' @export
spax_e2sfca <- function(demand, supply, distance,
                        decay_params = list(method = "gaussian", sigma = 30),
                        demand_normalize = "identity",
                        id_col = NULL, supply_cols = NULL,
                        indicator_names = NULL,
                        snap = FALSE) {
  # Validation (skip if snap = TRUE)
  if (!snap) {
    .chck_e2sfca(
      demand, supply, distance, decay_params,
      demand_normalize, id_col, supply_cols
    )
  }

  # Compute weights using decay function
  weights <- do.call(calc_decay, c(list(distance = distance), decay_params))

  # Process demand weights based on normalization method
  demand_weights <- calc_normalize(weights, method = demand_normalize, snap = snap)

  # Access weights remain unnormalized
  access_weights <- weights

  # Use compute_access for final calculation
  result <- compute_access(
    demand = demand,
    supply = supply,
    demand_weights = demand_weights,
    access_weights = access_weights,
    id_col = id_col,
    supply_cols = supply_cols,
    indicator_names = indicator_names,
    snap = snap
  )

  return(result)
}

#' Calculate Original Two-Step Floating Catchment Area (2SFCA) accessibility scores
#'
#' @description
#' Implements the original Two-Step Floating Catchment Area (2SFCA) method using
#' binary catchment areas, as proposed by Luo & Wang (2003). This foundational method
#' uses a single distance/time threshold to define service areas and computes
#' accessibility as a ratio of supply to demand within these catchments.
#'
#' @param demand SpatRaster representing spatial distribution of demand
#' @param supply vector, matrix, or data.frame containing supply capacity values
#' @param distance SpatRaster stack of travel times/distances to facilities
#' @param threshold Numeric value defining the catchment area cutoff (same units as distance)
#' @param id_col Character; column name for facility IDs if supply is a data.frame
#' @param supply_cols Character vector; names of supply columns if supply is a data.frame
#' @param snap Logical; if TRUE enable fast computation mode (default = FALSE)
#' @return SpatRaster of accessibility scores
#'
#' @details
#' The original Two-Step Floating Catchment Area (2SFCA) method operates in two steps:
#'
#' Step 1: For each facility j:
#' * Define a catchment area within threshold distance/time
#' * Sum the population of all demand locations i within the catchment
#' * Calculate supply-to-demand ratio Rj = Sj/sum(Pi)
#'
#' Step 2: For each demand location i:
#' * Define a catchment area within threshold distance/time
#' * Sum all facility ratios Rj within the catchment
#' * Final accessibility score Ai = sum(Rj)
#'
#' Key characteristics:
#' 1. Binary catchment areas (within threshold = 1, beyond = 0)
#' 2. Equal weights for all locations within catchment
#' 3. No normalization of demand weights
#' 4. Single threshold value for both steps
#'
#' Limitations addressed by later methods:
#' * No distance decay within catchments
#' * Artificial barriers at catchment boundaries
#' * Potential demand overestimation in overlapping areas
#'
#' @references
#' Luo, W., & Wang, F. (2003). Measures of Spatial Accessibility to Health Care in
#' a GIS Environment: Synthesis and a Case Study in the Chicago Region.
#' *Environment and Planning B: Planning and Design*, *30*(6), 865-884.
#' https://doi.org/10.1068/b29120
#'
#' @examples
#' # Load example data
#' library(terra)
#'
#' # Convert under-5 population density to proper format
#' pop_rast <- rast(u5pd)
#'
#' # Calculate accessibility to doctors with 30-minute catchment
#' result <- spax_2sfca(
#'   demand = pop_rast,
#'   supply = hc12_hos,
#'   distance = rast(hos_iscr),
#'   threshold = 30,  # 30-minute catchment
#'   id_col = "id",
#'   supply_cols = "s_doc"
#' )
#'
#' # Plot the results
#' plot(result, main = "Doctor Accessibility (Original 2SFCA)")
#'
#' # Calculate accessibility to multiple supply types
#' result_multi <- spax_2sfca(
#'   demand = pop_rast,
#'   supply = hc12_hos,
#'   distance = rast(hos_iscr),
#'   threshold = 30,
#'   id_col = "id",
#'   supply_cols = c("s_doc", "s_nurse")
#' )
#'
#' # Plot both doctor and nurse accessibility
#' plot(result_multi)
#'
#' @seealso
#' * [spax_e2sfca()] for the enhanced version with distance decay
#' * [compute_access()] for more flexible accessibility calculations
#'
#' @export
spax_2sfca <- function(demand, supply, distance, threshold,
                       id_col = NULL, supply_cols = NULL,
                       snap = FALSE) {
  # Create decay parameters for binary catchment
  decay_params <- list(
    method = "binary",
    sigma = threshold
  )

  # Use spax_e2sfca with binary catchment and no demand normalization
  result <- spax_e2sfca(
    demand = demand,
    supply = supply,
    distance = distance,
    decay_params = decay_params,
    demand_normalize = "identity", # No normalization for original 2SFCA
    id_col = id_col,
    supply_cols = supply_cols,
    snap = snap
  )

  return(result)
}
