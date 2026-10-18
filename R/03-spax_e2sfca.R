#' Core facility supply validation used by multiple functions
#' @keywords internal
.validate_facility_supply <- function(supply, id_col = NULL, supply_cols = NULL) {
  # Stop early if supply is an sf object
  if (inherits(supply, "sf")) {
    stop("Supply data is an sf object. Please use st_drop_geometry() first")
  }

  # Validate data.frame inputs
  if (is.data.frame(supply)) {
    if (is.null(id_col)) {
      stop("id_col must be specified when supply is a data.frame")
    }
    if (is.null(supply_cols)) {
      stop("supply_cols must be specified when supply is a data.frame")
    }

    .chck_cols_exist(supply, c(id_col, supply_cols), "supply")

    # Check if id_col values are unique (specific to e2sfca)
    if (!is.null(id_col) && any(duplicated(supply[[id_col]]))) {
      stop("Values in id_col must be unique")
    }
  }

  # Return number of facilities for convenience
  if (is.data.frame(supply) || is.matrix(supply)) {
    return(nrow(supply))
  } else {
    return(length(supply))
  }
}

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
  .chck_class(demand, "SpatRaster", "demand")
  .chck_class(demand_weights, "SpatRaster", "demand_weights")
  .chck_class(access_weights, "SpatRaster", "access_weights")

  # Validate supply and get facility count
  n_facilities <- .validate_facility_supply(supply, id_col, supply_cols)

  # Validate raster alignments
  .chck_raster_alignment(demand, demand_weights, "demand", "demand_weights")
  .chck_raster_alignment(demand, access_weights, "demand", "access_weights")

  # Validate weights match facility count
  .chck_lengths_match(
    nlyr(demand_weights), n_facilities,
    "demand_weights layers", "facilities"
  )
  .chck_lengths_match(
    nlyr(access_weights), n_facilities,
    "access_weights layers", "facilities"
  )

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
  # Check input types
  .chck_class(demand, "SpatRaster", "demand")
  .chck_class(distance, "SpatRaster", "distance")

  # Validate supply and get facility count
  n_facilities <- .validate_facility_supply(supply, id_col, supply_cols)

  # Validate decay_params
  .chck_class(decay_params, "list", "decay_params")
  if (is.null(decay_params$method)) {
    stop("decay_params must include 'method'")
  }

  # Validate demand_normalize
  valid_normalize <- c("identity", "standard", "semi")
  if (!demand_normalize %in% valid_normalize) {
    stop(sprintf(
      "demand_normalize must be one of: %s",
      paste(valid_normalize, collapse = ", ")
    ))
  }

  # Validate raster alignment
  .chck_raster_alignment(demand, distance, "demand", "distance")

  # Validate facility counts match
  .chck_lengths_match(
    nlyr(distance), n_facilities,
    "distance layers", "facilities"
  )

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
    if (!is.null(weight_ids) && !is.null(names(supply))) {
      # Named vector: match by name so supply order need not match the layers
      matched_idx <- match(weight_ids, names(supply))
      if (any(is.na(matched_idx))) {
        stop("Some weight layer IDs not found in supply data")
      }
      supply <- supply[matched_idx]
      ids <- weight_ids
    } else if (!is.null(weight_ids)) {
      ids <- weight_ids
    } else {
      ids <- paste0("facility_", seq_along(supply))
    }
    values <- as.matrix(unname(supply))
    cols <- "supply"
  } else {
    stop("Unsupported supply data type")
  }

  # Return processed results
  list(
    values = values,
    ids = ids,
    cols = cols
  )
}

# The facility table of an FCA result: the facility IDs and their supply
# measures, in the order of `supply`. A data frame gives its `id_col` and
# `supply_cols`; a matrix gives its row names (or facility_1, ...); a vector
# gives its names.
.fca_facility_table <- function(supply, id_col, supply_cols) {
  if (is.data.frame(supply)) {
    data.frame(
      id = supply[[id_col]],
      supply[supply_cols]
    )
  } else if (is.matrix(supply)) {
    data.frame(
      id = rownames(supply) %||% paste0("facility_", seq_len(nrow(supply))),
      as.data.frame(supply)
    )
  } else {
    data.frame(
      id = names(supply) %||% paste0("facility_", seq_along(supply)),
      supply = supply
    )
  }
}

# Decay weights for one decay_params list, computed as in spax_e2sfca().
.fca_decay_weights <- function(distance, params, snap) {
  do.call(calc_decay, c(list(distance = distance), params, list(snap = snap)))
}

# The decay weights of one decay_params list for the block engine.
# Returns `rasters`, the named SpatRasters the weights are read from, and
# `weights(b, rows)`, which gives the weights of the block rows `rows` as a
# matrix with one column per facility. A plain decay function is evaluated on
# the whole distance raster as in .fca_decay_weights(), and its weight raster is
# read in blocks. A built-in method or declared kernel is evaluated on block
# matrices with calc_decay(snap = TRUE); a SpatRaster in `params` (sigma, or an
# extra argument) is read in blocks and given to it as a vector (one layer)
# or matrix (one layer per facility). `key` names the rasters. When `snap` is
# FALSE, .chck_decay() runs here, before anything is opened.
.fca_decay_spec <- function(distance, params, key, snap) {
  method <- params$method
  if (is.function(method)) {
    nm <- paste0("weights_", key)
    weights <- .fca_decay_weights(distance, params, snap)
    # The weight layers belong to facilities by their names, as before; layers
    # with terra's default names (or none) are taken in distance-layer order.
    ids <- .fca_ids(distance)
    weight_ids <- .fca_ids(weights, "decay weights")
    if (terra::nlyr(weights) != length(ids)) {
      stop("a decay function must return one layer per distance layer")
    }
    positional <- identical(weight_ids, paste0("facility_", seq_along(weight_ids)))
    if (!positional && !setequal(weight_ids, ids)) {
      stop("the layer names returned by the decay function must be the distance layer names")
    }
    order <- if (positional) seq_along(ids) else match(ids, weight_ids)
    # The distance is read as well, for the finite-travel mask of decision D1.
    rasters <- c(list(distance = distance), stats::setNames(list(weights), nm))
    return(list(
      rasters = rasters,
      weights = function(b, rows) {
        W <- b[[nm]][rows, order, drop = FALSE]
        .check_finite_weights(b$distance[rows, , drop = FALSE], W)
        W
      }
    ))
  }

  builtin <- TRUE
  if (inherits(method, "spax_decay_kernel")) {
    .validate_decay_kernel(method)
    if (isTRUE(method$builtin)) {
      method <- method$name
    } else {
      builtin <- FALSE
    }
  }
  if (!builtin && inherits(params$sigma, "SpatRaster")) {
    stop("a variable catchment needs a built-in method or a plain function")
  }
  if (builtin && !snap) {
    .chck_decay(distance, method, params$sigma, snap)
  }
  if (builtin && !is.null(params$sigma) && !inherits(params$sigma, "SpatRaster") &&
      length(params$sigma) > 1L) {
    stop("sigma must be a single number or a SpatRaster")
  }

  others <- params[setdiff(names(params), "method")]
  is_raster <- vapply(others, inherits, logical(1), what = "SpatRaster")
  raster_args <- names(others)[is_raster]
  keys <- character()
  if (length(raster_args)) {
    keys <- stats::setNames(paste0(raster_args, "_", key), raster_args)
  }
  rasters <- c(list(distance = distance),
               stats::setNames(others[is_raster], unname(keys)))
  fixed <- others[!is_raster]

  list(
    rasters = rasters,
    weights = function(b, rows) {
      # Pass 2 asks for every row of the block: use the block as read, without a copy.
      all_rows <- length(rows) == nrow(b$distance)
      d <- if (all_rows) b$distance else b$distance[rows, , drop = FALSE]
      args <- fixed
      for (arg in names(keys)) {
        x <- b[[keys[[arg]]]]
        if (!all_rows) x <- x[rows, , drop = FALSE]
        if (ncol(x) > 1L && ncol(x) != ncol(b$distance)) {
          stop("`", arg, "` raster must have 1 layer or one layer per `distance` layer")
        }
        args[arg] <- list(if (ncol(x) == 1L) x[, 1L] else x)
      }
      if (builtin) {
        W <- do.call(calc_decay, c(list(distance = d, method = method, snap = TRUE), args))
        .check_finite_weights(d, W)
        W
      } else {
        # A declared kernel checks its weights as it evaluates them (D1).
        do.call(.calc_decay_declared, c(
          list(distance = d, kernel = method, snap = TRUE), args, list(.d1 = TRUE)
        ))
      }
    }
  )
}

# Selection weights of a block of weights `Tm` (one row per cell, one column per
# facility) as in calc_choice(): the weights of each cell, after multiplying by
# `attract^alpha` per facility, divided by their sum; 0 where the sum is not
# positive, missing weights stay missing. Negative weights are an error unless
# `snap`.
.fca_choice_block <- function(Tm, attract = NULL, alpha = 1, snap = FALSE) {
  if (!snap) {
    .chck_nonnegative_weights(Tm, "weights")
  }
  if (!is.null(attract)) {
    Tm <- Tm * rep(attract^alpha, each = nrow(Tm))
  }
  total <- rowSums(Tm, na.rm = TRUE)
  out <- Tm / total
  none <- which(!(total > 0))
  if (length(none)) {
    out[none, ] <- 0 * Tm[none, , drop = FALSE]
  }
  out
}

# The block-engine counterpart of the decay-based wrappers. `type` is "e2sfca",
# "m2sfca", "3sfca" or "huff"; it builds the kernel specification of the
# wrapper and runs the engine. `attractiveness` is the vector from
# .fca_attractiveness() for "huff".
.fca_block_run <- function(type, demand, supply, distance, decay_params,
                           selection_params = NULL, attractiveness = NULL,
                           alpha = 1, demand_normalize = "identity",
                           id_col = NULL, supply_cols = NULL,
                           indicator_names = NULL, snap = FALSE) {
  .chck_class(distance, "SpatRaster", "distance")
  decay <- .fca_decay_spec(distance, decay_params, "decay", snap)
  selection <- NULL
  if (type %in% c("3sfca", "huff") && !identical(selection_params, decay_params)) {
    selection <- .fca_decay_spec(distance, selection_params, "selection", snap)
  }
  # The first raster names the facilities and gives the grid of the result.
  rasters <- c(decay$rasters, selection$rasters)
  rasters <- rasters[!duplicated(names(rasters))]

  attract <- if (is.null(attractiveness)) NULL else unname(attractiveness)
  choice_kernel <- function(b, rows) {
    W <- decay$weights(b, rows)
    Tm <- if (is.null(selection)) W else selection$weights(b, rows)
    W * .fca_choice_block(Tm, attract, alpha, snap)
  }
  kernels <- switch(type,
    e2sfca = list(demand = decay$weights, access = decay$weights),
    m2sfca = list(
      demand = decay$weights,
      access = function(b, rows) {
        W <- decay$weights(b, rows)
        W * W
      }
    ),
    list(demand = choice_kernel, access = choice_kernel)
  )

  compute_fca(
    demand = demand, supply = supply, rasters = rasters, kernels = kernels,
    demand_normalize = demand_normalize, id_col = id_col,
    supply_cols = supply_cols, indicator_names = indicator_names, snap = snap
  )
}

# The block-engine counterpart of compute_access(). The access weights are
# reordered to the facility IDs of the demand weights.
.fca_access_block <- function(demand, supply, demand_weights, access_weights,
                              id_col = NULL, supply_cols = NULL,
                              indicator_names = NULL, snap = FALSE) {
  demand_ids <- .fca_ids(demand_weights, "demand_weights")
  access_ids <- .fca_ids(access_weights, "access_weights")
  if (!setequal(demand_ids, access_ids)) {
    stop("demand and access kernels must span the same facility ids")
  }
  pos <- match(demand_ids, access_ids)
  compute_fca(
    demand = demand, supply = supply,
    rasters = list(demand_weights = demand_weights,
                   access_weights = access_weights),
    kernels = list(
      demand = function(b, rows) {
        .check_user_weights(b$demand_weights[rows, , drop = FALSE])
      },
      access = function(b, rows) {
        .check_user_weights(b$access_weights[rows, pos, drop = FALSE])
      }
    ),
    demand_normalize = "identity", id_col = id_col, supply_cols = supply_cols,
    indicator_names = indicator_names, snap = snap
  )
}

# User weights of compute_access() may be missing but not infinite (SPAX-055,
# decision D1); this also holds under `snap`.
.check_user_weights <- function(W) {
  if (any(is.infinite(W))) {
    stop("weights must be finite or NA", call. = FALSE)
  }
  W
}

# selection_params has the form of decay_params: a list with a decay method.
.chck_selection_params <- function(selection_params) {
  .chck_class(selection_params, "list", "selection_params")
  if (is.null(selection_params$method)) {
    stop("selection_params must include 'method'")
  }
  invisible(TRUE)
}

# The attractiveness of Huff-2SFCA, one value per facility in the order of the
# distance layers, named by facility ID. NULL takes the supply measure, which
# must then be a single one. A supplied named vector is matched to the layer
# names and an unnamed one follows the layer order. Matching always applies;
# `snap` skips only the checks of type, length and value.
.fca_attractiveness <- function(attractiveness, supply, distance,
                                id_col, supply_cols, snap) {
  ids <- names(distance)
  if (is.null(attractiveness)) {
    values <- .help_process_supply(
      supply, id_col, supply_cols, weight_ids = ids
    )$values
    if (ncol(values) != 1L) {
      stop("give attractiveness when supply has several measures")
    }
    values <- values[, 1]
    label <- "the supply measure used as attractiveness"
  } else {
    if (!snap) {
      .chck_numeric_vector(attractiveness, "attractiveness", finite = FALSE)
      .chck_lengths_match(length(attractiveness), nlyr(distance),
                          "attractiveness", "distance layers")
    }
    values <- attractiveness
    if (!is.null(names(values))) {
      if (anyDuplicated(names(values)) || !setequal(names(values), ids)) {
        stop("names of attractiveness must be the distance layer names, ",
             "one for each facility")
      }
      values <- values[match(ids, names(values))]
    }
    label <- "attractiveness"
  }
  if (!snap && (!all(is.finite(values)) || any(values < 0))) {
    stop(label, " must contain only finite, nonnegative values")
  }
  setNames(as.numeric(values), ids)
}

#' Calculate spatial accessibility using weighted surfaces
#'
#' @description
#' General-purpose function to calculate spatial accessibility by combining
#' demand-side and supply-side weighted distributions. Supports multiple
#' supply measures and flexible weight specifications.
#'
#' @param demand SpatRaster representing spatial distribution of demand. Demand
#'        must be finite and nonnegative (use NA for unknown) unless `snap = TRUE`,
#'        which ignores other cells.
#' @param supply vector, matrix, or data.frame containing supply capacity values.
#'        If using an sf object, please use st_drop_geometry() first.
#' @param demand_weights Multi-layer SpatRaster of demand-side weights; values
#'        may be NA but not infinite
#' @param access_weights Multi-layer SpatRaster of accessibility-side weights;
#'        values may be NA but not infinite
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

  .fca_access_block(
    demand, supply, demand_weights, access_weights,
    id_col, supply_cols, indicator_names, snap
  )
}

#' Calculate Enhanced Two-Step Floating Catchment Area (E2SFCA) accessibility scores
#'
#' @description
#' Implements the Enhanced Two-Step Floating Catchment Area (E2SFCA) method as proposed
#' by Luo & Qi (2009). This method improves upon the original 2SFCA by incorporating
#' distance decay effects and allowing for variable catchment sizes, providing more
#' realistic measures of spatial accessibility to services.
#'
#' @param demand SpatRaster representing spatial distribution of demand. Demand
#'        must be finite and nonnegative (use NA for unknown) unless `snap = TRUE`,
#'        which ignores other cells.
#' @param supply vector, matrix, or data.frame containing supply capacity values
#' @param distance SpatRaster stack of travel times/distances to facilities
#' @param decay_params List of parameters for decay function:
#'        \itemize{
#'          \item method: "gaussian", "exponential", "power" or "binary", a
#'            custom decay function, or a kernel declared with
#'            \code{\link{decay_kernel}}. Weights must be finite where the
#'            distance is finite, so power decay needs a positive minimum
#'            distance
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
#' @return A spax object containing:
#'        \itemize{
#'          \item accessibility: SpatRaster of accessibility scores
#'          \item type: Character string "E2SFCA"
#'          \item parameters: List of model parameters used
#'          \item facilities: data.frame of facility-level information
#'          \item call: The original function call
#'        }
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
#' library(sf)
#'
#' # Cornwall population grid and travel time to each GP practice
#' pop_rast <- read_spax_example("cornwall_pop.tif")
#' travel <- read_spax_example("cornwall_gp_travel.tif")
#'
#' # Drop geometry for supply data
#' gp <- cornwall_gp |> st_drop_geometry()
#'
#' # Calculate accessibility with Gaussian decay
#' result <- spax_e2sfca(
#'   demand = pop_rast,
#'   supply = gp,
#'   distance = travel,
#'   decay_params = list(
#'     method = "gaussian",
#'     sigma = 10 # 10-minute characteristic travel time
#'   ),
#'   demand_normalize = "semi", # Prevent demand inflation
#'   id_col = "practice",
#'   supply_cols = "gp_fte_extg"
#' )
#'
#' # Extract and plot accessibility surface
#' plot(result$accessibility, main = "GP Accessibility (E2SFCA)")
#'
#' # Access facility-level information
#' head(result$facilities)
#'
#' # Compare different decay functions
#' result_exp <- spax_e2sfca(
#'   demand = pop_rast,
#'   supply = gp,
#'   distance = travel,
#'   decay_params = list(
#'     method = "exponential",
#'     sigma = 0.1
#'   ),
#'   demand_normalize = "semi",
#'   id_col = "practice",
#'   supply_cols = "gp_fte_extg"
#' )
#'
#' # Plot both for comparison
#' plot(c(result$accessibility, result_exp$accessibility),
#'   main = c("Gaussian Decay", "Exponential Decay")
#' )
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

  result <- .fca_block_run(
    "e2sfca", demand, supply, distance, decay_params,
    demand_normalize = demand_normalize, id_col = id_col,
    supply_cols = supply_cols, indicator_names = indicator_names, snap = snap
  )

  # Process facility information
  facilities <- .fca_facility_table(supply, id_col, supply_cols)

  # Create spax object
  .create_spax(
    accessibility = result,
    type = "E2SFCA",
    parameters = list(
      decay_params = decay_params,
      demand_normalize = demand_normalize
    ),
    facilities = facilities,
    call = match.call(),
    snap = snap
  )
}

#' Calculate Modified Two-Step Floating Catchment Area (M2SFCA) accessibility scores
#'
#' @description
#' Implements the Modified Two-Step Floating Catchment Area (M2SFCA) method
#' (Delamater, 2013). M2SFCA is identical to [spax_e2sfca()] in step 1, but in
#' step 2 the supply-to-demand ratios are weighted by the **squared** distance
#' decay. This discounts accessibility in sub-optimally configured systems --
#' where demand and supply are both far apart yet still within the catchment --
#' which standard 2SFCA/E2SFCA cannot distinguish.
#'
#' @inheritParams spax_e2sfca
#' @return A spax object (see [spax_e2sfca()]); `type` is "M2SFCA".
#'
#' @details
#' Step 1 (unchanged from E2SFCA): \eqn{R_j = S_j / \sum_i P_i W(d_{ij})}.
#'
#' Step 2 (modified): \eqn{A_i = \sum_j R_j \, W(d_{ij})^2}.
#'
#' M2SFCA is not a separate engine here: it is the shared FCA recipe with the
#' access-side kernel squared. It is exactly equivalent to
#' `compute_access(demand, supply, W, W^2)`.
#'
#' @references
#' Delamater, P. L. (2013). Spatial accessibility in suboptimally configured
#' health care systems: A modified two-step floating catchment area (M2SFCA)
#' metric. *Health & Place*, *24*, 30-43.
#' https://doi.org/10.1016/j.healthplace.2013.07.012
#'
#' @seealso [spax_e2sfca()], [spax_2sfca()], [compute_access()], [calc_decay()]
#' @export
spax_m2sfca <- function(demand, supply, distance,
                        decay_params = list(method = "gaussian", sigma = 30),
                        demand_normalize = "identity",
                        id_col = NULL, supply_cols = NULL,
                        indicator_names = NULL,
                        snap = FALSE) {
  # Validation (M2SFCA takes the same inputs as E2SFCA)
  if (!snap) {
    .chck_e2sfca(
      demand, supply, distance, decay_params,
      demand_normalize, id_col, supply_cols
    )
  }

  result <- .fca_block_run(
    "m2sfca", demand, supply, distance, decay_params,
    demand_normalize = demand_normalize, id_col = id_col,
    supply_cols = supply_cols, indicator_names = indicator_names, snap = snap
  )

  # Process facility information
  facilities <- .fca_facility_table(supply, id_col, supply_cols)

  # Create spax object
  .create_spax(
    accessibility = result,
    type = "M2SFCA",
    parameters = list(
      decay_params = decay_params,
      demand_normalize = demand_normalize
    ),
    facilities = facilities,
    call = match.call(),
    snap = snap
  )
}

#' Calculate Original Two-Step Floating Catchment Area (2SFCA) accessibility scores
#'
#' @description
#' Implements the original Two-Step Floating Catchment Area (2SFCA) method using
#' binary catchment areas, as proposed by Luo & Wang (2003). This foundational method
#' uses a single distance/time threshold to define service areas and computes
#' accessibility as a ratio of supply to demand within these catchments.
#'
#' @param demand SpatRaster representing spatial distribution of demand. Demand
#'        must be finite and nonnegative (use NA for unknown) unless `snap = TRUE`,
#'        which ignores other cells.
#' @param supply vector, matrix, or data.frame containing supply capacity values
#' @param distance SpatRaster stack of travel times/distances to facilities
#' @param threshold Numeric value defining the catchment area cutoff (same units as distance)
#' @param id_col Character; column name for facility IDs if supply is a data.frame
#' @param supply_cols Character vector; names of supply columns if supply is a data.frame
#' @param snap Logical; if TRUE enable fast computation mode (default = FALSE)
#' @return A spax object containing:
#'        \itemize{
#'          \item accessibility: SpatRaster of accessibility scores
#'          \item type: Character string "2SFCA"
#'          \item parameters: List containing threshold and binary decay parameters
#'          \item facilities: data.frame of facility-level information
#'          \item call: The original function call
#'        }
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
#' library(sf)
#'
#' # Load data
#' pop_rast <- read_spax_example("cornwall_pop.tif")
#' travel <- read_spax_example("cornwall_gp_travel.tif")
#'
#' # Drop geometry for supply data
#' gp <- cornwall_gp |> st_drop_geometry()
#'
#' # Calculate accessibility to GPs with a 15-minute catchment
#' result <- spax_2sfca(
#'   demand = pop_rast,
#'   supply = gp,
#'   distance = travel,
#'   threshold = 15, # 15-minute catchment
#'   id_col = "practice",
#'   supply_cols = "gp_fte_extg"
#' )
#'
#' # Plot the results
#' plot(result$accessibility, main = "GP Accessibility (Original 2SFCA)")
#'
#' # Calculate accessibility for two supply measures
#' result_multi <- spax_2sfca(
#'   demand = pop_rast,
#'   supply = gp,
#'   distance = travel,
#'   threshold = 15,
#'   id_col = "practice",
#'   supply_cols = c("gp_fte_extg", "gp_fte")
#' )
#'
#' # Compare accessibility for different supply measures
#' plot(result_multi$accessibility,
#'   main = c("GPs excluding trainees", "All GPs")
#' )
#'
#' @seealso
#' * [spax_e2sfca()] for the enhanced version with distance decay
#' * [compute_access()] for more flexible accessibility calculations
#'
#' @export
spax_2sfca <- function(demand, supply, distance, threshold,
                       id_col = NULL, supply_cols = NULL,
                       snap = FALSE) {
  if (!snap && (!is.numeric(threshold) || length(threshold) != 1 ||
                !is.finite(threshold) || threshold <= 0)) {
    stop("threshold must be a single positive number")
  }
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

  # Update type while preserving other components
  result$type <- "2SFCA"
  return(result)
}

#' Calculate Three-Step Floating Catchment Area (3SFCA) accessibility scores
#'
#' @description
#' This experimental function implements the three-step floating catchment area
#' (3SFCA) method of Wan, Zou and Sternberg (2012). A selection step precedes
#' the two steps of \code{\link{spax_e2sfca}}: it divides the demand at each
#' origin among the facilities with selection weights that sum to 1 at the
#' origin. A facility then gathers only its share of the demand at each origin,
#' and its supply-to-demand ratio is spread back to the origins with the same
#' shares.
#'
#' @inheritParams spax_e2sfca
#' @param decay_params List of parameters for the decay weights used in both
#'        steps:
#'        \itemize{
#'          \item method: "gaussian", "exponential", "power" or "binary", a
#'            custom decay function, or a kernel declared with
#'            \code{\link{decay_kernel}}. Weights must be finite where the
#'            distance is finite, so power decay needs a positive minimum
#'            distance
#'          \item sigma: decay parameter controlling the rate of distance decay
#'          \item Additional parameters passed to custom decay functions
#'        }
#' @param selection_params List of parameters, in the form of
#'        \code{decay_params}, for the decay weights that the selection weights
#'        are built from. The default is \code{decay_params}, so one decay both
#'        weights distance and selects facilities; give a different list to
#'        select with another decay. Its weights must also be finite where the
#'        distance is finite.
#' @return A spax object (see \code{\link{spax_e2sfca}}) whose \code{type} is
#'         "3SFCA". Its \code{parameters} are the named list
#'         \code{decay_params}, \code{selection_params}.
#'
#' @details
#' Write \eqn{D_i} for the demand at origin \eqn{i}, \eqn{S_j} for the supply of
#' facility \eqn{j}, \eqn{W_{ij}}{W_ij} for the decay weights from
#' \code{decay_params} and \eqn{T_{ij}}{T_ij} for the decay weights from
#' \code{selection_params}. The two are the same weights when
#' \code{selection_params} is identical to \code{decay_params}.
#' \itemize{
#'   \item Selection weights:
#'     \eqn{G_{ij} = T_{ij} / \sum_k T_{ik}}{G_ij = T_ij / sum_k T_ik}.
#'   \item Ratio of facility \eqn{j}:
#'     \eqn{R_j = S_j / \sum_i D_i W_{ij} G_{ij}}{R_j = S_j / sum_i D_i W_ij
#'     G_ij}.
#'   \item Accessibility of origin \eqn{i}:
#'     \eqn{A_i = \sum_j R_j W_{ij} G_{ij}}{A_i = sum_j R_j W_ij G_ij}.
#' }
#'
#' Both steps use the kernel \eqn{K_{ij} = W_{ij} G_{ij}}{K_ij = W_ij G_ij}.
#' The result equals \code{compute_access(demand, supply, demand_weights = K,
#' access_weights = K)} with \code{K = W * calc_choice(T)}. There is no
#' \code{demand_normalize} argument: the selection weights already divide the
#' demand of each origin among the facilities.
#'
#' The selection weights are normalized over every facility with a positive
#' weight at the origin, however far away it is. To restrict the selection to a
#' catchment, set the travel times beyond the limit to \code{NA} or \code{Inf}
#' before calling the function, or use a decay method that is zero beyond it,
#' for example \code{method = "binary"} in \code{selection_params}.
#'
#' Because both steps use the same kernel, the scores conserve supply: the sum
#' of \eqn{D_i A_i} over the origins with demand and a score equals the total
#' supply of the facilities that gather demand
#' (\eqn{\sum_i D_i K_{ij} > 0}{sum_i D_i K_ij > 0}). A facility that gathers no
#' demand has a ratio of zero. Several supply measures give independent
#' accessibility layers.
#'
#' @references
#' Wan, N., Zou, B., & Sternberg, T. (2012). A three-step floating catchment
#' area method for analyzing spatial access to health services.
#' \emph{International Journal of Geographical Information Science},
#' \emph{26}(6), 1073-1089. \doi{10.1080/13658816.2011.624987}
#'
#' @examples
#' # Load example data
#' library(terra)
#' library(sf)
#'
#' # Cornwall population grid and travel time to each GP practice
#' pop_rast <- read_spax_example("cornwall_pop.tif")
#' travel <- read_spax_example("cornwall_gp_travel.tif")
#'
#' # Drop geometry for supply data
#' gp <- cornwall_gp |> st_drop_geometry()
#'
#' # 3SFCA with Gaussian decay; the same decay gives the selection weights
#' result <- spax_3sfca(
#'   demand = pop_rast,
#'   supply = gp,
#'   distance = travel,
#'   decay_params = list(
#'     method = "gaussian",
#'     sigma = 10 # 10-minute characteristic travel time
#'   ),
#'   id_col = "practice",
#'   supply_cols = "gp_fte_extg"
#' )
#' plot(result$accessibility, main = "GP Accessibility (3SFCA)")
#'
#' # Select among the practices within 15 minutes only
#' result_15 <- spax_3sfca(
#'   demand = pop_rast,
#'   supply = gp,
#'   distance = travel,
#'   decay_params = list(method = "gaussian", sigma = 10),
#'   selection_params = list(method = "binary", sigma = 15),
#'   id_col = "practice",
#'   supply_cols = "gp_fte_extg"
#' )
#'
#' # Plot both for comparison
#' plot(c(result$accessibility, result_15$accessibility),
#'   main = c("Selection by decay", "Selection within 15 minutes")
#' )
#'
#' @seealso
#' \code{\link{spax_huff2sfca}} for selection weights that also depend on
#' facility attractiveness, \code{\link{spax_e2sfca}},
#' \code{\link{spax_m2sfca}}, \code{\link{compute_access}},
#' \code{\link{calc_decay}}, \code{\link{calc_choice}} and
#' \code{\link{decay_kernel}}.
#' \code{vignette("fca-methods", package = "spax")} compares the FCA methods on
#' the Cornwall data.
#'
#' @export
spax_3sfca <- function(demand, supply, distance,
                       decay_params = list(method = "gaussian", sigma = 30),
                       selection_params = decay_params,
                       id_col = NULL, supply_cols = NULL,
                       indicator_names = NULL,
                       snap = FALSE) {
  # Validation (3SFCA takes the inputs of E2SFCA, without normalization)
  if (!snap) {
    .chck_e2sfca(
      demand, supply, distance, decay_params,
      "identity", id_col, supply_cols
    )
    .chck_selection_params(selection_params)
  }

  result <- .fca_block_run(
    "3sfca", demand, supply, distance, decay_params,
    selection_params = selection_params, id_col = id_col,
    supply_cols = supply_cols, indicator_names = indicator_names, snap = snap
  )

  # Process facility information
  facilities <- .fca_facility_table(supply, id_col, supply_cols)

  # Create spax object
  .create_spax(
    accessibility = result,
    type = "3SFCA",
    parameters = list(
      decay_params = decay_params,
      selection_params = selection_params
    ),
    facilities = facilities,
    call = match.call(),
    snap = snap
  )
}

#' Calculate Huff-model Two-Step Floating Catchment Area (Huff-2SFCA) scores
#'
#' @description
#' This experimental function implements a floating catchment area method in
#' which the selection weights of \code{\link{spax_3sfca}} are replaced by a
#' Huff selection probability, following Luo (2014). The probability that an
#' origin selects a facility uses the attractiveness of the facilities and a
#' distance decay, so a more attractive facility gathers a larger share of the
#' demand at each origin.
#'
#' @inheritParams spax_3sfca
#' @param attractiveness Numeric vector of facility attractiveness with one
#'        finite, nonnegative value per facility. A named vector is matched to
#'        the layer names of \code{distance}; an unnamed vector follows the
#'        order of the layers. The default, \code{NULL}, uses the supply
#'        measure, which requires \code{supply} to have exactly one measure;
#'        give \code{attractiveness} when it has several.
#' @param alpha Nonnegative number: the exponent of attractiveness in the
#'        selection probability. With \code{alpha = 0} attractiveness has no
#'        effect and the result equals \code{\link{spax_3sfca}}.
#' @return A spax object (see \code{\link{spax_e2sfca}}) whose \code{type} is
#'         "Huff-2SFCA". Its \code{parameters} are the named list
#'         \code{decay_params}, \code{selection_params},
#'         \code{attractiveness} (the numeric vector in the order of the
#'         distance layers, named by facility ID) and \code{alpha}.
#'
#' @details
#' Write \eqn{D_i} for the demand at origin \eqn{i}, \eqn{S_j} for the supply of
#' facility \eqn{j}, \eqn{a_j} for its attractiveness, \eqn{W_{ij}}{W_ij} for
#' the decay weights from \code{decay_params} and \eqn{T_{ij}}{T_ij} for the
#' decay weights from \code{selection_params}. The two are the same weights when
#' \code{selection_params} is identical to \code{decay_params}.
#' \itemize{
#'   \item Selection probabilities:
#'     \eqn{G_{ij} = a_j^\alpha T_{ij} / \sum_k a_k^\alpha T_{ik}}{G_ij =
#'     a_j^alpha T_ij / sum_k a_k^alpha T_ik}.
#'   \item Ratio of facility \eqn{j}:
#'     \eqn{R_j = S_j / \sum_i D_i W_{ij} G_{ij}}{R_j = S_j / sum_i D_i W_ij
#'     G_ij}.
#'   \item Accessibility of origin \eqn{i}:
#'     \eqn{A_i = \sum_j R_j W_{ij} G_{ij}}{A_i = sum_j R_j W_ij G_ij}.
#' }
#'
#' The structure is that of \code{\link{spax_3sfca}}, to which the method
#' reduces when \code{alpha = 0} (a facility with zero attractiveness then
#' counts as 1, since \eqn{0^0 = 1}{0^0 = 1}). Both steps use the kernel
#' \eqn{K_{ij} = W_{ij} G_{ij}}{K_ij = W_ij G_ij}, and the result equals
#' \code{compute_access(demand, supply, demand_weights = K, access_weights = K)}
#' with \code{K = W * calc_choice(T, attractiveness, alpha)}. A facility with
#' zero attractiveness is never selected when \code{alpha} is positive. There is
#' no \code{demand_normalize} argument: the selection probabilities already
#' divide the demand of each origin among the facilities.
#'
#' The probabilities are normalized over every facility with a positive weight
#' at the origin, however far away it is. To restrict the selection to a
#' catchment, set the travel times beyond the limit to \code{NA} or \code{Inf}
#' before calling the function, or use a decay method that is zero beyond it,
#' for example \code{method = "binary"} in \code{selection_params}.
#'
#' Because both steps use the same kernel, the scores conserve supply: the sum
#' of \eqn{D_i A_i} over the origins with demand and a score equals the total
#' supply of the facilities that gather demand
#' (\eqn{\sum_i D_i K_{ij} > 0}{sum_i D_i K_ij > 0}). A facility that gathers no
#' demand has a ratio of zero. Several supply measures give independent
#' accessibility layers that share one attractiveness.
#'
#' @references
#' Luo, J. (2014). Integrating the Huff model and floating catchment area
#' methods to analyze spatial access to healthcare services.
#' \emph{Transactions in GIS}, \emph{18}(3), 436-448.
#' \doi{10.1111/tgis.12096}
#'
#' Wan, N., Zou, B., & Sternberg, T. (2012). A three-step floating catchment
#' area method for analyzing spatial access to health services.
#' \emph{International Journal of Geographical Information Science},
#' \emph{26}(6), 1073-1089. \doi{10.1080/13658816.2011.624987}
#'
#' @examples
#' # Load example data
#' library(terra)
#' library(sf)
#'
#' # Cornwall population grid and travel time to each GP practice
#' pop_rast <- read_spax_example("cornwall_pop.tif")
#' travel <- read_spax_example("cornwall_gp_travel.tif")
#'
#' # Drop geometry for supply data
#' gp <- cornwall_gp |> st_drop_geometry()
#'
#' # Attractiveness defaults to the supply measure (GP FTE excluding trainees)
#' result <- spax_huff2sfca(
#'   demand = pop_rast,
#'   supply = gp,
#'   distance = travel,
#'   decay_params = list(method = "gaussian", sigma = 10),
#'   id_col = "practice",
#'   supply_cols = "gp_fte_extg"
#' )
#' plot(result$accessibility, main = "GP Accessibility (Huff-2SFCA)")
#'
#' # Another attractiveness, named by practice, with a stronger effect
#' result_gp <- spax_huff2sfca(
#'   demand = pop_rast,
#'   supply = gp,
#'   distance = travel,
#'   decay_params = list(method = "gaussian", sigma = 10),
#'   attractiveness = setNames(gp$gp_fte, gp$practice), # all GPs
#'   alpha = 2,
#'   id_col = "practice",
#'   supply_cols = "gp_fte_extg"
#' )
#'
#' # Plot both for comparison
#' plot(c(result$accessibility, result_gp$accessibility),
#'   main = c("Attractiveness = supply", "All GPs, alpha = 2")
#' )
#'
#' @seealso
#' \code{\link{spax_3sfca}} for selection weights that do not depend on
#' attractiveness, \code{\link{spax_e2sfca}}, \code{\link{spax_m2sfca}},
#' \code{\link{compute_access}}, \code{\link{calc_decay}},
#' \code{\link{calc_choice}} and \code{\link{decay_kernel}}.
#' \code{vignette("fca-methods", package = "spax")} compares the FCA methods on
#' the Cornwall data.
#'
#' @export
spax_huff2sfca <- function(demand, supply, distance,
                           decay_params = list(method = "gaussian", sigma = 30),
                           selection_params = decay_params,
                           attractiveness = NULL, alpha = 1,
                           id_col = NULL, supply_cols = NULL,
                           indicator_names = NULL,
                           snap = FALSE) {
  # Validation (Huff-2SFCA takes the inputs of E2SFCA, without normalization)
  if (!snap) {
    .chck_e2sfca(
      demand, supply, distance, decay_params,
      "identity", id_col, supply_cols
    )
    .chck_selection_params(selection_params)
    .chck_nonnegative_scalar(alpha, "alpha")
  }

  # Attractiveness in the order of the distance layers, named by facility ID
  attractiveness <- .fca_attractiveness(
    attractiveness, supply, distance, id_col, supply_cols, snap
  )

  result <- .fca_block_run(
    "huff", demand, supply, distance, decay_params,
    selection_params = selection_params, attractiveness = attractiveness,
    alpha = alpha, id_col = id_col, supply_cols = supply_cols,
    indicator_names = indicator_names, snap = snap
  )

  # Process facility information
  facilities <- .fca_facility_table(supply, id_col, supply_cols)

  # Create spax object
  .create_spax(
    accessibility = result,
    type = "Huff-2SFCA",
    parameters = list(
      decay_params = decay_params,
      selection_params = selection_params,
      attractiveness = attractiveness,
      alpha = alpha
    ),
    facilities = facilities,
    call = match.call(),
    snap = snap
  )
}
