#' Core facility supply validation used by multiple functions
#' @noRd
.validate_facility_supply <- function(supply, id_col = NULL, supply_cols = NULL) {
  # Stop early if supply is an sf object
  if (inherits(supply, "sf")) {
    .stop("Supply data is an sf object. Please use st_drop_geometry() first")
  }

  # Validate data.frame inputs
  if (is.data.frame(supply)) {
    if (is.null(id_col)) {
      .stop("id_col must be specified when supply is a data.frame")
    }
    if (is.null(supply_cols)) {
      .stop("supply_cols must be specified when supply is a data.frame")
    }

    .chck_cols_exist(supply, c(id_col, supply_cols), "supply")

    # Check if id_col values are unique (specific to e2sfca)
    if (!is.null(id_col) && any(duplicated(supply[[id_col]]))) {
      .stop("Values in id_col must be unique")
    }
  }

  # Return number of facilities for convenience
  if (is.data.frame(supply) || is.matrix(supply)) {
    return(nrow(supply))
  } else {
    return(length(supply))
  }
}

# Demand groups (SPAX-056) ----------------------------------------------------

# The naming rule for group IDs, shared by the FCA wrappers and the allocation
# workflow: unique, non-empty, not NA, not "all" and without "." (so that
# "sigma.<group>" names split unambiguously). `what` names the IDs in the error.
.check_group_ids <- function(ids, what) {
  if (!length(ids) || is.null(ids) || anyNA(ids) || any(ids == "") ||
      anyDuplicated(ids) || any(ids == "all") ||
      any(grepl(".", ids, fixed = TRUE))) {
    stop(what, " must be unique, non-empty, not NA, not \"all\" ",
         "and without \".\"", call. = FALSE)
  }
  invisible(ids)
}

# The group IDs of a `demand` argument: NULL for one single-layer SpatRaster
# (ungrouped), the names of the list for a named list of single-layer
# SpatRasters (grouped, also a list of one). A multi-layer SpatRaster is not
# taken as groups. The group structure is checked always, also under `snap`:
# the names (unique, non-empty, not NA, not "all", no "."), the class and layer
# count of each element, and one geometry for all of them.
.fca_demand_groups <- function(demand) {
  if (inherits(demand, "SpatRaster")) {
    if (terra::nlyr(demand) > 1L) {
      stop("give demand groups as a named list of single-layer rasters",
           call. = FALSE)
    }
    return(NULL)
  }
  if (!is.list(demand)) {
    .chck_class(demand, "SpatRaster", "demand")
  }
  groups <- names(demand)
  .check_group_ids(groups, "demand group names")
  for (g in groups) {
    .chck_class(demand[[g]], "SpatRaster", paste0("demand group '", g, "'"))
    if (terra::nlyr(demand[[g]]) > 1L) {
      stop("give demand groups as a named list of single-layer rasters",
           call. = FALSE)
    }
  }
  for (g in groups[-1L]) {
    if (!terra::compareGeom(demand[[1L]], demand[[g]], stopOnError = FALSE)) {
      stop("all demand groups must have the same geometry", call. = FALSE)
    }
  }
  groups
}

# Whether an argument is given per group, and then its elements in group
# order. It is per group if and only if it is a list (an atomic named vector
# when `atomic`) whose names equal the group IDs as a set; the result is the
# elements reordered to `groups`. Names that overlap the group IDs only partly
# are an error. Otherwise the argument is shared and NULL is returned.
.fca_per_group <- function(x, groups, arg, atomic = FALSE, element = is.list) {
  form <- if (atomic) is.numeric(x) else is.list(x)
  nms <- if (form) names(x)
  if (is.null(nms) || !any(nms %in% groups)) {
    return(NULL)
  }
  # A shared argument whose own names match group names (groups called "sigma"
  # or "method", say) is not per group: its elements are not per-group values.
  if (!atomic && !all(vapply(x, element, logical(1)))) {
    return(NULL)
  }
  if (length(x) != length(groups) || anyDuplicated(nms) || !setequal(nms, groups)) {
    stop(arg, ": give one element per group: ", paste(groups, collapse = ", "),
         call. = FALSE)
  }
  x[groups]
}

# The travel stacks of the groups must name the same facilities in the same
# order and have one geometry (always checked).
.fca_check_stacks <- function(stacks, arg) {
  ids <- .fca_ids(stacks[[1L]], arg)
  for (s in stacks[-1L]) {
    if (!identical(.fca_ids(s, arg), ids) ||
        !terra::compareGeom(stacks[[1L]], s, stopOnError = FALSE)) {
      stop(if (grepl("weights", arg)) "weight" else "travel",
           " layers of every group must name the same facilities in ",
           "the same order; reorder with x[[names(first)]]", call. = FALSE)
    }
  }
  invisible(TRUE)
}

# An argument for each group, a list named by `groups`: its per-group elements
# or, when it is shared, the argument itself for every group (a shared raster
# stays one object, so it is read once). `stacks`: the elements are raster
# stacks of facilities (see .fca_check_stacks()).
.fca_expand_groups <- function(x, groups, arg, stacks = FALSE) {
  per <- .fca_per_group(x, groups, arg, element = if (stacks) {
    function(e) inherits(e, "SpatRaster")
  } else {
    is.list
  })
  if (is.null(per)) {
    return(stats::setNames(rep(list(x), length(groups)), groups))
  }
  if (stacks) {
    .fca_check_stacks(per, arg)
  }
  per
}

# The raster that names the facilities of a call: the first group's stack when
# the argument is given per group, else the argument.
.fca_first_stack <- function(x, groups, arg) {
  if (is.null(groups)) {
    return(x)
  }
  .fca_expand_groups(x, groups, arg, stacks = TRUE)[[1L]]
}

# The per-group decay, selection and travel arguments of a grouped call.
.fca_group_plan <- function(groups, distance, decay_params,
                            selection_params = NULL) {
  list(
    distance = .fca_expand_groups(distance, groups, "distance", stacks = TRUE),
    decay = .fca_expand_groups(decay_params, groups, "decay_params"),
    selection = if (!is.null(selection_params)) {
      .fca_expand_groups(selection_params, groups, "selection_params")
    }
  )
}

# The result of a grouped run in the form of a spax object: the layer table
# goes into `parameters` (with the group IDs) and off the raster. An ungrouped
# result is returned as it is.
.fca_group_result <- function(result, parameters) {
  layers <- attr(result, "layers")
  if (!is.null(layers)) {
    parameters$groups <- unique(layers$group[layers$group != "all"])
    parameters$layers <- layers
    attr(result, "layers") <- NULL
  }
  list(accessibility = result, parameters = parameters)
}

#' Validate inputs for compute_access
#' @param demand SpatRaster representing spatial distribution of demand, or a
#'   named list of SpatRasters (demand groups)
#' @param supply vector, matrix, or data.frame containing supply capacity values
#' @param demand_weights Multi-layer SpatRaster of demand-side weights, or with
#'   groups a named list of them (one per group)
#' @param access_weights Multi-layer SpatRaster of accessibility-side weights, or
#'   with groups a named list of them (one per group)
#' @param id_col Character; column name for facility IDs if supply is a data.frame
#' @param supply_cols Character vector; names of supply columns if supply is a data.frame
#' @param indicator_names Character vector; custom names for output accessibility layers
#' @noRd
.chck_compute_access <- function(demand, supply, demand_weights, access_weights,
                                 id_col = NULL, supply_cols = NULL,
                                 indicator_names = NULL) {
  # Input type validation; with groups, each group's weights are checked
  groups <- .fca_demand_groups(demand)
  if (is.null(groups)) {
    demands <- list(demand)
    dw <- list(demand_weights)
    aw <- list(access_weights)
  } else {
    demands <- demand
    dw <- .fca_expand_groups(demand_weights, groups, "demand_weights", stacks = TRUE)
    aw <- .fca_expand_groups(access_weights, groups, "access_weights", stacks = TRUE)
  }
  for (i in seq_along(dw)) {
    .chck_class(dw[[i]], "SpatRaster", "demand_weights")
    .chck_class(aw[[i]], "SpatRaster", "access_weights")
  }

  # Validate supply and get facility count
  n_facilities <- .validate_facility_supply(supply, id_col, supply_cols)

  for (i in seq_along(dw)) {
    # Validate raster alignments
    .chck_raster_alignment(demands[[i]], dw[[i]], "demand", "demand_weights")
    .chck_raster_alignment(demands[[i]], aw[[i]], "demand", "access_weights")

    # Validate weights match facility count
    .chck_lengths_match(
      nlyr(dw[[i]]), n_facilities,
      "demand_weights layers", "facilities"
    )
    .chck_lengths_match(
      nlyr(aw[[i]]), n_facilities,
      "access_weights layers", "facilities"
    )
  }

  invisible(TRUE)
}

#' Validate inputs for spax_e2sfca
#'
#' The checks of the FCA wrappers that take a decay (E2SFCA, M2SFCA, 3SFCA and
#' Huff-2SFCA, and 2SFCA through its binary decay) are two functions that run in
#' this order: the data (`.chck_e2sfca_data()`) and the decay and normalization
#' (`.chck_e2sfca_decay()`). `prep_data()` with rasters runs the data part
#' alone.
#' @param demand SpatRaster representing spatial distribution of demand, or a
#'   named list of SpatRasters (demand groups)
#' @param supply vector, matrix, or data.frame containing supply capacity values
#' @param distance SpatRaster stack of travel times/distances to facilities, or
#'   with groups a named list of them (one per group)
#' @param decay_params List of parameters for decay function, or with groups a
#'   named list of them (one per group)
#' @param demand_normalize Character specifying normalization method
#' @param id_col Character; column name for facility IDs if supply is a data.frame
#' @param supply_cols Character vector; names of supply columns if supply is a data.frame
#' @noRd
.chck_e2sfca <- function(demand, supply, distance, decay_params,
                         demand_normalize, id_col = NULL, supply_cols = NULL) {
  .chck_e2sfca_data(demand, supply, distance, id_col, supply_cols)
  .chck_e2sfca_decay(demand, decay_params, demand_normalize)
}

# The data part of .chck_e2sfca(): the demand groups, the travel stacks (one
# geometry, the same facilities in the same order for every group), the supply,
# the alignment of each stack with the demand and one layer per facility.
# `arg` names the travel argument in the messages (`prep_data()` calls it
# "travel").
.chck_e2sfca_data <- function(demand, supply, distance, id_col = NULL,
                              supply_cols = NULL, arg = "distance") {
  # Check input types; with groups, each group's travel is checked
  groups <- .fca_demand_groups(demand)
  if (is.null(groups)) {
    demands <- list(demand)
    dist <- list(distance)
  } else {
    demands <- demand
    dist <- .fca_expand_groups(distance, groups, arg, stacks = TRUE)
  }
  for (d in dist) {
    .chck_class(d, "SpatRaster", arg)
  }

  # Validate supply and get facility count
  n_facilities <- .validate_facility_supply(supply, id_col, supply_cols)

  for (i in seq_along(dist)) {
    # Validate raster alignment
    .chck_raster_alignment(demands[[i]], dist[[i]], "demand", arg)

    # Validate facility counts match
    .chck_lengths_match(
      nlyr(dist[[i]]), n_facilities,
      paste(arg, "layers"), "facilities"
    )
  }

  invisible(TRUE)
}

# The decay part of .chck_e2sfca(): the decay parameters (one list, or with
# groups one per group or shared) and the normalization of demand.
.chck_e2sfca_decay <- function(demand, decay_params, demand_normalize) {
  groups <- .fca_demand_groups(demand)
  decay <- if (is.null(groups)) {
    list(decay_params)
  } else {
    .fca_expand_groups(decay_params, groups, "decay_params")
  }

  # Validate decay_params
  for (dp in decay) {
    .chck_class(dp, "list", "decay_params")
    if (is.null(dp$method)) {
      .stop("decay_params must include 'method'")
    }
  }

  # Validate demand_normalize
  valid_normalize <- c("identity", "standard", "semi")
  if (!demand_normalize %in% valid_normalize) {
    .stop(sprintf(
      "demand_normalize must be one of: %s",
      paste(valid_normalize, collapse = ", ")
    ))
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
#' @noRd
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
        .stop("Some weight layer IDs not found in supply data")
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
        .stop("Some weight layer IDs not found in supply data")
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
    .stop("Unsupported supply data type")
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
# or matrix (one layer per facility). `key` names the rasters and `suffix` ends
# every raster key (`""`, or `".<group>"` for a demand group, so that no key is
# shared between groups by name). When `snap` is FALSE, .chck_decay() runs here,
# before anything is opened.
.fca_decay_spec <- function(distance, params, key, snap, suffix = "") {
  method <- params$method
  dist_key <- paste0("distance", suffix)
  if (is.function(method)) {
    if (!inherits(distance, "SpatRaster")) {
      stop("a plain decay function needs raster data; declare a kernel with decay_kernel()",
           call. = FALSE)
    }
    nm <- paste0("weights_", key, suffix)
    weights <- .fca_decay_weights(distance, params, snap)
    # The weight layers belong to facilities by their names, as before; layers
    # with terra's default names (or none) are taken in distance-layer order.
    ids <- .fca_ids(distance)
    weight_ids <- .fca_ids(weights, "decay weights")
    if (terra::nlyr(weights) != length(ids)) {
      .stop("a decay function must return one layer per distance layer")
    }
    positional <- identical(weight_ids, paste0("facility_", seq_along(weight_ids)))
    if (!positional && !setequal(weight_ids, ids)) {
      .stop("the layer names returned by the decay function must be the distance layer names")
    }
    order <- if (positional) seq_along(ids) else match(ids, weight_ids)
    # The distance is read as well, for the finite-travel mask of decision D1.
    rasters <- c(stats::setNames(list(distance), dist_key),
                 stats::setNames(list(weights), nm))
    return(list(
      rasters = rasters,
      weights = function(b, rows) {
        W <- b[[nm]][rows, order, drop = FALSE]
        .check_finite_weights(b[[dist_key]][rows, , drop = FALSE], W)
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
    .stop("a variable catchment needs a built-in method or a plain function")
  }
  if (builtin && !snap) {
    .chck_decay(distance, method, params$sigma, snap)
  }
  if (builtin && !is.null(params$sigma) && !inherits(params$sigma, "SpatRaster") &&
      length(params$sigma) > 1L) {
    .stop("sigma must be a single number or a SpatRaster")
  }

  others <- params[setdiff(names(params), "method")]
  is_raster <- vapply(others, inherits, logical(1), what = "SpatRaster")
  raster_args <- names(others)[is_raster]
  keys <- character()
  if (length(raster_args)) {
    keys <- stats::setNames(paste0(raster_args, "_", key, suffix), raster_args)
  }
  rasters <- c(stats::setNames(list(distance), dist_key),
               stats::setNames(others[is_raster], unname(keys)))
  fixed <- others[!is_raster]

  list(
    rasters = rasters,
    weights = function(b, rows) {
      # Pass 2 asks for every row of the block: use the block as read, without a copy.
      all_rows <- length(rows) == nrow(b[[dist_key]])
      d <- if (all_rows) b[[dist_key]] else b[[dist_key]][rows, , drop = FALSE]
      args <- fixed
      for (arg in names(keys)) {
        x <- b[[keys[[arg]]]]
        if (!all_rows) x <- x[rows, , drop = FALSE]
        if (ncol(x) > 1L && ncol(x) != ncol(b[[dist_key]])) {
          .stop("`", arg, "` raster must have 1 layer or one layer per `distance` layer")
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

# The kernels of one demand group for the block engine. `type` is "e2sfca",
# "m2sfca", "3sfca" or "huff" (any other is an error); it builds the kernel
# specification of the wrapper. `attractiveness` is the vector from .fca_attractiveness() for
# "huff". `suffix` ends every raster key of the group (see .fca_decay_spec()).
# Returns `rasters`, the named SpatRasters the kernels read (the group's
# distance first), and `kernels`, `list(demand, access)`. `distance` may also be
# a numeric matrix (origins by facilities: FCA on numeric data, SPAX-060 V3):
# the kernels then read it from the in-memory block of .fca_blocks() under the key
# "distance<suffix>", and `rasters` is NULL.
.fca_group_kernels <- function(type, distance, decay_params,
                               selection_params = NULL, attractiveness = NULL,
                               alpha = 1, snap = FALSE, suffix = "") {
  in_memory <- is.matrix(distance)
  if (!in_memory) {
    .chck_class(distance, "SpatRaster", "distance")
  }
  decay <- .fca_decay_spec(distance, decay_params, "decay", snap, suffix)
  selection <- NULL
  if (type %in% c("3sfca", "huff") && !identical(selection_params, decay_params)) {
    selection <- .fca_decay_spec(distance, selection_params, "selection", snap,
                                 suffix)
  }
  # The first raster names the facilities and gives the grid of the result.
  rasters <- if (!in_memory) {
    .fca_merge_rasters(c(decay$rasters, selection$rasters))
  }

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
    `3sfca` = ,
    huff = list(demand = choice_kernel, access = choice_kernel),
    stop("unknown FCA type '", type, "'", call. = FALSE)
  )
  list(rasters = rasters, kernels = kernels)
}

# The block-engine counterpart of the decay-based wrappers; see
# .fca_group_kernels(). `demand` is one raster, or a named list of rasters, one
# per demand group: then `distance`, `decay_params` and `selection_params` are
# shared or per group (see .fca_per_group()), every group gets its own kernels
# and raster keys, and the result has the layers of compute_fca() for groups,
# with the layer table as its `layers` attribute.
.fca_block_run <- function(type, demand, supply, distance, decay_params,
                           selection_params = NULL, attractiveness = NULL,
                           alpha = 1, demand_normalize = "identity",
                           id_col = NULL, supply_cols = NULL,
                           indicator_names = NULL, snap = FALSE) {
  groups <- .fca_demand_groups(demand)
  if (is.null(groups)) {
    spec <- .fca_group_kernels(type, distance, decay_params, selection_params,
                               attractiveness, alpha, snap)
  } else {
    plan <- .fca_group_plan(groups, distance, decay_params, selection_params)
    specs <- lapply(seq_along(groups), function(i) {
      .fca_group_kernels(type, plan$distance[[i]], plan$decay[[i]],
                         plan$selection[[i]], attractiveness, alpha, snap,
                         suffix = paste0(".", groups[i]))
    })
    spec <- list(
      rasters = do.call(c, lapply(specs, `[[`, "rasters")),
      kernels = stats::setNames(lapply(specs, `[[`, "kernels"), groups)
    )
  }
  compute_fca(
    demand = demand, supply = supply, rasters = spec$rasters,
    kernels = spec$kernels, demand_normalize = demand_normalize,
    id_col = id_col, supply_cols = supply_cols,
    indicator_names = indicator_names, snap = snap
  )
}

# What the FCA kinds are to the runner. `kind` is the kind of an FCA
# specification ("2sfca", "e2sfca", "m2sfca", "3sfca" or "huff2sfca"; see
# .fca_spec_args()): `type` is the type of .fca_block_run(), `label` the `type`
# of the result and `parameter_names` the elements of its `parameters` list, in
# the order of the wrappers.
.fca_kind_info <- function(kind) {
  type <- switch(kind,
    `2sfca` = , e2sfca = "e2sfca",
    m2sfca = "m2sfca",
    `3sfca` = "3sfca",
    huff2sfca = "huff",
    stop("unknown FCA kind '", kind, "'", call. = FALSE)
  )
  list(
    type = type,
    label = unname(.fca_spec_labels[[kind]]),
    parameter_names = switch(type,
      e2sfca = , m2sfca = c("decay_params", "demand_normalize"),
      `3sfca` = c("decay_params", "selection_params"),
      huff = c("decay_params", "selection_params", "attractiveness", "alpha")
    )
  )
}

# The shared runner of the FCA family: what each wrapper does after its
# validation. It runs the block engine (.fca_block_run()), builds the facility
# table and the parameters list of the wrapper and returns the spax object.
# The one-shots and spax() call it; `call` is stored as given (the caller's own
# call). `kind` is as in .fca_kind_info(). For Huff-2SFCA `attractiveness` is
# NULL (the supply measure) or the vector the caller gave; it is resolved here,
# with the first group's stack as the wrapper does, and stored resolved. The
# arguments the kind does not use (selection_params, attractiveness, alpha,
# demand_normalize) are ignored.
.fca_run <- function(kind, demand, supply, distance, decay_params,
                     selection_params = NULL, attractiveness = NULL, alpha = 1,
                     demand_normalize = "identity", id_col = NULL,
                     supply_cols = NULL, indicator_names = NULL, snap = FALSE,
                     call = NULL) {
  info <- .fca_kind_info(kind)
  if (identical(info$type, "huff")) {
    groups <- .fca_demand_groups(demand)
    # Attractiveness in the order of the distance layers (with groups, those of
    # the first group), named by facility ID
    attractiveness <- .fca_attractiveness(
      attractiveness, supply, .fca_first_stack(distance, groups, "distance"),
      id_col, supply_cols, snap
    )
  }

  result <- .fca_block_run(
    info$type, demand, supply, distance, decay_params,
    selection_params = selection_params, attractiveness = attractiveness,
    alpha = alpha, demand_normalize = demand_normalize, id_col = id_col,
    supply_cols = supply_cols, indicator_names = indicator_names, snap = snap
  )

  # Process facility information
  facilities <- .fca_facility_table(supply, id_col, supply_cols)

  # Create spax object
  out <- .fca_group_result(result, list(
    decay_params = decay_params,
    demand_normalize = demand_normalize,
    selection_params = selection_params,
    attractiveness = attractiveness,
    alpha = alpha
  )[info$parameter_names])
  .create_spax(
    accessibility = out$accessibility,
    type = info$label,
    parameters = out$parameters,
    facilities = facilities,
    call = call,
    snap = snap
  )
}

# The block-engine counterpart of compute_access(). The access weights are
# reordered to the facility IDs of the demand weights. `suffix` ends the raster
# keys, as in .fca_decay_spec().
.fca_access_kernels <- function(demand_weights, access_weights, suffix = "") {
  demand_ids <- .fca_ids(demand_weights, "demand_weights")
  access_ids <- .fca_ids(access_weights, "access_weights")
  if (!setequal(demand_ids, access_ids)) {
    .stop("demand and access kernels must span the same facility ids")
  }
  pos <- match(demand_ids, access_ids)
  dkey <- paste0("demand_weights", suffix)
  akey <- paste0("access_weights", suffix)
  list(
    rasters = stats::setNames(list(demand_weights, access_weights), c(dkey, akey)),
    kernels = list(
      demand = function(b, rows) {
        .check_user_weights(b[[dkey]][rows, , drop = FALSE])
      },
      access = function(b, rows) {
        .check_user_weights(b[[akey]][rows, pos, drop = FALSE])
      }
    )
  )
}

# With a named list of demand rasters (groups), `demand_weights` and
# `access_weights` are shared or per group, as in .fca_block_run().
.fca_access_block <- function(demand, supply, demand_weights, access_weights,
                              id_col = NULL, supply_cols = NULL,
                              indicator_names = NULL, snap = FALSE) {
  groups <- .fca_demand_groups(demand)
  if (is.null(groups)) {
    spec <- .fca_access_kernels(demand_weights, access_weights)
  } else {
    dw <- .fca_expand_groups(demand_weights, groups, "demand_weights", stacks = TRUE)
    aw <- .fca_expand_groups(access_weights, groups, "access_weights", stacks = TRUE)
    specs <- lapply(seq_along(groups), function(i) {
      .fca_access_kernels(dw[[i]], aw[[i]], suffix = paste0(".", groups[i]))
    })
    spec <- list(
      rasters = do.call(c, lapply(specs, `[[`, "rasters")),
      kernels = stats::setNames(lapply(specs, `[[`, "kernels"), groups)
    )
  }
  compute_fca(
    demand = demand, supply = supply, rasters = spec$rasters,
    kernels = spec$kernels,
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

# selection_params has the form of decay_params: a list with a decay method
# (with groups, shared or one such list per group).
.chck_selection_params <- function(selection_params, groups = NULL) {
  params <- if (is.null(groups)) {
    list(selection_params)
  } else {
    .fca_expand_groups(selection_params, groups, "selection_params")
  }
  for (p in params) {
    .chck_class(p, "list", "selection_params")
    if (is.null(p$method)) {
      .stop("selection_params must include 'method'")
    }
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
  .fca_attractiveness_ids(attractiveness, supply, names(distance), id_col,
                          supply_cols, snap)
}

# .fca_attractiveness() for facilities given by their IDs, in the order of the
# distance layers (or of the columns of a travel matrix).
.fca_attractiveness_ids <- function(attractiveness, supply, ids, id_col,
                                    supply_cols, snap) {
  if (is.null(attractiveness)) {
    values <- .help_process_supply(
      supply, id_col, supply_cols, weight_ids = ids
    )$values
    if (ncol(values) != 1L) {
      .stop("give attractiveness when supply has several measures")
    }
    values <- values[, 1]
    label <- "the supply measure used as attractiveness"
  } else {
    if (!snap) {
      .chck_numeric_vector(attractiveness, "attractiveness", finite = FALSE)
      .chck_lengths_match(length(attractiveness), length(ids),
                          "attractiveness", "distance layers")
    }
    values <- attractiveness
    if (!is.null(names(values))) {
      if (anyDuplicated(names(values)) || !setequal(names(values), ids)) {
        .stop("names of attractiveness must be the distance layer names, ",
             "one for each facility")
      }
      values <- values[match(ids, names(values))]
    }
    label <- "attractiveness"
  }
  if (!snap && (!all(is.finite(values)) || any(values < 0))) {
    .stop(label, " must contain only finite, nonnegative values")
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
#' @param demand SpatRaster representing spatial distribution of demand, or a
#'        named list of single-layer SpatRasters, one per demand group (see
#'        "Demand groups"). A multi-layer SpatRaster is refused. Demand
#'        must be finite and nonnegative (use NA for unknown) unless `snap = TRUE`,
#'        which ignores other cells.
#' @param supply vector, matrix, or data.frame containing supply capacity values.
#'        If using an sf object, please use st_drop_geometry() first.
#' @param demand_weights Multi-layer SpatRaster of demand-side weights; values
#'        may be NA but not infinite. With demand groups, either one stack
#'        shared by every group or a list of stacks named by the groups (one per
#'        group); the stacks of all groups must name the same facilities in the
#'        same order and have the same geometry.
#' @param access_weights Multi-layer SpatRaster of accessibility-side weights;
#'        values may be NA but not infinite. Given per group in the same way as
#'        `demand_weights`.
#' @param id_col Character; column name for facility IDs if supply is a data.frame
#' @param supply_cols Character vector; names of supply columns if supply is a data.frame
#' @param indicator_names Character vector; custom names for output accessibility layers
#' @param snap Logical; if TRUE enable fast computation mode (default = FALSE)
#' @return SpatRaster of accessibility scores. With demand groups, the layers
#'        are named `<measure>.<group>` for every group and `<measure>.all`
#'        for the overall layer, measure by measure.
#'
#' @section Demand groups:
#' When `demand` is a named list of single-layer rasters, the groups compete
#' for the same supply: the demand that reaches a facility is the sum over the
#' groups, each with its own demand weights. Each group gets its own
#' accessibility layer, and `<measure>.all` is the overall layer, the average
#' of the group scores weighted by the demand of the cell. A group with demand
#' in a cell that none of its weights reach counts with accessibility 0 there;
#' the overall layer is NA where any group's demand is missing or the total
#' demand is 0. Group names must be unique, non-empty, not `"all"` and without
#' a `.`. `demand_weights` and `access_weights` are shared by the groups, or a
#' list with one element per group, named by the groups. Shared weights should
#' be passed as one object, so that they are read once.
#' @examples
#' library(terra)
#'
#' # Cornwall population grid, and Gaussian weights of the travel time to each
#' # GP practice
#' pop <- read_spax_example("cornwall_pop.tif")
#' travel <- read_spax_example("cornwall_gp_travel.tif")
#' gp <- sf::st_drop_geometry(cornwall_gp)
#' w <- calc_decay(travel, method = "gaussian", sigma = 10)
#'
#' # The same weights in both steps give E2SFCA (spax_e2sfca()); the squared
#' # weights in the second step give M2SFCA (spax_m2sfca())
#' e2sfca <- compute_access(pop, gp, demand_weights = w, access_weights = w,
#'                          id_col = "practice", supply_cols = "gp_fte_extg",
#'                          indicator_names = "e2sfca")
#' m2sfca <- compute_access(pop, gp, demand_weights = w, access_weights = w^2,
#'                          id_col = "practice", supply_cols = "gp_fte_extg",
#'                          indicator_names = "m2sfca")
#' plot(c(e2sfca, m2sfca))
#' @export
#' @md
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

  result <- .fca_access_block(
    demand, supply, demand_weights, access_weights,
    id_col, supply_cols, indicator_names, snap
  )
  # With groups the layer table is not kept: the layer names say it.
  attr(result, "layers") <- NULL
  result
}

#' Calculate Enhanced Two-Step Floating Catchment Area (E2SFCA) accessibility scores
#'
#' @description
#' Implements the Enhanced Two-Step Floating Catchment Area (E2SFCA) method as proposed
#' by Luo & Qi (2009). This method improves upon the original 2SFCA by incorporating
#' distance decay effects and allowing for variable catchment sizes, providing more
#' realistic measures of spatial accessibility to services.
#'
#' @param demand SpatRaster representing spatial distribution of demand, or a
#'        named list of single-layer SpatRasters, one per demand group (see
#'        "Demand groups"). A multi-layer SpatRaster is refused. Demand
#'        must be finite and nonnegative (use NA for unknown) unless `snap = TRUE`,
#'        which ignores other cells.
#' @param supply vector, matrix, or data.frame containing supply capacity values
#' @param distance SpatRaster stack of travel times/distances to facilities.
#'        With demand groups, either one stack shared by every group or a list
#'        of stacks named by the groups (for example one per travel mode); the
#'        stacks of all groups must name the same facilities in the same order
#'        and have the same geometry (reorder with `x[[names(first)]]`).
#' @param decay_params List of parameters for decay function (with demand
#'        groups, one list shared by every group, or a list of such lists named
#'        by the groups):
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
#'          \item accessibility: SpatRaster of accessibility scores. With
#'            demand groups, the layers are named `<measure>.<group>` for every
#'            group and `<measure>.all` for the overall layer, measure by measure
#'          \item type: Character string "E2SFCA"
#'          \item parameters: List of model parameters used. With demand groups
#'            it also holds `groups` (the group IDs) and `layers` (a data frame
#'            of the columns `layer`, `measure` and `group`, the group being
#'            `"all"` for the overall layer)
#'          \item facilities: data.frame of facility-level information
#'          \item call: The original function call
#'        }
#'
#' @section Demand groups:
#' Give `demand` as a named list of single-layer rasters (for example age
#' bands) to let several groups compete for the same facilities. The names are
#' the group IDs: unique, non-empty, not `"all"` and without a `.`; all rasters
#' need the same geometry. A multi-layer raster is not taken as groups.
#'
#' Step 1 sums, over all groups, the demand that reaches each facility, with
#' the demand weights of each group; step 2 gives each group its own
#' accessibility from the shared ratios, with its own weights. The result has
#' one layer per group, and an overall layer `<measure>.all`: the average of
#' the group scores weighted by the demand of the cell. A group with demand in
#' a cell that its travel cannot reach counts with accessibility 0 in the
#' overall layer; the overall layer is NA where any group's demand is missing
#' or the total demand is 0. With one group the group layer equals the
#' ungrouped result.
#'
#' `decay_params` and `distance` (and `selection_params` of
#' [spax_3sfca()] and [spax_huff2sfca()], `threshold` of [spax_2sfca()]) are
#' each either shared by every group or given per group. An argument is per
#' group when it is a list (for `threshold`, a named numeric vector) whose
#' names are the group IDs, in any order; names that match only some of the
#' groups are refused. Any other value is shared and checked as for one
#' group. Pass a shared `distance` as one object, so that it is read once. A
#' plain-function decay builds a whole in-memory weight raster for each group.
#' `demand_normalize`, `indicator_names`, `id_col`, `supply_cols` and the
#' attractiveness of [spax_huff2sfca()] are shared.
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
#' # Two demand groups that compete for the same practices, with a fixed
#' # illustrative split of the population and a decay scale for each group
#' groups <- list(younger = pop_rast * 0.6, older = pop_rast * 0.4)
#' result_groups <- spax_e2sfca(
#'   demand = groups,
#'   supply = gp,
#'   distance = travel,
#'   decay_params = list(
#'     younger = list(method = "gaussian", sigma = 8),
#'     older = list(method = "gaussian", sigma = 15)
#'   ),
#'   id_col = "practice",
#'   supply_cols = "gp_fte_extg"
#' )
#' names(result_groups$accessibility)
#' result_groups$parameters$groups
#' plot(result_groups$accessibility)
#'
#' @seealso
#' * [spax_2sfca()] for the original method without distance decay
#' * [compute_access()] for more flexible accessibility calculations
#' * [calc_decay()] for available decay functions
#'
#' @export
#' @md
spax_e2sfca <- function(demand, supply, distance,
                        decay_params = list(method = "gaussian", sigma = 30),
                        demand_normalize = "identity",
                        id_col = NULL, supply_cols = NULL,
                        indicator_names = NULL,
                        snap = FALSE) {
  call <- match.call()
  # Validation (skip if snap = TRUE)
  if (!snap) {
    .chck_e2sfca(
      demand, supply, distance, decay_params,
      demand_normalize, id_col, supply_cols
    )
  }

  .fca_run(
    "e2sfca", demand, supply, distance, decay_params,
    demand_normalize = demand_normalize, id_col = id_col,
    supply_cols = supply_cols, indicator_names = indicator_names, snap = snap,
    call = call
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
#' @inheritSection spax_e2sfca Demand groups
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
#' @examples
#' library(terra)
#' library(sf)
#'
#' # Cornwall population grid and travel time to each GP practice
#' pop_rast <- read_spax_example("cornwall_pop.tif")
#' travel <- read_spax_example("cornwall_gp_travel.tif")
#' gp <- cornwall_gp |> st_drop_geometry()
#'
#' # M2SFCA with Gaussian decay
#' result <- spax_m2sfca(
#'   demand = pop_rast,
#'   supply = gp,
#'   distance = travel,
#'   decay_params = list(method = "gaussian", sigma = 10),
#'   id_col = "practice",
#'   supply_cols = "gp_fte_extg"
#' )
#' result
#'
#' # E2SFCA with the same decay, for comparison
#' result_e2 <- spax_e2sfca(
#'   demand = pop_rast,
#'   supply = gp,
#'   distance = travel,
#'   decay_params = list(method = "gaussian", sigma = 10),
#'   id_col = "practice",
#'   supply_cols = "gp_fte_extg"
#' )
#' plot(c(result_e2$accessibility, result$accessibility),
#'   main = c("E2SFCA", "M2SFCA")
#' )
#'
#' # The share of the E2SFCA score that M2SFCA keeps: the average decay weight
#' # of the practices in reach, low where they are far away
#' plot(result$accessibility / result_e2$accessibility,
#'   main = "M2SFCA / E2SFCA"
#' )
#' @export
#' @md
spax_m2sfca <- function(demand, supply, distance,
                        decay_params = list(method = "gaussian", sigma = 30),
                        demand_normalize = "identity",
                        id_col = NULL, supply_cols = NULL,
                        indicator_names = NULL,
                        snap = FALSE) {
  call <- match.call()
  # Validation (M2SFCA takes the same inputs as E2SFCA)
  if (!snap) {
    .chck_e2sfca(
      demand, supply, distance, decay_params,
      demand_normalize, id_col, supply_cols
    )
  }

  .fca_run(
    "m2sfca", demand, supply, distance, decay_params,
    demand_normalize = demand_normalize, id_col = id_col,
    supply_cols = supply_cols, indicator_names = indicator_names, snap = snap,
    call = call
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
#' @param demand SpatRaster representing spatial distribution of demand, or a
#'        named list of single-layer SpatRasters, one per demand group (see
#'        "Demand groups"). A multi-layer SpatRaster is refused. Demand
#'        must be finite and nonnegative (use NA for unknown) unless `snap = TRUE`,
#'        which ignores other cells.
#' @param supply vector, matrix, or data.frame containing supply capacity values
#' @param distance SpatRaster stack of travel times/distances to facilities.
#'        With demand groups, one stack shared by every group or a list of
#'        stacks named by the groups (see [spax_e2sfca()]).
#' @param threshold Numeric value defining the catchment area cutoff (same units
#'        as distance). With demand groups, one number shared by every group or
#'        a numeric vector named by the groups, one positive finite number per
#'        group.
#' @param id_col Character; column name for facility IDs if supply is a data.frame
#' @param supply_cols Character vector; names of supply columns if supply is a data.frame
#' @param snap Logical; if TRUE enable fast computation mode (default = FALSE)
#' @return A spax object containing:
#'        \itemize{
#'          \item accessibility: SpatRaster of accessibility scores (with demand
#'            groups, named as in [spax_e2sfca()])
#'          \item type: Character string "2SFCA"
#'          \item parameters: List containing threshold and binary decay
#'            parameters (and `groups` and `layers` with demand groups)
#'          \item facilities: data.frame of facility-level information
#'          \item call: The original function call
#'        }
#' @inheritSection spax_e2sfca Demand groups
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
#' @md
spax_2sfca <- function(demand, supply, distance, threshold,
                       id_col = NULL, supply_cols = NULL,
                       snap = FALSE) {
  call <- match.call()
  # With demand groups, the threshold is one number or one per group
  groups <- if (is.list(demand)) .fca_demand_groups(demand)
  thresholds <- if (!is.null(groups)) {
    .fca_per_group(threshold, groups, "threshold", atomic = TRUE)
  }
  if (!snap) {
    for (th in if (is.null(thresholds)) list(threshold) else thresholds) {
      if (!is.numeric(th) || length(th) != 1 || !is.finite(th) || th <= 0) {
        .stop("threshold must be a single positive number")
      }
    }
  }
  # Create decay parameters for binary catchment
  decay_params <- if (is.null(thresholds)) {
    list(method = "binary", sigma = threshold)
  } else {
    lapply(thresholds, function(th) list(method = "binary", sigma = th))
  }

  # Validation as for E2SFCA with binary catchment and no demand normalization
  if (!snap) {
    .chck_e2sfca(
      demand, supply, distance, decay_params,
      "identity", id_col, supply_cols
    )
  }

  # E2SFCA with binary catchment and no demand normalization, as the original
  # 2SFCA
  .fca_run(
    "2sfca", demand, supply, distance, decay_params,
    demand_normalize = "identity", id_col = id_col,
    supply_cols = supply_cols, snap = snap, call = call
  )
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
#'        steps (with demand groups, one list shared by every group, or a list
#'        of such lists named by the groups):
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
#'        distance is finite. With demand groups, shared or a list named by the
#'        groups; a group whose selection list is identical to its decay list
#'        reuses the decay weights.
#' @inheritSection spax_e2sfca Demand groups
#' @return A spax object (see \code{\link{spax_e2sfca}}) whose \code{type} is
#'         "3SFCA". Its \code{parameters} are the named list
#'         \code{decay_params}, \code{selection_params} (and \code{groups}
#'         and \code{layers} with demand groups).
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
  call <- match.call()
  # Validation (3SFCA takes the inputs of E2SFCA, without normalization)
  groups <- .fca_demand_groups(demand)
  if (!snap) {
    .chck_e2sfca(
      demand, supply, distance, decay_params,
      "identity", id_col, supply_cols
    )
    .chck_selection_params(selection_params, groups)
  }

  .fca_run(
    "3sfca", demand, supply, distance, decay_params,
    selection_params = selection_params, id_col = id_col,
    supply_cols = supply_cols, indicator_names = indicator_names, snap = snap,
    call = call
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
#' @inheritSection spax_e2sfca Demand groups
#' @param attractiveness Numeric vector of facility attractiveness with one
#'        finite, nonnegative value per facility, shared by the demand groups. A
#'        named vector is matched to the layer names of \code{distance} (with
#'        groups, of the first group's stack); an unnamed vector follows the
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
  call <- match.call()
  # Validation (Huff-2SFCA takes the inputs of E2SFCA, without normalization)
  groups <- .fca_demand_groups(demand)
  if (!snap) {
    .chck_e2sfca(
      demand, supply, distance, decay_params,
      "identity", id_col, supply_cols
    )
    .chck_selection_params(selection_params, groups)
    .chck_nonnegative_scalar(alpha, "alpha")
  }

  .fca_run(
    "huff2sfca", demand, supply, distance, decay_params,
    selection_params = selection_params, attractiveness = attractiveness,
    alpha = alpha, id_col = id_col, supply_cols = supply_cols,
    indicator_names = indicator_names, snap = snap, call = call
  )
}
