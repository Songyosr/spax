# Allocation models on raster data: the cells become origins (SPAX-060, V3) ----------------

# Raster data (prep_data() with a SpatRaster as demand) holds the grids. An allocation model
# works on compact numeric inputs, so spax() converts the data on each call: the cells with
# finite, positive demand are the origins, with their travel to every facility. Nothing is
# cached, so the conversion is the same every time and a result does not depend on a live
# raster (see decision E2 of the design note).

# The prepared inputs (`ae_prepared_inputs`) of an allocation specification on raster data.
#
# * Demand is read block by block (.raster_blocks()), and the D2 rule of the FCA engine
#   applies: negative or infinite demand is an error. The origins are the cells with finite,
#   positive demand; with demand groups, the cells whose total demand is finite and positive,
#   so a cell with demand missing in a group is not active (an origin with unknown group
#   demand is unknown as a whole, as in numeric group preparation).
# * The travel of the origins is read block by block as well, only for the blocks that
#   have origins, never over all cells and layers at once.
# * Origin IDs are the cell numbers of the grid as integers' text ("100000", not "1e+05"),
#   the registry holds the active cells only (counts at other cells are outside the data),
#   and `spatial` records the grid and the cell of every origin.
# * Facility IDs come from the data (layer names, or facility_1.. for default names); supply
#   must be one measure.
# * Travel is NA or infinite for a pair that is absent (`missing_travel = "legacy_zero"`);
#   an active cell that reaches no facility is kept; a CLM sends its demand to the outside
#   option, and a Huff model (no outside option) allocates none of it.
#
# The inputs are recorded as from raster data with the strict kernel domain of numeric
# preparation: input_policy$source is "raster" and kernel_domain "strict".
.raster_to_origins <- function(data, spec) {
  if (!inherits(data, "spax_raster_data") || !inherits(spec, "spax_allocation_spec")) {
    stop("internal error: .raster_to_origins() needs raster data and an allocation ",
         "specification", call. = FALSE)
  }
  groups <- data$groups
  facility_ids <- data$facility_ids
  grid <- if (is.null(groups)) data$demand else data$demand[[1L]]
  if (terra::ncell(grid) > .Machine$integer.max) {
    stop("the grid has too many cells", call. = FALSE)
  }

  # One supply measure, checked before any raster is read.
  supply <- .help_process_supply(data$supply, id_col = data$id_col,
                                 supply_cols = data$supply_cols, weight_ids = facility_ids)$values
  if (ncol(supply) != 1L) {
    stop("the allocation model uses one supply measure: choose it with ",
         "prep_data(supply_cols = )", call. = FALSE)
  }
  supply <- stats::setNames(as.numeric(supply[, 1L]), facility_ids)

  active <- .raster_active_cells(data$demand, groups)
  keep <- active$cells
  ids <- as.character(keep)
  travel <- .raster_travel_rows(data$travel, groups, keep, facility_ids, ids)

  if (is.null(groups)) {
    demand <- stats::setNames(active$demand[, 1L], ids)
  } else {
    demand <- lapply(stats::setNames(groups, groups), function(g) {
      stats::setNames(active$demand[, g], ids)
    })
  }
  prepared <- .prepare_allocation(
    demand = demand, supply = supply, distance = travel, origin_ids = ids,
    facility_ids = facility_ids, missing_travel = "legacy_zero",
    spatial = list(template = grid, cell_index = keep), metadata = data$metadata
  )
  prepared$input_policy$source <- "raster"
  prepared
}

# The active cells of the demand: `cells` (integer cell numbers, increasing) and `demand`
# (a matrix with one row per cell and one column per group, or one column ungrouped, named
# by the groups). The demand rasters are read in row blocks. A cell is active when its
# total demand is finite and positive (a group with missing demand makes the total missing).
.raster_active_cells <- function(demand, groups) {
  rasters <- if (is.null(groups)) {
    list(demand = demand)
  } else {
    stats::setNames(demand[groups], paste0("demand.", groups))
  }
  blocks <- .raster_blocks(rasters)
  close <- .raster_open(rasters, blocks$same)
  on.exit(close(), add = TRUE)
  cells <- vector("list", length(blocks$starts))
  values <- vector("list", length(blocks$starts))
  for (k in seq_along(blocks$starts)) {
    read <- .raster_read(rasters, blocks$same, blocks$starts[k], blocks$n[k])
    D <- do.call(cbind, unname(lapply(read, function(m) m[, 1L])))
    # D2: negative or infinite demand is refused, as by the FCA engine.
    if (any(D < 0 | is.infinite(D), na.rm = TRUE)) {
      stop("demand must be finite and nonnegative; use NA for unknown", call. = FALSE)
    }
    total <- if (ncol(D) == 1L) D[, 1L] else rowSums(D)
    hit <- which(is.finite(total) & total > 0)
    first <- (blocks$starts[k] - 1L) * blocks$ncol
    cells[[k]] <- first + hit
    values[[k]] <- D[hit, , drop = FALSE]
  }
  close()
  cells <- as.integer(unlist(cells))
  if (!length(cells)) {
    stop("no cell has positive demand", call. = FALSE)
  }
  demand <- do.call(rbind, values)
  colnames(demand) <- if (is.null(groups)) "demand" else groups
  list(cells = cells, demand = demand)
}

# The travel of the cells `keep` (increasing cell numbers) to the facilities: one matrix
# with the origin IDs `ids` as rows and the facility IDs as columns, or with a stack per
# demand group a list of such matrices by group (a stack shared by groups is read once).
# The stacks are read in row blocks and only the rows of `keep` are copied.
.raster_travel_rows <- function(travel, groups, keep, facility_ids, ids) {
  per_group <- !inherits(travel, "SpatRaster")
  stacks <- if (per_group) {
    stats::setNames(travel[groups], paste0("travel.", groups))
  } else {
    list(travel = travel)
  }
  blocks <- .raster_blocks(stacks)
  close <- .raster_open(stacks, blocks$same)
  on.exit(close(), add = TRUE)
  distinct <- unique(unname(blocks$same))
  rows <- stats::setNames(lapply(distinct, function(key) {
    matrix(NA_real_, length(keep), length(facility_ids))
  }), distinct)
  for (k in seq_along(blocks$starts)) {
    first <- (blocks$starts[k] - 1L) * blocks$ncol
    last <- first + blocks$n[k] * blocks$ncol
    # The kept cells of this block (`keep` is increasing): positions lo:hi.
    lo <- findInterval(first, keep) + 1L
    hi <- findInterval(last, keep)
    if (hi < lo) next
    hit <- lo:hi
    read <- .raster_read(stacks, blocks$same, blocks$starts[k], blocks$n[k])
    for (key in distinct) {
      rows[[key]][hit, ] <- read[[key]][keep[hit] - first, , drop = FALSE]
    }
  }
  close()
  for (key in distinct) {
    dimnames(rows[[key]]) <- list(ids, facility_ids)
  }
  if (!per_group) {
    return(rows[[1L]])
  }
  stats::setNames(rows[unname(blocks$same[paste0("travel.", groups)])], groups)
}
