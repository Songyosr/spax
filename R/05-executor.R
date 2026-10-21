# SPAX-020 / SPAX-007: private compact matrix executor ----------------------
#
# Engine-instance #1 of the platform (see notes "Spax Platform Vision"): the
# no-state, single-pass T_theta. The checked terra layer decides meaning and
# geometry; this layer does the repeated numeric work on compact
# matrices. Public API is unchanged -- these are private; the plan below serves
# the internal SAE/HAAE reference models.
#
# The atom catalog is documentation; a function exists only where there is a
# contract worth testing. Executor functions are promoted when the operation is:
# repeated AND (faster as a unit OR carries a guard/stability rule worth
# centralizing). Pointwise math without a guard stays as plain vectorized R.
#
# Executor catalog:
#   .contract      contract a vector/matrix over one margin via crossprod/%*%
#   .scale         row/col scaling; broadcast folds into this
#   .normalize     margin normalization; policy modes live in the planner
#   .rewrap_cells  compact vector -> raster boundary utility
#
# Grouping is not a special case: a group-collapse / split / group-normalize is
# .contract / a broadcast against a 0/1 membership (incidence) matrix. `by =
# <axis>` later == "contract against that axis's incidence matrix." So product
# axes (subgroup/mode/supply-class) reuse these primitives with an incidence
# operand -- no new atoms.
#
# Substrate: the compute (gather/ratio/spread) is plain crossprod/%*%/elementwise.
# terra is only the I/O skin -- the extractor (.fca_compact_plan) and the rewrap.
# The plan holds plain matrices, so a non-terra / sparse front-end can feed the
# same primitives unchanged.
#
# Storage seam: contracts are written with crossprod / %*%, which are generic
# over base dense matrices AND Matrix::dgCMatrix. The dense/sparse choice is a
# property of what the extractor emits, not of these primitives (see the
# kernel-density check: hospital ~69% dense, PHC ~13% -> sparse drop-in later).

# Executor primitives --------------------------------------------------------

#' Scale the rows or columns of a matrix by a vector (the `scale`/broadcast atom)
#'
#' `margin = "rows"`: row i is multiplied by `s[i]`; `"cols"`: column j by `s[j]`.
#' Row/column scaling is a contraction against a diagonal matrix -- the broadcast
#' half of the grammar. Dense/sparse-clean: a base matrix uses recycling; a
#' `Matrix::` matrix uses a `Diagonal()` multiply (sparse stays sparse).
#' @keywords internal
.scale <- function(K, s, margin = c("rows", "cols")) {
  margin <- match.arg(margin)
  if (is.matrix(K)) {
    if (margin == "rows") {
      return(K * s)                        # length(s) == nrow(K): scales each row
    }
    return(K * rep(s, each = nrow(K)))     # scales each column
  }
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("scaling a Matrix object needs the 'Matrix' package")
  }
  D <- Matrix::Diagonal(x = s)
  if (margin == "rows") D %*% K else K %*% D
}

#' Contract a vector against a matrix over one margin (the `contract` atom)
#'
#' `over = "rows"` sums over dim 1 (gather over origins I): result is one value
#' per column (J). `over = "cols"` sums over dim 2 (spread over facilities J):
#' result is one value per row (I). The same op expresses group-collapse when
#' `K` is a 0/1 membership/incidence matrix -- grouping is not a special case.
#'
#' `K` may be a base matrix OR a `Matrix::` sparse matrix: `crossprod`/`%*%` are
#' generic, so dense vs sparse is a property of the plan, not of this code. The
#' axis->margin mapping is the plan's job (a dense-array backend stores an
#' explicit axis-to-dimension map); this primitive speaks plain matrix margins.
#' @keywords internal
.contract <- function(v, K, over = c("rows", "cols")) {
  over <- match.arg(over)
  out <- if (over == "rows") {
    crossprod(K, v)              # sum over dim 1; rows = ncol(K)
  } else {
    K %*% v                      # sum over dim 2; rows = nrow(K)
  }
  if (NCOL(out) == 1L) {
    as.vector(out)
  } else {
    out
  }
}

#' Normalize a matrix over one margin
#'
#' `over = "cols"` sums columns and scales rows; `over = "rows"` sums rows and
#' scales columns. Policy choices such as "identity" and "semi" belong in the
#' planner; this executor always divides by the full margin sum with a zero-row
#' guard.
#' @keywords internal
.normalize <- function(x, over = c("rows", "cols")) {
  over <- match.arg(over)
  if (over == "cols") {
    denom <- rowSums(x, na.rm = TRUE)
    factor <- ifelse(denom > 0, 1 / denom, 0)
    return(.scale(x, factor, margin = "rows"))
  }
  denom <- colSums(x, na.rm = TRUE)
  factor <- ifelse(denom > 0, 1 / denom, 0)
  .scale(x, factor, margin = "cols")
}

#' Write a value vector back onto a raster template
#'
#' `kept_cell_index = NULL` means `values` already covers every template cell
#' (the FCA access surface is defined on the full grid). When the output itself
#' is compacted, pass the kept cell indices and unfilled cells become NA.
#' @keywords internal
.rewrap_cells <- function(values, template, kept_cell_index = NULL) {
  if (is.null(kept_cell_index)) {
    return(terra::setValues(template, values))
  }
  v <- rep(NA_real_, terra::ncell(template))
  v[kept_cell_index] <- values
  terra::setValues(template, v)
}

#' Apply FCA demand-normalization policy to the compact gather kernel
#' @keywords internal
.fca_normalize_kernel <- function(K, method = "identity") {
  method <- match.arg(method, c("identity", "standard", "semi"))
  if (method == "identity") {
    return(K)
  }
  if (method == "standard") {
    return(.normalize(K, over = "cols"))
  }
  rs <- rowSums(K, na.rm = TRUE)
  .scale(K, ifelse(rs > 1, 1 / rs, 1), margin = "rows")
}

# Execution plan (extractor) -------------------------------------------------

#' Compile FCA inputs into a compact numeric execution plan
#'
#' The semantic boundary: validate geometry once, then drop to plain matrices.
#' Active-demand compaction (`is.finite(D) & D > 0`) removes inactive origin
#' rows from the gather -- exact, since zero/NA-demand cells contribute nothing
#' to facility load. The plan also records an access-side compaction (cells
#' reachable by at least one facility) for a spread step; the SAE/HAAE plans
#' use the demand-side fields.
#'
#' Kernels are taken as `SpatRaster`s here (the proven oracle shape); the
#' boundary check mirrors the alignment contract of [compute_access()].
#' @keywords internal
.fca_compact_plan <- function(demand, supply, demand_kernel, access_kernel,
                              demand_normalize = "identity",
                              id_col = NULL, supply_cols = NULL,
                              indicator_names = NULL) {
  .chck_raster_alignment(demand_kernel[[1]], access_kernel[[1]],
                         "demand_kernel", "access_kernel")
  .chck_raster_alignment(demand, demand_kernel[[1]], "demand", "demand_kernel")

  ids <- names(demand_kernel)
  processed <- .help_process_supply(supply, id_col = id_col,
                                    supply_cols = supply_cols, weight_ids = ids)
  D  <- terra::values(demand, mat = TRUE)     # [I x demand batches]
  Kd <- terra::values(demand_kernel, mat = TRUE)   # [I x J], NA beyond reach
  Ka <- terra::values(access_kernel, mat = TRUE)   # [I x J]

  demand_batches <- names(demand)
  if (is.null(demand_batches)) {
    demand_batches <- paste0("demand_", seq_len(ncol(D)))
  }
  demand_is_batch <- ncol(D) > 1L
  supply_is_batch <- ncol(processed$values) > 1L
  if (demand_is_batch && supply_is_batch) {
    stop("batching both demand layers and supply measures needs explicit product-axis semantics")
  }
  measures <- if (is.null(indicator_names)) {
    if (demand_is_batch) demand_batches else processed$cols
  } else {
    indicator_names
  }

  # gather operates on the active-demand rows only (compaction)
  keep <- which(rowSums(is.finite(D) & D > 0) > 0)
  D0 <- D[keep, , drop = FALSE]
  D0[!is.finite(D0)] <- 0
  D0 <- if (ncol(D0) == 1L) D0[, 1] else D0
  Kd0  <- Kd[keep, , drop = FALSE]
  Kd0[is.na(Kd0)] <- 0
  Kd0  <- .fca_normalize_kernel(Kd0, method = demand_normalize)

  # spread keeps cells reachable by >=1 facility; cells unreachable by every
  # facility stay NA (the access of an all-NA cell is NA). Within a kept row, an
  # NA facility contributes 0.
  access_kept <- which(rowSums(is.finite(Ka)) > 0)
  Ka0 <- Ka[access_kept, , drop = FALSE]
  Ka0[is.na(Ka0)] <- 0

  list(
    D_active          = D0,
    Kd_active         = Kd0,
    Ka_kept           = Ka0,
    S                 = as.matrix(processed$values),   # [J x measures]
    facility_ids      = ids,
    measures          = measures,
    demand_batches    = demand_batches,
    demand_kept_index = keep,         # active-demand rows fed to the gather
    access_kept_index = access_kept,  # reachable cells the spread writes back
    template          = demand_kernel[[1]]
  )
}

# Block FCA engine (SPAX-055) -----------------------------------------------
#
# The FCA wrappers run on this engine: the rasters are read in row blocks into
# plain matrices, and the same contraction primitives as above do the work in
# two passes.

#' Facility IDs of a raster stack
#'
#' The layer names, or `facility_1..J` by position when the names are terra
#' defaults (`lyr.N`) or missing. Duplicate IDs are an error.
#' @keywords internal
.fca_ids <- function(x, arg = "distance") {
  .chck_class(x, "SpatRaster", arg)
  layer_names <- names(x)
  semantic <- !(is.null(layer_names) || any(is.na(layer_names)) ||
                  any(layer_names == "")) &&
    !all(grepl("^lyr\\.[0-9]+$", layer_names))
  ids <- if (semantic) layer_names else paste0("facility_", seq_len(terra::nlyr(x)))
  if (anyDuplicated(ids)) {
    stop("index$layer contains duplicate coordinate tuples")
  }
  ids
}

# Whether two SpatRasters are one and the same object (the same C++ object
# behind them). identical() cannot be used: terra defines it for two
# SpatRasters as a comparison of their values, which reads both rasters and is
# FALSE when they contain missing cells, even for the same object.
.same_raster <- function(x, y) {
  px <- .raster_pointer(x)
  !is.null(px) && identical(px, .raster_pointer(y))
}

# The C++ object behind a SpatRaster. terra keeps it in an internal slot
# ("pntr" in terra 1.8); a test checks that a raster is recognized as itself, so
# a change of that slot in terra shows up there rather than in user code.
.raster_pointer <- function(x) {
  tryCatch(x@pntr, error = function(e) NULL)
}

#' Row blocks for reading a list of rasters
#'
#' Plans the blocks without opening anything. `same` maps each element to the
#' name of its first element that is the same raster object, so a raster passed
#' twice is read (and counted) once. A block holds at most `max_values` values over the
#' distinct rasters, and at least one row.
#' @param rasters Named list of SpatRasters with the same number of rows and
#'   columns.
#' @param max_values Values per block over all layers read.
#' @return `list(starts, n, ncol, same)`: first row and number of rows of each
#'   block, columns per row, and the alias map.
#' @keywords internal
.raster_blocks <- function(rasters,
                           max_values = getOption("spax.fca_block_values", 2e6)) {
  nms <- names(rasters)
  same <- stats::setNames(nms, nms)
  for (i in seq_along(rasters)[-1L]) {
    for (j in seq_len(i - 1L)) {
      if (.same_raster(rasters[[j]], rasters[[i]])) {
        same[i] <- nms[j]
        break
      }
    }
  }
  distinct <- unique(unname(same))
  layers <- sum(vapply(rasters[distinct], terra::nlyr, numeric(1)))
  nr <- terra::nrow(rasters[[1L]])
  nc <- terra::ncol(rasters[[1L]])
  per <- min(nr, max(1, floor(max_values / (nc * layers))))
  starts <- seq.int(1L, nr, by = as.integer(per))
  list(starts = starts,
       n = as.integer(pmin(per, nr - starts + 1L)),
       ncol = as.integer(nc),
       same = same)
}

#' Open the distinct rasters of a list for block reading
#'
#' Returns a function that closes them again. Call it as
#' `close <- .raster_open(rasters, same); on.exit(close(), add = TRUE)`; if
#' one raster fails to open, the ones already opened are closed.
#' @keywords internal
.raster_open <- function(rasters, same) {
  distinct <- unique(unname(same))
  opened <- character()
  close_all <- function() {
    for (nm in opened) {
      try(terra::readStop(rasters[[nm]]), silent = TRUE)
    }
    opened <<- character()
    invisible(NULL)
  }
  tryCatch(
    for (nm in distinct) {
      terra::readStart(rasters[[nm]])
      opened <- c(opened, nm)
    },
    error = function(e) {
      close_all()
      stop(e)
    }
  )
  close_all
}

#' Read one block of rows from every raster of a list
#'
#' Returns a named list of numeric matrices with `n * ncol` rows (cell index
#' of row k is `(start - 1) * ncol + k`) and one column per layer. Each
#' distinct raster is read once and shared with its aliases.
#' @keywords internal
.raster_read <- function(rasters, same, start, n, only = names(same)) {
  same <- same[only]
  distinct <- unique(unname(same))
  values <- lapply(stats::setNames(distinct, distinct), function(nm) {
    x <- rasters[[nm]]
    # terra returns the values layer by layer, which is the column order of a cells
    # x layers matrix: setting the dimensions avoids the copy that mat = TRUE makes.
    v <- terra::readValues(x, start, n, 1, terra::ncol(x))
    dim(v) <- c(n * terra::ncol(x), terra::nlyr(x))
    v
  })
  stats::setNames(values[unname(same)], names(same))
}

# The rows of one kernel of a kernel specification, checked for shape. D1
# (SPAX-055) is checked inside the kernel closures, where the weights are
# computed and the finite-travel mask is known, so it also applies under `snap`.
.fca_kernel_rows <- function(kernel, b, rows, J) {
  K <- kernel(b, rows)
  if (!is.matrix(K) || nrow(K) != length(rows) || ncol(K) != J) {
    stop("a kernel must return one row per cell and one column per facility")
  }
  K
}

#' Combine lists of named rasters, keeping one raster per key
#'
#' A key that occurs twice must name one and the same raster object (see
#' `.same_raster()`), which is then read once; two different rasters under one
#' key are an internal error, since a kernel would read the wrong one.
#' @keywords internal
.fca_merge_rasters <- function(rasters) {
  nms <- names(rasters)
  first <- match(nms, nms)
  for (i in which(first != seq_along(nms))) {
    if (!.same_raster(rasters[[first[i]]], rasters[[i]])) {
      stop("internal error: the raster key '", nms[i],
           "' names two different rasters", call. = FALSE)
    }
  }
  rasters[first == seq_along(nms)]
}

#' Two-pass block FCA engine
#'
#' Pass 1 reads each block, takes for each demand group the cells with finite
#' positive demand, evaluates the group's demand kernel on those rows, replaces
#' missing weights by 0, applies the demand normalization and accumulates the
#' demand of all groups that reaches each facility, `U`. The ratio is
#' `R = supply / U` (0 where not finite). Pass 2 evaluates the access kernel of
#' each group on every row of each block and writes `A = K R` for the rows with
#' at least one finite kernel value; the other cells stay NA. Without groups
#' this is the one-group case of the same loops.
#'
#' With `groups` the output also holds an overall layer per measure, computed in
#' pass 2 from the demand blocks: with `A*` equal to the group's access, or 0
#' where the group cannot reach the cell, the overall access of a cell is
#' `sum(D_g A*_g) / sum(D_g)` over the groups with positive demand. It is NA where
#' any group's demand is NA or the total demand is 0. Only demand that is finite
#' and positive counts (under `snap`, negative or infinite demand is ignored as
#' in the group layers).
#' @param rasters Named list of SpatRasters read in blocks; it must contain the
#'   demand rasters (key `demand`, or `demand.<group>` with `groups`; one layer
#'   each) and whatever the kernels read. A key may not name two different
#'   rasters (see `.fca_merge_rasters()`).
#' @param kernels `list(demand = function(b, rows), access = function(b, rows))`.
#'   `b` is the list of block matrices from `.raster_read()` and `rows` the
#'   block rows to compute; each returns a `length(rows) x J` matrix in
#'   facility-ID order. With `groups`, a list of such lists named by the group
#'   IDs.
#' @param supply Numeric J x M matrix in facility-ID order.
#' @param normalize Demand normalization of the gather kernel.
#' @param snap When FALSE, negative or infinite demand is refused (D2); when
#'   TRUE such cells are not active. Weight checks (D1) live in the kernels.
#' @param groups `NULL` for one ungrouped demand raster, or the character
#'   vector of group IDs (also for a single group).
#' @param block `NULL` (the rasters are read in blocks), or the whole input as
#'   one in-memory block: a list with the keys that `rasters` would hold (`demand`,
#'   or `demand.<group>` with `groups`, and whatever the kernels read, such as
#'   `distance`), each a matrix with one row per origin and one column per layer
#'   (a plain vector is one column). `rasters` is then ignored and nothing is
#'   opened or read; both passes, the NA rule and the overall layer are the same
#'   code as for rasters (SPAX-060, FCA on numeric data).
#' @return A matrix with one row per cell of the grid (per origin of `block`).
#'   Ungrouped, one column
#'   per supply measure. Grouped, `M * (G + 1)` columns in measure-major order:
#'   for each measure the groups in the order of `groups`, then the overall
#'   layer.
#' @keywords internal
.fca_blocks <- function(rasters, kernels, supply, normalize = "identity",
                        snap = FALSE, groups = NULL, block = NULL) {
  supply <- as.matrix(supply)
  J <- nrow(supply)
  M <- ncol(supply)
  grouped <- !is.null(groups)
  if (grouped) {
    if (anyDuplicated(groups) || !setequal(names(kernels), groups)) {
      stop("kernels must be a list with one element per group")
    }
    kernels <- kernels[groups]
    dkeys <- paste0("demand.", groups)
  } else {
    kernels <- list(kernels)
    dkeys <- "demand"
  }
  G <- length(dkeys)
  # Output columns of group g (and the overall layer, g = G + 1) per measure.
  per <- if (grouped) G + 1L else 1L
  cols_of <- function(g) (seq_len(M) - 1L) * per + g

  if (is.null(block)) {
    blocks <- .raster_blocks(rasters)
    rest <- setdiff(names(blocks$same), dkeys)
    close <- .raster_open(rasters, blocks$same)
    on.exit(close(), add = TRUE)

    # A grid that fits in one block is read once and used by both passes.
    single <- length(blocks$starts) == 1L
    cached <- if (single) .raster_read(rasters, blocks$same, blocks$starts, blocks$n)
    n_cells <- terra::ncell(rasters[[1L]])
  } else {
    # The input is in memory: one block of all origins, used by both passes.
    block <- .fca_memory_block(block, dkeys)
    n_cells <- nrow(block[[dkeys[1L]]])
    blocks <- list(starts = 1L, n = as.integer(n_cells), ncol = 1L)
    rest <- setdiff(names(block), dkeys)
    single <- TRUE
    cached <- block
  }

  # Pass 1: the demand that reaches each facility, from all groups.
  U <- numeric(J)
  for (k in seq_along(blocks$starts)) {
    # Demand first: a block with no demand needs no travel values in this pass.
    Dk <- if (single) cached[dkeys] else
      .raster_read(rasters, blocks$same, blocks$starts[k], blocks$n[k],
                   only = dkeys)
    b <- if (single) cached
    for (g in seq_len(G)) {
      D <- Dk[[dkeys[g]]][, 1L]
      active <- which(is.finite(D) & D > 0)
      # D2 (SPAX-055): negative or infinite demand is refused (all rows of the
      # block) unless `snap`, where such cells are simply not active.
      if (!snap && any(D < 0 | is.infinite(D), na.rm = TRUE)) {
        stop("demand must be finite and nonnegative; use NA for unknown",
             call. = FALSE)
      }
      if (!length(active)) next
      if (is.null(b)) {
        b <- c(Dk, .raster_read(rasters, blocks$same, blocks$starts[k],
                                blocks$n[k], only = rest))
      }
      Kd <- .fca_kernel_rows(kernels[[g]]$demand, b, active, J)
      Kd[is.na(Kd)] <- 0
      Kd <- .fca_normalize_kernel(Kd, normalize)
      U <- U + .contract(D[active], Kd, "rows")
    }
  }
  R <- supply / U
  R[!is.finite(R)] <- 0

  # Pass 2: spread the ratios back to the cells that reach a facility; with
  # groups, also the overall layer, which needs the demand blocks.
  out <- matrix(NA_real_, n_cells, M * per)
  for (k in seq_along(blocks$starts)) {
    b <- if (single) cached else
      .raster_read(rasters, blocks$same, blocks$starts[k], blocks$n[k],
                   only = if (grouped) names(blocks$same) else rest)
    nb <- blocks$n[k] * blocks$ncol
    rows <- seq_len(nb)
    first <- (as.numeric(blocks$starts[k]) - 1) * blocks$ncol
    if (grouped) {
      num <- matrix(0, nb, M)       # sum of D_g A*_g over the groups
      total <- numeric(nb)          # sum of D_g
      unknown <- logical(nb)        # a group's demand is missing
    }
    for (g in seq_len(G)) {
      Ka <- .fca_kernel_rows(kernels[[g]]$access, b, rows, J)
      reach <- which(rowSums(is.finite(Ka)) > 0)
      A <- NULL
      if (length(reach)) {
        K <- Ka[reach, , drop = FALSE]
        K[is.na(K)] <- 0
        A <- as.matrix(.contract(R, K, "cols"))
        out[first + reach, cols_of(g)] <- A
      }
      if (grouped) {
        D <- b[[dkeys[g]]][, 1L]
        unknown <- unknown | is.na(D)
        positive <- is.finite(D) & D > 0
        total[positive] <- total[positive] + D[positive]
        # A cell the group cannot reach counts with access 0 (decision D1); a
        # group without demand in the cell is skipped, not multiplied by NA.
        hit <- which(positive[reach])
        if (length(hit)) {
          cells <- reach[hit]
          num[cells, ] <- num[cells, , drop = FALSE] +
            D[cells] * A[hit, , drop = FALSE]
        }
      }
    }
    if (grouped) {
      overall <- num / total
      overall[unknown | total == 0, ] <- NA
      out[first + rows, cols_of(G + 1L)] <- overall
    }
  }
  out
}

# The in-memory block of .fca_blocks(): a named list of matrices with one row per
# origin; a plain vector is one column. The demand keys must be there.
.fca_memory_block <- function(block, dkeys) {
  if (!is.list(block) || is.null(names(block)) || anyDuplicated(names(block))) {
    stop("internal error: a block must be a list with unique names", call. = FALSE)
  }
  missing <- setdiff(dkeys, names(block))
  if (length(missing)) {
    stop("internal error: the block has no '", missing[1L], "'", call. = FALSE)
  }
  block <- lapply(block, function(x) {
    if (is.null(dim(x))) matrix(x, ncol = 1L) else x
  })
  rows <- vapply(block, nrow, numeric(1))
  if (any(rows != rows[[1L]])) {
    stop("internal error: every element of a block needs one row per origin",
         call. = FALSE)
  }
  block
}

#' Layer names and table of an FCA result
#'
#' `measures` are the supply measure (indicator) names and `groups` the group
#' IDs, or `NULL` when ungrouped. Grouped layers are named `<measure>.<group>`
#' and `<measure>.all`, in measure-major order (measure 1: the groups in group
#' order, then `all`; measure 2: ...). The table has one row per layer with the
#' columns `layer`, `measure` and `group` (`"all"` for the overall layer);
#' ungrouped, the layers are the measures and `group` is `NA`.
#' @keywords internal
.fca_layer_table <- function(measures, groups = NULL) {
  if (is.null(groups)) {
    return(data.frame(layer = measures, measure = measures,
                      group = NA_character_, stringsAsFactors = FALSE))
  }
  g <- c(groups, "all")
  measure <- rep(measures, each = length(g))
  group <- rep(g, times = length(measures))
  data.frame(layer = paste(measure, group, sep = "."), measure = measure,
             group = group, stringsAsFactors = FALSE)
}

#' Put a cell-by-measure matrix on a raster grid, one layer per column
#' @keywords internal
.fca_rewrap <- function(values, template, names) {
  out <- terra::rast(template, nlyrs = ncol(values))
  out <- terra::setValues(out, values)
  names(out) <- names
  out
}
