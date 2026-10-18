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

#' Two-pass block FCA engine
#'
#' Pass 1 reads each block, takes the cells with finite positive demand,
#' evaluates the demand kernel on those rows, replaces missing weights by 0,
#' applies the demand normalization and accumulates the demand that reaches
#' each facility, `U`. The ratio is `R = supply / U` (0 where not finite).
#' Pass 2 evaluates the access kernel on every row of each block and writes
#' `A = K R` for the rows with at least one finite kernel value; the other
#' cells stay NA.
#' @param rasters Named list of SpatRasters read in blocks; it must contain
#'   `demand` (one layer) and whatever the kernels read.
#' @param kernels `list(demand = function(b, rows), access = function(b, rows))`.
#'   `b` is the list of block matrices from `.raster_read()` and `rows` the
#'   block rows to compute; each returns a `length(rows) x J` matrix in
#'   facility-ID order.
#' @param supply Numeric J x M matrix in facility-ID order.
#' @param normalize Demand normalization of the gather kernel.
#' @param snap When FALSE, negative or infinite demand is refused (D2); when
#'   TRUE such cells are not active. Weight checks (D1) live in the kernels.
#' @return A matrix with one row per cell of the grid and one column per
#'   supply measure.
#' @keywords internal
.fca_blocks <- function(rasters, kernels, supply, normalize = "identity",
                        snap = FALSE) {
  supply <- as.matrix(supply)
  J <- nrow(supply)
  blocks <- .raster_blocks(rasters)
  close <- .raster_open(rasters, blocks$same)
  on.exit(close(), add = TRUE)

  # A grid that fits in one block is read once and used by both passes.
  single <- length(blocks$starts) == 1L
  cached <- if (single) .raster_read(rasters, blocks$same, blocks$starts, blocks$n)

  # Pass 1: the demand that reaches each facility.
  U <- numeric(J)
  for (k in seq_along(blocks$starts)) {
    # Demand first: a block with no demand needs no travel values in this pass.
    D <- if (single) cached$demand[, 1L] else
      .raster_read(rasters, blocks$same, blocks$starts[k], blocks$n[k],
                   only = "demand")$demand[, 1L]
    active <- which(is.finite(D) & D > 0)
    # D2 (SPAX-055): negative or infinite demand is refused (all rows of the
    # block) unless `snap`, where such cells are simply not active.
    if (!snap && any(D < 0 | is.infinite(D), na.rm = TRUE)) {
      stop("demand must be finite and nonnegative; use NA for unknown",
           call. = FALSE)
    }
    if (!length(active)) next
    if (single) {
      b <- cached
    } else {
      b <- .raster_read(rasters, blocks$same, blocks$starts[k], blocks$n[k],
                        only = setdiff(names(blocks$same), "demand"))
      b$demand <- matrix(D)
    }
    Kd <- .fca_kernel_rows(kernels$demand, b, active, J)
    Kd[is.na(Kd)] <- 0
    Kd <- .fca_normalize_kernel(Kd, normalize)
    U <- U + .contract(D[active], Kd, "rows")
  }
  R <- supply / U
  R[!is.finite(R)] <- 0

  # Pass 2: spread the ratios back to the cells that reach a facility.
  out <- matrix(NA_real_, terra::ncell(rasters[[1L]]), ncol(supply))
  for (k in seq_along(blocks$starts)) {
    # Demand is not needed to spread the ratios.
    b <- if (single) cached else
      .raster_read(rasters, blocks$same, blocks$starts[k], blocks$n[k],
                   only = setdiff(names(blocks$same), "demand"))
    rows <- seq_len(blocks$n[k] * blocks$ncol)
    Ka <- .fca_kernel_rows(kernels$access, b, rows, J)
    reach <- which(rowSums(is.finite(Ka)) > 0)
    if (!length(reach)) next
    A <- Ka[reach, , drop = FALSE]
    A[is.na(A)] <- 0
    cells <- (as.numeric(blocks$starts[k]) - 1) * blocks$ncol + reach
    out[cells, ] <- .contract(R, A, "cols")
  }
  out
}

#' Put a cell-by-measure matrix on a raster grid, one layer per column
#' @keywords internal
.fca_rewrap <- function(values, template, names) {
  out <- terra::rast(template, nlyrs = ncol(values))
  out <- terra::setValues(out, values)
  names(out) <- names
  out
}
