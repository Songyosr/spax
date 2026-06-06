# SPAX-020 / SPAX-007: private compact matrix executor ----------------------
#
# Engine-instance #1 of the platform (see notes "Spax Platform Vision"): the
# no-state, single-pass T_theta. The checked spax_field / terra layer decides
# meaning and geometry; this layer does the repeated numeric work on compact
# matrices. Public API is unchanged -- these are private, tests/benchmarks only.
#
# The atom catalog is documentation; a function exists only where there is a
# contract worth testing. Executor functions are promoted when the operation is:
# repeated AND (faster as a unit OR carries a guard/stability rule worth
# centralizing). Pointwise math without a guard stays as plain vectorized R.
#
# Executor catalog:
#   .contract      contract a vector/matrix over one margin via crossprod/%*%
#   .scale         row/col scaling; broadcast folds into this
#   .safe_ratio    zero-safe guarded division
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
# The terra-free core (.compute_fca_plan) consumes a plan of plain matrices, so a
# non-terra / sparse front-end can feed the same executor unchanged.
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

#' Zero-safe elementwise ratio (matrix form of `.ae_ratio`)
#' @keywords internal
.safe_ratio <- function(num, denom, zero = 0) {
  out <- num / denom
  out[!is.finite(out)] <- zero
  out
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
#' to facility load. The spread keeps cells reachable by at least one facility
#' (cells unreachable by all stay NA, matching compute_fca).
#'
#' Kernels are taken as `SpatRaster`s here (the proven oracle shape); the
#' boundary check mirrors `compute_fca()`'s alignment contract.
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
  # facility stay NA (matches compute_fca's na.rm aggregate, which yields NA for
  # an all-NA cell). Within a kept row, an NA facility contributes 0.
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

#' Execute an FCA plan on the compact substrate (terra-free interpreter)
#'
#' `gather -> ratio -> spread` as matrix ops. Consumes only the plan's plain
#' matrices/vectors -- no terra -- and returns one access vector per measure
#' (over the plan's reachable cells). Multiple supply measures are an
#' independent batch axis: `S[J, M]` is divided by the shared `U[J]`, then spread
#' with one matrix-matrix contraction `Ka %*% R`. The kernels in `plan` may be
#' dense base matrices or `Matrix::` sparse matrices; the same code path handles
#' both. This is also the hot loop an AE fixed point would iterate (no rewrap
#' inside).
#' @keywords internal
.compute_fca_plan <- function(plan) {
  U <- .contract(plan$D_active, plan$Kd_active, over = "rows")   # [J]
  if (!is.null(dim(U))) {
    S <- matrix(plan$S[, 1], nrow = nrow(U), ncol = ncol(U))
    R <- .safe_ratio(S, U, zero = 0)                                # [J x demand batches]
  } else {
    R <- .safe_ratio(plan$S, U, zero = 0)                            # [J x measures]
  }
  A <- plan$Ka_kept %*% R                                          # [reachable cells x measures]
  lapply(seq_len(ncol(A)), function(m) as.vector(A[, m]))
}

#' Run FCA over the compact matrix substrate (== compute_fca, no raster hot loop)
#'
#' terra front-end (extract) -> terra-free executor -> terra back-end (rewrap).
#' Equivalent to `compute_fca()` within float tolerance; private, tests/benchmarks.
#' @keywords internal
.compute_fca_matrix <- function(demand, supply, demand_kernel, access_kernel,
                                demand_normalize = "identity",
                                id_col = NULL, supply_cols = NULL,
                                indicator_names = NULL) {
  plan <- .fca_compact_plan(
    demand, supply, demand_kernel, access_kernel,
    demand_normalize = demand_normalize, id_col = id_col,
    supply_cols = supply_cols, indicator_names = indicator_names
  )

  access <- .compute_fca_plan(plan)                              # terra-free core

  layers <- lapply(access, function(A) {
    .rewrap_cells(A, plan$template, kept_cell_index = plan$access_kept_index)
  })
  out <- terra::rast(layers)
  names(out) <- plan$measures
  out
}
