# SPAX-020 / SPAX-007: private compact-matrix FCA execution path -------------
#
# Engine-instance #1 of the platform (see notes "Spax Platform Vision"): the
# no-state, single-pass T_theta. The checked spax_field / terra layer decides
# meaning and geometry; this layer does the repeated numeric work on compact
# matrices. Public API is unchanged -- these are private, tests/benchmarks only.
#
# Atoms as opcodes (named in tensor terms on purpose, not fca_fast_step_*):
#   .k_scale           row/col scaling = broadcast (diagonal contract) == .ae_lift (broadcast half)
#   .k_normalize       choice/Huff split over J (3SFCA)             == .ae_normalize by cell axis
#   .k_contract        contract a vector vs a matrix over one margin == .ae_gather/.ae_spread/.ae_aggregate
#   .k_ratio           zero-safe elementwise division               == .ae_ratio
#   .k_rewrap_cells    write a value vector back onto a raster grid  == result rewrap
#
# Grouping is not a special case: a group-collapse / split / group-normalize is
# .k_contract / a broadcast against a 0/1 membership (incidence) matrix. `by =
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

# Kernel primitives ----------------------------------------------------------

#' Scale the rows or columns of a matrix by a vector (the `scale`/broadcast atom)
#'
#' `margin = "rows"`: row i is multiplied by `s[i]`; `"cols"`: column j by `s[j]`.
#' Row/column scaling is a contraction against a diagonal matrix -- the broadcast
#' half of the grammar. Dense/sparse-clean: a base matrix uses recycling; a
#' `Matrix::` matrix uses a `Diagonal()` multiply (sparse stays sparse).
#' @keywords internal
.k_scale <- function(K, s, margin = c("rows", "cols")) {
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

#' Row-normalize a kernel matrix by cell (choice/Huff split over facilities)
#'
#' Matrix form of `.ae_normalize(kernel, by = cell_axis, method = ...)`, written
#' as a diagonal row-scaling so it is dense/sparse-clean: each origin row is
#' scaled by 1/(row sum). `identity` is a no-op; `standard` zeros rows summing to
#' 0; `semi` only scales rows whose sum exceeds 1 (else leaves them unchanged).
#' @param K matrix `[I x J]` (base or Matrix; NA already zeroed by the extractor).
#' @keywords internal
.k_normalize <- function(K, method = "identity", a0 = 0) {
  if (method == "identity") {
    return(K)
  }
  rs <- rowSums(K, na.rm = TRUE) + a0
  factor <- switch(method,
    standard = ifelse(rs > 0, 1 / rs, 0),  # rows summing to 0 -> zeroed
    semi     = ifelse(rs > 1, 1 / rs, 1),  # only over-1 rows are scaled
    stop("unsupported .k_normalize method: ", method)
  )
  .k_scale(K, factor, margin = "rows")
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
.k_contract <- function(v, K, over = c("rows", "cols")) {
  over <- match.arg(over)
  if (over == "rows") {
    as.vector(crossprod(K, v))   # sum over dim 1; length = ncol(K)
  } else {
    as.vector(K %*% v)           # sum over dim 2; length = nrow(K)
  }
}

#' Zero-safe elementwise ratio (matrix form of `.ae_ratio`)
#' @keywords internal
.k_ratio <- function(num, denom, zero = 0) {
  out <- num / denom
  out[!is.finite(out)] <- zero
  out
}

#' Write a value vector back onto a raster template
#'
#' `kept_cell_index = NULL` means `values` already covers every template cell
#' (the FCA access surface is defined on the full grid). When the output itself
#' is compacted, pass the kept cell indices and unfilled cells become NA.
#' @keywords internal
.k_rewrap_cells <- function(values, template, kept_cell_index = NULL) {
  if (is.null(kept_cell_index)) {
    return(terra::setValues(template, values))
  }
  v <- rep(NA_real_, terra::ncell(template))
  v[kept_cell_index] <- values
  terra::setValues(template, v)
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
  measures <- if (is.null(indicator_names)) processed$cols else indicator_names

  D  <- terra::values(demand)[, 1]
  Kd <- terra::values(demand_kernel, mat = TRUE)   # [I x J], NA beyond reach
  Ka <- terra::values(access_kernel, mat = TRUE)   # [I x J]

  # gather operates on the active-demand rows only (compaction)
  keep <- which(is.finite(D) & D > 0)
  Kd0  <- Kd[keep, , drop = FALSE]
  Kd0[is.na(Kd0)] <- 0
  Kd0  <- .k_normalize(Kd0, method = demand_normalize)

  # spread keeps cells reachable by >=1 facility; cells unreachable by every
  # facility stay NA (matches compute_fca's na.rm aggregate, which yields NA for
  # an all-NA cell). Within a kept row, an NA facility contributes 0.
  access_kept <- which(rowSums(is.finite(Ka)) > 0)
  Ka0 <- Ka[access_kept, , drop = FALSE]
  Ka0[is.na(Ka0)] <- 0

  list(
    D_active          = D[keep],
    Kd_active         = Kd0,
    Ka_kept           = Ka0,
    S                 = as.matrix(processed$values),   # [J x measures]
    facility_ids      = ids,
    measures          = measures,
    demand_kept_index = keep,         # active-demand rows fed to the gather
    access_kept_index = access_kept,  # reachable cells the spread writes back
    template          = demand_kernel[[1]]
  )
}

#' Execute an FCA plan on the compact substrate (terra-free interpreter)
#'
#' `gather -> ratio -> spread` as matrix ops. Consumes only the plan's plain
#' matrices/vectors -- no terra -- and returns one access vector per measure
#' (over the plan's reachable cells). The kernels in `plan` may be dense base
#' matrices or `Matrix::` sparse matrices; this loop is identical either way,
#' which is what lets a non-terra / sparse front-end reuse it unchanged. This
#' is also the hot loop an AE fixed point would iterate (no rewrap inside).
#' @keywords internal
.compute_fca_plan <- function(plan) {
  U <- .k_contract(plan$D_active, plan$Kd_active, over = "rows")   # [J]
  lapply(seq_len(ncol(plan$S)), function(m) {
    R <- .k_ratio(plan$S[, m], U, zero = 0)                        # [J]
    .k_contract(R, plan$Ka_kept, over = "cols")                    # [reachable cells]
  })
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
    .k_rewrap_cells(A, plan$template, kept_cell_index = plan$access_kept_index)
  })
  out <- terra::rast(layers)
  names(out) <- plan$measures
  out
}
