# SPAX-020 / SPAX-007: private compact-matrix FCA execution path -------------
#
# Engine-instance #1 of the platform (see notes "Spax Platform Vision"): the
# no-state, single-pass T_theta. The checked spax_field / terra layer decides
# meaning and geometry; this layer does the repeated numeric work on compact
# matrices. Public API is unchanged -- these are private, tests/benchmarks only.
#
# Atoms as opcodes (named in tensor terms on purpose, not fca_fast_step_*):
#   .k_normalize       choice/Huff split over J (3SFCA)             == .ae_normalize by cell axis
#   .k_contract_left   gather:  U_j = sum_i w_i K_ij  = crossprod    == .ae_gather
#   .k_contract_right  spread:  A_i = sum_j K_ij r_j  = K %*% r      == .ae_spread
#   .k_ratio           zero-safe elementwise division               == .ae_ratio
#   .k_rewrap_cells    write a value vector back onto a raster grid  == result rewrap
#
# Storage seam: contracts are written with crossprod / %*%, which are generic
# over base dense matrices AND Matrix::dgCMatrix. The dense/sparse choice is a
# property of what the extractor emits, not of these primitives (see the
# kernel-density check: hospital ~69% dense, PHC ~13% -> sparse drop-in later).

# Kernel primitives ----------------------------------------------------------

#' Row-normalize a kernel matrix by cell (choice/Huff split over facilities)
#'
#' Matrix form of `.ae_normalize(kernel, by = cell_axis, method = ...)`: each
#' row (origin cell) is divided by its sum across facilities. `identity` is a
#' no-op; `standard` divides by the row sum (0 where the row sums to 0); `semi`
#' divides only where the row sum exceeds 1 (else leaves the row unchanged).
#' @param K numeric matrix `[I x J]` (NA already zeroed by the extractor).
#' @keywords internal
.k_normalize <- function(K, method = "identity", a0 = 0) {
  if (method == "identity") {
    return(K)
  }
  rs <- rowSums(K, na.rm = TRUE) + a0
  if (method == "standard") {
    out <- sweep(K, 1, ifelse(rs > 0, rs, 1), "/")
    out[rs <= 0, ] <- 0
    return(out)
  }
  if (method == "semi") {
    return(sweep(K, 1, ifelse(rs > 1, rs, 1), "/"))
  }
  stop("unsupported .k_normalize method: ", method)
}

#' Contract over the origin axis I (gather): `U_j = sum_i w_i K_ij`
#' @param w numeric vector over I (length `nrow(K)`).
#' @param K numeric matrix `[I x J]`.
#' @return numeric vector over J (length `ncol(K)`).
#' @keywords internal
.k_contract_left <- function(w, K) {
  as.vector(crossprod(K, w))
}

#' Contract over the facility axis J (spread): `A_i = sum_j K_ij r_j`
#' @param K numeric matrix `[I x J]`.
#' @param r numeric vector over J (length `ncol(K)`).
#' @return numeric vector over I (length `nrow(K)`).
#' @keywords internal
.k_contract_right <- function(K, r) {
  as.vector(K %*% r)
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

#' Run FCA over the compact matrix substrate (== compute_fca, no raster hot loop)
#'
#' `normalize -> gather -> ratio -> spread -> rewrap` as matrix ops. Equivalent
#' to `compute_fca()` within float tolerance; private, for tests/benchmarks.
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

  U <- .k_contract_left(plan$D_active, plan$Kd_active)        # [J]

  layers <- lapply(seq_len(ncol(plan$S)), function(m) {
    R <- .k_ratio(plan$S[, m], U, zero = 0)                  # [J]
    A <- .k_contract_right(plan$Ka_kept, R)                  # [reachable cells]
    .k_rewrap_cells(A, plan$template,
                    kept_cell_index = plan$access_kept_index)
  })

  out <- terra::rast(layers)
  names(out) <- plan$measures
  out
}
