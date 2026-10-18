# Shared fixtures and a base-R oracle for the legacy FCA, field, raster-tool
# and PMF tests. Every value is fixed; nothing here draws random numbers.

# A small raster with fixed values. `vals` fills cells row by row, one layer
# after another (terra order) and is recycled to fill every cell. The default
# grid is terra's global lon/lat extent, so no PROJ lookup is needed.
toy_rast <- function(vals, nrows = 3, ncols = 3, nlyr = 1, names = NULL) {
  r <- terra::rast(nrows = nrows, ncols = ncols, nlyrs = nlyr)
  terra::values(r) <- rep_len(vals, nrows * ncols * nlyr)
  if (!is.null(names)) {
    names(r) <- names
  }
  r
}

# An n-layer 2 x 2 raster whose layer i holds the value i, for spax_field
# tests. Without `names` the layers keep terra's positional names (lyr.1, ...).
field_rast <- function(n = 1, names = NULL) {
  toy_rast(rep(seq_len(n), each = 4), nrows = 2, ncols = 2, nlyr = n,
           names = names)
}

# The layer index of a raster field or the node index of a vector field.
field_index_frame <- function(field) {
  if (inherits(field, "spax_raster_field")) .field_layer_index(field)
  else .field_node_index(field)
}

# The 3 x 3 FCA toy: demand 10..90 and two facilities whose distances run
# 1..5 from opposite corners. Supply comes as a data frame, a matrix and a
# named vector with the same values.
fca_toy <- function() {
  ids <- c("facility1", "facility2")
  list(
    demand = toy_rast(seq(10, 90, by = 10)),
    distance = toy_rast(c(1, 2, 3, 2, 3, 4, 3, 4, 5,
                          5, 4, 3, 4, 3, 2, 3, 2, 1),
                        nlyr = 2, names = ids),
    supply_df = data.frame(id = ids, doctors = c(10, 15), nurses = c(20, 25)),
    supply_matrix = matrix(c(10, 15, 20, 25), nrow = 2,
                           dimnames = list(ids, c("doctors", "nurses"))),
    supply_vector = c(facility1 = 10, facility2 = 15)
  )
}

# Floating catchment algebra written out in base R, for checking package
# results without package kernels, normalization or contraction.
# `D` is a demand vector over cells, `Wd`/`Wa` are cell-by-facility weight
# matrices (NA = unreachable) and `S` is a facility vector or a
# facility-by-measure matrix. Returns a cell-by-measure matrix:
#   Wd normalized per cell ("standard": divide by the row sum; "semi": only
#   where the row sum exceeds 1), U = colSums(D * Wd), R = S / U (0 where U
#   is 0), A = Wa %*% R, NA where a cell reaches no facility.
fca_oracle <- function(D, Wd, Wa = Wd, S, normalize = "identity") {
  S <- as.matrix(S)
  rs <- rowSums(Wd, na.rm = TRUE)
  Wd <- switch(normalize,
    identity = Wd,
    standard = Wd * ifelse(rs > 0, 1 / rs, 0),
    semi = Wd / ifelse(rs > 1, rs, 1)
  )
  U <- colSums(D * Wd, na.rm = TRUE)
  R <- S / U
  R[!is.finite(R)] <- 0
  Wa0 <- Wa
  Wa0[is.na(Wa0)] <- 0
  A <- Wa0 %*% R
  A[rowSums(!is.na(Wa)) == 0, ] <- NA
  unname(A)
}

# Run `fun()` under the field engine and under the block engine of
# SPAX-055 (option spax.fca_engine) and return both results. `block_values`
# sets spax.fca_block_values for the block run: 1 gives one-row blocks.
fca_engines <- function(fun, block_values = NULL) {
  field <- withr::with_options(list(spax.fca_engine = "field"), fun())
  block <- withr::with_options(
    list(spax.fca_engine = "block", spax.fca_block_values = block_values), fun()
  )
  list(field = field, block = block)
}

# A 3 x 4 grid with `nlyr` facilities for the block engine. Demand is missing
# in cell 7 and zero in cell 5; cell 9 reaches no facility and cell 3 does not
# reach facility 1.
fca_block_fixture <- function(nlyr = 3) {
  ids <- paste0("f", seq_len(nlyr))
  dist <- vapply(seq_len(nlyr), function(j) ((1:12) * j) %% 7 + 1, numeric(12))
  dist[9, ] <- NA
  dist[3, 1] <- NA
  D <- c(10, 20, 30, 40, 0, 60, NA, 80, 90, 100, 110, 120)
  list(
    ids = ids, D = D, dist = dist,
    demand = toy_rast(D, nrows = 3, ncols = 4),
    distance = toy_rast(c(dist), nrows = 3, ncols = 4, nlyr = nlyr, names = ids),
    S = matrix(seq(5, by = 3, length.out = nlyr * 2), nlyr, 2,
               dimnames = list(ids, c("a", "b")))
  )
}
