# Shared fixtures and a base-R oracle for the legacy FCA, raster-tool
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
# Demand groups (SPAX-056): when `D` is a list of demand vectors, `Wd` and `Wa`
# are lists of weight matrices (one per group; one matrix is shared by all).
# The groups compete for the same supply: U sums over the groups. The result is
# `list(groups = list of cell-by-measure matrices, all = matrix)`, where `all`
# is the demand-weighted access of the cell over the groups with positive
# demand, a group that cannot reach the cell counting as 0, and NA where any
# group's demand is missing or the total demand is 0.
fca_oracle <- function(D, Wd, Wa = Wd, S, normalize = "identity") {
  if (is.list(D)) {
    return(fca_oracle_groups(D, Wd, Wa, S, normalize))
  }
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

fca_oracle_groups <- function(D, Wd, Wa, S, normalize) {
  S <- as.matrix(S)
  G <- length(D)
  if (is.matrix(Wd)) Wd <- rep(list(Wd), G)
  if (is.matrix(Wa)) Wa <- rep(list(Wa), G)
  norm <- function(W) {
    rs <- rowSums(W, na.rm = TRUE)
    switch(normalize,
      identity = W,
      standard = W * ifelse(rs > 0, 1 / rs, 0),
      semi = W / ifelse(rs > 1, rs, 1)
    )
  }
  U <- 0
  for (g in seq_len(G)) {
    U <- U + colSums(D[[g]] * norm(Wd[[g]]), na.rm = TRUE)
  }
  R <- S / U
  R[!is.finite(R)] <- 0
  A <- lapply(seq_len(G), function(g) {
    W0 <- Wa[[g]]
    W0[is.na(W0)] <- 0
    a <- W0 %*% R
    a[rowSums(!is.na(Wa[[g]])) == 0, ] <- NA
    unname(a)
  })
  n <- length(D[[1L]])
  all <- matrix(NA_real_, n, ncol(S))
  for (i in seq_len(n)) {
    d <- vapply(D, function(x) x[i], numeric(1))
    if (anyNA(d) || sum(d) == 0) next
    num <- numeric(ncol(S))
    for (g in which(d > 0)) {
      a <- A[[g]][i, ]
      a[is.na(a)] <- 0
      num <- num + d[g] * a
    }
    all[i, ] <- num / sum(d)
  }
  list(groups = A, all = all)
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
