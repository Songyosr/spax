# Two-pass block FCA engine

Pass 1 reads each block, takes for each demand group the cells with
finite positive demand, evaluates the group's demand kernel on those
rows, replaces missing weights by 0, applies the demand normalization
and accumulates the demand of all groups that reaches each facility,
\`U\`. The ratio is \`R = supply / U\` (0 where not finite). Pass 2
evaluates the access kernel of each group on every row of each block and
writes \`A = K R\` for the rows with at least one finite kernel value;
the other cells stay NA. Without groups this is the one-group case of
the same loops.

## Usage

``` r
.fca_blocks(
  rasters,
  kernels,
  supply,
  normalize = "identity",
  snap = FALSE,
  groups = NULL,
  block = NULL
)
```

## Arguments

- rasters:

  Named list of SpatRasters read in blocks; it must contain the demand
  rasters (key \`demand\`, or \`demand.\<group\>\` with \`groups\`; one
  layer each) and whatever the kernels read. A key may not name two
  different rasters (see \`.fca_merge_rasters()\`).

- kernels:

  \`list(demand = function(b, rows), access = function(b, rows))\`.
  \`b\` is the list of block matrices from \`.raster_read()\` and
  \`rows\` the block rows to compute; each returns a \`length(rows) x
  J\` matrix in facility-ID order. With \`groups\`, a list of such lists
  named by the group IDs.

- supply:

  Numeric J x M matrix in facility-ID order.

- normalize:

  Demand normalization of the gather kernel.

- snap:

  When FALSE, negative or infinite demand is refused (D2); when TRUE
  such cells are not active. Weight checks (D1) live in the kernels.

- groups:

  \`NULL\` for one ungrouped demand raster, or the character vector of
  group IDs (also for a single group).

- block:

  \`NULL\` (the rasters are read in blocks), or the whole input as one
  in-memory block: a list with the keys that \`rasters\` would hold
  (\`demand\`, or \`demand.\<group\>\` with \`groups\`, and whatever the
  kernels read, such as \`distance\`), each a matrix with one row per
  origin and one column per layer (a plain vector is one column).
  \`rasters\` is then ignored and nothing is opened or read; both
  passes, the NA rule and the overall layer are the same code as for
  rasters (SPAX-060, FCA on numeric data).

## Value

A matrix with one row per cell of the grid (per origin of \`block\`).
Ungrouped, one column per supply measure. Grouped, \`M \* (G + 1)\`
columns in measure-major order: for each measure the groups in the order
of \`groups\`, then the overall layer.

## Details

With \`groups\` the output also holds an overall layer per measure,
computed in pass 2 from the demand blocks: with \`A\*\` equal to the
group's access, or 0 where the group cannot reach the cell, the overall
access of a cell is \`sum(D_g A\*\_g) / sum(D_g)\` over the groups with
positive demand. It is NA where any group's demand is NA or the total
demand is 0. Only demand that is finite and positive counts (under
\`snap\`, negative or infinite demand is ignored as in the group
layers).
