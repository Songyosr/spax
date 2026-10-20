# Internal FCA driver

Checks the grids, binds the supply to the facility IDs, runs the
two-pass block engine and returns a SpatRaster on the grid of the first
raster of \`rasters\`. That raster also names the facilities; the
wrappers pass the distance raster first.

## Usage

``` r
compute_fca(
  demand,
  supply,
  rasters,
  kernels,
  demand_normalize = "identity",
  id_col = NULL,
  supply_cols = NULL,
  indicator_names = NULL,
  snap = FALSE
)
```

## Arguments

- demand:

  One-layer SpatRaster of demand, or a named list of them (one per
  demand group).

- supply:

  Vector, matrix or data frame, as for \[compute_access()\].

- rasters:

  Named list of the other SpatRasters the kernels read; the first one
  has one layer per facility. A key must not name two different rasters.

- kernels:

  \`list(demand = function(b, rows), access = function(b, rows))\` as
  for \`.fca_blocks()\`, or a list of them over the groups.

- demand_normalize:

  Normalization of the demand kernel.

- id_col:

  Character; column name for facility IDs if supply is a data.frame

- supply_cols:

  Character vector; names of supply columns if supply is a data.frame

- indicator_names:

  Character vector; custom names for output accessibility layers

- snap:

  Logical; if TRUE enable fast computation mode (default = FALSE)

## Value

SpatRaster of accessibility.

## Details

Ungrouped, \`demand\` is one SpatRaster, \`kernels\` one kernel pair and
the result has one layer per supply measure. Grouped, \`demand\` is a
named list of one-layer SpatRasters (the names are the group IDs),
\`kernels\` a list over the groups, named by group ID, of such pairs,
and \`rasters\` holds the rasters the kernels read, every key ending in
\`.\<group\>\` (see \`.fca_decay_spec()\`). The result then has the
layers \`\<measure\>.\<group\>\` and \`\<measure\>.all\` (see
\`.fca_blocks()\` and \`.fca_layer_table()\`), and a \`layers\`
attribute: the layer table, a data frame with the columns \`layer\`,
\`measure\` and \`group\` (\`"all"\` for the overall layer). The
attribute is read by the wrapper right away, which stores it in
\`parameters\$layers\`; it does not survive raster arithmetic.
