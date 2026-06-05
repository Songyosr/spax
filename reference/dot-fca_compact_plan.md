# Compile FCA inputs into a compact numeric execution plan

The semantic boundary: validate geometry once, then drop to plain
matrices. Active-demand compaction (\`is.finite(D) & D \> 0\`) removes
inactive origin rows from the gather – exact, since zero/NA-demand cells
contribute nothing to facility load. The spread keeps cells reachable by
at least one facility (cells unreachable by all stay NA, matching
compute_fca).

## Usage

``` r
.fca_compact_plan(
  demand,
  supply,
  demand_kernel,
  access_kernel,
  demand_normalize = "identity",
  id_col = NULL,
  supply_cols = NULL,
  indicator_names = NULL
)
```

## Details

Kernels are taken as \`SpatRaster\`s here (the proven oracle shape); the
boundary check mirrors \`compute_fca()\`'s alignment contract.
