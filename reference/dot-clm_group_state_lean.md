# Lean variant of \`.clm_group_state()\` (SPAX-061)

The same arithmetic in the same order, returning the vectors, the
by-group outputs (origin or facility by group) and the requested count
matrices, but no pooled allocation matrix. See \`.huff_state_lean()\`.

## Usage

``` r
.clm_group_state_lean(
  a,
  kernels,
  D_groups,
  v0,
  keep = character(),
  demand = NULL
)
```
