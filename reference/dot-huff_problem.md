# Construct a private static Huff / aggregate-CLM problem

The `dT/dstate = 0` member of the ladder: the map is constant, the
equilibrium is reached in one pass, and the decay parameter enters only
through the allocation output (not the state target). It calibrates
through the shared NFXP runner unchanged. `demand` can be checked
[`.prepare_allocation()`](https://songyosr.github.io/spax/reference/dot-prepare_allocation.md)
output; in that case omit raw `supply`, `distance` and supply selectors.
Raster arguments remain compatible.

## Usage

``` r
.huff_problem(
  demand,
  supply = NULL,
  distance = NULL,
  family = c("gaussian", "exponential", "power"),
  kappa = 1,
  beta = 1,
  v0 = 0,
  fit_v0 = FALSE,
  fit_beta = FALSE,
  id_col = NULL,
  supply_cols = NULL,
  allocation = c("clm", "huff_decay"),
  kernel = NULL
)
```

## Arguments

- kernel:

  `NULL` for a built-in `family`, or a custom `spax_decay_kernel`
  declaration passed on to
  [`.huff_spec()`](https://songyosr.github.io/spax/reference/dot-huff_spec.md).
