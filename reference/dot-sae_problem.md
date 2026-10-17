# Construct a private SAE problem

`demand` can be checked
[`.prepare_allocation()`](https://songyosr.github.io/spax/reference/dot-prepare_allocation.md)
output; in that case omit raw `supply`, `distance` and supply selectors.
Raster arguments remain compatible.

## Usage

``` r
.sae_problem(
  demand,
  supply = NULL,
  distance = NULL,
  family = c("gaussian", "exponential", "power"),
  kappa = 1,
  beta = 20,
  eps = 1e-08,
  id_col = NULL,
  supply_cols = NULL
)
```
