# Construct a private HAAE problem

`demand` can be checked
[`.prepare_allocation()`](https://songyosr.github.io/spax/reference/dot-prepare_allocation.md)
output; in that case omit raw `supply`, `distance` and supply selectors.
Raster arguments remain compatible.

## Usage

``` r
.haae_problem(
  demand,
  supply = NULL,
  distance = NULL,
  family = c("gaussian", "exponential", "power"),
  kappa = 1,
  eps = 1e-08,
  a_min = 1e-06,
  a_max = Inf,
  id_col = NULL,
  supply_cols = NULL
)
```
