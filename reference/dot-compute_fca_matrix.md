# Run FCA over the compact matrix substrate (== compute_fca, no raster hot loop)

terra front-end (extract) -\> terra-free executor -\> terra back-end
(rewrap). Equivalent to \`compute_fca()\` within float tolerance;
private, tests/benchmarks.

## Usage

``` r
.compute_fca_matrix(
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
