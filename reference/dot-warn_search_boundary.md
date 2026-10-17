# Warn when a calibrated parameter lands on its search-box bound

A boundary optimum usually means the search box, not the model, picked
the answer. `par` is the optimized value on the log scale (matching the
optimizer).

## Usage

``` r
.warn_search_boundary(par, lower, upper, name = "parameter")
```
