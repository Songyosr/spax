# Internal typed FCA engine

Implements normalize -\> gather -\> ratio -\> spread over spax_field
inputs. Public wrappers unwrap the returned field to preserve existing
return types.

## Usage

``` r
compute_fca(
  demand,
  supply,
  demand_kernel,
  access_kernel,
  demand_normalize = "identity",
  id_col = NULL,
  supply_cols = NULL,
  indicator_names = NULL,
  snap = FALSE
)
```
