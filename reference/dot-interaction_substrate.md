# Compile checked raster inputs into a compact interaction substrate

Select finite positive-demand cells before extracting distances,
avoiding a full distance matrix solely to discard inactive rows.
All-active inputs use the direct matrix reader. Raw cell extraction
preserves numeric travel-time values, missing edges, layer order and the
original cell index map. This does not change the fitting support policy
or evaluate omitted prediction cells. The shared finalizer records this
raster route's legacy missing-edge policy and stores plain template
geometry plus cell mapping for safe serialization.

## Usage

``` r
.interaction_substrate(
  demand,
  supply,
  distance,
  id_col = NULL,
  supply_cols = NULL
)
```
