# Calculate spatial accessibility using weighted surfaces

General-purpose function to calculate spatial accessibility by combining
demand-side and supply-side weighted distributions. Supports multiple
supply measures and flexible weight specifications.

## Usage

``` r
compute_access(
  demand,
  supply,
  demand_weights,
  access_weights,
  id_col = NULL,
  supply_cols = NULL,
  indicator_names = NULL,
  snap = FALSE
)
```

## Arguments

- demand:

  SpatRaster representing spatial distribution of demand

- supply:

  vector, matrix, or data.frame containing supply capacity values. If
  using an sf object, please use st_drop_geometry() first.

- demand_weights:

  Multi-layer SpatRaster of demand-side weights

- access_weights:

  Multi-layer SpatRaster of accessibility-side weights

- id_col:

  Character; column name for facility IDs if supply is a data.frame

- supply_cols:

  Character vector; names of supply columns if supply is a data.frame

- indicator_names:

  Character vector; custom names for output accessibility layers

- snap:

  Logical; if TRUE enable fast computation mode (default = FALSE)

## Value

SpatRaster of accessibility scores
