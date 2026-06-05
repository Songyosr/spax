# Validate inputs for compute_access

Validate inputs for compute_access

## Usage

``` r
.chck_compute_access(
  demand,
  supply,
  demand_weights,
  access_weights,
  id_col = NULL,
  supply_cols = NULL,
  indicator_names = NULL
)
```

## Arguments

- demand:

  SpatRaster representing spatial distribution of demand

- supply:

  vector, matrix, or data.frame containing supply capacity values

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
