# Validate inputs for spax_e2sfca

Validate inputs for spax_e2sfca

## Usage

``` r
.chck_e2sfca(
  demand,
  supply,
  distance,
  decay_params,
  demand_normalize,
  id_col = NULL,
  supply_cols = NULL
)
```

## Arguments

- demand:

  SpatRaster representing spatial distribution of demand

- supply:

  vector, matrix, or data.frame containing supply capacity values

- distance:

  SpatRaster stack of travel times/distances to facilities

- decay_params:

  List of parameters for decay function

- demand_normalize:

  Character specifying normalization method

- id_col:

  Character; column name for facility IDs if supply is a data.frame

- supply_cols:

  Character vector; names of supply columns if supply is a data.frame
