# Validate inputs for spax_e2sfca

The checks of the FCA wrappers that take a decay (E2SFCA, M2SFCA, 3SFCA
and Huff-2SFCA, and 2SFCA through its binary decay) are two functions
that run in this order: the data (\`.chck_e2sfca_data()\`) and the decay
and normalization (\`.chck_e2sfca_decay()\`). \`prep_data()\` with
rasters runs the data part alone.

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

  SpatRaster representing spatial distribution of demand, or a named
  list of SpatRasters (demand groups)

- supply:

  vector, matrix, or data.frame containing supply capacity values

- distance:

  SpatRaster stack of travel times/distances to facilities, or with
  groups a named list of them (one per group)

- decay_params:

  List of parameters for decay function, or with groups a named list of
  them (one per group)

- demand_normalize:

  Character specifying normalization method

- id_col:

  Character; column name for facility IDs if supply is a data.frame

- supply_cols:

  Character vector; names of supply columns if supply is a data.frame
