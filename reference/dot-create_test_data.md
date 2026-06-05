# Create test data for spax functions

Creates a small test dataset for internal testing of spax functions.
Uses seeded random values for reproducibility.

## Usage

``` r
.create_test_data(seed = 42)
```

## Arguments

- seed:

  Numeric seed for random number generation (default = 42)

## Value

A list containing:

- demand:

  SpatRaster of population/demand

- distance:

  Multi-layer SpatRaster of distances

- supply_df:

  data.frame of supply locations

- supply_matrix:

  matrix version of supply data

- supply_vector:

  vector version of supply data

- boundary:

  sf polygon of study area
