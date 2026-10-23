# Distribute values across weighted surfaces

General-purpose function to distribute values across space using a stack
of weight rasters. Each input value is spread according to its
corresponding weight layer.

## Usage

``` r
spread_weighted(
  values,
  weights,
  value_cols = NULL,
  full_output = FALSE,
  name_prefix = NULL,
  snap = FALSE
)
```

## Arguments

- values:

  Numeric vector, matrix, or data.frame of values to distribute:

  - Vector: Must match number of weight layers

  - Matrix: Rows match weight layers, columns are different measures

  - Data.frame: Same as matrix but requires `value_cols`

- weights:

  Multi-layer SpatRaster where each layer represents one unit's weighted
  distribution surface

- value_cols:

  Character vector of column names if values is a data.frame

- full_output:

  Logical; whether to return intermediate calculations (default FALSE)

- name_prefix:

  Character string to prepend to output names (default NULL)

- snap:

  Logical; if TRUE, avoid input validation for performance (default
  FALSE)

## Value

If `full_output = FALSE`, the total distribution only: a SpatRaster with
one layer per measure. If `full_output = TRUE`, a list containing:

- `total_distribution`: SpatRaster stack of total distribution per
  measure

- `unit_distribution`: the distribution of each unit, a SpatRaster with
  one layer per unit (for several measures, a list of them, one per
  measure)

## Examples

``` r
# Example 1: Basic vector input using package data
# Spread GP full-time equivalents across space using distance-decay weights
travel <- read_spax_example("cornwall_gp_travel.tif")
weights <- calc_decay(travel, method = "gaussian", sigma = 10)
gp_distribution <- spread_weighted(cornwall_gp$gp_fte_extg, weights)

# Example 2: Multiple supply measures
supply_df <- sf::st_drop_geometry(cornwall_gp)[c("gp_fte_extg", "gp_fte")]
distributions <- spread_weighted(supply_df, weights,
  value_cols = c("gp_fte_extg", "gp_fte")
)

# Example 3: With full output for detailed analysis
result <- spread_weighted(cornwall_gp$gp_fte_extg, weights,
  full_output = TRUE
)
terra::plot(result$total_distribution, main = "Total Distribution")

terra::plot(result$unit_distribution[[1]], main = "First Unit Distribution")
```
