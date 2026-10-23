# Calculate demand captured by each service site

Specialized version of
[`gather_weighted()`](https://songyosr.github.io/spax/reference/gather_weighted.md)
for accessibility analysis. Calculates the potential demand for each
service site by applying pre-computed spatial weights to a demand
raster.

## Usage

``` r
gather_demand(demand, weights)
```

## Arguments

- demand:

  SpatRaster representing spatial distribution of demand

- weights:

  Multi-layer SpatRaster where:

  - Each layer represents one service site

  - Values are probability weights (0-1) from distance decay

  - Layer names should match site IDs

## Value

data.frame with:

- `location_id`: identifier matching weight layer names

- `potential_demand`: weighted sum of demand for each site

## Examples

``` r
library(terra)

# Load data
pop <- read_spax_example("cornwall_pop.tif")
travel <- read_spax_example("cornwall_gp_travel.tif")

# Calculate probability weights using gaussian decay
weights <- calc_decay(
  travel,
  method = "gaussian",
  sigma = 10
) |>
  calc_normalize(method = "semi") # Normalize to ensure proper probabilities

# Calculate potential demand for each GP practice
practice_demands <- gather_demand(pop, weights)
head(practice_demands)
#>   location_id potential_demand
#> 1      L82001        11945.558
#> 2      L82003         7094.023
#> 3      L82004        11031.416
#> 4      L82006        10782.701
#> 5      L82007         7922.204
#> 6      L82008         8759.973
```
