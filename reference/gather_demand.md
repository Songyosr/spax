# Calculate demand captured by each service site

Specialized version of gather_weighted() for accessibility analysis.
Calculates the potential demand for each service site by applying
pre-computed spatial weights to a demand raster.

## Usage

``` r
gather_demand(demand, weights)
```

## Arguments

- demand:

  SpatRaster representing spatial distribution of demand

- weights:

  Multi-layer SpatRaster where: - Each layer represents one service
  site - Values are probability weights (0-1) from distance decay -
  Layer names should match site IDs

## Value

data.frame with: - location_id: identifier matching weight layer names -
potential_demand: weighted sum of demand for each site

## Examples

``` r
library(terra)

# Load data
u5pd <- read_spax_example("u5pd.tif")
hos_iscr <- read_spax_example("hos_iscr.tif")

# Calculate probability weights using gaussian decay
weights <- calc_decay(
  hos_iscr,
  method = "gaussian",
  sigma = 30
) |>
  calc_normalize(method = "semi") # Normalize to ensure proper probabilities

# Calculate potential demand for each hospital
hospital_demands <- gather_demand(u5pd, weights)
head(hospital_demands)
#>   location_id potential_demand
#> 1        c172         7948.891
#> 2        c173         8205.902
#> 3        c174         4792.858
#> 4        c175         7542.494
#> 5        c176         6329.155
#> 6        c177         4503.939
```
