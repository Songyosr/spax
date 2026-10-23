# Compute Spatial Choice Probabilities

Converts spatial weights to choice probabilities. When used with
attractiveness values, implements Huff model logic. Without
attractiveness, behaves like standard weight normalization.

## Usage

``` r
calc_choice(weights, attractiveness = NULL, alpha = 1, a0 = 0, snap = FALSE)
```

## Arguments

- weights:

  Multi-layer SpatRaster of spatial weights (e.g., from
  [`calc_decay()`](https://songyosr.github.io/spax/reference/calc_decay.md))

- attractiveness:

  Optional numeric vector of facility attractiveness values

- alpha:

  Numeric parameter for attractiveness sensitivity (default = 1)

- a0:

  Non-negative numeric value representing outside option weight (default
  = 0)

- snap:

  Logical; if `TRUE`, a fast computation mode, only for inputs known to
  be valid, with:

  - Minimal validation

  - Essential preprocessing only

## Value

SpatRaster stack of choice probabilities (layers sum to 1 at each
location)

## Examples

``` r
library(terra)
#> terra 1.9.50
# load data
distance_raster <- read_spax_example("cornwall_gp_travel.tif")

# Calculate decay weights using gaussian decay (minutes)
weights <- calc_decay(distance_raster, method = "gaussian", sigma = 10)

# Basic usage (equivalent to calc_normalize)
p1 <- calc_choice(weights)

# With Huff model using GP full-time equivalents as attractiveness
p2 <- calc_choice(weights,
  attractiveness = cornwall_gp$gp_fte_extg, # GPs excluding trainees
  alpha = 1.5
)

# Plot to compare, two panels side by side
old_par <- par(mfrow = c(1, 2))
plot(p1[[1]], main = "Basic Choice Prob.")
plot(p2[[1]], main = "GP-weighted Choice Prob.")

par(old_par)

# With outside option (a0)
p3 <- calc_choice(weights,
  attractiveness = cornwall_gp$gp_fte_extg,
  a0 = 0.1
)
```
