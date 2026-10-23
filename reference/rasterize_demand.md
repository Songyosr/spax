# Convenience wrapper for Bayesian spatial updating

Convenience function that handles the common case of updating a spatial
distribution with vector-based observations. Combines the functionality
of transform_pmf, transform_likelihood, and update_pmf into a single
workflow.

## Usage

``` r
rasterize_demand(
  x,
  value_col,
  prior = NULL,
  template = NULL,
  density = FALSE,
  standardize = FALSE
)
```

## Arguments

- x:

  SpatVector containing observations

- value_col:

  Character name of column containing values

- prior:

  Optional SpatRaster prior PMF. If NULL, a uniform prior over the
  non-missing cells of the template's first layer is used.

- template:

  SpatRaster template (required if prior = NULL)

- density:

  Logical; if TRUE return density surface

- standardize:

  Must be FALSE. Standardized (z-score) likelihood values include
  negative values, so they cannot form a posterior PMF.

## Value

SpatRaster containing posterior PMF or density surface

## Examples

``` r
library(terra)

# Registered patients by Cornwall small area, on the population grid
pop <- read_spax_example("cornwall_pop.tif")
areas <- vect(cornwall_lsoa)

# Uniform prior over the populated grid
result1 <- rasterize_demand(areas, value_col = "registered", template = pop)

# Population-based prior, returned as a density surface
prior <- transform_pmf(pop)
result2 <- rasterize_demand(
  areas,
  value_col = "registered",
  prior = prior,
  density = TRUE
)
```
