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

  Optional SpatRaster prior PMF (if NULL, uses uniform prior)

- template:

  SpatRaster template (required if prior = NULL)

- density:

  Logical; if TRUE return density surface

- standardize:

  Logical; if TRUE standardize likelihood values

## Value

SpatRaster containing posterior PMF or density surface

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage with uniform prior
result1 <- rasterize_demand(
  case_spatial,
  value_col = "cases",
  template = pop
)

# With population-based prior
prior <- transform_pmf(pop)
result2 <- rasterize_demand(
  case_spatial,
  value_col = "cases",
  prior = prior,
  density = TRUE
)
} # }
```
