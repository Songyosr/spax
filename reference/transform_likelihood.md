# Transform Input to Likelihood Surface

Creates a likelihood surface representing the relative intensity of
values compared to what would be expected under a reference
distribution. This is useful for Bayesian updating of spatial
probability distributions.

The likelihood surface shows where values are higher or lower than
expected:

- Values \> 1 indicate higher than expected

- Values = 1 indicate as expected

- Values \< 1 indicate lower than expected

## Usage

``` r
transform_likelihood(
  x,
  value_col = NULL,
  template = NULL,
  method = "mean",
  ref_value = NULL,
  standardize = FALSE,
  snap = FALSE
)
```

## Arguments

- x:

  SpatRaster or SpatVector containing observed values. Point values in
  the same cell add up (missing values are ignored); a polygon's value
  is copied to every cell it covers.

- value_col:

  Character; name of column if x is SpatVector

- template:

  SpatRaster; template for rasterization if x is SpatVector

- method:

  Character; method to compute reference value:

  - `"mean"`: Compare to mean value (default)

  - `"sum"`: Compare to sum

  - `"custom"`: Compare to provided `ref_value`

- ref_value:

  Numeric; custom reference value if `method = "custom"`

- standardize:

  Logical; if TRUE return z-scores instead of ratios

- snap:

  Logical; if TRUE skip validation

## Value

SpatRaster of likelihood values

## Examples

``` r
# From raster input
pop <- read_spax_example("cornwall_pop.tif")
lik1 <- transform_likelihood(pop)

# From vector input: density of residents aged 65+ by small area. Rasterizing
# copies a polygon's value to every cell it covers, so use a density, not a count.
areas <- terra::vect(cornwall_lsoa)
areas$dens_65 <- areas$pop_65p / terra::expanse(areas, unit = "km")
lik2 <- transform_likelihood(
  areas,
  value_col = "dens_65",
  template = pop
)

# With custom reference
lik3 <- transform_likelihood(
  pop,
  method = "custom",
  ref_value = 100
)
```
