# Transform input into a probability mass function (PMF)

Converts raster or vector inputs into a probability mass function where
all values sum to 1. Raster inputs are normalised directly, while vector
inputs are first rasterised using a supplied template.

## Usage

``` r
transform_pmf(
  x,
  value_col = NULL,
  template = NULL,
  return_total = FALSE,
  snap = FALSE
)
```

## Arguments

- x:

  A \`SpatRaster\` containing values to normalise, or a \`SpatVector\`
  whose attribute values will be rasterised.

- value_col:

  Character name of the attribute column to use when \`x\` is a
  \`SpatVector\`. Ignored for raster inputs.

- template:

  \`SpatRaster\` providing geometry for rasterising vector inputs.
  Required when \`x\` is a \`SpatVector\`.

- return_total:

  Logical; when \`TRUE\`, return both the PMF raster and the total sum
  of the input values. Default is \`FALSE\`.

- snap:

  Logical; skip validation checks when \`TRUE\`. Default is \`FALSE\`.

## Value

If \`return_total = FALSE\`, returns a \`SpatRaster\` whose cells sum
to 1. If \`return_total = TRUE\`, returns a list with components:

- pmf:

  \`SpatRaster\` of probabilities

- total:

  Numeric total of the original values

## Examples

``` r
if (FALSE) { # \dontrun{
pop <- terra::rast(u5pd)

# Basic PMF
pmf <- transform_pmf(pop)

# Include total for later reuse
result <- transform_pmf(pop, return_total = TRUE)
result$total
} # }
```
