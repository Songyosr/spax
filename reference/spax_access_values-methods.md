# Methods for FCA indices on numeric data

What to do with the result of
[`spax()`](https://songyosr.github.io/spax/reference/spax.md) for an FCA
specification
([`spec_e2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md)
and the others) on numeric data
([`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md)
with demand, supply and a travel matrix): a list with `access`, a matrix
with one row per active origin (named by the origin IDs) and one column
per layer, `type`, `parameters`, `facilities` and `call`. With demand
groups the columns are `supply.<group>` and `supply.all`, the layers of
the raster route. Origins that are not in the data (zero demand, unknown
demand, left out) have no row and so no value.

## Usage

``` r
# S3 method for class 'spax_access_values'
format(x, ...)

# S3 method for class 'spax_access_values'
print(x, ...)

# S3 method for class 'spax_access_values'
summary(object, ...)

# S3 method for class 'summary.spax_access_values'
print(x, digits = 4L, ...)

# S3 method for class 'spax_access_values'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x, object:

  A `spax_access_values`, or for
  [`print()`](https://rdrr.io/r/base/print.html) of the summary, a
  `summary.spax_access_values`.

- ...:

  Unused.

- digits:

  Number of significant digits printed in the summary.

- row.names, optional:

  Standard
  [`as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html)
  arguments.

## Value

[`print()`](https://rdrr.io/r/base/print.html) returns its argument
invisibly,
[`summary()`](https://rspatial.github.io/terra/reference/summary.html) a
`summary.spax_access_values` and
[`as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html)
a data frame.

## Details

- [`print()`](https://rdrr.io/r/base/print.html) shows the type, the
  layers, the decay and the normalization.

- [`summary()`](https://rspatial.github.io/terra/reference/summary.html)
  returns a `summary.spax_access_values`: for each layer the number of
  origins with a value, its minimum, mean and maximum, and the coverage,
  the origins whose access is above zero and their proportion.

- [`as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html)
  gives the table of the access values: `origin_id` and one column per
  layer.

## See also

[`spax()`](https://songyosr.github.io/spax/reference/spax.md),
[`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md)

Other allocation interface:
[`decay_gaussian()`](https://songyosr.github.io/spax/reference/decay_gaussian.md),
[`obs_flows()`](https://songyosr.github.io/spax/reference/obs_flows.md),
[`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md),
[`spax()`](https://songyosr.github.io/spax/reference/spax.md),
[`spax_allocation-methods`](https://songyosr.github.io/spax/reference/spax_allocation-methods.md),
[`spax_huff()`](https://songyosr.github.io/spax/reference/spax_huff.md),
[`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)

## Examples

``` r
# Mainland Cornwall: residents by small area, GP FTE by practice and the
# travel minutes between them
areas <- sf::st_drop_geometry(cornwall_lsoa[!cornwall_lsoa$scilly, ])
gp <- sf::st_drop_geometry(cornwall_gp[!cornwall_gp$scilly, ])
d <- prep_data(stats::setNames(areas$pop, areas$lsoa),
               stats::setNames(gp$gp_fte_extg, gp$practice),
               cornwall_travel[areas$lsoa, gp$practice])

# E2SFCA with a Gaussian decay of 10 minutes, at the small areas
access <- spax(d, spec_e2sfca(decay_gaussian(sigma = 10)))
access
#> <spax access> E2SFCA, 1 layer at 336 origins x 54 facilities
#>   layers      supply
#>   decay       gaussian (sigma = 10)
#>   normalize   identity
summary(access)
#> <spax access summary> E2SFCA
#>   origins     336
#>   decay       gaussian (sigma = 10)
#>   normalize   identity
#> 
#>   layer with_value       min      mean       max covered proportion
#>  supply        336 0.0001679 0.0005318 0.0007182     336          1
head(as.data.frame(access))
#>   origin_id       supply
#> 1 E01018750 0.0005968168
#> 2 E01018751 0.0005968168
#> 3 E01018752 0.0006006746
#> 4 E01018753 0.0003819479
#> 5 E01018754 0.0003194073
#> 6 E01018755 0.0002856953
```
