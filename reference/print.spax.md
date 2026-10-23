# Print method for spax objects

Prints a concise summary of a spax object, showing key information about
the accessibility analysis without detailed statistics.

## Usage

``` r
# S3 method for class 'spax'
print(x, ...)
```

## Arguments

- x:

  A spax object created by an FCA function, such as
  [`spax_2sfca`](https://songyosr.github.io/spax/reference/spax_2sfca.md),
  [`spax_e2sfca`](https://songyosr.github.io/spax/reference/spax_e2sfca.md),
  [`spax_m2sfca`](https://songyosr.github.io/spax/reference/spax_m2sfca.md),
  [`spax_3sfca`](https://songyosr.github.io/spax/reference/spax_3sfca.md)
  or
  [`spax_huff2sfca`](https://songyosr.github.io/spax/reference/spax_huff2sfca.md)

- ...:

  Currently ignored, for extensibility

## Value

Invisibly returns the input object

## Examples

``` r
# E2SFCA on the Cornwall population grid, with a Gaussian decay of 10 minutes
pop <- read_spax_example("cornwall_pop.tif")
travel <- read_spax_example("cornwall_gp_travel.tif")
result <- spax_e2sfca(pop, sf::st_drop_geometry(cornwall_gp), travel,
                      decay_params = list(method = "gaussian", sigma = 10),
                      id_col = "practice", supply_cols = "gp_fte_extg")
print(result)
#> <spax access> E2SFCA, 1 layer on 132 x 271 cells
#> 
#> Spatial Properties:
#> - Resolution: 0.00833333 x 0.00833333
#> - Dimensions: 132 x 271
#> 
#> Measures: 1
#> - gp_fte_extg
#> 
#> Facilities: 55
#> 
#> Parameters:
#> - Decay function: gaussian (sigma = 10)
#> - Demand normalization: identity 
```
