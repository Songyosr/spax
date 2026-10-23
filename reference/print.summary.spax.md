# Print function for spax summary objects

Formats and displays summary statistics for spax objects in a readable
format. This is a placeholder until S3 method export issues are
resolved.

## Usage

``` r
# S3 method for class 'summary.spax'
print(x, digits = 4, ...)
```

## Arguments

- x:

  A summary.spax object returned by summary.spax()

- digits:

  Number of digits to round numeric values (default = 4)

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

# The summary, to three significant digits
print(summary(result), digits = 3)
#> <spax access summary> E2SFCA
#> --------------------------------------------- 
#> 
#> Accessibility Measures:
#> 
#> gp_fte_extg:
#>   Range: 2.67e-05 to 0.0077
#>   Mean (SD): 0.00051 (0.000428)
#>   Quantiles:
#>     Min.:    2.67e-05
#>     1st Qu.: 0.000378
#>     Median:  0.000488
#>     3rd Qu.: 0.000606
#>     Max.:    0.0077
#> 
#>   Coverage: 100.0% (6934 of 6934 cells with a value have accessibility > 0)
#> 
#> Facility Statistics:
#> -------------------
#> Total Facilities: 55
#> 
#> gp_fte_extg:
#>   Total Supply: 321
#>   Mean (SD): 5.83 (3.11)
#>   Range: 1.13 to 13.4
```
