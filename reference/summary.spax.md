# Summary method for spax objects

Provides detailed statistical summaries of accessibility analysis
results, including accessibility scores, facility statistics, and
model-specific metrics.

## Usage

``` r
# S3 method for class 'spax'
summary(object, quantiles = c(0, 0.25, 0.5, 0.75, 1), ...)
```

## Arguments

- object:

  A spax object created by an FCA function, such as
  [`spax_2sfca`](https://songyosr.github.io/spax/reference/spax_2sfca.md),
  [`spax_e2sfca`](https://songyosr.github.io/spax/reference/spax_e2sfca.md),
  [`spax_m2sfca`](https://songyosr.github.io/spax/reference/spax_m2sfca.md),
  [`spax_3sfca`](https://songyosr.github.io/spax/reference/spax_3sfca.md)
  or
  [`spax_huff2sfca`](https://songyosr.github.io/spax/reference/spax_huff2sfca.md)

- quantiles:

  Numeric vector of probabilities for quantile computation

- ...:

  Currently ignored, for extensibility

## Value

A summary.spax object. For each accessibility layer, `coverage` counts
the cells with a value (`cells_total`; cells outside the study area,
such as sea, are missing and excluded) and the cells whose accessibility
is above zero (`cells_covered`), with their `proportion`.

## Examples

``` r
# E2SFCA on the Cornwall population grid for two supply measures: GPs
# excluding GPs in training, and all GPs
pop <- read_spax_example("cornwall_pop.tif")
travel <- read_spax_example("cornwall_gp_travel.tif")
result <- spax_e2sfca(pop, sf::st_drop_geometry(cornwall_gp), travel,
                      decay_params = list(method = "gaussian", sigma = 10),
                      id_col = "practice",
                      supply_cols = c("gp_fte_extg", "gp_fte"))
summary(result)
#> <spax access summary> E2SFCA
#> --------------------------------------------- 
#> 
#> Accessibility Measures:
#> 
#> gp_fte_extg:
#>   Range: 2.666e-05 to 0.007704
#>   Mean (SD): 0.0005095 (0.0004277)
#>   Quantiles:
#>     Min.:    2.666e-05
#>     1st Qu.: 0.0003781
#>     Median:  0.0004877
#>     3rd Qu.: 0.0006064
#>     Max.:    0.007704
#> 
#>   Coverage: 100.0% (6934 of 6934 cells with a value have accessibility > 0)
#> 
#> gp_fte:
#>   Range: 3.029e-05 to 0.007704
#>   Mean (SD): 0.0006255 (0.000435)
#>   Quantiles:
#>     Min.:    3.029e-05
#>     1st Qu.: 0.0004671
#>     Median:  0.0006074
#>     3rd Qu.: 0.0007397
#>     Max.:    0.007704
#> 
#>   Coverage: 100.0% (6934 of 6934 cells with a value have accessibility > 0)
#> 
#> Facility Statistics:
#> -------------------
#> Total Facilities: 55
#> 
#> gp_fte_extg:
#>   Total Supply: 320.6
#>   Mean (SD): 5.829 (3.113)
#>   Range: 1.133 to 13.36
#> 
#> gp_fte:
#>   Total Supply: 395.8
#>   Mean (SD): 7.196 (3.904)
#>   Range: 1.787 to 15.87

# Deciles instead of quartiles, and the coverage of the first measure: the
# cells with a value, and those with access above zero
s <- summary(result, quantiles = seq(0, 1, by = 0.1))
s$accessibility$gp_fte_extg$quantiles
#>           0%          10%          20%          30%          40%          50% 
#> 2.666368e-05 2.981623e-04 3.556487e-04 4.000099e-04 4.427923e-04 4.876841e-04 
#>          60%          70%          80%          90%         100% 
#> 5.227812e-04 5.756836e-04 6.367468e-04 6.879626e-04 7.703781e-03 
s$accessibility$gp_fte_extg$coverage
#> $cells_total
#> [1] 6934
#> 
#> $cells_covered
#> [1] 6934
#> 
#> $proportion
#> [1] 1
#> 
```
