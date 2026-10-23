# Print a saved allocation multistart summary

Preserves all selected-fit, per-start, boundary, disagreement and
message fields returned by
[`summary()`](https://rspatial.github.io/terra/reference/summary.html),
including searches with no eligible fit.

## Usage

``` r
# S3 method for class 'summary.ae_multistart_fit'
print(x, digits = 4, ...)
```

## Arguments

- x:

  A summary of an allocation multistart fit.

- digits:

  Display precision; saved values are unchanged.

- ...:

  Additional print arguments.

## Value

The summary, invisibly.

## Examples

``` r
# West Cornwall: the small areas and practices west of easting 170 km
areas <- sf::st_drop_geometry(cornwall_lsoa)
areas <- areas[!areas$scilly & areas$cent_x < 170000, ]
gp <- sf::st_drop_geometry(cornwall_gp)
gp <- gp[!gp$scilly & gp$easting < 170000, ]
inputs <- prepare_allocation(stats::setNames(areas$registered, areas$lsoa),
                             stats::setNames(gp$gp_fte_extg, gp$practice),
                             cornwall_travel[areas$lsoa, gp$practice])

# A fit to the registrations of these residents at these practices
flows <- cornwall_flows[cornwall_flows$lsoa %in% areas$lsoa &
                          cornwall_flows$practice %in% gp$practice, ]
totals <- sapply(split(flows$patients, flows$practice), sum)
fit <- fit_allocation(clm_allocation(inputs, fit_v0 = TRUE), totals,
                      loss = "poisson")

# The starts, the selected fit and its residuals, to three significant digits
print(summary(fit), digits = 3)
#> <allocation fit summary>
#> status: selected; selected start: scan1
#> loss disagreement: FALSE; parameter disagreement: FALSE
#> Supplied-start diagnostics (not a global-optimum or identification guarantee):
#>  start     loss eligible optim_convergence solver_converged residual seconds
#>  scan1 -1200000     TRUE                 0             TRUE        0   0.036
#>  scan2 -1200000     TRUE                 0             TRUE        0   0.061
#>  scan3 -1200000     TRUE                 0             TRUE        0   0.048
#>  boundary   status                                    optim_message
#>     FALSE eligible CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL
#>     FALSE eligible CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL
#>     FALSE eligible CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL
#>  solver_message log_parameter_gap loss_gap
#>       converged          0.00e+00 0.00e+00
#>       converged          8.51e-07 0.00e+00
#>       converged          6.20e-06 1.16e-09
#> 
#> Selected fit:
#> <allocation fit result summary>
#> model: conditional logit
#> family: gaussian (sigma = travel scale)
#> allocation_form: clm
#> 
#> theta:
#>  parameter estimate
#>      sigma     13.2
#>         v0      1.0
#> 
#> fit:
#>      loss convergence seconds spectral_radius gradient
#>  -1200000           0   0.034               0     TRUE
#> 
#> predicted:
#>   min  q25 median mean   q75   max
#>  2850 4560  10000 9740 12500 22400
#> 
#> state:
#>  min  q25 median mean  q75  max
#>    2 3.47   5.89 6.03 7.91 13.3
#> 
#> residual:
#>    min   q25 median mean  q75  max
#>  -4880 -1290   -926 41.7 2210 5250
```
