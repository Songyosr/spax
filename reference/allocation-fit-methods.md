# Inspect saved allocation fits without recomputation

A fit from
[`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md)
keeps every start and the selected fit, `fit$best`; both have these
methods, which read what the fit saved. Coefficients and fitted targets
are read from the selected fit. Multistart summaries retain failed
starts, boundaries, warnings/errors and descriptive disagreement. A
search with no eligible fit remains inspectable by summary, while
coefficient and fitted extraction fail clearly.
[`print()`](https://rdrr.io/r/base/print.html) of a fit shows its starts
and the one selected; [`print()`](https://rdrr.io/r/base/print.html) of
the selected fit shows its estimates, loss and convergence, and its
[`summary()`](https://rspatial.github.io/terra/reference/summary.html)
adds the ranges of the fitted values, of the model state and of the
residuals.

## Usage

``` r
# S3 method for class 'ae_problem_nfxp_fit'
coef(object, ...)

# S3 method for class 'ae_multistart_fit'
coef(object, ...)

# S3 method for class 'ae_problem_nfxp_fit'
fitted(object, ...)

# S3 method for class 'ae_multistart_fit'
fitted(object, ...)

# S3 method for class 'ae_multistart_fit'
summary(object, ...)

# S3 method for class 'ae_problem_nfxp_fit'
summary(object, ...)

# S3 method for class 'ae_multistart_fit'
print(x, ...)

# S3 method for class 'ae_problem_nfxp_fit'
print(x, digits = 4, ...)

# S3 method for class 'summary.ae_problem_nfxp_fit'
print(x, digits = 4, ...)
```

## Arguments

- object, x:

  A fit from
  [`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md)
  (an `ae_multistart_fit`) or its selected fit `fit$best` (an
  `ae_problem_nfxp_fit`); for the print method of a summary, the summary
  of the selected fit.

- ...:

  Additional generic arguments; currently unused.

- digits:

  Significant digits of the printed numbers.

## Value

[`coef()`](https://rdrr.io/r/stats/coef.html) returns the saved named
parameter vector.
[`fitted()`](https://rdrr.io/r/stats/fitted.values.html) returns saved
canonical fitted targets.
[`summary()`](https://rspatial.github.io/terra/reference/summary.html)
of a fit returns a list containing the selected fit summary, per-start
table, boundaries, disagreement and messages;
[`summary()`](https://rspatial.github.io/terra/reference/summary.html)
of the selected fit returns its model and family, its parameters, its
loss and convergence, and the ranges of the fitted values, of the state
and (when the fit kept its observed values) of the residuals.
[`print()`](https://rdrr.io/r/base/print.html) returns its input
invisibly. These diagnostics establish neither identification nor global
optimality.

## See also

[`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md)

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

# The selected estimates and the modeled registrations at each practice
coef(fit)
#>     sigma        v0 
#> 13.223212  1.001842 
round(fitted(fit))
#> L82018 L82036 L82038 L82041 L82042 L82044 L82047 L82056 L82059 L82068 L82070 
#>  17829  15924  13051  22441  11935  10758   6762   2851   3674   7954   5444 
#> L82620 Y01050 Y01051 Y01922 
#>   3535  10004   3367  10545 

# Every start, the selected fit and its residuals
summary(fit)
#> <allocation fit summary>
#> status: selected; selected start: scan1
#> loss disagreement: FALSE; parameter disagreement: FALSE
#> Supplied-start diagnostics (not a global-optimum or identification guarantee):
#>  start     loss eligible optim_convergence solver_converged residual seconds
#>  scan1 -1201000     TRUE                 0             TRUE        0   0.048
#>  scan2 -1201000     TRUE                 0             TRUE        0   0.062
#>  scan3 -1201000     TRUE                 0             TRUE        0   0.047
#>  boundary   status                                    optim_message
#>     FALSE eligible CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL
#>     FALSE eligible CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL
#>     FALSE eligible CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL
#>  solver_message log_parameter_gap  loss_gap
#>       converged         0.000e+00 0.000e+00
#>       converged         8.514e-07 0.000e+00
#>       converged         6.201e-06 1.164e-09
#> 
#> Selected fit:
#> <allocation fit result summary>
#> model: conditional logit
#> family: gaussian (sigma = travel scale)
#> allocation_form: clm
#> 
#> theta:
#>  parameter estimate
#>      sigma   13.220
#>         v0    1.002
#> 
#> fit:
#>      loss convergence seconds spectral_radius gradient
#>  -1201000           0   0.045               0     TRUE
#> 
#> predicted:
#>   min  q25 median mean   q75   max
#>  2851 4559  10000 9738 12490 22440
#> 
#> state:
#>  min  q25 median  mean   q75   max
#>    2 3.47  5.893 6.025 7.913 13.27
#> 
#> residual:
#>    min   q25 median  mean  q75  max
#>  -4876 -1292 -925.6 41.65 2210 5251

# The selected fit itself has the same methods: its estimates, loss and
# convergence, and a summary with the ranges of its values and residuals
coef(fit$best)
#>     sigma        v0 
#> 13.223212  1.001842 
fit$best
#> <allocation fit result>
#> model: conditional logit
#> family: gaussian (sigma = travel scale)
#> allocation_form: clm
#> theta:
#>   sigma: 13.22
#>   v0: 1.002
#> loss: -1201000
#> optim_convergence: 0
#> seconds: 0.045
#> spectral_radius: 0
#> gradient: TRUE
summary(fit$best)
#> <allocation fit result summary>
#> model: conditional logit
#> family: gaussian (sigma = travel scale)
#> allocation_form: clm
#> 
#> theta:
#>  parameter estimate
#>      sigma   13.220
#>         v0    1.002
#> 
#> fit:
#>      loss convergence seconds spectral_radius gradient
#>  -1201000           0   0.045               0     TRUE
#> 
#> predicted:
#>   min  q25 median mean   q75   max
#>  2851 4559  10000 9738 12490 22440
#> 
#> state:
#>  min  q25 median  mean   q75   max
#>    2 3.47  5.893 6.025 7.913 13.27
#> 
#> residual:
#>    min   q25 median  mean  q75  max
#>  -4876 -1292 -925.6 41.65 2210 5251
```
