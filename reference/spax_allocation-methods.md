# Methods for allocation results

What to do with the result of
[`spax()`](https://songyosr.github.io/spax/reference/spax.md),
[`spax_huff()`](https://songyosr.github.io/spax/reference/spax_huff.md)
or
[`spax_clm()`](https://songyosr.github.io/spax/reference/spax_huff.md).
These methods read the saved fit or evaluation. A fit does not solve the
model again, and an output that was not computed is evaluated when asked
for and not stored. A fit in which no start converged has no estimates:
[`coef()`](https://rdrr.io/r/stats/coef.html) gives `NULL` and the other
methods that need outputs stop with "no fit was selected; see
summary(x)";
[`summary()`](https://rspatial.github.io/terra/reference/summary.html),
[`print()`](https://rdrr.io/r/base/print.html) and
`as.data.frame(type = "starts")` still show the starts.

## Usage

``` r
# S3 method for class 'spax_allocation'
print(x, ...)

# S3 method for class 'spax_allocation'
summary(object, ...)

# S3 method for class 'summary.spax_allocation'
print(x, ...)

# S3 method for class 'spax_allocation'
coef(object, ...)

# S3 method for class 'spax_allocation'
fitted(object, ...)

# S3 method for class 'spax_allocation'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  type = c("parameters", "facilities", "origins", "flows", "starts"),
  ...
)

# S3 method for class 'spax_allocation'
predict(
  object,
  travel,
  demand = NULL,
  outputs = c("allocation", "outside_share"),
  ...
)
```

## Arguments

- ...:

  Further arguments: for
  [`predict()`](https://rspatial.github.io/terra/reference/predict.html),
  those of
  [`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md)
  (for instance `missing_travel`); otherwise unused.

- object, x:

  A `spax_allocation`, or for
  [`print()`](https://rdrr.io/r/base/print.html) of the summary, a
  `summary.spax_allocation`.

- row.names, optional:

  Standard
  [`as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html)
  arguments.

- type:

  The table to return (see above).

- travel:

  For
  [`predict()`](https://rspatial.github.io/terra/reference/predict.html):
  the requested-origin-by-facility travel matrix, `distance` of
  [`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md),
  in the travel unit of the data.

- demand:

  For
  [`predict()`](https://rspatial.github.io/terra/reference/predict.html):
  optional demand of the requested origins.

- outputs:

  For
  [`predict()`](https://rspatial.github.io/terra/reference/predict.html):
  the outputs to return, `"allocation"` and `"outside_share"` by
  default; see
  [`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md)
  for the others.

## Value

[`print()`](https://rdrr.io/r/base/print.html) returns its argument
invisibly.
[`summary()`](https://rspatial.github.io/terra/reference/summary.html)
returns a `summary.spax_allocation`.
[`coef()`](https://rdrr.io/r/stats/coef.html) returns a named numeric
vector (or `NULL`),
[`fitted()`](https://rdrr.io/r/stats/fitted.values.html) a numeric
vector or matrix,
[`as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html)
a data frame and
[`predict()`](https://rspatial.github.io/terra/reference/predict.html)
the `ae_clm_prediction` of
[`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md).

## Details

- [`print()`](https://rdrr.io/r/base/print.html) shows the
  specification, the data, the parameters and, for a fit, the selected
  start, the outside counts used (if any) and the default search.

- [`summary()`](https://rspatial.github.io/terra/reference/summary.html)
  returns a `summary.spax_allocation` with the specification lines
  (`spec`), the parameters table (`parameters`), the fit summary of
  [`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md)
  (`fit`, `NULL` when evaluated) and the `mode`; its print method shows
  them, with the per-start table for a fit.

- [`coef()`](https://rdrr.io/r/stats/coef.html) returns the free
  parameters, fitted or given. Held values are in `x$held`. In evaluate
  mode, where every parameter has a value,
  [`coef()`](https://rdrr.io/r/stats/coef.html) returns only the decay
  `sigma` (or the `sigma` of each group), the parameter the evaluation
  takes as free; the other numbers (`v0`, `beta`, `kappa`) are listed as
  held in `as.data.frame(x, type = "parameters")`.

- [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) returns the
  modelled counts for the observations of a fit (as
  [fitted.ae_multistart_fit()](https://songyosr.github.io/spax/reference/allocation-fit-methods.md)
  does), and the modelled facility utilization, named by facility, for
  an evaluation. For a fit with outside counts (see
  [`obs_flows()`](https://songyosr.github.io/spax/reference/obs_flows.md))
  this is the full target, including the modelled outside count as the
  column, element or row `"(outside)"`.

- [`as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html)
  returns one of the tables below.

- [`predict()`](https://rspatial.github.io/terra/reference/predict.html)
  predicts requested origins from the saved fit or evaluation with
  [`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md).
  Models with demand groups are refused, as there.

**Tables of
[`as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html):**

|  |  |
|----|----|
| `type` | rows and columns |
| `"parameters"` | `parameter`, `value`, `status`: the free parameters (`"fitted"` or `"given"`), then the held values (`"held"`) |
| `"facilities"` | the facilities of the data: `facility_id`, `supply`, `attractiveness`, `utilization`, and `observed` for a fit to facility totals (the sum over groups for totals by group, `NA` when a group is unobserved); the `"(outside)"` entry of a fit with outside counts is not a facility and is left out |
| `"origins"` | the active origins: `origin_id`, `demand`, `access` (the share of demand that goes to a facility of the data), `outside_share` (pooled over the groups), and for a flow fit with outside counts `outside_observed` (`NA` where unknown) and `outside_modelled` (demand times the outside share) |
| `"flows"` | the origin-facility cells with modelled flow or observed count above 0: `origin_id`, `facility_id`, `flow` (demand times allocation), and `observed` for a fit to flows (`NA` where unobserved) |
| `"starts"` | the table of starts of the fit (`seconds` is the time each took) |

## See also

[`spax()`](https://songyosr.github.io/spax/reference/spax.md)

Other allocation interface:
[`decay_gaussian()`](https://songyosr.github.io/spax/reference/decay_gaussian.md),
[`obs_flows()`](https://songyosr.github.io/spax/reference/obs_flows.md),
[`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md),
[`spax()`](https://songyosr.github.io/spax/reference/spax.md),
[`spax_access_values-methods`](https://songyosr.github.io/spax/reference/spax_access_values-methods.md),
[`spax_huff()`](https://songyosr.github.io/spax/reference/spax_huff.md),
[`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)

## Examples

``` r
# West Cornwall: the small areas and practices west of easting 170 km
areas <- sf::st_drop_geometry(cornwall_lsoa)
areas <- areas[!areas$scilly & areas$cent_x < 170000, ]
gp <- sf::st_drop_geometry(cornwall_gp)
gp <- gp[!gp$scilly & gp$easting < 170000, ]
travel <- cornwall_travel[areas$lsoa, gp$practice]
d <- prep_data(stats::setNames(areas$registered, areas$lsoa),
               stats::setNames(gp$gp_fte_extg, gp$practice), travel)

# The conditional logit model fitted to the registrations of these residents
# at these practices
flows <- cornwall_flows[cornwall_flows$lsoa %in% areas$lsoa &
                          cornwall_flows$practice %in% gp$practice, ]
totals <- obs_totals(tapply(flows$patients, flows$practice, sum))
clm <- spax(d, spec_clm(), observed = totals)
clm
#> <spax allocation> conditional logit with an outside option
#>   decay       gaussian (sigma = travel scale): fit
#>   attraction  (kappa * supply)^beta: kappa = 1, beta = 1
#>   outside     v0: fit
#>   data        85 origins x 15 facilities
#> 
#>   parameter  value  status
#>   sigma      13.22  fitted
#>   v0         1.002  fitted
#>   beta       1      held
#>   kappa      1      held
#> 
#>   selected    scan1 (start 1 of 3); 3 of 3 starts converged
#>   boundary    none
#>   starts      default search, 45 of 45 grid points, loss poisson
summary(clm)
#> <spax allocation summary> conditional logit with an outside option
#>   decay       gaussian (sigma = travel scale): fit
#>   attraction  (kappa * supply)^beta: kappa = 1, beta = 1
#>   outside     v0: fit
#> 
#>   parameter  value  status
#>   sigma      13.22  fitted
#>   v0         1.002  fitted
#>   beta       1      held
#>   kappa      1      held
#> 
#> <allocation fit summary>
#> status: selected; selected start: scan1
#> loss disagreement: FALSE; parameter disagreement: FALSE
#> Supplied-start diagnostics (not a global-optimum or identification guarantee):
#>  start     loss eligible optim_convergence solver_converged residual seconds
#>  scan1 -1201000     TRUE                 0             TRUE        0   0.041
#>  scan2 -1201000     TRUE                 0             TRUE        0   0.060
#>  scan3 -1201000     TRUE                 0             TRUE        0   0.044
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
#>  -1201000           0   0.039               0     TRUE
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
coef(clm)
#>     sigma        v0 
#> 13.223212  1.001842 
round(fitted(clm))
#> L82018 L82036 L82038 L82041 L82042 L82044 L82047 L82056 L82059 L82068 L82070 
#>  17829  15924  13051  22441  11935  10758   6762   2851   3674   7954   5444 
#> L82620 Y01050 Y01051 Y01922 
#>   3535  10004   3367  10545 

# The practices with their observed and modeled registrations, and the
# small areas with the modeled share of their residents at these practices
head(as.data.frame(clm, type = "facilities"))
#>   facility_id    supply attractiveness utilization observed
#> 1      L82018 12.280000      12.280000    17829.37    12578
#> 2      L82036  7.946667       7.946667    15923.55    12284
#> 3      L82038  7.880000       7.880000    13051.36    11932
#> 4      L82041 13.266667      13.266667    22441.46    25799
#> 5      L82042  8.013333       8.013333    11934.83     9403
#> 6      L82044  5.893333       5.893333    10758.49     8687
head(as.data.frame(clm, type = "origins"))
#>   origin_id demand    access outside_share
#> 1 E01018825   1883 0.9714886    0.02851144
#> 2 E01018861   1639 0.9818273    0.01817269
#> 3 E01018862   1519 0.9820142    0.01798584
#> 4 E01018863   1927 0.9828645    0.01713554
#> 5 E01018864   1898 0.9830059    0.01699405
#> 6 E01018865   2368 0.9822427    0.01775733

# Modeled outside shares from travel times, here of three of the small areas
predict(clm, travel[1:3, ], outputs = "outside_share")$outputs$outside_share
#>  E01018825  E01018861  E01018862 
#> 0.02851144 0.01817269 0.01798584 
```
