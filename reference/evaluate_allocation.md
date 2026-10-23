# Evaluate a declared allocation model at supplied fixed parameters

Solves once and constructs compact coverage from the saved evaluated
outputs. The equilibrium result carries its prediction snapshot; adding
coverage does not retain the original model or create a scenario type.
The caller changes system supply/demand by preparing a new model
explicitly.

## Usage

``` r
evaluate_allocation(model, theta, coverage_args = list(), ...)
```

## Arguments

- model:

  A static CLM from
  [`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md)
  or
  [`huff_allocation()`](https://songyosr.github.io/spax/reference/huff_allocation.md).

- theta:

  A complete model parameter vector. Named parameters align to the model
  contract; unnamed values follow its declared order. This operation
  estimates no parameters and is not a causal intervention estimator.
  For a model with demand groups, a parameter held fixed by
  `clm_allocation(fixed = )` may be given only with its fixed value, and
  unknown names are an error.

- coverage_args:

  Named reporting controls for compact coverage, including optional
  `norm`, `units`, reporting floors and classification cuts. A norm must
  have the same declared supply-per-demand scale as capacity
  attribution.

- ...:

  Named controls passed to the fixed-parameter solver, including
  `requested_outputs`. For a model with demand groups, request the
  per-group outputs here, for example
  `requested_outputs = c("utilization_by_group", "access_by_group", "outside_share_by_group")`;
  the other outputs are pooled over the groups (demand-weighted). A
  group's own access and outside share are `NA` on rows where that group
  has no demand and its travel there is not usable.

  The expected counts that can be requested with `requested_outputs` are
  `flow` (origins by facilities), `flow_outside` (`flow` with one more
  column, `"(outside)"`, holding each origin's expected outside count
  `D_i * outside_share_i`), `utilization_outside` (`utilization` with
  one more element, `"(outside)"`, the expected outside count of all
  origins) and, with demand groups, `utilization_by_group` and
  `utilization_outside_by_group` (with one more row, `"(outside)"`, the
  expected outside count of each group). The outside counts are zero for
  a model without an outside option; they are the outputs that
  [`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md)
  fits to observed outside counts.

## Value

An `ae_equilibrium` with its portable prediction snapshot (none for a
model with demand groups, which cannot be predicted yet) and compact
`coverage`. Coverage uses raw supply units without kappa; proportional
attributed capacity is not a delivered-service measure or a hard
constraint on expected utilization. In `outputs` the vectors are named
by facility ID (`target`, `utilization`, `attractiveness`, and
`utilization_outside`, whose last element is `"(outside)"`) or by origin
ID (`access`, `outside_share`), and the matrices by their two axes:
origins by facilities (`allocation`, `flow`) and, with demand groups,
facilities or origins by groups.

## See also

Other allocation steps:
[`bootstrap_fit()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md),
[`calc_demand()`](https://songyosr.github.io/spax/reference/calc_demand.md),
[`calc_travel_time()`](https://songyosr.github.io/spax/reference/calc_travel_time.md),
[`check_fit()`](https://songyosr.github.io/spax/reference/check_fit.md),
[`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md),
[`compare_fits()`](https://songyosr.github.io/spax/reference/compare_fits.md),
[`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md),
[`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md),
[`huff_allocation()`](https://songyosr.github.io/spax/reference/huff_allocation.md),
[`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md),
[`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md)

## Examples

``` r
# West Cornwall: the small areas and practices west of easting 170 km
areas <- sf::st_drop_geometry(cornwall_lsoa)
areas <- areas[!areas$scilly & areas$cent_x < 170000, ]
gp <- sf::st_drop_geometry(cornwall_gp)
gp <- gp[!gp$scilly & gp$easting < 170000, ]
demand <- stats::setNames(areas$registered, areas$lsoa)
supply <- stats::setNames(gp$gp_fte_extg, gp$practice)
travel <- cornwall_travel[areas$lsoa, gp$practice]
model <- clm_allocation(prepare_allocation(demand, supply, travel), fit_v0 = TRUE)

# The model at a travel scale of 10 minutes and an outside weight of 1
ev <- evaluate_allocation(model, c(sigma = 10, v0 = 1))
ev$coverage
#> <allocation coverage> gaussian (sigma=10, v0=1): A=0.0006053; rho=0.9707; abar=0.0006236 
#> conservation: PASS  relative difference: 0 
#> facilities: 15  excluded (zero load): 0 
#> support: fitted positive-demand origins 

# Expected registrations at each practice
round(ev$outputs$utilization)
#> L82018 L82036 L82038 L82041 L82042 L82044 L82047 L82056 L82059 L82068 L82070 
#>  16239  16060  13494  23063  11912  11144   6753   2357   3323   8026   5628 
#> L82620 Y01050 Y01051 Y01922 
#>   3598  10343   3468   9525 

# Expected registrations by small area and practice, with the expected
# outside count as one more column
flow <- evaluate_allocation(model, c(sigma = 10, v0 = 1),
                            requested_outputs = "flow_outside")$outputs$flow_outside
round(flow[1:3, c("L82047", "(outside)")])
#>           L82047 (outside)
#> E01018825      5        73
#> E01018861     99        41
#> E01018862    113        37

# A scenario: Marazion Surgery (L82047) closes. Prepare the changed inputs
# and evaluate them at the same values
closed <- clm_allocation(prepare_allocation(demand, replace(supply, "L82047", 0),
                                            travel), fit_v0 = TRUE)
change <- evaluate_allocation(closed, c(sigma = 10, v0 = 1))$outputs$utilization -
  ev$outputs$utilization
round(change)
#> L82018 L82036 L82038 L82041 L82042 L82044 L82047 L82056 L82059 L82068 L82070 
#>    683    874   1041    606    262    342  -6753     87    141    283    434 
#> L82620 Y01050 Y01051 Y01922 
#>     87    798    266    625 
```
