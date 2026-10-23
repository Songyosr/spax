# Run an allocation model on prepared data

Experimental. `spax()` binds a model specification
([`spec_huff()`](https://songyosr.github.io/spax/reference/spec_clm.md),
[`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md))
to prepared data
([`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md))
and either fits the parameters that are `"fit"` to observed counts
([`obs_flows()`](https://songyosr.github.io/spax/reference/obs_flows.md),
[`obs_totals()`](https://songyosr.github.io/spax/reference/obs_flows.md))
or, when every parameter has a value, evaluates the model at those
values.
[`spax_huff()`](https://songyosr.github.io/spax/reference/spax_huff.md)
and
[`spax_clm()`](https://songyosr.github.io/spax/reference/spax_huff.md)
do the same from raw inputs in one call.

## Usage

``` r
spax(
  data,
  model,
  observed = NULL,
  starts = NULL,
  lower = NULL,
  upper = NULL,
  ...
)
```

## Arguments

- data:

  Prepared inputs: the result of
  [`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md).

- model:

  A model specification:
  [`spec_huff()`](https://songyosr.github.io/spax/reference/spec_clm.md)
  or
  [`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md),
  or an FCA specification such as
  [`spec_e2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md).

- observed:

  Observed counts for a model with a parameter to fit: an
  [`obs_flows()`](https://songyosr.github.io/spax/reference/obs_flows.md)
  or
  [`obs_totals()`](https://songyosr.github.io/spax/reference/obs_flows.md)
  object, or raw counts (see Details). `NULL` (the default) for a model
  with a value for every parameter.

- starts, lower, upper:

  Optional starting values and bounds for the parameters being fitted,
  as in
  [`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md).
  They are refused when nothing is fitted.

- ...:

  Fit mode: further arguments of
  [`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md)
  (`gradient`, `control`, `lambda`, `tol`, `max_iter`, `norm`,
  `keep_fits`, ...); `output`, `loss` and `loss_args` are refused.
  Evaluate mode: further arguments of
  [`evaluate_allocation()`](https://songyosr.github.io/spax/reference/evaluate_allocation.md),
  for example `requested_outputs`.

## Value

For an FCA specification on raster data, a `spax` object (class
`c("spax", "spax_result")`), see
[`spax_e2sfca()`](https://songyosr.github.io/spax/reference/spax_e2sfca.md);
on numeric data, a `spax_access_values` (class
`c("spax_access_values", "spax_result")`), see
[spax_access_values-methods](https://songyosr.github.io/spax/reference/spax_access_values-methods.md).
For an allocation model, an object of class
`c("spax_allocation", "spax_result")`: a list with `model` (the bound
`ae_problem`), `spec`, `observed` (a list with `spec`, the observation
as declared, `target`, the counts aligned to the data, and `output`;
`NULL` in evaluate mode), `fit` (the `ae_multistart_fit`, in fit mode),
`evaluation` (the `ae_equilibrium`, in evaluate mode), `theta` (the
named free parameters, fitted or given; `NULL` when no start was
selected), `held` (the named held values, including `kappa`), `mode`
(`"fit"` or `"evaluate"`) and `call`. See
[spax_allocation-methods](https://songyosr.github.io/spax/reference/spax_allocation-methods.md)
for what to do with it.

## Details

**FCA indices.** With raster data
([`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md)
with a `SpatRaster` as demand) and an FCA specification
([`spec_2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md),
[`spec_e2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md),
[`spec_m2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md),
[`spec_3sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md),
[`spec_huff2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md)),
`spax()` computes the index and returns the same object as the matching
one-shot
([`spax_2sfca()`](https://songyosr.github.io/spax/reference/spax_2sfca.md),
[`spax_e2sfca()`](https://songyosr.github.io/spax/reference/spax_e2sfca.md)
and the others): the `type`, `parameters` and accessibility layers are
identical, and `call` is the call of `spax()`. An index is computed, not
fitted: `observed`, `starts`, `lower`, `upper` and `...` are refused.
Group lists of `decay` and `selection` and a `threshold` by group are
matched to the demand groups of the data by name and put in their order.

**FCA indices on numeric data.** With numeric data (demand, supply and a
travel matrix) an FCA specification gives the index at the active
origins, computed by the same engine and the same rules (the cells that
reach no facility are `NA`, and the overall layer of demand groups is
the same). The result is a `spax_access_values`, a list with `access`, a
matrix with one row per active origin and one column per layer, not a
raster (see
[spax_access_values-methods](https://songyosr.github.io/spax/reference/spax_access_values-methods.md)).
Numeric data has one supply measure, `"supply"`, so there is one
measure, and an origin with zero or unknown demand has no row. A raster
`sigma` (a variable catchment) is refused: "a raster sigma needs raster
data".

**Allocation models on raster data.** With raster data and an allocation
model
([`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md),
[`spec_huff()`](https://songyosr.github.io/spax/reference/spec_clm.md)),
`spax()` first converts the data to origins, on each call and without
keeping anything: the cells with finite, positive demand are the
origins, their IDs are the cell numbers (`"843"`) and each has its
travel to every facility; a missing or infinite travel time is an absent
pair, and a cell that reaches no facility stays an origin: with
[`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)
its demand goes to the outside option, while
[`spec_huff()`](https://songyosr.github.io/spax/reference/spec_clm.md)
has no outside option and allocates none of it.
[`obs_totals()`](https://songyosr.github.io/spax/reference/obs_flows.md)
works as it is, and
[`obs_flows()`](https://songyosr.github.io/spax/reference/obs_flows.md)
is keyed by cell number: counts at cells that are not origins are
dropped with a message. The model then runs as on numeric data, so every
step function accepts the result, and fits to the same data can be
compared. See
[`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md)
for the conversion and its memory.

**Fit mode.** Some parameter is `"fit"`. `observed` is needed. The
counts are aligned with the data (see
[`obs_flows()`](https://songyosr.github.io/spax/reference/obs_flows.md)),
the model is bound to the data and
[`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md)
fits it. The loss is set by the observation and the output it is
compared with by its kind, so `output`, `loss` and `loss_args` are
refused in `...`. Starts, bounds and optimizer settings come from the
data when they are not given: `starts`, `lower` and `upper` are optional
(see the default search in
[`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md)).
`spax()` always passes analytic gradients and the strict optimizer
settings `control = list(maxit = 300, factr = 1e3, pgtol = 1e-2)`, also
when you give `starts`, unless `...` gives `gradient` or `control`
itself. The result does not solve the model again: it reads the selected
fit. A search in which no start converged gives a warning and a result
with no estimates;
[`summary()`](https://rspatial.github.io/terra/reference/summary.html)
shows the starts and `as.data.frame(type = "starts")` their table.

**Outside counts.** An observation that declares outside counts (the
`outside` argument of
[`obs_flows()`](https://songyosr.github.io/spax/reference/obs_flows.md)
and
[`obs_totals()`](https://songyosr.github.io/spax/reference/obs_flows.md))
is fitted with the outputs `"flow_outside"`, `"utilization_outside"` or
`"utilization_outside_by_group"` of
[`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md):
the observed counts with the outside count as one more column, element
or row named `"(outside)"`, and the Poisson loss. The model needs an
outside option
([`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)
with an outside weight above 0 somewhere): with a model without one
([`spec_huff()`](https://songyosr.github.io/spax/reference/spec_clm.md),
or `outside = 0` everywhere) `spax()` stops with "outside counts need a
model with an outside option (spec_clm())" before the fit. The loss
arguments of the observation pass through unchanged. The printed result
has a line `outside` that says for how many origins (or groups) the
counts are used, or that the outside total is.
[`fitted()`](https://rdrr.io/r/stats/fitted.values.html) returns the
modelled counts for the full target, the `"(outside)"` entry included,
and `as.data.frame(type = "origins")` adds `outside_observed` and
`outside_modelled` for flows (see
[spax_allocation-methods](https://songyosr.github.io/spax/reference/spax_allocation-methods.md)).

**Evaluate mode.** Every parameter has a number. Nothing is fitted, so
`observed`, `starts`, `lower` and `upper` are refused, and the model is
evaluated once at the given values. In this version an ungrouped model
with something to fit cannot hold its decay `sigma`; give the decay
`sigma = "fit"`, or give `beta` and the outside weight too to evaluate.

**Observed.** Besides
[`obs_flows()`](https://songyosr.github.io/spax/reference/obs_flows.md)
and
[`obs_totals()`](https://songyosr.github.io/spax/reference/obs_flows.md),
`observed` can be a named numeric vector (facility totals) or a matrix,
read as flows when the row names are origins and the column names
facilities of the data, and as totals by group when the column names are
the groups of the data.

**What a result holds.** A `spax_allocation` stores the bound model, the
specification, the observations as aligned to the data and the fit, or
the evaluation. [`coef()`](https://rdrr.io/r/stats/coef.html) gives the
free parameters (fitted or given) and the held values are in `$held` and
in `as.data.frame(x)`. Outputs that the fit did not compute, for
instance the flows of a fit to facility totals, are computed when asked
for and not stored. The step functions
([`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md),
[`check_allocation()`](https://songyosr.github.io/spax/reference/check_fit.md),
[`compare_allocations()`](https://songyosr.github.io/spax/reference/compare_fits.md),
[`bootstrap_allocation()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md))
accept the result itself: each takes the bound model and the fit (or,
for prediction, the evaluation) from it. Use
[`saveRDS()`](https://rspatial.github.io/terra/reference/serialize.html)
to save a result.

## See also

[`spax_huff()`](https://songyosr.github.io/spax/reference/spax_huff.md)
and
[`spax_clm()`](https://songyosr.github.io/spax/reference/spax_huff.md)
for one call from raw inputs.

Other allocation interface:
[`decay_gaussian()`](https://songyosr.github.io/spax/reference/decay_gaussian.md),
[`obs_flows()`](https://songyosr.github.io/spax/reference/obs_flows.md),
[`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md),
[`spax_access_values-methods`](https://songyosr.github.io/spax/reference/spax_access_values-methods.md),
[`spax_allocation-methods`](https://songyosr.github.io/spax/reference/spax_allocation-methods.md),
[`spax_huff()`](https://songyosr.github.io/spax/reference/spax_huff.md),
[`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)

## Examples

``` r
# Mainland Cornwall: registered residents, GP FTE and travel minutes
areas <- sf::st_drop_geometry(cornwall_lsoa[!cornwall_lsoa$scilly, ])
gp <- sf::st_drop_geometry(cornwall_gp[!cornwall_gp$scilly, ])
travel <- cornwall_travel[areas$lsoa, gp$practice]
supply <- stats::setNames(gp$gp_fte_extg, gp$practice)

# Huff: power decay and a fitted attractiveness exponent, fitted to the flows
# of residents to their practices. Demand is the residents registered at the
# mainland practices, since the Huff model has no outside option.
d_huff <- prep_data(stats::setNames(areas$registered_in, areas$lsoa), supply, travel)
flows <- obs_flows(cornwall_flows, origin = "lsoa", facility = "practice",
                   count = "patients")
huff <- spax(d_huff, spec_huff(), observed = flows)
#> observed: 2,290 counts from 1 origin outside the data or left out by it are dropped; 5 counts at 1 facility outside the data are dropped (the Huff model has no outside option)
huff
#> <spax allocation> Huff model (no outside option)
#>   decay       power (sigma = exponent): fit
#>   attraction  (kappa * supply)^beta: kappa = 1, beta = fit
#>   outside     none
#>   data        336 origins x 54 facilities
#> 
#>   parameter  value   status
#>   sigma      2.393   fitted
#>   beta       0.4863  fitted
#>   v0         0       held
#>   kappa      1       held
#> 
#>   selected    scan2 (start 2 of 3); 3 of 3 starts converged
#>   boundary    none
#>   starts      default search, 15 of 15 grid points, loss poisson
coef(huff)
#>     sigma      beta 
#> 2.3928431 0.4862684 
head(as.data.frame(huff, type = "facilities"))
#>   facility_id    supply attractiveness utilization
#> 1      L82001 10.298887       3.108049   17683.864
#> 2      L82003  4.693333       2.120900    4503.766
#> 3      L82004  4.341333       2.042001   12293.328
#> 4      L82006  7.973333       2.744347   14070.002
#> 5      L82007  2.888800       1.675068    7105.921
#> 6      L82008  4.670400       2.115854    9347.930

# Conditional logit with a fitted outside weight, fitted to practice totals
d <- prep_data(stats::setNames(areas$registered, areas$lsoa), supply, travel)
totals <- obs_totals(tapply(cornwall_flows$patients, cornwall_flows$practice, sum))
clm <- spax(d, spec_clm(decay_gaussian(), outside = "fit"), observed = totals)
#> observed: 2,279 counts at 1 facility outside the data are left out
summary(clm)
#> <spax allocation summary> conditional logit with an outside option
#>   decay       gaussian (sigma = travel scale): fit
#>   attraction  (kappa * supply)^beta: kappa = 1, beta = 1
#>   outside     v0: fit
#> 
#>   parameter  value     status
#>   sigma      2.559     fitted
#>   v0         0.002249  fitted
#>   beta       1         held
#>   kappa      1         held
#> 
#> <allocation fit summary>
#> status: selected; selected start: scan3
#> loss disagreement: TRUE; parameter disagreement: TRUE
#> Supplied-start diagnostics (not a global-optimum or identification guarantee):
#>  start     loss eligible optim_convergence solver_converged residual seconds
#>  scan1 -5036000     TRUE                 0             TRUE        0   0.047
#>  scan2 -5036000     TRUE                 0             TRUE        0   0.184
#>  scan3 -5037000     TRUE                 0             TRUE        0   0.082
#>  boundary   status                                    optim_message
#>     FALSE eligible CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL
#>     FALSE eligible CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL
#>     FALSE eligible CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL
#>  solver_message log_parameter_gap loss_gap
#>       converged               7.1      673
#>       converged               7.1      673
#>       converged               0.0        0
#> 
#> Selected fit:
#> <allocation fit result summary>
#> model: conditional logit
#> family: gaussian (sigma = travel scale)
#> allocation_form: clm
#> 
#> theta:
#>  parameter estimate
#>      sigma 2.559000
#>         v0 0.002249
#> 
#> fit:
#>      loss convergence seconds spectral_radius gradient
#>  -5037000           0    0.08               0     TRUE
#> 
#> predicted:
#>   min  q25 median  mean   q75   max
#>  2962 5437   8439 10850 15440 42450
#> 
#> state:
#>    min   q25 median  mean  q75   max
#>  1.133 3.272   4.88 5.765 7.93 13.36
#> 
#> residual:
#>    min   q25 median   mean  q75  max
#>  -5753 -1929 -721.6 -238.1 1240 9697

# The same conditional logit fitted to the flows, with the outside counts known:
# demand counts the registered residents, so each origin's outside count is its
# demand minus its counts at the practices of the data
known <- obs_flows(cornwall_flows, origin = "lsoa", facility = "practice",
                   count = "patients", outside = "known")
clm_known <- spax(d, spec_clm(), observed = known)
#> observed: 2,290 counts from 1 origin outside the data or left out by it are dropped; 5 counts at 1 facility outside the data are treated as the outside option
clm_known
#> <spax allocation> conditional logit with an outside option
#>   decay       gaussian (sigma = travel scale): fit
#>   attraction  (kappa * supply)^beta: kappa = 1, beta = 1
#>   outside     v0: fit
#>   data        336 origins x 54 facilities
#> 
#>   parameter  value    status
#>   sigma      4.619    fitted
#>   v0         0.04864  fitted
#>   beta       1        held
#>   kappa      1        held
#> 
#>   selected    scan1 (start 1 of 3); 3 of 3 starts converged
#>   boundary    none
#>   outside     counts used for 336 of 336 origins
#>   starts      default search, 45 of 45 grid points, loss poisson

# Nothing to fit: evaluate at given values, with no observations
spax(d_huff, spec_huff(decay_power(sigma = 1.5), beta = 1))
#> <spax allocation> Huff model (no outside option)
#>   decay       power (sigma = exponent): 1.5
#>   attraction  (kappa * supply)^beta: kappa = 1, beta = 1
#>   outside     none
#>   data        336 origins x 54 facilities
#> 
#>   parameter  value  status
#>   sigma      1.5    given
#>   v0         0      held
#>   beta       1      held
#>   kappa      1      held
```
