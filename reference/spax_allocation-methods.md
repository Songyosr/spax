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
[`spax_huff()`](https://songyosr.github.io/spax/reference/spax_huff.md),
[`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)
