# Calibrate a declared allocation model from explicit starting points

Count targets and loss choice are separate. Weighted SSE retains the
existing default; Poisson is explicit. Custom loss/gradient functions
pass through. Observation-indexed loss arguments must be in canonical
order. Starts, boundaries, failed runs and disagreement retain the
existing multistart result; a selected fit is not a claim of
identification or global optimality.

## Usage

``` r
fit_allocation(
  model,
  observed,
  starts,
  lower,
  upper,
  output = "utilization",
  loss = "weighted_sse",
  loss_args = list(),
  loss_grad = NULL,
  gradient = FALSE,
  ...
)
```

## Arguments

- model:

  A corrected CLM declared over checked prepared inputs. A kernel
  declared `smooth = FALSE` cannot be fitted.

- observed:

  Numeric vector or origin-by-facility matrix. Named axes match declared
  IDs independently; unnamed axes are positional. Keep all IDs and use
  NA for unobserved entries. The count-valued `utilization` and `flow`
  targets reject negative nonmissing observations; fractional counts are
  allowed. Other output domains pass through to the existing fitter.

- starts:

  A finite positive matrix, one explicit independent start per row, with
  exactly the named model parameter columns. No random starts are added.
  Each start must lie inside the supplied positive bounds.

- lower, upper:

  Complete named positive parameter bounds with lower \< upper. For a
  declared kernel, the starts and bounds of sigma must lie within the
  bounds of the declaration.

- output:

  Exact model output to calibrate; default facility utilization. `flow`
  is expected origin-facility counts, while `allocation` is an inside
  probability operator. Requesting counts does not select a likelihood.

- loss:

  `"weighted_sse"`, `"poisson"`, or a function accepting predicted and
  observed values. Weighted SSE divides squared residuals by
  observed+eta, with eta=1 by default. Poisson omits the data-only
  log-factorial constant and uses eps=1e-9 by default; joint fitting
  does not force total matching.

- loss_args:

  Named auxiliary arguments passed unchanged to loss callbacks. Any
  observation-indexed values must already follow canonical observed
  order.

- loss_grad:

  Optional matching parameter-gradient callback. Built-in choices supply
  their existing analytic loss-gradient adapters. A custom loss requires
  its own loss_grad when gradient=TRUE; none is invented.

- gradient:

  Logical; use the existing implicit sensitivity route when TRUE. FALSE
  retains the underlying fitter's default behavior.

- ...:

  Named multistart or fitting controls, including solver tolerances,
  optimizer `control`, `keep_fits` and descriptive disagreement
  tolerances.

## Value

The existing `ae_multistart_fit`, retaining selected fit, all per-start
diagnostics, boundaries, warnings/errors and descriptive disagreement.

## Details

A model whose decay kernel was declared with `smooth = FALSE` (see
[`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md))
is refused, because fitting needs a kernel that is smooth in sigma.
Evaluate such a kernel at fixed parameters with
[`evaluate_allocation()`](https://songyosr.github.io/spax/reference/evaluate_allocation.md),
for example over a grid of sigma and v0.

## See also

[allocation-fit-methods](https://songyosr.github.io/spax/reference/allocation-fit-methods.md)
for methods on saved fits.

Other experimental allocation workflow:
[`bootstrap_allocation()`](https://songyosr.github.io/spax/reference/bootstrap_allocation.md),
[`calc_demand()`](https://songyosr.github.io/spax/reference/calc_demand.md),
[`calc_travel_time()`](https://songyosr.github.io/spax/reference/calc_travel_time.md),
[`check_allocation()`](https://songyosr.github.io/spax/reference/check_allocation.md),
[`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md),
[`compare_allocations()`](https://songyosr.github.io/spax/reference/compare_allocations.md),
[`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md),
[`evaluate_allocation()`](https://songyosr.github.io/spax/reference/evaluate_allocation.md),
[`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md),
[`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md)
