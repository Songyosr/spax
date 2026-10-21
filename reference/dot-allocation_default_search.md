# Default starting values and bounds for an allocation fit

The default search of
[`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md)
(SPAX-058): a rule that takes the starting values and the bounds of the
fitted parameters from the data. It is called after the loss is resolved
and the target is bound once.

## Usage

``` r
.allocation_default_search(
  problem,
  target,
  output,
  loss,
  loss_args,
  lower = NULL,
  upper = NULL,
  solver_args = list(),
  starts = NULL
)
```

## Arguments

- problem:

  A corrected CLM `ae_problem`.

- target:

  The bound target
  ([`.bind_problem_target()`](https://songyosr.github.io/spax/reference/dot-bind_problem_target.md)).

- output:

  The count output being fitted.

- loss, loss_args:

  The resolved loss function and its arguments.

- lower, upper:

  `NULL`, or named bounds for some of the parameters.

- solver_args:

  Solver controls given to the fit: `lambda`, `tol`, `max_iter`, `norm`,
  `check`.

- starts:

  The starts given by the caller, or `NULL`. Given starts widen the
  default bounds; the scan runs only to find default bounds of v0.

## Value

A list with the `starts` matrix (`NULL` when `starts` were given), the
merged `lower` and `upper` and the `record` kept in the fit's
`default_search` field (`NULL` when no scan ran).

## Details

1.  The domain of every kernel is checked against the travel times
    first.

2.  For each free sigma, the engine's kernel is probed at the smallest
    positive travel, a far travel and the largest travel on
    `10^seq(-8, 8, by = 1/8)`; a value qualifies when the weights at the
    nearest and the far travel are positive (the largest travel may
    underflow to zero), the nearest facility of every origin keeps
    weight (`log w(far) - log w(d_lo) >= -200`) and the kernel still
    decays over the range (`w(top) / w(d_lo) <= 0.99`).

3.  Each free sigma gets a grid of 15 log-spaced values over its range,
    from the steepest to the flattest kernel. Group-specific sigmas walk
    together.

4.  A fitted outside weight v0 is seeded from the observed outside share
    on the target mask, `s / (1 - s)` times the demand-weighted median
    of the total opportunity of the origins, times 0.1, 1 and 10. A
    fully observed target with `s <= 0` is an error, and with
    `s < 0.001` a warning.

5.  The loss is evaluated at every grid point by the fitter's own code
    path.

6.  The starts are the best point of up to three sigma values: local
    minima along the grid first, ordered by loss.

7.  Default bounds: the sigma range, `[min / 1000, max * 1000]` of the
    best v0 values (the lower one not below the seed for an outside
    share of 1e-6, at the grid point with the lowest loss), `[0.05, 5]`
    for beta; given entries replace the default ones.

With no free sigma (every sigma held through `fixed`) there is no probe
and no sigma grid: one point at the held values, whose starts are the
three v0 seeds (or beta = 0.5, 1, 2 when only beta is free).
