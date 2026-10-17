# Compare static, ratio-feedback and free-attractiveness CLM fits

Fits one origin-by-facility flow matrix five ways with a Poisson loss
and reports how much each model improves on the static CLM:

## Usage

``` r
.ratio_identification(
  inputs,
  observed,
  starts,
  lower,
  upper,
  gamma_upper = 5,
  b_upper = 3,
  solve_control = list(lambda = 0.4, tol = 1e-10, max_iter = 5000)
)
```

## Arguments

- inputs:

  Prepared inputs from
  [`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md).

- observed:

  Origin-by-facility flow matrix with the prepared IDs as dimnames;
  unobserved entries are not allowed.

- starts:

  Matrix of explicit `sigma`/`v0` starts, one row per start.

- lower, upper:

  Named bounds for `sigma` and `v0`.

- gamma_upper, b_upper:

  Upper bounds for gamma and b.

- solve_control:

  Named solver controls for the feedback solves.

## Value

A data frame with one row per model: parameters, number of fitted
parameters `k`, Poisson `loss`, `converged`, `gain` over the static fit
and `share_of_free_gain`, with the interpretation as attribute `note`.

## Details

- `static`: sigma and v0 (`feedback_scale = 1`);

- `static_b`: adds the supply elasticity b;

- `ratio`: adds the feedback coefficient gamma;

- `ratio_b`: both b and gamma;

- `free`: a separate attractiveness for every facility.

The free model can reproduce any equilibrium attractiveness that a
feedback rule produces, so it bounds what a cross-section can attribute
to feedback. When the ratio models recover most of the free model's
gain, the gain is equally consistent with facility differences unrelated
to load, including supply that follows load (for example staffing set by
list size). In a single cross-section gamma is identified only through
the assumed load-response form and cannot be separated from
load-correlated facility differences; separating them needs supply that
changes over time or capacity that binds.
