# Fit several AE problems against one shared target and compare them

The nested static-vs-feedback question (DEC-012): fit every problem in
the ladder against the *same* observed target with the *same* loss, then
put the legs side by side. Comparability is guaranteed by construction;
the result reports, it does not decide. `init`/`lower`/`upper` are
shared across problems, or a named list keyed by problem label for
per-problem theta contracts. Solver/runner options in `...` are shared
by all legs.

## Usage

``` r
.fit_problem_ladder(
  problems,
  observed,
  init,
  lower,
  upper,
  output = "utilization",
  loss = .weighted_sse_loss,
  loss_grad = .weighted_sse_gradient,
  loss_args = list(eta = 1),
  ...
)
```
