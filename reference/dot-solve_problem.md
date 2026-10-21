# Solve an AE problem at fixed theta

Successful built-in corrected CLM evaluations attach a plain O(J)
prediction snapshot containing the actual specification, theta, aligned
supply/load and declared input policy/units. Requested-location
prediction uses this record after serialization without retaining the
problem or original travel matrix. A model with a declared decay kernel
(see
[`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md))
attaches the same record with the declaration, which is plain data,
appended to the specification; the record then has a separate schema
version.

## Usage

``` r
.solve_problem(
  problem,
  theta,
  x0 = NULL,
  lambda = 1,
  tol = 1e-08,
  max_iter = 1000,
  norm = c("max", "l2"),
  keep_history = TRUE,
  warn = TRUE,
  check = TRUE,
  diagnostics = TRUE,
  keep_state_history = FALSE,
  requested_outputs = NULL,
  keep_step = FALSE,
  keep = NULL
)
```

## Arguments

- requested_outputs:

  Optional output names to materialize in addition to the provider's
  default outputs. `NULL` retains the legacy output callback.

- keep_step:

  If `TRUE`, the bound step is returned as `$step`, so a gradient at the
  same theta need not bind again. Only the optimizer asks for it, never
  the final fresh solve.

- keep:

  Lean evaluation (see
  [`.bind_theta()`](https://songyosr.github.io/spax/reference/dot-bind_theta.md));
  only the optimizer and the default scan pass it, never the final fresh
  solve.
