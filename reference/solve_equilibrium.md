# Solve a fixed point with damped Picard iteration

Solves \`x = map(x, ...)\` by iterating \`x_next = (1 - lambda) \* x +
lambda \* map(x, ...)\`.

## Usage

``` r
solve_equilibrium(
  map,
  x0,
  lambda = 1,
  tol = 1e-08,
  max_iter = 1000,
  norm = c("max", "l2"),
  keep_history = TRUE,
  warn = TRUE,
  check = TRUE,
  keep_state_history = FALSE,
  ...
)
```
