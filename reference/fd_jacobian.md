# Finite-difference Jacobian of a vector-valued function

Column `j` is a central difference with step `eps * max(abs(x[j]), 1)`.
With finite `lower` or `upper`, no evaluation point leaves the bounds:
where one side of a central step would, the difference is one-sided with
the same step (forward, or backward when only the lower side fits). When
neither side fits, the larger feasible one-sided step is used,
`max(upper - x, x - lower)`, and a coordinate with no room at all gives
a zero column. Inside the bounds the result is exactly the unbounded
one. The `"forward"` method steps up when that stays inside the bounds
and down otherwise.

## Usage

``` r
fd_jacobian(
  fn,
  x,
  ...,
  eps = 1e-06,
  method = c("central", "forward"),
  check = TRUE,
  lower = -Inf,
  upper = Inf,
  lazy_f0 = FALSE
)
```

## Arguments

- fn:

  A function of a numeric vector returning a numeric vector.

- x:

  The point at which to differentiate, a numeric vector.

- ...:

  Further arguments passed to `fn`.

- eps:

  Relative step size.

- method:

  `"central"` or `"forward"` differences.

- check:

  Whether to check that `fn` returns numeric vectors.

- lower, upper:

  Inclusive bounds of the evaluation points, recycled to the length of
  `x`.

- lazy_f0:

  If `TRUE`, `fn(x)` is evaluated (and checked as usual) only when a
  coordinate needs a one-sided or room step; the result is identical. An
  invalid `fn(x)` is then detected only when it is used.
