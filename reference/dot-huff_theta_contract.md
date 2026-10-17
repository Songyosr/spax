# Theta contract for the static Huff/CLM spec

`fit_v0 = TRUE` promotes the outside-option mass `v0`, and
`fit_beta = TRUE` the supply elasticity `beta` (attractiveness
`a_j = (kappa S_j)^beta`), from constants to fitted parameters,
estimated jointly in `(sigma, [v0], [beta])`. The default `clm`
allocation is the conditional logit (DEC-013). Appended order is
`sigma`, then `v0`, then `beta`; all bounded `[0, Inf)` with positive
search bounds from the caller. A declared decay kernel replaces the
bounds of `sigma` with its own.

## Usage

``` r
.huff_theta_contract(
  fit_v0 = FALSE,
  fit_beta = FALSE,
  sigma_lower = 0,
  sigma_upper = Inf
)
```

## Arguments

- fit_v0, fit_beta:

  Whether `v0` and `beta` are fitted parameters.

- sigma_lower, sigma_upper:

  Inclusive bounds of `sigma`; the declaration's `lower` and `upper` for
  a declared decay kernel.
