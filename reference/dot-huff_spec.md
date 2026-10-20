# Construct a static Huff / aggregate-CLM model spec

The no-state member of the ladder: attractiveness is exogenous
(`a_j = (kappa * S_j)^beta`), allocation is one Huff pass, and `v0` is
the optional outside-option mass (`0` = pure Huff). With `fit_v0` /
`fit_beta`, the outside option and/or the supply elasticity join the
fitted theta; otherwise they (and `kappa`) are constants.
`allocation = "clm"` is the conditional-logit share (DEC-013);
`"huff_decay"` preserves the historical extra decay factor used for
one-pass HAAE comparisons.

## Usage

``` r
.huff_spec(
  family = c("gaussian", "exponential", "power"),
  kappa = 1,
  beta = 1,
  v0 = 0,
  fit_v0 = FALSE,
  fit_beta = FALSE,
  allocation = c("clm", "huff_decay"),
  kernel = NULL,
  grouped = NULL
)
```

## Arguments

- kernel:

  `NULL` for a built-in `family`, or a custom `spax_decay_kernel`
  declaration (see
  [`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md)).
  The declaration then names the family, its bounds become the bounds of
  `sigma`, and the spec carries it as `spec$kernel`; built-in specs have
  no `kernel` element.
