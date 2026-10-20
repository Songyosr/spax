# Theta contract of a grouped static CLM spec

Names in order: `sigma` or `sigma.<g>` for every group (when `"sigma"`
is in `by_group`), then `v0` or `v0.<g>` when `v0` is fitted, then
`beta` when it is fitted; the parameters in `spec$fixed` are removed.
The bounds of a sigma are those of the declared kernel of its group
(`[0, Inf)` for a built-in one); the extra `group` field gives the group
of each parameter (`NA` when shared).

## Usage

``` r
.huff_group_theta_contract(spec)
```

## Arguments

- spec:

  A grouped spec with `groups`, `by_group`, `families`, `fixed`,
  `fit_v0` and `fit_beta`.
