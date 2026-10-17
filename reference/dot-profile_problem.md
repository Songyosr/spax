# Profile one parameter of a prepared AE problem

Each supplied positive `values` point fixes `parameter` and optimizes
every remaining nuisance parameter through
[`.fit_problem_nfxp()`](https://songyosr.github.io/spax/reference/dot-fit_problem_nfxp.md).
The prepared substrate is shared. Each point owns fresh solver/reuse
state; `warm_start` carries only the preceding eligible nuisance
estimate into the next outer optimization. Supplied order is retained.
Observations are bound to canonical output IDs once before the point
loop; nuisance-parameter wrappers preserve that identity and observation
mask.

## Usage

``` r
.profile_problem(
  problem,
  observed,
  parameter,
  values,
  init,
  lower,
  upper,
  ...,
  warm_start = TRUE,
  keep_fits = FALSE
)
```

## Details

`init`, `lower`, and `upper` name the complete original theta. Named
`...` arguments are ordinary
[`.fit_problem_nfxp()`](https://songyosr.github.io/spax/reference/dot-fit_problem_nfxp.md)
controls. Failed, nonconverged, invalid and boundary points remain
visible. The lowest eligible loss wins; ties retain the first supplied
point. Rich fits are retained only for the winner unless
`keep_fits = TRUE`.

This returns likelihood or loss profile values only. A confidence
interval additionally requires a justified loss scale, threshold and
degrees of freedom; none is inferred here. Fixing a nuisance value to
satisfy an external moment or total is a different estimator and is not
this helper.
