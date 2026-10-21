# Refuse group-specific parameters that the target cannot identify

A model with demand groups fitted with free group-specific parameters
(`sigma.<g>`, `v0.<g>`) needs group-labelled targets:
`utilization_by_group` (or `utilization_outside_by_group`, whose outside
row does not count as an observed facility cell) with observed counts
for every group that has such a parameter, and positive demand in that
group. Aggregated targets leave the group parameters multimodal and
weakly identified (SPAX-057, D2). Fixed group parameters and shared
parameters are allowed with any target; ungrouped problems return at
once. The check runs inside
[`.bind_problem_target()`](https://songyosr.github.io/spax/reference/dot-bind_problem_target.md),
in both its branches, so every fitting path (multistart, profile, nfxp,
ladder, bootstrap and inference refits) refuses once, before any start.
Group-specific free parameters are read from the names of the theta
contract (a profile subproblem rebuilds the contract from its names
alone).

## Usage

``` r
.check_group_targets(problem, target)
```

## Arguments

- problem:

  An `ae_problem`.

- target:

  A bound target (`ae_bound_target`).
