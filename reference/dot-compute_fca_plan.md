# Execute an FCA plan on the compact substrate (terra-free interpreter)

\`gather -\> ratio -\> spread\` as matrix ops. Consumes only the plan's
plain matrices/vectors – no terra – and returns one access vector per
measure (over the plan's reachable cells). The kernels in \`plan\` may
be dense base matrices or \`Matrix::\` sparse matrices; this loop is
identical either way, which is what lets a non-terra / sparse front-end
reuse it unchanged. This is also the hot loop an AE fixed point would
iterate (no rewrap inside).

## Usage

``` r
.compute_fca_plan(plan)
```
