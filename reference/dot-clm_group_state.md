# Evaluate the grouped static CLM at exogenous a

Per group g the inside choice probability \`P_g\` has its own kernel and
outside weight \`v0\[g\]\`; the pooled allocation is the demand-weighted
\`P_bar = sum_g D_g P_g / D\` with \`D\` the total demand, so the pooled
outputs are the ungrouped ones on the total demand: \`utilization =
sum_g U_g = crossprod(P_bar, D)\`, \`access = rowSums(P_bar)\` and
\`outside_share\` the demand-weighted outside probability (\`access +
outside_share = 1\`). The by-group outputs \`utilization_by_group\`
(facility by group), \`access_by_group\` and \`outside_share_by_group\`
(origin by group) are returned too; the group probability matrices are
not kept.

## Usage

``` r
.clm_group_state(a, kernels, D_groups, v0)
```

## Arguments

- a:

  Attractiveness, one value per facility.

- kernels:

  A list with the weight matrix (origin by facility) of every group, in
  the order of the columns of \`D_groups\`.

- D_groups:

  Demand by origin (rows) and group (columns), nonnegative with a
  positive total on every row.

- v0:

  Outside-option weight of each group.
