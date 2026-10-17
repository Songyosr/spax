# Inspect saved allocation result tables

These methods read saved values without solving, fitting, resampling or
constructing maps. Fit tables use `type = "parameters"` and contain
parameter names and estimates. Other selectors are rejected. A
multistart fit uses the selected fit; when none is eligible, use
[`summary()`](https://rspatial.github.io/terra/reference/summary.html)
to inspect all supplied starts instead.

## Usage

``` r
# S3 method for class 'ae_problem_nfxp_fit'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  type = c("parameters"),
  ...
)

# S3 method for class 'ae_multistart_fit'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  type = c("parameters"),
  ...
)

# S3 method for class 'ae_coverage'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  type = c("regional", "cells", "zones"),
  zones = NULL,
  ...
)

# S3 method for class 'ae_allocation_bootstrap'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  type = c("estimates", "draws", "starts"),
  ...
)
```

## Arguments

- x:

  A fitted allocation result, compact coverage, or bootstrap result.

- row.names, optional:

  Standard
  [`as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html)
  arguments.

- type:

  Saved table to extract; choices depend on the result class.

- ...:

  Additional generic arguments.

- zones:

  For coverage with `type = "zones"`: a data frame with `origin_id` and
  `zone` columns.

## Value

A data frame of saved values. Coverage tables additionally carry
`support`, `input_metadata` and `reporting_settings` attributes.

## Details

Coverage tables preserve the recorded support, input-unit declarations
and reporting settings as attributes. `type = "cells"` returns retained
positive demand origins, including their canonical IDs and any existing
spatial cell index; it does not expand to unknown or zero-demand
prediction locations. Reported and raw conditional capacity remain
separate saved columns. `type = "zones"` aggregates the saved cells to
the zones in `zones` (a data frame with `origin_id` and `zone` columns
covering every retained origin), averaging contact and attributed
capacity over demand and conditional capacity over contacting demand, so
that the zone values keep the same relationship as the cell values.
Reporting floors and classes are applied after aggregation.

Bootstrap tables contain saved point/interval estimates, every attempted
draw, or every attempted start. Failed draws, inner boundaries and
refusal statuses are retained. Extraction neither creates intervals nor
changes eligibility.
