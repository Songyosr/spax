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

## Examples

``` r
# West Cornwall: the small areas and practices west of easting 170 km
areas <- sf::st_drop_geometry(cornwall_lsoa)
areas <- areas[!areas$scilly & areas$cent_x < 170000, ]
gp <- sf::st_drop_geometry(cornwall_gp)
gp <- gp[!gp$scilly & gp$easting < 170000, ]
inputs <- prepare_allocation(stats::setNames(areas$registered, areas$lsoa),
                             stats::setNames(gp$gp_fte_extg, gp$practice),
                             cornwall_travel[areas$lsoa, gp$practice])

# A fit to the registrations of these residents at these practices
flows <- cornwall_flows[cornwall_flows$lsoa %in% areas$lsoa &
                          cornwall_flows$practice %in% gp$practice, ]
totals <- sapply(split(flows$patients, flows$practice), sum)
model <- clm_allocation(inputs, fit_v0 = TRUE)
fit <- fit_allocation(model, totals, loss = "poisson")
as.data.frame(fit)
#>   parameter  estimate
#> 1     sigma 13.223212
#> 2        v0  1.001842

# Coverage at the estimates: for the region, by small area, and by the
# rural-urban class of the small areas
coverage <- evaluate_allocation(model, coef(fit))$coverage
as.data.frame(coverage)
#>     zone demand       rho            A contact_need     abar_raw masked
#> 1 region 149306 0.9783587 0.0006053144     146074.8 0.0006187039  FALSE
#>           abar     quadrant
#> 1 0.0006187039 unclassified
head(as.data.frame(coverage, type = "cells"))
#>           origin_id registry_index demand       rho            A contact_need
#> E01018825 E01018825              1   1883 0.9714886 0.0005915637     1829.313
#> E01018861 E01018861              2   1639 0.9818273 0.0006340469     1609.215
#> E01018862 E01018862              3   1519 0.9820142 0.0006239507     1491.680
#> E01018863 E01018863              4   1927 0.9828645 0.0006160261     1893.980
#> E01018864 E01018864              5   1898 0.9830059 0.0006125868     1865.745
#> E01018865 E01018865              6   2368 0.9822427 0.0005996822     2325.951
#>               abar_raw masked         abar     quadrant
#> E01018825 0.0006089250  FALSE 0.0006089250 unclassified
#> E01018861 0.0006457825  FALSE 0.0006457825 unclassified
#> E01018862 0.0006353785  FALSE 0.0006353785 unclassified
#> E01018863 0.0006267661  FALSE 0.0006267661 unclassified
#> E01018864 0.0006231772  FALSE 0.0006231772 unclassified
#> E01018865 0.0006105234  FALSE 0.0006105234 unclassified
zones <- data.frame(origin_id = areas$lsoa, zone = areas$ruc21)
as.data.frame(coverage, type = "zones", zones = zones)
#>                                               zone demand       rho
#> 1 Smaller rural: Further from a major town or city  36789 0.9750877
#> 2         Urban: Further from a major town or city  80364 0.9801664
#> 3  Larger rural: Further from a major town or city  32153 0.9775829
#>              A contact_need     abar_raw masked         abar     quadrant
#> 1 0.0006014435     35872.50 0.0006168096  FALSE 0.0006168096 unclassified
#> 2 0.0006064560     78770.09 0.0006187276  FALSE 0.0006187276 unclassified
#> 3 0.0006068900     31432.22 0.0006208066  FALSE 0.0006208066 unclassified
```
