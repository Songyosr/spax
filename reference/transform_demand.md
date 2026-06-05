# Transform Case Count Data into Spatial Distribution

Transforms case count data from vector format into either a probability
mass function (PMF) or density surface, optionally using an existing PMF
as a weighting surface.

## Usage

``` r
transform_demand(
  x,
  value_col,
  pmf = NULL,
  template = NULL,
  density = FALSE,
  full_output = FALSE,
  snap = FALSE
)
```

## Arguments

- x:

  SpatVector containing case counts

- value_col:

  Character; name of column containing case counts

- pmf:

  Optional SpatRaster PMF surface for weighting

- template:

  Optional SpatRaster template (used if PMF not provided)

- density:

  Logical; if TRUE returns density surface, if FALSE returns PMF

- full_output:

  Logical; if TRUE returns list with additional information

- snap:

  Logical; if TRUE skip validation

## Value

If full_output = FALSE, returns SpatRaster (PMF or density). If
full_output = TRUE, returns list with: - surface: SpatRaster (PMF or
density) - totals: data.frame with original and redistributed totals by
area

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic PMF creation
result1 <- transform_demand(
  case_spatial,
  value_col = "cases",
  template = pop
)

# Create density surface with existing PMF
pop_pmf <- transform_pmf(pop)
result2 <- transform_demand(
  case_spatial,
  value_col = "cases",
  pmf = pop_pmf,
  density = TRUE,
  full_output = TRUE
)
} # }
```
