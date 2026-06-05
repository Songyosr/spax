# Calculate spatial accessibility scores

Specialized version of \[spread_weighted()\] for accessibility analysis.
Distributes supply-to-demand ratios across space using probability-based
catchment areas to calculate accessibility scores.

## Usage

``` r
spread_access(
  ratios,
  weights,
  ratio_cols = NULL,
  full_output = FALSE,
  plot_prefix = NULL
)
```

## Arguments

- ratios:

  Numeric vector, matrix, or data.frame of supply-to-demand ratios: -
  Vector: Must match number of weight layers - Matrix: Rows match weight
  layers, columns are different measures - Data.frame: Same as matrix
  but requires ratio_cols parameter

- weights:

  Multi-layer SpatRaster where each layer represents one site's
  probability-based catchment area

- ratio_cols:

  Character vector of column names if ratios is a data.frame

- full_output:

  Logical; whether to return intermediate calculations (default FALSE)

- plot_prefix:

  Character string to prepend to output names (default NULL)

## Value

If full_output = TRUE, returns list containing: - access_scores:
SpatRaster stack of accessibility scores per measure - site_specific:
List of site-specific accessibility SpatRasters If full_output = FALSE,
returns only access_scores SpatRaster
