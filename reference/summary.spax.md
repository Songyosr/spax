# Summary method for spax objects

Provides detailed statistical summaries of accessibility analysis
results, including accessibility scores, facility statistics, and
model-specific metrics.

## Usage

``` r
# S3 method for class 'spax'
summary(x, quantiles = c(0, 0.25, 0.5, 0.75, 1), ...)
```

## Arguments

- x:

  A spax object created by an FCA function, such as
  [`spax_2sfca`](https://songyosr.github.io/spax/reference/spax_2sfca.md),
  [`spax_e2sfca`](https://songyosr.github.io/spax/reference/spax_e2sfca.md),
  [`spax_m2sfca`](https://songyosr.github.io/spax/reference/spax_m2sfca.md),
  [`spax_3sfca`](https://songyosr.github.io/spax/reference/spax_3sfca.md)
  or
  [`spax_huff2sfca`](https://songyosr.github.io/spax/reference/spax_huff2sfca.md)

- quantiles:

  Numeric vector of probabilities for quantile computation

- ...:

  Currently ignored, for extensibility

## Value

A summary.spax object. For each accessibility layer, `coverage` counts
the cells with a value (`cells_total`; cells outside the study area,
such as sea, are missing and excluded) and the cells whose accessibility
is above zero (`cells_covered`), with their `proportion`.
