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

  A spax object created by spax_2sfca() or spax_e2sfca()

- quantiles:

  Numeric vector of probabilities for quantile computation

- ...:

  Currently ignored, for extensibility

## Value

A summary.spax object
