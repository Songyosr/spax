# Inspect a saved facility bootstrap

These methods display saved results, including refused original fits,
unfinished attempts and unavailable intervals. Counts distinguish
planned, attempted and successful fits; each quantity retains its own
finite-success count and reporting status. Fixed-input identities have
no sampling interval.

## Usage

``` r
# S3 method for class 'ae_allocation_bootstrap'
print(x, digits = 4, ...)

# S3 method for class 'ae_allocation_bootstrap'
summary(object, ...)

# S3 method for class 'summary.ae_allocation_bootstrap'
print(x, digits = 4, ...)

# S3 method for class 'ae_allocation_bootstrap'
plot(x, quantity = NULL, type = c("interval", "draws"), ...)
```

## Arguments

- x, object:

  An `ae_allocation_bootstrap` result.

- digits:

  Display precision.

- ...:

  Additional generic or graphics arguments.

- quantity:

  One saved quantity name; defaults to the first estimate row.

- type:

  Plot a saved interval or successful-draw distribution.

## Value

[`summary()`](https://rspatial.github.io/terra/reference/summary.html)
returns saved estimates, original fit, status, counts, diagnostics,
capacity support and declarations (and the coverage note, when the
result has one) in a `summary.ae_allocation_bootstrap` object. Print and
plot methods return their input invisibly.

## Details

[`plot()`](https://rspatial.github.io/terra/reference/plot.html)
displays one named saved quantity. Its default is the first saved
quantity. The interval view draws a saved point and available percentile
endpoints only when the overall run is complete and that quantity's
saved status permits reporting; an unavailable interval is labelled
explicitly. This also suppresses stale finite endpoints in refused or
incomplete saved objects. The draw view plots finite values from
successful attempted fits, retaining inner boundaries and displaying the
number plotted. Neither view implies empirical observation-law
validation, simultaneous coverage or causal interpretation of a
scenario.
