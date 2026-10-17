# Print a saved allocation multistart summary

Preserves all selected-fit, per-start, boundary, disagreement and
message fields returned by
[`summary()`](https://rspatial.github.io/terra/reference/summary.html),
including searches with no eligible fit.

## Usage

``` r
# S3 method for class 'summary.ae_multistart_fit'
print(x, digits = 4, ...)
```

## Arguments

- x:

  A summary of an allocation multistart fit.

- digits:

  Display precision; saved values are unchanged.

- ...:

  Additional print arguments.

## Value

The summary, invisibly.
