# Inspect saved allocation fits without recomputation

Coefficients and fitted targets are read from the selected fit.
Multistart summaries retain failed starts, boundaries, warnings/errors
and descriptive disagreement. A search with no eligible fit remains
inspectable by summary, while coefficient and fitted extraction fail
clearly.

## Usage

``` r
# S3 method for class 'ae_problem_nfxp_fit'
coef(object, ...)

# S3 method for class 'ae_multistart_fit'
coef(object, ...)

# S3 method for class 'ae_problem_nfxp_fit'
fitted(object, ...)

# S3 method for class 'ae_multistart_fit'
fitted(object, ...)

# S3 method for class 'ae_multistart_fit'
summary(object, ...)
```

## Arguments

- object:

  An existing NFXP fit or multistart result from allocation fitting.

- ...:

  Additional generic arguments; currently unused.

## Value

[`coef()`](https://rdrr.io/r/stats/coef.html) returns the saved named
parameter vector.
[`fitted()`](https://rdrr.io/r/stats/fitted.values.html) returns saved
canonical fitted targets.
[`summary()`](https://rspatial.github.io/terra/reference/summary.html)
returns a list containing the selected fit summary, per-start table,
boundaries, disagreement and messages. These diagnostics establish
neither identification nor global optimality.

## See also

[`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md)
