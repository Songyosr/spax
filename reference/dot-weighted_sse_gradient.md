# Gradient of weighted SSE with respect to parameters

\`sensitivity\` is d predicted / d theta, with one row per observation
and one column per parameter.

## Usage

``` r
.weighted_sse_gradient(predicted, observed, sensitivity, eta = 1)
```
