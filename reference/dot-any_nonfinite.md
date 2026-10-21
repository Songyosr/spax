# Does a numeric object hold any NA, NaN or infinite value?

Same answer as \`any(!is.finite(x))\`. For a double it scans with
\`anyNA()\`, \`min()\` and \`max()\`, which allocate nothing, where
\`is.finite()\` allocates a logical vector as long as \`x\`. Other types
take the original test.

## Usage

``` r
.any_nonfinite(x)
```
