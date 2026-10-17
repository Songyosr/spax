# Normalize a matrix over one margin

\`over = "cols"\` sums columns and scales rows; \`over = "rows"\` sums
rows and scales columns. Policy choices such as "identity" and "semi"
belong in the planner; this executor always divides by the full margin
sum with a zero-row guard.

## Usage

``` r
.normalize(x, over = c("rows", "cols"))
```
