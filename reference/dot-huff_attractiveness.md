# Exogenous Huff attractiveness a_j = (kappa \* S_j)^beta

Classic Huff uses \`beta = 1\` (attractiveness proportional to supply).
A global scale on \`a\` cancels when \`v0 = 0\`. With a positive outside
option, scaling attractiveness changes the inside/outside balance unless
\`v0\` is scaled by the same factor.

## Usage

``` r
.huff_attractiveness(supply, kappa = 1, beta = 1)
```
