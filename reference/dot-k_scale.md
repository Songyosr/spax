# Scale the rows or columns of a matrix by a vector (the \`scale\`/broadcast atom)

\`margin = "rows"\`: row i is multiplied by \`s\[i\]\`; \`"cols"\`:
column j by \`s\[j\]\`. Row/column scaling is a contraction against a
diagonal matrix – the broadcast half of the grammar. Dense/sparse-clean:
a base matrix uses recycling; a \`Matrix::\` matrix uses a
\`Diagonal()\` multiply (sparse stays sparse).

## Usage

``` r
.k_scale(K, s, margin = c("rows", "cols"))
```
