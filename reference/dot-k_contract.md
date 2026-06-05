# Contract a vector against a matrix over one margin (the \`contract\` atom)

\`over = "rows"\` sums over dim 1 (gather over origins I): result is one
value per column (J). \`over = "cols"\` sums over dim 2 (spread over
facilities J): result is one value per row (I). The same op expresses
group-collapse when \`K\` is a 0/1 membership/incidence matrix –
grouping is not a special case.

## Usage

``` r
.k_contract(v, K, over = c("rows", "cols"))
```

## Details

\`K\` may be a base matrix OR a \`Matrix::\` sparse matrix:
\`crossprod\`/\` generic, so dense vs sparse is a property of the plan,
not of this code. The axis-\>margin mapping is the plan's job (a
dense-array backend stores an explicit axis-to-dimension map); this
primitive speaks plain matrix margins.
