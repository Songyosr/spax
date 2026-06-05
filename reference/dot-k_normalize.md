# Row-normalize a kernel matrix by cell (choice/Huff split over facilities)

Matrix form of \`.ae_normalize(kernel, by = cell_axis, method = ...)\`,
written as a diagonal row-scaling so it is dense/sparse-clean: each
origin row is scaled by 1/(row sum). \`identity\` is a no-op;
\`standard\` zeros rows summing to 0; \`semi\` only scales rows whose
sum exceeds 1 (else leaves them unchanged).

## Usage

``` r
.k_normalize(K, method = "identity", a0 = 0)
```

## Arguments

- K:

  matrix \`\[I x J\]\` (base or Matrix; NA already zeroed by the
  extractor).
