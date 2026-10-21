# Lean variant of \`.huff_state()\` for the corrected CLM form (SPAX-061)

The same arithmetic in the same order as \`.huff_state(allocation =
"clm")\`, so every returned vector is bit-identical, but only the
vectors (target, utilization, attractiveness, access, outside_share) and
the requested count matrices are returned: no allocation, share or
opportunity matrix outlives the call. \`keep\` names the count outputs
to keep (\`"flow"\`, \`"flow_outside"\`), computed with the expressions
of the bind's \`outputs_requested\`.

## Usage

``` r
.huff_state_lean(a, plan, v0 = 0, keep = character(), demand = NULL)
```

## Arguments

- demand:

  Active demand, for the count outputs.
