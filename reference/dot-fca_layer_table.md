# Layer names and table of an FCA result

\`measures\` are the supply measure (indicator) names and \`groups\` the
group IDs, or \`NULL\` when ungrouped. Grouped layers are named
\`\<measure\>.\<group\>\` and \`\<measure\>.all\`, in measure-major
order (measure 1: the groups in group order, then \`all\`; measure 2:
...). The table has one row per layer with the columns \`layer\`,
\`measure\` and \`group\` (\`"all"\` for the overall layer); ungrouped,
the layers are the measures and \`group\` is \`NA\`.

## Usage

``` r
.fca_layer_table(measures, groups = NULL)
```
