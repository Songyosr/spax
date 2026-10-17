# Fix one problem parameter without rebuilding the prepared substrate

The returned problem exposes only the nuisance-parameter contract. Its
bind closure reconstructs the full theta in the original order and
delegates to the original prepared problem. Analytic state-map parameter
Jacobians, when present, are reduced to the nuisance columns; output
sensitivities continue to use the standard problem machinery.

## Usage

``` r
.fix_problem_parameter(problem, parameter, value)
```
