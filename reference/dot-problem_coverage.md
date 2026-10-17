# Coverage outputs of a CLM allocation problem at specified parameters

Solves once if `state` is absent; otherwise evaluates outputs at that
state. This entry point also supports older saved problems without
refitting targets. `solve_control` supplies named solver controls, such
as damping for feedback. Failed solves are rejected before coverage
extraction.

## Usage

``` r
.problem_coverage(problem, theta, state = NULL, ..., solve_control = list())
```
