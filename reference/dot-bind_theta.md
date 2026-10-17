# Bind theta once and return the state-only inner-loop step

Providers keep the required `outputs(x)` callback. An optional
`outputs_requested(x, requested_outputs)` callback adds requested
outputs while retaining `target` and `utilization`. Without it, requests
read the legacy outputs. `state_independent_outputs` explicitly names
outputs whose partial derivative with respect to state is zero for every
valid state and theta. Only these outputs bypass implicit state
differentiation; the promise is the provider's responsibility and is
never inferred from a model name.

## Usage

``` r
.bind_theta(problem, theta)
```
