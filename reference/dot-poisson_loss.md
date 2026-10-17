# Poisson negative log-likelihood for count calibration targets

Up to the data constant \`sum(log(y!))\`, \`NLL = sum(mu - y log mu)\`
with \`mu\` the predicted count. Jointly fitting \`(sigma, v0)\` uses
both the count total and its facility distribution. It does not
generally force an exact total match; that requires an explicit
total-matching constraint or a suitable independently fitted scale
parameter.

## Usage

``` r
.poisson_loss(predicted, observed, eps = 1e-09)
```
