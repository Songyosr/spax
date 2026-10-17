# Gradient of the Poisson NLL with respect to parameters

This is the gradient of
[`.poisson_loss()`](https://songyosr.github.io/spax/reference/dot-poisson_loss.md),
floor included. A prediction below `eps` adds the constant
`eps - y log(eps)` to the loss, so it adds nothing to the gradient,
whatever its observed count. Quasi-Newton optimizers check every step
against the loss, so a gradient that kept the slope `1 - y / eps` below
the floor would point where the loss does not fall and stop line
searches early (DEC-029).

## Usage

``` r
.poisson_gradient(predicted, observed, sensitivity, eps = 1e-09)
```
