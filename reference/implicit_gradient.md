# Fixed-point implicit gradient under F(x, theta) = x - T_theta(x)

\`jac_state\` is dT/dx and \`jac_param\` is dT/dtheta, both evaluated at
the selected fixed point. Returns dx\*/dtheta = (I - dT/dx)^(-1)
dT/dtheta.

## Usage

``` r
implicit_gradient(jac_state, jac_param)
```
