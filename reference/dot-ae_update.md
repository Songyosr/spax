# Damped update of a retained state toward a realized target

mix_lambda(omega, target) = (1 - lambda) \* omega + lambda \* target.
The standard AE relaxation step; lambda in (0, 1\].

## Usage

``` r
.ae_update(state, target, lambda = 1)
```
