# Require an AE outputs list to carry the contracted keys

Every bound step's `outputs(x)` must expose `target` and `utilization`
so runners and loss functions can bind to known quantities without
defensive NULL checks.

## Usage

``` r
.validate_ae_outputs(outputs)
```
