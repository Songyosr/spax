# Describe a calibration target: shape, observation mask, observed values

`NA` cells mark unobserved targets (e.g. HSAF flow suppression below 11
cases) and are excluded from the loss; predicted/observed comparison is
over flattened (column-major) values at the observed cells only.

## Usage

``` r
.fit_target_meta(observed, name = "observed")
```
