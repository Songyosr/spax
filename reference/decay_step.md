# Declare a step or inverse decay for an FCA specification

Experimental. `decay_step()` and `decay_inverse()` are decays for the
specifications of floating catchment area indices
([`spec_2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md),
[`spec_e2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md)
and the others). They are neither estimated nor available to an
allocation model such as
[`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md),
which refuses them.

## Usage

``` r
decay_step(threshold)

decay_inverse()

# S3 method for class 'spax_fca_decay'
format(x, ...)

# S3 method for class 'spax_fca_decay'
print(x, ...)
```

## Arguments

- threshold:

  A single finite number greater than 0: the largest travel time in the
  catchment.

- x:

  A decay object.

- ...:

  Unused.

## Value

A `spax_fca_decay`: a list with `method` (`"binary"` and `sigma`, the
threshold as a double, or `"inverse"` with no `sigma`).

## Details

`decay_step(threshold)` gives weight 1 to a travel time up to
`threshold` and weight 0 beyond it (the catchment of the two-step
method). `decay_inverse()` gives the weight `1 / (d + 1)` to a travel
time `d`, as the `"inverse"` method of
[`calc_decay()`](https://songyosr.github.io/spax/reference/calc_decay.md)
does. They are stored as `list(method = "binary", sigma = threshold)`
and `list(method = "inverse")`, the form the FCA functions take in
`decay_params`.

## See also

Other access interface:
[`spec_2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md)

## Examples

``` r
decay_step(15)
#> <spax decay> step (threshold): 15
decay_inverse()
#> <spax decay> inverse: 1/(d + 1)
```
