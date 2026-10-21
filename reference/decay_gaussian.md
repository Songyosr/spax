# Declare a decay for a model specification

Experimental. A decay object names a distance decay kernel and says
whether its parameter `sigma` is estimated when the model is fitted or
held at a number. It is one slot of a model specification such as
[`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md) or
[`spec_huff()`](https://songyosr.github.io/spax/reference/spec_clm.md),
which holds no data and is used with
[`spax()`](https://songyosr.github.io/spax/reference/spax.md).

## Usage

``` r
decay_gaussian(sigma = "fit")

decay_exponential(sigma = "fit")

decay_power(sigma = "fit")

decay_custom(kernel, sigma = "fit")

# S3 method for class 'spax_decay'
format(x, ...)

# S3 method for class 'spax_decay'
print(x, ...)
```

## Arguments

- sigma:

  `"fit"`, or a single finite number greater than 0 that holds `sigma`
  at that value.

- kernel:

  A kernel declared with
  [`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md).

- x:

  A decay object.

- ...:

  Unused.

## Value

A `spax_decay`: a list with the kernel declaration (`kernel`, a
`spax_decay_kernel`) and `sigma` (`"fit"` or a number).

## Details

`decay_gaussian()`, `decay_exponential()` and `decay_power()` use the
built-in kernels of
[`calc_decay()`](https://songyosr.github.io/spax/reference/calc_decay.md);
`decay_custom()` uses a kernel declared with
[`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md).
What `sigma` means depends on the kernel: for the Gaussian kernel it is
a scale in travel units, for the exponential kernel a rate (the weight
is `exp(-sigma * d)`) and for the power kernel an exponent. For a
declared kernel it is the kernel's `parameter`. The power kernel is
undefined at distance zero.

`sigma = "fit"` (the default) lets the fit estimate it. A number holds
it at that value. A number must be positive and must lie within the
bounds the kernel declares for `sigma` (the built-in kernels allow any
positive number).

A model specification also accepts an exact built-in name (`"gaussian"`,
`"exponential"` or `"power"`) or a
[`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md)
where it expects a decay; both mean `sigma = "fit"`. There is no partial
matching.

## See also

Other allocation interface:
[`obs_flows()`](https://songyosr.github.io/spax/reference/obs_flows.md),
[`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md),
[`spax()`](https://songyosr.github.io/spax/reference/spax.md),
[`spax_allocation-methods`](https://songyosr.github.io/spax/reference/spax_allocation-methods.md),
[`spax_huff()`](https://songyosr.github.io/spax/reference/spax_huff.md),
[`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)

## Examples

``` r
decay_gaussian()
#> <spax decay> gaussian (sigma = travel scale): fit
decay_power(sigma = 1.5)
#> <spax decay> power (sigma = exponent): 1.5

# A declared kernel with bounds on sigma
plateau <- decay_kernel(
  function(distance, sigma, plateau = 5) {
    ifelse(distance <= plateau, 1, exp(-(distance - plateau)^2 / (2 * sigma^2)))
  },
  name = "plateau", parameter = "scale", lower = 1, upper = 20,
  args = list(plateau = 5)
)
decay_custom(plateau)
#> <spax decay> plateau (sigma = scale): fit
decay_custom(plateau, sigma = 8)
#> <spax decay> plateau (sigma = scale): 8
```
