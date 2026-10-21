# Specify a floating catchment area index without data

Experimental. `spec_2sfca()`, `spec_e2sfca()`, `spec_m2sfca()`,
`spec_3sfca()` and `spec_huff2sfca()` declare a floating catchment area
(FCA) index with its decay and its options, as the data-free
counterparts of
[`spax_2sfca()`](https://songyosr.github.io/spax/reference/spax_2sfca.md),
[`spax_e2sfca()`](https://songyosr.github.io/spax/reference/spax_e2sfca.md),
[`spax_m2sfca()`](https://songyosr.github.io/spax/reference/spax_m2sfca.md),
[`spax_3sfca()`](https://songyosr.github.io/spax/reference/spax_3sfca.md)
and
[`spax_huff2sfca()`](https://songyosr.github.io/spax/reference/spax_huff2sfca.md).
A specification holds no data and no fitted value: an FCA index is
computed, not fitted, so every decay needs its `sigma` (or its
threshold) here.

## Usage

``` r
spec_2sfca(threshold)

spec_e2sfca(
  decay = decay_gaussian(sigma = 30),
  normalize = c("identity", "standard", "semi")
)

spec_m2sfca(
  decay = decay_gaussian(sigma = 30),
  normalize = c("identity", "standard", "semi")
)

spec_3sfca(decay = decay_gaussian(sigma = 30), selection = NULL)

spec_huff2sfca(
  decay = decay_gaussian(sigma = 30),
  selection = NULL,
  attractiveness = "supply",
  alpha = 1
)

# S3 method for class 'spax_fca_spec'
format(x, ...)
```

## Arguments

- threshold:

  For `spec_2sfca()`: a single finite number greater than 0, the largest
  travel time in the catchment, or a named numeric vector of such
  numbers, one per demand group.

- decay:

  The decay of the weights: a decay with a value for `sigma`
  ([`decay_gaussian()`](https://songyosr.github.io/spax/reference/decay_gaussian.md),
  [`decay_exponential()`](https://songyosr.github.io/spax/reference/decay_gaussian.md),
  [`decay_power()`](https://songyosr.github.io/spax/reference/decay_gaussian.md),
  [`decay_custom()`](https://songyosr.github.io/spax/reference/decay_gaussian.md)),
  [`decay_step()`](https://songyosr.github.io/spax/reference/decay_step.md)
  or
  [`decay_inverse()`](https://songyosr.github.io/spax/reference/decay_step.md);
  or a named list of these by demand group.

- normalize:

  The normalization of demand: `"identity"` (the default), `"standard"`
  or `"semi"`.

- selection:

  The decay of the selection weights, in the form of `decay`; `NULL`
  (the default) uses `decay`.

- attractiveness:

  `"supply"` (the default), or a numeric vector named by facility with
  finite values of 0 or more.

- alpha:

  A single finite number of 0 or more: the exponent of the
  attractiveness in the selection probability.

- x:

  An FCA specification.

- ...:

  Unused.

## Value

A `spax_fca_spec` (also a `spax_spec`): a list with `kind` (`"2sfca"`,
`"e2sfca"`, `"m2sfca"`, `"3sfca"` or `"huff2sfca"`), `decay`,
`normalize`, `selection`, `attractiveness`, `alpha` and `threshold`. A
field that the method does not have is `NULL`.

## Details

**Decays.** A decay is
[`decay_gaussian()`](https://songyosr.github.io/spax/reference/decay_gaussian.md),
[`decay_exponential()`](https://songyosr.github.io/spax/reference/decay_gaussian.md)
or
[`decay_power()`](https://songyosr.github.io/spax/reference/decay_gaussian.md)
with a number for `sigma`, or with a `SpatRaster` of `sigma` values (a
variable catchment);
[`decay_step()`](https://songyosr.github.io/spax/reference/decay_step.md)
or
[`decay_inverse()`](https://songyosr.github.io/spax/reference/decay_step.md);
or
[`decay_custom()`](https://songyosr.github.io/spax/reference/decay_gaussian.md)
with a kernel declared by
[`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md)
and a number for `sigma`. `"fit"` is refused, and so is a bare kernel
name, which means `sigma = "fit"`. With demand groups, `decay` and
`selection` can be a named list by group, and `threshold` a named
numeric vector by group.

- `spec_2sfca()`: the original two-step method with a binary catchment
  of travel time `threshold`, with no demand normalization.

- `spec_e2sfca()` and `spec_m2sfca()`: the enhanced and the modified
  methods with distance decay `decay`. `normalize` is the normalization
  of demand: `"identity"`, `"standard"` or `"semi"` (see
  [`calc_normalize()`](https://songyosr.github.io/spax/reference/calc_normalize.md)).

- `spec_3sfca()`: the three-step method. `selection` is the decay the
  selection weights are built from; `NULL` (the default) means the same
  decay as `decay`.

- `spec_huff2sfca()`: the Huff-model selection. `attractiveness` is
  `"supply"` (the supply measure) or a numeric vector named by facility
  with one finite value of 0 or more per facility, and `alpha` is its
  exponent.

## See also

[`decay_step()`](https://songyosr.github.io/spax/reference/decay_step.md),
[`decay_gaussian()`](https://songyosr.github.io/spax/reference/decay_gaussian.md),
[`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md)

Other access interface:
[`decay_step()`](https://songyosr.github.io/spax/reference/decay_step.md)

## Examples

``` r
spec_2sfca(threshold = 15)
#> <spax spec> 2SFCA
#>   decay  step (threshold): 15
spec_e2sfca(decay = decay_gaussian(sigma = 10))
#> <spax spec> E2SFCA
#>   decay      gaussian (sigma = travel scale): 10
#>   normalize  identity
spec_m2sfca(decay = decay_exponential(sigma = 0.2), normalize = "standard")
#> <spax spec> M2SFCA
#>   decay      exponential (sigma = rate): 0.2
#>   normalize  standard

# Selection with another decay than the weights
spec_3sfca(decay = decay_gaussian(10), selection = decay_inverse())
#> <spax spec> 3SFCA
#>   decay      gaussian (sigma = travel scale): 10
#>   selection  inverse: 1/(d + 1)

# Huff selection with an attractiveness named by facility
spec_huff2sfca(decay = decay_gaussian(10),
               attractiveness = c(a = 2, b = 1), alpha = 2)
#> <spax spec> Huff-2SFCA
#>   decay           gaussian (sigma = travel scale): 10
#>   selection       same as decay
#>   attractiveness  2 values ^ alpha 2

# Demand groups: a decay for each group
spec_e2sfca(decay = list(young = decay_gaussian(8), old = decay_step(20)))
#> <spax spec> E2SFCA
#>   decay      by group: young gaussian (8), old step (20)
#>   normalize  identity
```
