# Specify an allocation model without data

Experimental. `spec_clm()` and `spec_huff()` declare an allocation model
with its decay kernel and its parameters, each either estimated
(`"fit"`) or held at a number. A specification holds no data: it is
checked as far as that is possible without data, and it is bound to
prepared inputs when it is used with
[`spax()`](https://songyosr.github.io/spax/reference/spax.md).

## Usage

``` r
spec_clm(
  decay = decay_gaussian(),
  outside = "fit",
  beta = 1,
  kappa = 1,
  by_group = character()
)

spec_huff(
  decay = decay_power(),
  beta = "fit",
  kappa = 1,
  by_group = character()
)

# S3 method for class 'spax_spec'
format(x, ...)

# S3 method for class 'spax_spec'
print(x, ...)
```

## Arguments

- decay:

  A decay:
  [`decay_gaussian()`](https://songyosr.github.io/spax/reference/decay_gaussian.md),
  [`decay_exponential()`](https://songyosr.github.io/spax/reference/decay_gaussian.md),
  [`decay_power()`](https://songyosr.github.io/spax/reference/decay_gaussian.md),
  [`decay_custom()`](https://songyosr.github.io/spax/reference/decay_gaussian.md),
  a
  [`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md)
  or one of `"gaussian"`, `"exponential"` and `"power"` (the last two
  forms mean `sigma = "fit"`). Or a named list of these with one element
  per demand group.

- outside:

  The outside weight `v0`: `"fit"` (the default), `0` (no outside
  option), a single positive number (held), a named numeric vector by
  group (held), or a named list by group whose elements are `"fit"` or a
  number.

- beta:

  The attractiveness exponent: `"fit"`, or a single finite number
  greater than 0 that holds it.

- kappa:

  A single finite number greater than 0, the scale of the attractiveness
  `(kappa * supply)^beta`. It is held.

- by_group:

  For data with demand groups, the parameters that differ by group:
  `"sigma"` and, for `spec_clm()`, `"v0"`. A single decay or outside
  weight listed here must be fitted (`"fit"`), because a held value is
  one number; hold values by group through a list or vector instead.

- x:

  A model specification.

- ...:

  Unused.

## Value

A `spax_spec`: a list with `kind` (`"clm"` or `"huff"`), `decay`,
`outside` (`0` for `spec_huff()`), `beta`, `kappa` and `by_group`.

## Details

**Conditional logit** (`spec_clm()`). Demand at an origin goes to each
facility in proportion to its attractiveness times the decay weight of
the travel time, and to an outside option with weight `v0`. The
attractiveness of a facility is `(kappa * supply)^beta`. `outside = 0`
removes the outside option, which is the Huff model.

**Huff** (`spec_huff()`). The same model without an outside option. Its
default decay is a power kernel and its default `beta` is fitted.

**Parameters.** A parameter is a number (held) or `"fit"` (estimated),
in its own slot:

- the decay's `sigma`, in the decay object
  ([`decay_gaussian()`](https://songyosr.github.io/spax/reference/decay_gaussian.md)
  and its relatives);

- the outside weight `v0`, in `outside` (conditional logit only);

- the attractiveness exponent `beta`.

`kappa` is a scale that is always held.

**Demand groups.** For data with demand groups, a named list of decays
gives each group its own kernel; a number in an element holds that
group's `sigma`. `outside` can be a named numeric vector (held by group)
or a named list whose elements are `"fit"` or a number. `by_group` lets
one fitted decay (`"sigma"`) or one fitted outside weight (`"v0"`)
differ by group. Group-specific parameters can be fitted only to counts
recorded by group; a held `sigma` cannot differ by group unless the
decay is given by group.

## See also

Other allocation interface:
[`decay_gaussian()`](https://songyosr.github.io/spax/reference/decay_gaussian.md),
[`obs_flows()`](https://songyosr.github.io/spax/reference/obs_flows.md),
[`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md),
[`spax()`](https://songyosr.github.io/spax/reference/spax.md),
[`spax_allocation-methods`](https://songyosr.github.io/spax/reference/spax_allocation-methods.md),
[`spax_huff()`](https://songyosr.github.io/spax/reference/spax_huff.md)

## Examples

``` r
# Huff: power decay and fitted exponent, no outside option
spec_huff()
#> <spax spec> Huff model (no outside option)
#>   decay       power (sigma = exponent): fit
#>   attraction  (kappa * supply)^beta: kappa = 1, beta = fit
#>   outside     none

# Conditional logit with a fitted outside weight, and with one held at 0.05
spec_clm(decay = decay_gaussian(), outside = "fit")
#> <spax spec> conditional logit with an outside option
#>   decay       gaussian (sigma = travel scale): fit
#>   attraction  (kappa * supply)^beta: kappa = 1, beta = 1
#>   outside     v0: fit
spec_clm(decay = decay_exponential(), outside = 0.05)
#> <spax spec> conditional logit with an outside option
#>   decay       exponential (sigma = rate): fit
#>   attraction  (kappa * supply)^beta: kappa = 1, beta = 1
#>   outside     v0: 0.05

# Evaluate at given values: everything is a number
spec_huff(decay = decay_power(sigma = 1.5), beta = 1)
#> <spax spec> Huff model (no outside option)
#>   decay       power (sigma = exponent): 1.5
#>   attraction  (kappa * supply)^beta: kappa = 1, beta = 1
#>   outside     none

# Demand groups: a kernel for each group and outside weights by group
spec_clm(
  decay = list(young = decay_gaussian(5), old = decay_power(2)),
  outside = list(young = "fit", old = 0)
)
#> <spax spec> conditional logit with an outside option
#>   decay       by group: young gaussian (5), old power (2)
#>   attraction  (kappa * supply)^beta: kappa = 1, beta = 1
#>   outside     v0 by group: young fit, old 0
```
