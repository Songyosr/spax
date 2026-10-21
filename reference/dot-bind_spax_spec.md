# Bind a model specification to prepared inputs

The private step inside
[`spax()`](https://songyosr.github.io/spax/reference/spax.md) that turns
a data-free `spax_allocation_spec` (see
[`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md))
and checked `ae_prepared_inputs` into the existing `ae_problem`, built
through
[`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md).
It checks the demand groups, translates the specification into the
arguments of
[`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md),
decides whether the model is fitted or evaluated and refuses a decay
that is undefined at a zero travel time.

## Usage

``` r
.bind_spax_spec(spec, data)
```

## Arguments

- spec:

  A `spax_allocation_spec` from
  [`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)
  or
  [`spec_huff()`](https://songyosr.github.io/spax/reference/spec_clm.md).

- data:

  An `ae_prepared_inputs`, from
  [`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md).

## Value

A list: `problem` (the `ae_problem`), `mode` (`"fit"` or `"evaluate"`),
`theta_given` (`NULL` in fit mode; in evaluate mode the named values of
the free parameters, in the order of `problem$theta$names`) and `held`,
a named numeric vector of the values that are neither fitted nor given:
held `sigma` or `sigma.<group>` (fit mode only, since in evaluate mode
sigma is in `theta_given`), `v0` or `v0.<group>` (`v0 = 0` for the Huff
model), `beta` when it is a number and `kappa`.

## Details

**Mode.** The mode is `"fit"` when any parameter is free (`"fit"` in its
slot) and `"evaluate"` when every parameter has a value. In fit mode the
held values of a model with demand groups go through `fixed`; an
ungrouped model cannot hold its sigma while something else is fitted. In
evaluate mode `sigma` is bound as a free parameter and its value is
returned as `theta_given`, for
[`evaluate_allocation()`](https://songyosr.github.io/spax/reference/evaluate_allocation.md);
nothing goes into `fixed`.
