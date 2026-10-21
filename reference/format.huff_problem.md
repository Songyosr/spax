# Format and print a bound allocation model

A model bound to its data prints like the specification it came from
(see
[`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)):
the decay, the attractiveness and the outside weight, then a data line
with the numbers of active origins and facilities. The free parameters
of the model show as `fit` and the held ones as their value. A model of
the historical `huff_decay` allocation (also one saved before the
allocation was recorded), and the internal no-decay comparator, print
one line and the names of their parameters.

## Usage

``` r
# S3 method for class 'huff_problem'
format(x, ...)

# S3 method for class 'huff_problem'
print(x, ...)
```

## Arguments

- x:

  A model, an `ae_problem` of the Huff or conditional logit family.

- ...:

  Unused.
