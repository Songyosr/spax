# Print method for spax objects

Prints a concise summary of a spax object, showing key information about
the accessibility analysis without detailed statistics.

## Usage

``` r
# S3 method for class 'spax'
print(x, ...)
```

## Arguments

- x:

  A spax object created by an FCA function, such as
  [`spax_2sfca`](https://songyosr.github.io/spax/reference/spax_2sfca.md),
  [`spax_e2sfca`](https://songyosr.github.io/spax/reference/spax_e2sfca.md),
  [`spax_m2sfca`](https://songyosr.github.io/spax/reference/spax_m2sfca.md),
  [`spax_3sfca`](https://songyosr.github.io/spax/reference/spax_3sfca.md)
  or
  [`spax_huff2sfca`](https://songyosr.github.io/spax/reference/spax_huff2sfca.md)

- ...:

  Currently ignored, for extensibility

## Value

Invisibly returns the input object
