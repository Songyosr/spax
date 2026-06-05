# Coerce raw data or pass through an existing field

Internal DEC-008 two-door input helper. Raw inputs go through the
validated backend constructors; prepared fields are trusted after a
domain check.

## Usage

``` r
.as_spax_field(
  x,
  domain,
  role = "unknown",
  frame = NULL,
  allow_positional = FALSE,
  snap = FALSE
)
```
