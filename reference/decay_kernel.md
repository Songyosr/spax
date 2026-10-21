# Declare a distance decay kernel

Declares the decay weights \\w(d; \sigma)\\ that
[`calc_decay()`](https://songyosr.github.io/spax/reference/calc_decay.md)
and the FCA functions apply to a travel time or distance \\d\\. A
declaration is plain data: it names a built-in kernel, or it records
your own function as the exact text of its code (comments and layout are
not kept), together with what its parameter `sigma` means, the bounds
`sigma` may take and any fixed extra arguments. Nothing but data is
stored, so a declaration can be compared, saved and reloaded in a fresh
session. This experimental function lets one declaration be used
wherever a decay `method` is accepted. A declaration can also be passed
as `family` to
[`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md).

## Usage

``` r
decay_kernel(
  fun,
  name = NULL,
  parameter = NULL,
  lower = 0,
  upper = Inf,
  zero_distance = TRUE,
  smooth = TRUE,
  args = list()
)

# S3 method for class 'spax_decay_kernel'
print(x, ...)
```

## Arguments

- fun:

  A built-in kernel name (`"gaussian"`, `"exponential"` or `"power"`),
  or a closure `function(distance, sigma, ...)` that returns the
  weights.

- name:

  A single string naming a custom kernel. It starts with a letter and
  continues with letters, digits, `_`, `.` or `-`, and it is not (in any
  capitalization) `gaussian`, `exponential`, `power`, `inverse`,
  `binary` or `none`. Required for a function; not allowed for a
  built-in kernel.

- parameter:

  A single nonempty string saying what `sigma` means for the kernel, for
  example `"scale"`. It labels printed output. Required for a function.

- lower, upper:

  Inclusive bounds on `sigma`, with `0 <= lower < upper <= Inf`.
  Wherever the kernel is used, `sigma` must also be positive, so a
  `lower` of 0 allows any positive `sigma` but not 0 itself.

- zero_distance:

  `FALSE` declares the kernel undefined at distance zero, as the power
  kernel is: the check at declaration then leaves zero out of its grid,
  and a model from
  [`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md)
  refuses prepared inputs that contain a finite zero travel time when it
  is evaluated or fitted.

- smooth:

  `FALSE` declares a kernel that is not smooth in `sigma` (steps or
  thresholds). It can be evaluated, predicted and used in FCA: a model
  built with
  [`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md)
  can be evaluated at fixed parameters with
  [`evaluate_allocation()`](https://songyosr.github.io/spax/reference/evaluate_allocation.md)
  and used for requested-location prediction with
  [`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md),
  but
  [`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md)
  refuses it, because model fitting and bootstrapping need a kernel that
  is smooth in `sigma`. Evaluate such a kernel at fixed parameters
  instead, for example over a grid of `sigma` and `v0`.

- args:

  A named list of fixed extra arguments for `fun`. Its names must be
  arguments of `fun` other than `distance` and `sigma`; its values must
  be numeric, logical or character vectors without attributes.

- x:

  A `spax_decay_kernel`.

- ...:

  Unused.

## Value

A `spax_decay_kernel`: a list of plain data with the fields `name`,
`builtin`, `parameter`, `lower`, `upper`, `zero_distance`, `smooth`,
`args` and `code` (the exact text of the function's code, with comments
and layout not kept; `NA` for a built-in kernel).

## Details

**Built-in kernels.** `decay_kernel("gaussian")`,
`decay_kernel("exponential")` and `decay_kernel("power")` declare the
kernels of the same names in
[`calc_decay()`](https://songyosr.github.io/spax/reference/calc_decay.md).
They take no other argument and behave exactly like the method strings.
Their `sigma` is a travel scale, a rate and an exponent; the power
kernel is undefined at distance zero.

**Your own function.** `fun` must be a closure whose first two arguments
are `distance` and `sigma`, and it must return one weight per distance.
It must be elementwise: each distance gets its own weight, independent
of the other distances, because it may receive a vector or a matrix of
distances. Every other argument needs a default value, an entry in
`args`, or is `...`. The function is stored as text and rebuilt in the
base environment whenever it is used, so it may use only its arguments,
base R, `pkg::fn` calls (for example
[`stats::pnorm`](https://rdrr.io/r/stats/Normal.html)) and `args`. It
cannot refer to objects in your workspace.

**Checks at declaration.** The function is run on a grid of distances
(0.1 to 1000, and 0 unless `zero_distance = FALSE`) at a few values of
`sigma` inside the bounds, including a positive `lower` and a finite
`upper` bound themselves. Every call must return a numeric vector with
one value per distance that is finite, nonnegative and non-increasing in
distance, and the values must be identical whether the function is run
from its stored text or as given. A function whose weight for a distance
depends on the other distances is refused as well. A function that fails
a check is refused with the reason.

**Saving.** The stored code is parsed, and turned into a function
without running it, whenever the kernel is used; running the kernel runs
that code. As with any R function, use declarations only from sources
you trust. Fits, evaluations and predictions that use a declared kernel
carry its code, and
[`saveRDS()`](https://rspatial.github.io/terra/reference/serialize.html)
saves it with them. Using such a result again, for example to predict
from it after
[`readRDS()`](https://rspatial.github.io/terra/reference/serialize.html),
runs that code, so read saved results only from sources you trust, as
for any file that holds functions such as a loss function. To rebuild a
model for a saved fit, take the declaration from the fit
(`fit$best$prediction_snapshot$spec$kernel`) rather than declaring the
function again, because the stored text can differ between R versions.

## See also

Other allocation steps:
[`bootstrap_fit()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md),
[`calc_demand()`](https://songyosr.github.io/spax/reference/calc_demand.md),
[`calc_travel_time()`](https://songyosr.github.io/spax/reference/calc_travel_time.md),
[`check_fit()`](https://songyosr.github.io/spax/reference/check_fit.md),
[`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md),
[`compare_fits()`](https://songyosr.github.io/spax/reference/compare_fits.md),
[`evaluate_allocation()`](https://songyosr.github.io/spax/reference/evaluate_allocation.md),
[`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md),
[`huff_allocation()`](https://songyosr.github.io/spax/reference/huff_allocation.md),
[`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md),
[`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md)

## Examples

``` r
# A built-in declaration behaves like its method string
decay_kernel("gaussian")
#> <spax_decay_kernel> gaussian (built-in)
#> sigma: travel scale
#> bounds on sigma: 0 to Inf
#> defined at distance 0: yes
#> smooth in sigma: yes
#> fixed arguments: none
#> function:
#> function (distance, sigma = 11.64884, ...) 
#> {
#>     return(exp(-(distance^2)/(2 * sigma^2)))
#> }

# A plateau kernel: full weight within 5 minutes, Gaussian decay beyond
plateau <- decay_kernel(
  function(distance, sigma, plateau = 5) {
    ifelse(distance <= plateau, 1, exp(-(distance - plateau)^2 / (2 * sigma^2)))
  },
  name = "plateau", parameter = "scale", args = list(plateau = 5)
)
plateau
#> <spax_decay_kernel> plateau
#> sigma: scale
#> bounds on sigma: 0 to Inf
#> defined at distance 0: yes
#> smooth in sigma: yes
#> fixed arguments: plateau = 5
#> function:
#> function (distance, sigma, plateau = 5) 
#> {
#>     ifelse(distance <= plateau, 1, exp(-(distance - plateau)^2/(2 * 
#>         sigma^2)))
#> }

calc_decay(c(0, 5, 10, 20, NA), plateau, sigma = 8)
#> [1] 1.0000000 1.0000000 0.8225776 0.1724216        NA
```
