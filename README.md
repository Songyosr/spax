
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spax: spatial accessibility and allocation modeling

`spax` supports two user workflows: public floating catchment area (FCA)
methods for accessibility surfaces, and an experimental static
conditional-logit model (CLM) workflow for fitting allocation to
observations, inspecting results, and evaluating supply scenarios. Both
use explicit demand, facility supply and travel inputs. Their
normalization and output meanings remain model-specific.

Existing `spax_2sfca()`, `spax_e2sfca()`, `spax_m2sfca()` and
`compute_access()` workflows remain supported. The allocation functions
are an additional route; they do not replace the FCA functions. Internal
feedback-model machinery is private and is not part of the experimental
CLM interface.

## Install the experimental branch

The allocation workflow described here is on `codex-assist-main`.
Install that branch explicitly rather than relying on the repository’s
default branch:

``` r
# install.packages("pak")
pak::pak("Songyosr/spax@codex-assist-main")
```

This is an experimental development interface, not a new stable release.
For reproducible work, record the source commit as well as the package
version and retain the input IDs, units, observation declaration and
saved results.

Package and function help are available from an installed package.
Installations that include built articles also provide the complete
allocation vignette:

``` r
help(package = "spax")
help("spax-package", package = "spax")
help("allocation-compatibility", package = "spax")
browseVignettes("spax")
# When the installation includes built vignettes:
vignette("allocation-workflow", package = "spax")
```

## Two small generated examples

These six origins and three facilities are artificial. Demand represents
request opportunities during one week, travel is in minutes, and supply
has two independent indicators: appointment slots and staff. No
geographic or empirical conclusion is implied.

``` r
library(spax)

origin_ids <- paste0("o", 1:6)
facility_ids <- paste0("f", 1:3)
demand <- setNames(c(120, 90, 150, 110, 80, 130), origin_ids)
travel <- matrix(c(3, 8, 12, 20, 15, 28,
                   15, 6, 4, 10, 18, 22,
                   27, 20, 14, 8, 4, 3), nrow = 6,
  dimnames = list(origin_ids, facility_ids))
supply <- data.frame(id = facility_ids,
  appointment_slots = c(80, 60, 90), staff = c(4, 3, 5))
```

### Public FCA: separate accessibility indicators

FCA entry points take a demand raster and one aligned travel layer per
facility. Layer names match facility IDs. Multiple supply columns
produce independent accessibility indicators; this example does not
model interactions between staff and appointment capacity.

``` r
grid <- terra::rast(nrows = 2, ncols = 3, xmin = 0, xmax = 3,
  ymin = 0, ymax = 2, crs = "EPSG:3857")
terra::values(grid) <- unname(demand)
travel_grid <- terra::rast(grid, nlyrs = length(facility_ids))
terra::values(travel_grid) <- travel
names(travel_grid) <- facility_ids

fca <- spax_e2sfca(grid, supply, travel_grid,
  decay_params = list(method = "gaussian", sigma = 12),
  demand_normalize = "identity", id_col = "id",
  supply_cols = c("appointment_slots", "staff"))
round(terra::values(fca$accessibility), 3)
#>      appointment_slots staff
#> [1,]             0.306 0.015
#> [2,]             0.369 0.019
#> [3,]             0.393 0.020
#> [4,]             0.343 0.018
#> [5,]             0.366 0.020
#> [6,]             0.262 0.014
```

The two columns have different supply-per-demand units. They are
accessibility scores, not contact probabilities. Normalization is a
model choice: this example uses the unnormalized demand kernel
(`"identity"`). See `?spax_e2sfca` for the other options, and use
`summary(fca)` or `plot(fca)` to inspect the saved result.

### Experimental CLM: fit one allocation system

The CLM route prepares numeric vectors and an origin-by-facility travel
matrix. It uses one supply vector per system. Here appointment slots
affect attraction and proportional capacity attribution; the staff
column is not combined with them. This is a static choice-allocation
model with an explicit outside option.

``` r
slots <- setNames(supply$appointment_slots, supply$id)
units <- list(demand_units = "request_opportunities", demand_period = "one_week",
  supply_units = "appointment_slots", supply_period = "one_week",
  travel_units = "minutes")
inputs <- prepare_allocation(demand, slots, travel, metadata = units)
model <- clm_allocation(inputs, family = "gaussian", v0 = 25)

# Generate rounded facility counts at a travel scale of 12 minutes.
score <- exp(-travel^2 / (2 * 12^2)) * rep(slots, each = length(demand))
observed <- round(colSums(demand * score / (rowSums(score) + 25)))
fit <- fit_allocation(model, observed,
  starts = rbind(short = c(sigma = 6), long = c(sigma = 20)),
  lower = c(sigma = 2), upper = c(sigma = 40),
  loss = "poisson", gradient = TRUE)
as.data.frame(fit, type = "parameters")
#>   parameter estimate
#> 1     sigma 11.94629
```

Poisson fitting is chosen explicitly; the fitting default is weighted
SSE. Counts, the likelihood, and the observation process are separate
choices. The outside weight is fixed in this small example. Inspect
`summary(fit)` for all supplied starts, failures and boundaries;
agreement among starts does not establish identification or a global
optimum.

``` r
prediction <- predict_allocation(fit, travel, demand = demand,
  outputs = c("rho", "A", "abar"))
data.frame(origin = prediction$origin_ids, prediction$outputs)
#>    origin       rho         A      abar
#> o1     o1 0.8172420 0.3493482 0.4274721
#> o2     o2 0.8475457 0.3519677 0.4152787
#> o3     o3 0.8574077 0.3487075 0.4066998
#> o4     o4 0.8426571 0.3328562 0.3950079
#> o5     o5 0.8491669 0.3388356 0.3990213
#> o6     o6 0.8052133 0.3105690 0.3856978
```

For a valid choice row, `rho` is the probability of an inside contact;
the outside category is not automatically unmet need. `A` is attributed
supply per demand opportunity, and `abar = A/rho` is attributed supply
per inside contact when defined. Neither measures delivered service or
enforces a hard capacity limit. Inspect `prediction$validity` before
using or mapping values. Requested prediction uses saved facility loads;
adding requested rows or changing their demand does not refit or update
that system.

## Continue the allocation workflow

| Function | Purpose |
|:---|:---|
| `prepare_allocation()` | Align IDs, validate inputs, record units and support, and prepare compact numeric inputs. |
| `clm_allocation()` | Declare corrected static CLM with Gaussian, exponential or power travel decay. |
| `fit_allocation()` | Fit a selected output from explicit starts and retain diagnostics. |
| `evaluate_allocation()` | Evaluate supplied parameters, including a newly prepared supply scenario. |
| `predict_allocation()` | Predict requested origins against saved facility loads; maps are explicit. |
| `bootstrap_allocation()` | Run the separately supported conditional facility-case bootstrap. |

A supply scenario requires preparing the changed system and evaluating
it at the selected parameters. Supply can change attraction, allocation
and loads as well as attributed capacity. This is a model-based
comparison, not a causal intervention estimate.

The bootstrap has a narrower scope than fitting: Gaussian static CLM
with jointly fitted `sigma` and `v0`, `kappa = beta = 1`, complete whole
facility counts and an unweighted Poisson original fit. It requires an
explicit event, reference period and independent-facility sampling
declaration. The small fixed-`v0` fit above is not eligible. The
installed allocation vignette supplies the larger generated example, all
399 attempts, paired supply comparison and fresh-session
reporting/export.

Conditional percentile intervals require an eligible original, a
complete run of 399 attempts and at least 380 finite successful values
for the quantity. Failed attempts and successful inner-boundary fits
remain visible. Validation in generated 80-origin/40-facility designs
under tested fixed-input laws is not a universal coverage guarantee or
evidence for an application’s observation law. Regional attributed
capacity per demand is a fixed-input identity, without a bootstrap
interval. No spatial simultaneous intervals are provided.

## IDs, support and saved results

Keep facility IDs on supply, travel and observations; named axes align
independently. Unnamed inputs are positional. Distinguish known zero
demand, unknown demand and excluded origins. Only included positive
demand contributes to fitted facility loads. For CLM prediction, unknown
travel and known absence (`Inf`) have different meanings; inspect
validity flags rather than replacing all missing values with zero.
Declare compatible units and reference periods; the interface does not
convert them.

Use `coef()`, `fitted()`, `summary()`, `plot()` and `as.data.frame()` to
inspect saved allocation results. Bootstrap tables select `"estimates"`,
`"draws"` or `"starts"`; coverage tables select `"regional"` or
`"cells"`. CSV drops R attributes, so keep an RDS bundle with
declarations and support metadata. See
`help("allocation-compatibility", package = "spax")` before reusing
older private fits: corrected CLM and legacy `huff_decay` have different
output meanings, and older objects may need regeneration for current
prediction/reporting.

## Contributing and license

Report reproducible issues or discuss larger changes on the [project
repository](https://github.com/Songyosr/spax). Package code uses the MIT
license; see [LICENSE](LICENSE). Consult dataset help for source
attribution and the original sources for terms governing third-party
example data.
