# How sure are the estimates? Bootstrap intervals

A fitted allocation model gives one value for each parameter. This guide
puts a range around those values: bootstrap intervals for the model’s
two fitted parameters, for the share of demand that reaches a facility,
and for the change that a supply scenario makes. The question is how far
the estimates could move if the observed facility totals had come out
differently.
[`bootstrap_fit()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md)
(also named
[`bootstrap_allocation()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md))
answers it by refitting the model many times to resampled facility
totals. The guide runs on generated data with a known truth instead of
[The Cornwall GP example
data](https://songyosr.github.io/spax/articles/cornwall-data.md), for
the reasons in the next section. The
[`spax()`](https://songyosr.github.io/spax/reference/spax.md) interface
and the allocation model are experimental and may change in later
versions.

## Why generated data

Generated data have a known truth, so you can see whether the intervals
contain it. They also meet the bootstrap’s assumption that each
facility’s total is an independent count over one period. The Cornwall
data do not. The registrations are a stock: who is registered where on
one date. The Cornwall model is also fitted to flows from small areas to
practices, not to practice totals. [Where do residents
register?](https://songyosr.github.io/spax/articles/allocation-workflow.md)
fits that model.

``` r

library(spax)
library(dplyr)
```

## A generated network with a known truth

The model is the conditional logit model (CLM) of [Where do residents
register?](https://songyosr.github.io/spax/articles/allocation-workflow.md).
Each origin’s demand, here service requests, is shared among the
facilities in proportion to their supply, here service slots, discounted
by travel time. An outside option competes with the facilities and takes
the requests that go to none of them. For origin $`i`$ and facility
$`j`$, the share of requests is

``` math
P_{ij} = \frac{S_j \, w(d_{ij})}{\sum_k S_k \, w(d_{ik}) + v_0},
```

where $`S_j`$ is the supply of facility $`j`$, $`d_{ij}`$ the travel
time in minutes and $`w(d) = \exp(-d^2 / 2\sigma^2)`$ a Gaussian weight.
The travel scale σ sets how fast a facility’s pull falls with travel
time: at σ minutes it keeps about 61% of its weight, and at 2σ about
14%. The outside weight v0 is set against the facilities’ weighted
supply: an origin whose facilities add up to a weighted supply of v0
sends half its requests outside.

The code builds travel times between origins and facilities on a wavy
strip, computes the shares at a known truth (`probability`) and draws
each facility’s total as a Poisson count around its modeled total. It
then fits the model with
[`spax()`](https://songyosr.github.io/spax/reference/spax.md), as in
[Where do residents
register?](https://songyosr.github.io/spax/articles/allocation-workflow.md),
to the facility totals declared by
[`obs_totals()`](https://songyosr.github.io/spax/reference/obs_flows.md).

``` r

units <- list(
  demand_units = "requests",
  demand_period = "one_reference_period",
  supply_units = "service_slots",
  supply_period = "one_reference_period",
  travel_units = "minutes"
)

# Travel minutes between 80 origins and 40 facilities on a wavy strip
origin_x <- seq(2, 58, length.out = 80)
origin_y <- 8 * sin(seq_len(80) * 0.73)
facility_x <- seq(4, 56, length.out = 40)
facility_y <- 7 * sin(seq_len(40) * 1.31)
travel <- sqrt(
  outer(origin_x, facility_x, "-")^2 + outer(origin_y, facility_y, "-")^2
) + 2
dimnames(travel) <- list(paste0("origin", 1:80), paste0("facility", 1:40))

# Demand at each origin and supply at each facility
demand <- setNames(80 + 35 * (1 + sin(seq_len(80) * 0.23)), rownames(travel))
supply <- setNames(rep(c(5, 7, 6, 8, 9), 8), colnames(travel))

# True choice probabilities under the CLM
truth <- c(sigma = 12, v0 = 45)
attraction <- exp(-travel^2 / (2 * truth[["sigma"]]^2)) *
  rep(supply, each = length(demand))
probability <- attraction / (rowSums(attraction) + truth[["v0"]])

# Observed facility totals: independent Poisson counts around the truth
set.seed(2490001)
observed <- setNames(rpois(40, colSums(probability * demand)), names(supply))

# Fit the CLM to the facility totals
data <- prep_data(demand, supply, travel, metadata = units)
result <- spax(data, spec_clm(), observed = obs_totals(observed))
rbind(truth = truth, estimate = coef(result))
```

``` spax-output
            sigma       v0
truth    12.00000 45.00000
estimate 13.27969 48.57531
```

The network has 80 origins and 40 facilities. The fit uses the facility
totals, because they are what the bootstrap resamples.
[`spax()`](https://songyosr.github.io/spax/reference/spax.md) found its
starting values and search bounds from the data, and every bootstrap
refit reuses them. The fit estimates σ = 13.3 minutes and v0 = 48.6,
against a truth of 12 and 45. The gap is expected: the 40 totals are one
random draw, and the bootstrap describes how much the estimates could
vary from draw to draw.

## Declare the observation and a scenario

The bootstrap needs a declaration of how the counts arose: what was
counted, over what period, and how the facilities were sampled. This
declaration is the observation law. spax stores it with the result but
cannot check it. `sampling` must be `"independent_facilities"`, the only
design the bootstrap supports. Independent sampling of facilities is
your assumption. Here it holds, because the totals were drawn that way.

A scenario is optional: the same model with changed supply. The
bootstrap evaluates the baseline and the scenario together at each
refitted set of parameter values, so the change between them gets an
interval of its own. The scenario is a model declared with
[`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md)
on data prepared like the baseline, with only the supply changed;
`fit_v0 = TRUE` matches the fitted outside weight of
[`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md).

``` r

# Scenario: 20% more supply at facility2
scenario_supply <- supply
scenario_supply["facility2"] <- 1.2 * scenario_supply["facility2"]
scenario <- clm_allocation(
  prep_data(demand, scenario_supply, travel, metadata = units),
  fit_v0 = TRUE
)

# What was observed, over what period, and how facilities were sampled
observation <- list(
  event = "allocated service requests",
  period = "one_reference_period",
  sampling = "independent_facilities"
)

# Run the bootstrap only when rebuilding the stored result (see below)
rebuild_boot <- identical(Sys.getenv("SPAX_REBUILD_VIGNETTE_BOOTSTRAP"), "true")
```

The scenario raises the supply of facility2 from 7 to 8.4 service slots
and changes nothing else.

## Run the bootstrap

[`bootstrap_fit()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md)
makes 399 attempts by default. Each attempt draws 40 facilities with
replacement from the 40, refits the model to their totals and records
the new estimates. A facility drawn twice counts twice in the fit, and
one not drawn does not count. Every facility and its supply stay in the
network: only the weight of each total in the fit changes. Each attempt
uses its own seed, the given seed plus the attempt number, so a run
repeats exactly. The function takes the fitted result, the observation,
the scenario and a seed. `attempts` sets another number of attempts, at
least 199: more make the interval endpoints depend less on the random
draws and take proportionally longer, while fewer give noisier intervals
that tend to be narrower. Choose the number and the seed before the run.
Runs with the same seed share their first attempts, so a larger run is
the smaller one with more attempts added; changing the number after
seeing a result is not part of the method.

``` r

boot <- bootstrap_fit(
  result,
  observation = observation,
  scenario = scenario,
  seed = 20260930
)
```

Before the first attempt,
[`bootstrap_fit()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md)
checks that the original fit is usable: converged, with a score close
enough to zero, each parameter pinned down by the totals and away from
the bounds of its search. A model declared with the step functions works
too: give the model and its fit, as in
`bootstrap_fit(model, fit, observation = observation, seed = 20260930)`.

### The stored result

The 399 refits take close to a minute, so the
[`bootstrap_fit()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md)
call above does not run when this guide is built. The guide reads the
result of that exact call from `allocation-bootstrap.rds`, which is
installed with the package, so you can read it too. To follow along with
your own run, run the
[`bootstrap_fit()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md)
call above and skip the next chunk. The script
`data-raw/precompute-vignette-bootstrap.R`, in the package repository,
rebuilds the file. The code below reads the stored result; when the
guide is built, it also checks that the result belongs to the inputs and
the fit computed above.

``` r

# The stored result of the call above, installed with spax
boot <- readRDS(system.file("extdata", "allocation-bootstrap.rds", package = "spax"))

c(
  status = boot$status,
  attempted = boot$attempted,
  successful = boot$successful,
  built_with_spax = attr(boot, "spax_version"),
  running_spax = as.character(packageVersion("spax"))
)
```

``` spax-output
         status       attempted      successful built_with_spax    running_spax 
     "complete"           "399"           "399"    "0.6.0.9000"         "0.7.0" 
```

All 399 attempts were made and 399 succeeded. The result was built with
spax 0.6.0.9000 and is read under 0.7.0. Rerunning the call under the
running version gives the same estimates and intervals.

## Read the intervals

[`as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html)
gives the point estimate and the interval of each quantity, and
`type = "draws"` gives every attempt.

``` r

parameters <- c("sigma", "v0")

# Point estimates and intervals for the two parameters
estimates <- as.data.frame(boot)
filter(estimates, quantity %in% parameters)
```

``` spax-output
  quantity estimate    lower    upper                          status
1    sigma 13.27969 10.54105 18.61638 conditional_pointwise_bootstrap
2       v0 48.57531 35.02844 69.06907 conditional_pointwise_bootstrap
  successful attempted planned
1        399       399     399
2        399       399     399
```

``` r

# The first few of the 399 attempts
draws <- as.data.frame(boot, type = "draws")
draws |>
  select(attempt, status, all_of(parameters)) |>
  head()
```

``` spax-output
  attempt   status    sigma       v0
1       1 selected 12.86513 48.73043
2       2 selected 13.21244 50.51980
3       3 selected 12.14212 40.43821
4       4 selected 12.43516 42.49469
5       5 selected 12.10434 43.16452
6       6 selected 14.70454 53.31157
```

Each interval is a 95% percentile interval: the middle 95% of the
refitted values. The status `conditional_pointwise_bootstrap` means that
the interval holds the inputs fixed and covers that quantity on its own.
The interval for σ runs from 10.5 to 18.6 minutes, and the interval for
v0 from 35.0 to 69.1. Each row of the draws is one attempt with its
refitted parameters, and `selected` marks an attempt whose refit
succeeded.

The table has three more rows:

``` r

filter(estimates, !quantity %in% parameters)
```

``` spax-output
        quantity   estimate     lower      upper
1   regional_rho 0.67108079 0.6511258  0.6896859
2 delta_contacts 9.87225371 8.9083090 11.0227976
3     regional_A 0.03041016        NA         NA
                           status successful attempted planned
1 conditional_pointwise_bootstrap        399       399     399
2 conditional_pointwise_bootstrap        399       399     399
3            fixed_input_identity        399       399     399
```

`regional_rho` is the share of all requests that the model sends to a
facility rather than the outside option: 67.1%, with an interval from
65.1% to 69.0%. `delta_contacts` is the change the scenario makes in the
requests that reach a facility: 9.9 more, with an interval from 8.9 to
11.0. `regional_A`, the supply per unit of demand, depends only on the
inputs, so it has no interval.

The intervals follow fixed rules:

- A run makes all its attempts, 399 unless `attempts` says otherwise,
  chosen before the run. A failed attempt stays in the record and is not
  replaced. A refit that ends at a bound of its search range (an inner
  boundary) stays in the distribution.
- An interval needs all the attempts and finite values from at least 95%
  of them, rounded up: 380 of 399. It also needs a usable original fit:
  away from the bounds of its search, converged precisely, and with each
  parameter pinned down by the totals.
- Coverage was studied with 399 attempts. A result with another number
  says so in a note.
- Each refit starts from the same starting values and search bounds as
  the original fit: the ones you gave, or the ones found from the data
  when you gave none. Facility totals can fit almost as well at quite
  different travel scales, so every refit tries all the starts and keeps
  its best fit.

When a rule fails, the result keeps its point estimates and the reasons,
and withholds the intervals.

## Check against the truth

The generated data have a known truth, so the intervals can be set
beside it.

``` r

# Two panels side by side; each adds the true value as a dashed line
oldpar <- par(mfrow = c(1, 2))
for (parameter in c("sigma", "v0")) {
  plot(boot, quantity = parameter, main = paste(parameter, "(dashed line: truth)"))
  abline(v = truth[[parameter]], col = "orange", lwd = 2, lty = 2)
}
```

![Two interval plots, for sigma and for v0, each showing the point
estimate and its bootstrap percentile interval, with the true value as
an orange dashed line inside the
interval.](facility-bootstrap_files/figure-html/bootstrap-truth-1.png)

``` r

par(oldpar)
```

The dot is the estimate and the line its 95% interval, with σ in
minutes. Both intervals contain the true values, shown as dashed lines.
The caption above each panel says that the run is complete, that the
quantity has an interval and how many attempts gave a finite value.
These are two separate intervals, not a joint range for σ and v0, and
they rest on the declared sampling. One generated example whose
intervals contain the truth does not show that 95% of intervals would
contain it in another application.

## Tables and saving

The result has its own [`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rspatial.github.io/terra/reference/summary.html),
[`plot()`](https://rspatial.github.io/terra/reference/plot.html) and
[`as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html)
methods.
[`as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html)
gives the estimates, every draw (`type = "draws"`) or every start of
every attempt (`type = "starts"`), failures included.
[`summary()`](https://rspatial.github.io/terra/reference/summary.html)
and [`plot()`](https://rspatial.github.io/terra/reference/plot.html)
read the saved values and resample nothing. Save the whole result with
[`saveRDS()`](https://rspatial.github.io/terra/reference/serialize.html):
it keeps the declared observation, units and seed, which a CSV table
drops. The code below saves the result, reads it back and confirms that
the draws are unchanged.

``` r

# Save the result and read it back
saved <- tempfile(fileext = ".rds")
saveRDS(boot, saved)
restored <- readRDS(saved)

# The restored result gives the same draws, with nothing recomputed
identical(
  as.data.frame(restored, type = "draws"),
  as.data.frame(boot, type = "draws")
)
```

``` spax-output
[1] TRUE
```

``` r

unlink(saved)
```

## Next

- [What if a practice
  closed?](https://songyosr.github.io/spax/articles/scenario.md): a
  supply scenario on the Cornwall data, at fixed parameter values.
- [Where do residents
  register?](https://songyosr.github.io/spax/articles/allocation-workflow.md):
  the model fitted to the Cornwall registrations, and what those data
  identify.
- [Reading
  results](https://songyosr.github.io/spax/articles/reading-results.md):
  what intervals, scenarios and fits show, and what they do not.
