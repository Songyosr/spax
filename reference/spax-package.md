# Spatial accessibility and allocation modeling

Public floating catchment area (FCA) methods and an experimental
corrected static conditional-logit model (CLM) workflow share demand,
facility supply and travel inputs. Their normalization and output
meanings remain model-specific. Internal feedback models are private; a
shared execution engine does not establish universal mathematical
nesting among the models.

## Choose a workflow

[`spax_2sfca`](https://songyosr.github.io/spax/reference/spax_2sfca.md),
[`spax_e2sfca`](https://songyosr.github.io/spax/reference/spax_e2sfca.md)
and
[`spax_m2sfca`](https://songyosr.github.io/spax/reference/spax_m2sfca.md)
remain supported public FCA entry points;
[`compute_access`](https://songyosr.github.io/spax/reference/compute_access.md)
is also retained. The experimental
[`spax_3sfca`](https://songyosr.github.io/spax/reference/spax_3sfca.md)
and
[`spax_huff2sfca`](https://songyosr.github.io/spax/reference/spax_huff2sfca.md)
add selection weights that divide each origin's demand among the
facilities. These take raster demand and facility travel layers.
Multiple supply columns produce independent accessibility indicators,
not interacting resource constraints.

The six experimental allocation operations are
[`prepare_allocation`](https://songyosr.github.io/spax/reference/prepare_allocation.md),
[`clm_allocation`](https://songyosr.github.io/spax/reference/clm_allocation.md),
[`fit_allocation`](https://songyosr.github.io/spax/reference/fit_allocation.md),
[`evaluate_allocation`](https://songyosr.github.io/spax/reference/evaluate_allocation.md),
[`predict_allocation`](https://songyosr.github.io/spax/reference/predict_allocation.md)
and
[`bootstrap_allocation`](https://songyosr.github.io/spax/reference/bootstrap_allocation.md).
They describe one static CLM system with one supply vector and an
outside option. The first five support Gaussian, exponential and power
decay and kernels declared with
[`decay_kernel`](https://songyosr.github.io/spax/reference/decay_kernel.md)
(fitting needs a kernel that is smooth in `sigma`); bootstrap has the
narrower scope below. Broader feedback-model declarations and
general-purpose inference are not part of this experimental interface.

## Inputs, observations and support

Preserve facility and origin IDs on numeric inputs. Named axes align
independently; unnamed inputs are positional. For FCA, travel layer
names identify facilities. In the checked allocation route, only
included positive demand contributes to fitted facility loads. Known
zero, unknown and excluded demand remain distinct. Requested predictions
have their own support: unknown travel is not the same as a known absent
connection (`Inf`).

Declare compatible demand, supply and travel units and reference
periods. The interface records declarations without converting units.
Specify whether observations are event counts over a period or a stock
such as registered caseload at a reference date; a model output does not
determine that meaning. The fitting default is weighted SSE. Choose the
Poisson objective explicitly when appropriate; choosing it is not
evidence for an observation law.

## Allocation outputs and scenarios

For a valid choice denominator, `allocation` contains inside
probabilities, `rho` is their row sum and `flow` is demand times
allocation. `utilization` sums those modeled counts by facility. The
outside category is not automatically unmet need. `A` attributes raw
supply per demand opportunity and `abar = A/rho` attributes supply per
inside contact when defined. These are not delivered service or hard
capacity constraints. Check saved validity flags, especially for
unsupported positive supply, unknown travel and zero choice
denominators, before reporting or mapping.

Prediction uses saved facility loads and does not refit or update the
system when requested origins or their demand change. For a supply
scenario, prepare the changed system and evaluate it at selected
parameters. Supply can change attraction, allocation and loads as well
as capacity attribution. This is a model-based comparison, not a causal
intervention estimate.

## Conditional bootstrap scope

`bootstrap_allocation` requires a static CLM with a fitted outside mass
(`v0`) and a decay kernel that is smooth in `sigma`, either built in or
declared with
[`decay_kernel`](https://songyosr.github.io/spax/reference/decay_kernel.md).
The supply elasticity `beta` may be fixed or fitted, and `kappa` and a
fixed `beta` may be any positive values. It also requires complete whole
facility counts and an unweighted built-in Poisson original fit. Declare
the event, reference period and independent-facility sampling assumption
explicitly. Non-overlapping registrations alone do not establish
independence. Demand, travel, supply and the included network stay fixed
while facility observation multiplicities are resampled. Every physical
facility remains in the allocation network.

Conditional percentile intervals require an eligible original fit, a
complete run of 399 attempts and at least 380 finite successful values
per quantity. Refused and incomplete results remain inspectable. Failed
attempts are not replaced; successful inner-boundary estimates remain in
the distribution. Coverage was evaluated for Gaussian decay with fitted
`sigma` and `v0`, in generated 80-origin/40-facility designs under
tested fixed-input laws; other kernels and a fitted `beta` use the same
procedure without a separate coverage study. This is not a universal
coverage guarantee or validation of an application's observation law.
Regional attributed capacity per demand is a fixed-input identity and
has no bootstrap interval. No simultaneous spatial intervals are
provided. See
[`bootstrap_allocation`](https://songyosr.github.io/spax/reference/bootstrap_allocation.md)
for the full restrictions.

## Compatibility and migration

Existing public FCA calls remain supported. Their independent supply
columns and accessibility scores do not become CLM probabilities by
changing function names. Normalization choices remain part of the FCA
model specification.

Corrected static CLM and the older private `huff_decay` route have
different equations and output meanings. Do not reinterpret saved legacy
outputs as corrected contact or count outputs. Older private fits may
lack the compact source snapshot or metadata needed for prediction and
reporting; regenerate them from verified inputs rather than renaming
their classes. Named observation axes align to canonical input IDs.
Custom loss arguments indexed by observations must already use that
canonical order.

Save input declarations and support metadata with fitted results,
preferably in an RDS bundle. CSV exports discard R attributes. Inspect
saved results with `coef`, `fitted`, `summary`, `plot` and
`as.data.frame`; these methods do not silently fit or bootstrap again.
Record the package version, and the source commit when using a
development version from the `develop` branch.

## Installed guides and status

Version 0.3.0 is the first release that includes the experimental
allocation interface, which may change in later versions. Package and
function help are installed with the package. Use
[`help(package = "spax")`](https://songyosr.github.io/spax/reference) to
browse them. Installations with built articles also expose
`browseVignettes("spax")`. Start with
[`vignette("spax", package = "spax")`](https://songyosr.github.io/spax/articles/spax.md),
which computes one FCA surface;
[`vignette("cornwall-data", package = "spax")`](https://songyosr.github.io/spax/articles/cornwall-data.md)
describes the bundled Cornwall GP example data. The allocation workflow,
[`vignette("allocation-workflow", package = "spax")`](https://songyosr.github.io/spax/articles/allocation-workflow.md),
fits the model to the Cornwall GP registrations and covers prediction, a
supply scenario, maps, export and fresh-session save/reload. The
conditional bootstrap,
[`vignette("facility-bootstrap", package = "spax")`](https://songyosr.github.io/spax/articles/facility-bootstrap.md),
uses a generated network.

## See also

Useful links:

- <https://github.com/songyosr/spax>

- <https://songyosr.github.io/spax/>

- Report bugs at <https://github.com/Songyosr/spax/issues>

## Author

**Maintainer**: Songyos Rajborirug <songyos.r@psu.ac.th>
([ORCID](https://orcid.org/0000-0002-5000-7409))
