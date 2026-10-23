#' Spatial accessibility and allocation modeling
#'
#' A modular engine for spatial access and allocation models. Methods of the
#' two-step floating catchment area (2SFCA) family and an experimental static conditional logit
#' allocation model with an outside option share the same demand, facility
#' supply and travel inputs, optionally split into demand groups, the same
#' decay kernels and one numeric engine. The allocation model includes the Huff
#' model, declared with \code{\link{huff_allocation}}, and extends it with an
#' outside option for demand that goes elsewhere. It can be evaluated, fitted to
#' observed use, compared and used for prediction and scenarios. Output meanings
#' remain model-specific.
#'
#' @section Choose a workflow:
#' \code{\link{spax_2sfca}}, \code{\link{spax_e2sfca}} and
#' \code{\link{spax_m2sfca}} compute accessibility scores of the 2SFCA family, and
#' \code{\link{compute_access}} computes them from given decay weights. The
#' experimental \code{\link{spax_3sfca}} and \code{\link{spax_huff2sfca}} add
#' selection weights that divide each origin's demand among the facilities.
#' These take raster demand and facility travel layers. Multiple supply columns
#' produce independent accessibility indicators, not interacting resource
#' constraints.
#'
#' The experimental allocation interface runs a model in one call:
#' \code{\link{spax}} binds a model specification (\code{\link{spec_clm}} or
#' \code{\link{spec_huff}}, with a decay such as \code{\link{decay_gaussian}})
#' to data from \code{\link{prep_data}}, and fits it to counts declared with
#' \code{\link{obs_flows}} or \code{\link{obs_totals}}, or evaluates it when
#' every parameter has a value. \code{\link{spax_clm}} and
#' \code{\link{spax_huff}} do the same from raw inputs.
#'
#' Underneath, the six experimental allocation steps are
#' \code{\link{prepare_allocation}}, \code{\link{clm_allocation}},
#' \code{\link{fit_allocation}}, \code{\link{evaluate_allocation}},
#' \code{\link{predict_allocation}} and \code{\link{bootstrap_allocation}};
#' use them for full control. They describe one static CLM system with one
#' supply vector and an outside option; \code{\link{huff_allocation}} declares
#' the same system without the outside option, the classic Huff model. The
#' first five support Gaussian, exponential and power decay and kernels
#' declared with \code{\link{decay_kernel}} (fitting needs a kernel that is
#' smooth in \code{sigma}); bootstrap has the narrower scope below. Prediction,
#' the bootstrap (\code{\link{bootstrap_fit}}), \code{\link{check_fit}} and
#' \code{\link{compare_fits}} (also named \code{check_allocation()} and
#' \code{compare_allocations()}) accept a result of \code{spax()} in place
#' of the model and its fit. Broader feedback-model declarations and
#' general-purpose inference are not part of this experimental interface.
#'
#' @section Inputs, observations and support:
#' Preserve facility and origin IDs on numeric inputs. Named axes align
#' independently; unnamed inputs are positional. For the 2SFCA family, travel layer names
#' identify facilities. In the allocation route, only included positive
#' demand contributes to fitted facility loads. Known zero, unknown and excluded
#' demand remain distinct. Requested predictions have their own support:
#' unknown travel is not the same as a known absent connection (\code{Inf}).
#'
#' Declare compatible demand, supply and travel units and reference periods.
#' The interface records declarations without converting units. Specify whether
#' observations are event counts over a period or a stock such as registered
#' caseload at a reference date; a model output does not determine that meaning.
#' \code{\link{obs_flows}} and \code{\link{obs_totals}} fit with the Poisson
#' loss by default; \code{\link{fit_allocation}} with given starts defaults to
#' weighted SSE. Choosing the Poisson objective is not evidence for an
#' observation law.
#'
#' @section Allocation outputs and scenarios:
#' For a valid choice denominator, \code{allocation} contains inside
#' probabilities, \code{rho} is their row sum and \code{flow} is demand times
#' allocation. \code{utilization} sums those modeled counts by facility.
#' The outside category is not automatically unmet need.
#' \code{A} attributes raw supply per demand opportunity and
#' \code{abar = A/rho} attributes supply per inside contact when defined.
#' These are not delivered service or hard capacity constraints. Check saved
#' validity flags, especially for unsupported positive supply, unknown travel
#' and zero choice denominators, before reporting or mapping.
#'
#' Prediction uses saved facility loads and does not refit or update the system
#' when requested origins or their demand change. For a supply scenario, prepare
#' the changed system and evaluate it at selected parameters. Supply can change
#' attraction, allocation and loads as well as capacity attribution. This is a
#' model-based comparison, not a causal intervention estimate.
#'
#' @section Conditional bootstrap scope:
#' \code{bootstrap_allocation} requires a static CLM with a fitted outside mass
#' (\code{v0}) and a decay kernel that is smooth in \code{sigma}, either built in
#' or declared with \code{\link{decay_kernel}}. The supply elasticity \code{beta}
#' may be fixed or fitted, and \code{kappa} and a fixed \code{beta} may be any
#' positive values. It also requires complete whole facility counts and an
#' unweighted built-in Poisson original fit. Declare the event, reference period
#' and independent-facility sampling assumption explicitly.
#' Non-overlapping registrations alone do not establish independence.
#' Demand, travel, supply and the included network stay fixed while facility
#' observation multiplicities are resampled. Every physical facility remains
#' in the allocation network.
#'
#' Conditional percentile intervals require an eligible original fit, a complete
#' run (399 attempts by default) and finite successful values from at least 95
#' percent of the attempts per quantity (380 of 399).
#' Refused and incomplete results remain inspectable. Failed attempts are not
#' replaced; successful inner-boundary estimates remain in the distribution.
#' Coverage was evaluated for Gaussian decay with fitted \code{sigma} and
#' \code{v0} and 399 attempts, in generated 80-origin/40-facility designs under
#' tested fixed-input laws; other kernels, a fitted \code{beta} and other numbers
#' of attempts use the same procedure without a separate coverage study. This is not a universal coverage guarantee or
#' validation of an application's observation law. Regional attributed capacity
#' per demand is a fixed-input identity and has no bootstrap interval. No
#' simultaneous spatial intervals are provided. See
#' \code{\link{bootstrap_allocation}} for the full restrictions.
#'
#' @section Accessibility scores, saved results:
#' The accessibility scores of the 2SFCA family and the allocation model answer
#' different questions: the independent supply columns and accessibility scores
#' of a 2SFCA-family method are not choice probabilities. Normalization choices
#' are part of the 2SFCA-family specification. One numeric engine computing several models does not make
#' them nested.
#'
#' Named observation axes align to the input IDs. Custom loss arguments indexed
#' by observations must already use that order.
#'
#' Save input declarations and support metadata with fitted results, preferably
#' in an RDS file. CSV exports discard R attributes. Inspect saved results with
#' \code{coef}, \code{fitted}, \code{summary}, \code{plot} and
#' \code{as.data.frame}; these methods do not fit or bootstrap again. Results
#' saved by earlier versions may lack what prediction and reporting need; fit
#' them again from the inputs. Record the package version with saved results.
#'
#' @section Installed guides and status:
#' The \code{spax()} interface and the allocation model are experimental and
#' may change in later versions. Package and function help are installed with
#' the package; use \code{help(package = "spax")} to browse them. Installations
#' with built articles also expose \code{browseVignettes("spax")}. Start with
#' \code{vignette("spax", package = "spax")}, which computes accessibility scores for
#' Cornwall; \code{vignette("cornwall-data", package = "spax")} describes the
#' bundled example data. \code{vignette("model-choice", package = "spax")}
#' compares accessibility scores with the allocation model,
#' \code{vignette("allocation-workflow", package = "spax")} fits the model to
#' the Cornwall GP registrations, \code{vignette("scenario", package = "spax")}
#' compares a practice closure at the fitted values, and
#' \code{vignette("extending", package = "spax")} covers your own decay curve,
#' raster data and grid maps. \code{vignette("facility-bootstrap", package =
#' "spax")} gives bootstrap intervals on a generated network, and
#' \code{vignette("reading-results", package = "spax")} says what each result
#' measures.
#'
#' @aliases allocation-compatibility
#' @keywords package
"_PACKAGE"

## usethis namespace: start
#' @importFrom stats rbinom rnbinom rpois setNames sd
#' @import terra
#' @importFrom sf st_drop_geometry
## usethis namespace: end
NULL
# sf is imported so that loading spax registers sf's methods for the sf
# example datasets (cornwall_gp, cornwall_lsoa).
