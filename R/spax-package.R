#' Spatial accessibility and allocation modeling
#'
#' Public floating catchment area (FCA) methods and an experimental corrected
#' static conditional-logit model (CLM) workflow share demand, facility supply
#' and travel inputs. Their
#' normalization and output meanings remain model-specific. Internal feedback
#' models are private; a shared execution engine does not establish universal
#' mathematical nesting among the models.
#'
#' @section Choose a workflow:
#' \code{\link{spax_2sfca}}, \code{\link{spax_e2sfca}} and
#' \code{\link{spax_m2sfca}} remain supported public FCA entry points;
#' \code{\link{compute_access}} is also retained. These take raster demand and
#' facility travel layers. Multiple supply columns produce independent
#' accessibility indicators, not interacting resource constraints.
#'
#' The six experimental allocation operations are
#' \code{\link{prepare_allocation}}, \code{\link{clm_allocation}},
#' \code{\link{fit_allocation}}, \code{\link{evaluate_allocation}},
#' \code{\link{predict_allocation}} and \code{\link{bootstrap_allocation}}.
#' They describe one static CLM system with one supply vector and an outside
#' option. The first five support Gaussian, exponential and power decay;
#' bootstrap has the narrower scope below. Broader feedback-model declarations
#' and general-purpose inference are not part of this experimental interface.
#'
#' @section Inputs, observations and support:
#' Preserve facility and origin IDs on numeric inputs. Named axes align
#' independently; unnamed inputs are positional. For FCA, travel layer names
#' identify facilities. In the checked allocation route, only included positive
#' demand contributes to fitted facility loads. Known zero, unknown and excluded
#' demand remain distinct. Requested predictions have their own support:
#' unknown travel is not the same as a known absent connection (\code{Inf}).
#'
#' Declare compatible demand, supply and travel units and reference periods.
#' The interface records declarations without converting units. Specify whether
#' observations are event counts over a period or a stock such as registered
#' caseload at a reference date; a model output does not determine that meaning.
#' The fitting default is weighted SSE. Choose the Poisson objective explicitly
#' when appropriate; choosing it is not evidence for an observation law.
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
#' \code{bootstrap_allocation} requires Gaussian static CLM with jointly fitted
#' \code{sigma} and \code{v0}, \code{kappa = beta = 1}, complete whole facility
#' counts and an unweighted built-in Poisson original fit. Declare the event,
#' reference period and independent-facility sampling assumption explicitly.
#' Non-overlapping registrations alone do not establish independence.
#' Demand, travel, supply and the included network stay fixed while facility
#' observation multiplicities are resampled. Every physical facility remains
#' in the allocation network.
#'
#' Conditional percentile intervals require an eligible original fit, a complete
#' run of 399 attempts and at least 380 finite successful values per quantity.
#' Refused and incomplete results remain inspectable. Failed attempts are not
#' replaced; successful inner-boundary estimates remain in the distribution.
#' Validation in generated 80-origin/40-facility designs under tested fixed-input
#' laws is not a universal coverage guarantee or validation of an application's
#' observation law. Regional attributed capacity per demand is a fixed-input
#' identity and has no bootstrap interval. No simultaneous spatial intervals
#' are provided. See \code{\link{bootstrap_allocation}} for the full restrictions.
#'
#' @section Compatibility and migration:
#' Existing public FCA calls remain supported. Their independent supply columns
#' and accessibility scores do not become CLM probabilities by changing function
#' names. Normalization choices remain part of the FCA model specification.
#'
#' Corrected static CLM and the older private \code{huff_decay} route have
#' different equations and output meanings. Do not reinterpret saved legacy
#' outputs as corrected contact or count outputs. Older private fits may lack
#' the compact source snapshot or metadata needed for prediction and reporting;
#' regenerate them from verified inputs rather than renaming their classes.
#' Named observation axes align to canonical input IDs. Custom loss arguments
#' indexed by observations must already use that canonical order.
#'
#' Save input declarations and support metadata with fitted results, preferably
#' in an RDS bundle. CSV exports discard R attributes. Inspect saved results with
#' \code{coef}, \code{fitted}, \code{summary}, \code{plot} and
#' \code{as.data.frame}; these methods do not silently fit or bootstrap again.
#' Record the source commit as well as the package version while using the
#' experimental development branch.
#'
#' @section Installed guides and development status:
#' The experimental interface is developed on \code{codex-assist-main}; it is
#' not a new stable release. Package and function help are installed with the
#' package. Use \code{help(package = "spax")} to browse them. Installations
#' with built articles also expose \code{browseVignettes("spax")} and
#' \code{vignette("allocation-workflow", package = "spax")}. The allocation
#' article uses generated data and covers fitting, paired supply comparison,
#' conditional bootstrap, maps, export and fresh-session save/reload.
#'
#' @aliases allocation-compatibility
#' @keywords package
"_PACKAGE"

## usethis namespace: start
#' @importFrom raster raster
#' @importFrom stats rbinom rnbinom rpois runif setNames sd
#' @import terra
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_hline geom_histogram geom_vline geom_point
#' @importFrom ggplot2 scale_color_viridis_d scale_fill_viridis_d scale_size_continuous
#' @importFrom ggplot2 labs mean_cl_normal theme element_text theme_minimal
#' @importFrom tibble tibble
## usethis namespace: end
NULL

# Non-standard-evaluation column names used in data.frame pipelines
# (e.g. gather_demand). Declared to satisfy R CMD check's "no visible
# binding for global variable" note.
utils::globalVariables(c("unit_id", "weighted_sum"))
