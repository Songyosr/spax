# Experimental portable static-CLM workflow ---------------------------------

#' Prepare checked numeric inputs for an experimental allocation workflow
#'
#' Named axes align independently; unnamed axes are positional. Positive
#' included demand enters facility-load support. Zero, unknown and explicitly
#' excluded origins retain separate support statuses. This experimental entry
#' point delegates to the existing compact preparation boundary.
#'
#' Demand groups (age bands, insurance types) are given as a named list of
#' demand vectors, one per group, with the same origin IDs. An origin is active
#' when it is included, every group demand is known and the total is positive;
#' an origin with a group demand missing is unknown as a whole under
#' `missing_demand = "exclude"`. The prepared object then also carries the
#' group IDs and the demand and travel by group (`D_groups`, `distance_groups`).
#' @param demand Numeric origin vector, finite and nonnegative or explicitly
#'   permitted missing values. Negative or infinite demand is rejected even on
#'   excluded origins. Named demand matches canonical origin IDs. For demand
#'   groups, a named list by group of such named vectors with identical name
#'   sets (any order); the group names must be unique, non-empty, not `NA`, not
#'   `"all"` and without `"."`. A matrix is refused.
#' @param supply Finite nonnegative facility vector. Known zero supply is
#'   retained; unknown supply is rejected. Names match canonical facility IDs.
#' @param distance Numeric origin-by-facility travel matrix. Complete named
#'   rows and columns match IDs independently; unnamed axes are positional. With
#'   demand groups, one matrix shared by the groups or a named list of matrices
#'   with one element per group, each aligned by origin and facility IDs. Missing
#'   travel for a group with zero demand at an origin is ignored.
#' @param origin_ids,facility_ids Complete unique nonblank canonical IDs. Their
#'   order determines prepared order. The default (`NULL`) takes the names of
#'   the demand vector (of the first group for grouped demand) and the column
#'   names of the travel matrix; identities are never inferred from equal axis
#'   lengths.
#' @param included Nonmissing logical origin mask (default: all `TRUE`). Names
#'   align independently; an unnamed mask follows supplied demand order (that of
#'   the first group for grouped demand) and is reordered with demand.
#' @param missing_demand Default `"error"` rejects included missing demand.
#'   `"exclude"` omits it from loads while recording unknown demand, separately
#'   from known zero or explicit exclusion. Positive included demand remains
#'   active even when every inside edge is absent.
#' @param missing_travel Default `"error"` rejects NA/NaN on active fit rows.
#'   `"legacy_zero"` explicitly omits unknown edges and records that policy.
#'   Finite nonnegative travel is known, positive infinity is an absent edge,
#'   and negative travel is invalid. Omitted rows are validated when requested
#'   by prediction. Model-specific finite domains, such as positive power
#'   distances, are checked when binding parameters; no imputation bypasses them.
#' @param spatial Optional list with a one-layer `terra::SpatRaster` `template`
#'   and a complete unique in-range `cell_index` vector, optionally named by
#'   origin ID. Registry positions are not raster cells. Only plain geometry and
#'   the aligned mapping are saved. Numeric work needs no spatial metadata.
#' @param metadata Optional named list of scalar strings or NA for
#'   `demand_units`, `demand_period`, `supply_units`, `supply_period`, and
#'   `travel_units`. Omitted fields remain NA. These are caller declarations;
#'   no unit conversion or dimensional compatibility check is performed.
#' @return The existing `ae_prepared_inputs`, with a full origin registry and
#'   distinct support statuses plus compact active load arrays. No duplicate
#'   full travel matrix is retained. Pass it to [clm_allocation()] rather than
#'   editing its internal fields. Expected counts use demand units/period;
#'   proportional capacity attribution uses supply per unit of demand. With
#'   demand groups the total demand is `D_active`, and `group_ids`, `D_groups`
#'   and `distance_groups` are added (`distance_active` is `NULL` when travel is
#'   given per group).
#' @family experimental allocation workflow
#' @export
#' @md
prepare_allocation <- function(demand, supply, distance,
                               origin_ids = NULL,
                               facility_ids = NULL,
                               included = NULL,
                               missing_demand = c("error", "exclude"),
                               missing_travel = c("error", "legacy_zero"),
                               spatial = NULL, metadata = list()) {
  .prepare_allocation(demand, supply, distance, origin_ids = origin_ids,
    facility_ids = facility_ids, included = included, missing_demand = missing_demand,
    missing_travel = missing_travel, spatial = spatial, metadata = metadata)
}

#' Prepare data for an allocation model
#'
#' Experimental. `prep_data()` checks and aligns the demand, supply and travel
#' time inputs of an allocation model once, so that model specifications such
#' as [spec_clm()] and [spec_huff()] can be bound to them with `spax()`. In
#' this version it is [prepare_allocation()] under another name, with `travel`
#' for `distance`: it returns the identical object, an `ae_prepared_inputs`,
#' and every argument is passed through. The step functions
#' ([clm_allocation()] and the others) accept the object too.
#'
#' Printing the object lists its origins and facilities, the demand groups, the
#' unit labels when they were given, the active origins whose travel is unknown
#' and the policies for missing demand and travel.
#'
#' @inheritParams prepare_allocation
#' @param travel Numeric origin-by-facility travel matrix. It is the
#'   `distance` argument of [prepare_allocation()]: the same alignment by IDs
#'   applies, and with demand groups it can be one matrix shared by the groups
#'   or a named list of matrices with one element per group.
#' @param x Prepared inputs, an `ae_prepared_inputs`.
#' @param ... Unused.
#' @return The existing `ae_prepared_inputs`, as returned by
#'   [prepare_allocation()]. `format()` returns the printed lines as a character
#'   vector and `print()` returns its argument invisibly.
#' @family allocation interface
#' @examples
#' # Mainland Cornwall: registered residents, GP FTE and travel minutes
#' areas <- sf::st_drop_geometry(cornwall_lsoa[!cornwall_lsoa$scilly, ])
#' gp <- sf::st_drop_geometry(cornwall_gp[!cornwall_gp$scilly, ])
#' demand <- stats::setNames(areas$registered, areas$lsoa)
#' supply <- stats::setNames(gp$gp_fte_extg, gp$practice)
#' travel <- cornwall_travel[areas$lsoa, gp$practice]
#' prep_data(demand, supply, travel,
#'           metadata = list(demand_units = "registered residents",
#'                           supply_units = "GP FTE", travel_units = "minutes"))
#' @export
#' @md
prep_data <- function(demand, supply, travel,
                      origin_ids = NULL,
                      facility_ids = NULL,
                      included = NULL,
                      missing_demand = c("error", "exclude"),
                      missing_travel = c("error", "legacy_zero"),
                      spatial = NULL, metadata = list()) {
  prepare_allocation(demand, supply, distance = travel, origin_ids = origin_ids,
    facility_ids = facility_ids, included = included, missing_demand = missing_demand,
    missing_travel = missing_travel, spatial = spatial, metadata = metadata)
}

#' @rdname prep_data
#' @method format ae_prepared_inputs
#' @export
#' @md
format.ae_prepared_inputs <- function(x, ...) {
  line <- function(label, text) sprintf("  %-10s  %s", label, text)
  active <- length(x$origin_ids)
  total <- length(if (is.null(x$all_origin_ids)) x$origin_ids else x$all_origin_ids)
  origins <- paste0(.format_total(total), " origins (", .format_total(active), " active",
                    if (total > active) paste0(", ", .format_total(total - active), " excluded"),
                    ")")
  meta <- x$input_metadata
  # A label that was given; unit labels are caller declarations, never converted.
  given <- function(value) {
    if (is.character(value) && length(value) == 1L && !is.na(value)) value
  }
  labelled <- function(units, period) {
    period <- given(period)
    c(given(units), if (!is.null(period)) paste0("(", period, ")"))
  }
  unknown <- sum(x$active_unknown_travel)
  policy <- x$input_policy
  policy_text <- function(value) if (is.character(value) && length(value) == 1L) value else "unknown"
  supply <- labelled(meta$supply_units, meta$supply_period)
  c(paste0("<spax data> ", origins, " x ", .format_total(length(x$facility_ids)), " facilities"),
    if (!is.null(x$group_ids)) line("groups", paste(x$group_ids, collapse = ", ")),
    line("demand", paste(c(.format_total(sum(x$D_active)),
                           labelled(meta$demand_units, meta$demand_period)), collapse = " ")),
    if (length(supply)) line("supply", paste(supply, collapse = " ")),
    line("travel", paste0(if (!is.null(given(meta$travel_units))) paste0(meta$travel_units, "; "),
                          "unknown for ", unknown, " active ",
                          if (unknown == 1L) "origin" else "origins")),
    line("policies", paste0("missing demand: ", policy_text(policy$missing_demand),
                            "; missing travel: ", policy_text(policy$missing_travel))))
}

#' @rdname prep_data
#' @method print ae_prepared_inputs
#' @export
#' @md
print.ae_prepared_inputs <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

#' Declare corrected static CLM over checked allocation inputs
#'
#' Declare corrected static CLM over checked allocation inputs.
#'
#' @param inputs An `ae_prepared_inputs` object from checked preparation.
#' @param family Travel kernel: `"gaussian"`, `"exponential"`, `"power"`, or a
#'   declaration from [decay_kernel()]. Gaussian sigma is a travel scale,
#'   exponential sigma a rate, and power sigma an exponent. Power requires
#'   strictly positive finite travel distances. A built-in declaration, such as
#'   `decay_kernel("gaussian")`, is the same as its string. For a declaration of
#'   your own function, sigma has the meaning the declaration gives it, the
#'   declaration's `lower` and `upper` bounds apply to sigma (starts, bounds and
#'   parameters outside them are refused, and sigma must be positive), and
#'   `zero_distance = FALSE` refuses a finite zero travel time in the prepared
#'   inputs.
#' @param kappa Positive supply scale used in attractiveness, not in reported
#'   raw supply or capacity attribution.
#' @param beta Positive supply elasticity in `(kappa * supply)^beta`.
#' @param v0 Nonnegative outside-option weight, not an outside probability.
#'   With zero outside weight and zero inside opportunity, the existing guard
#'   returns zero mass; that row is not a normalized choice distribution.
#' @param fit_v0,fit_beta `TRUE` or `FALSE`: whether to promote these constants
#'   to fitted parameters.
#' @param by_group For inputs with demand groups: the parameters that differ
#'   between the groups, a subset of `"sigma"` and `"v0"` (default none: one
#'   sigma and one v0 for all groups). `"v0"` requires `fit_v0 = TRUE`. The
#'   parameters are then named `sigma.<group>` and `v0.<group>`.
#' @param fixed For inputs with demand groups: a named numeric vector of
#'   parameters held fixed at the given values, named like the parameters of the
#'   model (for example `sigma.a` or `v0.b = 0`). They are not fitted and not
#'   part of `theta`.
#' @details With demand groups (see [prepare_allocation()]), group `g` chooses
#'   among the facilities with its own kernel and outside weight, from its own
#'   travel times: `P_ijg = a_j W_g(d_ijg) / (v0_g + sum_k a_k W_g(d_ikg))`. The
#'   allocation, contact (`access`) and outside share of the model are the
#'   demand-weighted pooled values, so facility load, coverage and flows are the
#'   ordinary computations on the total demand. Per-group outputs
#'   (`utilization_by_group`, `access_by_group`, `outside_share_by_group`) are
#'   materialized when requested. With every parameter and the travel shared by
#'   the groups, the model equals the ungrouped model on the total demand.
#'   `family` may be given by group, a named list of kernel names or
#'   declarations with one element per group (sigma is then group specific);
#'   `v0` may be a named numeric vector by group when `fit_v0 = FALSE`.
#' @return The existing private `ae_problem` for corrected static CLM.
#'   Parameter order is sigma, then v0 and beta when their flags are true. With
#'   demand groups sigma and v0 are `sigma.<group>` and `v0.<group>` for the
#'   parameters in `by_group`.
#' @family experimental allocation workflow
#' @export
#' @md
clm_allocation <- function(inputs, family = c("gaussian", "exponential", "power"),
                            kappa = 1, beta = 1, v0 = 0,
                            fit_v0 = FALSE, fit_beta = FALSE,
                            by_group = character(), fixed = NULL) {
  .chck_class(inputs, "ae_prepared_inputs", "inputs")
  if (is.null(inputs$group_ids)) {
    if (length(by_group) || !is.null(fixed)) {
      stop("`by_group` and `fixed` need prepared inputs with demand groups ",
           "(demand given as a named list by group)")
    }
    decay <- .resolve_decay_family(family)
    return(.huff_problem(inputs, family = decay$family, kappa = kappa, beta = beta,
                          v0 = v0, fit_v0 = fit_v0, fit_beta = fit_beta,
                          allocation = "clm", kernel = decay$kernel))
  }
  args <- .clm_group_args(inputs$group_ids, family, v0, fit_v0, fit_beta,
                          by_group, fixed)
  .huff_problem(inputs, family = args$family, kappa = kappa, beta = beta,
                 v0 = args$v0, fit_v0 = fit_v0, fit_beta = fit_beta,
                 allocation = "clm", kernel = args$kernel, grouped = args$grouped)
}

#' Declare the Huff model over checked allocation inputs
#'
#' Declare the Huff model, a conditional logit allocation without an outside
#' option, over checked allocation inputs.
#'
#' The Huff model (Huff, 1963) gives each facility a share of each origin's
#' demand in proportion to its attractiveness times a decay in travel time:
#' `P_ij = a_j W(d_ij) / sum_k a_k W(d_ik)`, where `a_j = (kappa * S_j)^beta`
#' for facility supply `S_j`. Every origin's demand is shared among the
#' facilities, so the shares at an origin add up to 1.
#'
#' This is the conditional logit model of [clm_allocation()] without an outside
#' option: `huff_allocation()` calls `clm_allocation()` with `v0 = 0` and returns
#' the same kind of model, so [fit_allocation()], [evaluate_allocation()],
#' [predict_allocation()], [check_allocation()] and [compare_allocations()] work
#' on it unchanged. Use `clm_allocation(fit_v0 = TRUE)` when part of the demand
#' goes to facilities that are not in the data.
#'
#' With the default power decay, `W(d) = d^(-sigma)`, so spax's `sigma` is
#' Huff's distance exponent and `beta` is his exponent on attractiveness. Both
#' are fitted by default (`fit_beta = TRUE`); with `beta = 1` and
#' `fit_beta = FALSE`, attractiveness is proportional to supply. Power decay is
#' undefined at a travel time of 0, so use a positive minimum travel time or
#' another kernel (`family`).
#'
#' The facility bootstrap [bootstrap_allocation()] needs a fitted outside weight
#' and so does not apply to a Huff model.
#'
#' @inheritParams clm_allocation
#' @param family Travel kernel, as in [clm_allocation()]: `"power"` (the
#'   default, Huff's form), `"gaussian"`, `"exponential"`, a declaration from
#'   [decay_kernel()], or for inputs with demand groups a named list with one
#'   of these per group.
#' @param beta Positive exponent on attractiveness in `(kappa * supply)^beta`:
#'   the value held fixed when `fit_beta = FALSE`.
#' @param fit_beta `TRUE` (the default) fits `beta` as a parameter; `FALSE`
#'   holds it at the given value.
#' @param by_group For inputs with demand groups: `"sigma"` lets the decay
#'   differ by group (default none). `"v0"` is refused, since the Huff model has
#'   no outside option.
#' @param fixed For inputs with demand groups: a named numeric vector of
#'   parameters held fixed, as in [clm_allocation()]. Names of outside weights
#'   (`v0`) are refused.
#' @return The existing private `ae_problem` for corrected static CLM with no
#'   outside option. Parameter order is sigma, then beta when it is fitted. With
#'   demand groups sigma is `sigma.<group>` for the groups in `by_group`.
#' @references
#' Huff DL (1963). A probabilistic analysis of shopping center trade areas.
#' *Land Economics* 39(1):81-90. \doi{10.2307/3144521}
#' @seealso [clm_allocation()] for the general model with an outside option.
#' @family experimental allocation workflow
#' @examples
#' # Mainland Cornwall: small areas share their registered residents among the
#' # practices in proportion to attractiveness times travel-time decay.
#' areas <- sf::st_drop_geometry(cornwall_lsoa[!cornwall_lsoa$scilly, ])
#' gp <- sf::st_drop_geometry(cornwall_gp[!cornwall_gp$scilly, ])
#' travel <- cornwall_travel[areas$lsoa, gp$practice]
#'
#' # Without an outside option, demand is the residents registered at the
#' # mainland practices
#' demand <- stats::setNames(areas$registered_in, areas$lsoa)
#' supply <- stats::setNames(gp$gp_fte_extg, gp$practice)
#' inputs <- prepare_allocation(demand, supply, travel)
#'
#' # Power decay with a fitted attractiveness exponent
#' model <- huff_allocation(inputs)
#'
#' # Evaluate at given parameters: distance exponent 2, attractiveness
#' # exponent 1
#' evaluate_allocation(model, c(sigma = 2, beta = 1))
#'
#' # Fit practice totals of registrations from two starts
#' totals <- tapply(cornwall_flows$patients, cornwall_flows$practice, sum)
#' observed <- stats::setNames(numeric(ncol(travel)), colnames(travel))
#' observed[intersect(names(totals), names(observed))] <-
#'   totals[intersect(names(totals), names(observed))]
#' fit <- fit_allocation(
#'   model, observed,
#'   starts = rbind(a = c(sigma = 1, beta = 0.5), b = c(sigma = 3, beta = 1.5)),
#'   lower = c(sigma = 0.1, beta = 0.05), upper = c(sigma = 10, beta = 5),
#'   loss = "poisson"
#' )
#' fit$best$theta
#' @export
#' @md
huff_allocation <- function(inputs, family = "power", kappa = 1, beta = 1,
                            fit_beta = TRUE, by_group = character(),
                            fixed = NULL) {
  no_outside <- paste0("the Huff model has no outside option; use spec_clm() or ",
                       "clm_allocation(fit_v0 = TRUE) for one")
  if (is.character(by_group) && "v0" %in% by_group) {
    stop(no_outside, call. = FALSE)
  }
  if (!is.null(fixed) && !is.null(names(fixed)) &&
      any(grepl("^v0(\\.|$)", names(fixed)))) {
    stop(no_outside, call. = FALSE)
  }
  clm_allocation(inputs, family = family, kappa = kappa, beta = beta, v0 = 0,
                 fit_v0 = FALSE, fit_beta = fit_beta, by_group = by_group,
                 fixed = fixed)
}

.check_allocation_model <- function(model) {
  .chck_class(model, "ae_problem", "model")
  if (!identical(model$model, "huff") ||
      !identical(model$metadata$spec$allocation, "clm")) {
    stop("the allocation workflow requires a corrected static CLM")
  }
  invisible(model)
}

# Whether an object belongs to a model with demand groups: a model, prepared
# inputs, an equilibrium or a fit (a multistart result by its selected fit). The
# grouped fit and equilibrium objects carry a `groups` marker.
.is_grouped_object <- function(x) {
  if (inherits(x, "ae_multistart_fit")) x <- x$best
  if (is.null(x)) return(FALSE)
  if (inherits(x, "ae_prepared_inputs")) return(!is.null(x$group_ids))
  if (inherits(x, "ae_problem")) {
    return(!is.null(x$metadata$spec[["groups", exact = TRUE]]) ||
             !is.null(x$substrate$group_ids))
  }
  is.list(x) && !is.null(x[["groups", exact = TRUE]])
}

# Stop when any object belongs to a grouped model: the function has no grouped
# form yet (SPAX-057, Phase B1).
.refuse_grouped <- function(fn, ...) {
  if (any(vapply(list(...), .is_grouped_object, logical(1)))) {
    stop(fn, " is not available for grouped models yet", call. = FALSE)
  }
  invisible(TRUE)
}

#' Calibrate a declared allocation model, by default from starts found in the data
#'
#' Count targets and loss choice are separate. Weighted SSE retains the existing
#' default when `starts` are given; Poisson is explicit. Custom loss/gradient
#' functions pass through. Observation-indexed loss arguments must be in
#' canonical order. Starts, boundaries, failed runs and disagreement retain the
#' existing multistart result; a selected fit is not a claim of identification or
#' global optimality.
#'
#' A model whose decay kernel was declared with `smooth = FALSE` (see
#' [decay_kernel()]) is refused, because fitting needs a kernel that is smooth in
#' sigma. Evaluate such a kernel at fixed parameters with [evaluate_allocation()],
#' for example over a grid of sigma and v0.
#'
#' **Default search.** For the count outputs `starts`, `lower` and `upper` are
#' optional. When `starts` is `NULL` they come from the data (`loss` must then be
#' named, because the estimator is not chosen silently):
#' 1. the decay kernel is probed at the nearest, a far and the largest travel
#'    time of the data for the range of sigma over which it still decays over the
#'    travel times, whatever the unit of the travel times;
#' 2. sigma takes 15 log-spaced values over that range, from the steepest to the
#'    flattest kernel (group-specific sigmas walk together);
#' 3. a fitted outside weight `v0` is seeded from the observed outside share,
#'    times 0.1, 1 and 10, and a fitted `beta` starts at 1;
#' 4. the loss is evaluated at every point, by the code the fit itself uses;
#' 5. the fit starts from the best point of up to three sigma values (local
#'    minima along the grid first), named `scan1`, `scan2`, ...
#'
#' The default bounds are the probed range for sigma; for `v0`, from a thousandth
#' of the smallest to a thousand times the largest of the best values on the grid;
#' and 0.05 to 5 for `beta`. Bounds you give replace the default ones one by one,
#' and starts you give widen the default bounds to include them (the scan then
#' runs only to find a default bound of `v0`). The search is recorded in the
#' `default_search` element of the result.
#'
#' When the starts come from the search, `gradient` and `control` default (each
#' unless you give it) to analytic gradients, when the loss has one, and strict
#' optimizer settings, `control = list(maxit = 300, factr = 1e3, pgtol = 1e-8)`,
#' because the optimizer defaults can stop early at a different outside weight.
#' Given `starts` keep the earlier defaults.
#'
#' A model in which every sigma is held (demand groups with `fixed`) has no sigma to
#' probe or scan: the starts are the three `v0` seeds at the held values, or, with
#' only `beta` free, `beta` = 0.5, 1 and 2.
#'
#' A fully observed target whose counts reach the total demand has no outside
#' share for `v0` to explain: the search stops with an error (use no outside
#' option: `fit_v0 = FALSE`), and
#' warns when the counts are within 0.1% of the total demand. Starts and bounds
#' remain required for any output other than `utilization`, `flow` and
#' `utilization_by_group`, and calls that give `starts`, `lower` and `upper` run
#' exactly as before.
#'
#' The cost is about one solve of the model per grid point, 15 to 45 solves: about
#' a second on the 336 by 54 Cornwall example, about 24 seconds for 20,000 origins
#' and 400 facilities, which is roughly the cost of one strict start. Give your own
#' starts to skip it.
#' @param model A corrected CLM declared over checked prepared inputs. A kernel
#'   declared `smooth = FALSE` cannot be fitted.
#' @param observed Numeric vector or origin-by-facility matrix. Named axes match
#'   declared IDs independently; unnamed axes are positional. Keep all IDs and
#'   use NA for unobserved entries. The count-valued `utilization`, `flow` and
#'   `utilization_by_group` targets reject negative nonmissing observations;
#'   fractional counts are allowed. Other output domains pass through to the
#'   existing fitter. For `utilization_by_group` (a model with demand groups) a
#'   facility-by-group matrix of observed counts with both axes named (group
#'   columns by group ID, facility rows by facility ID, any order).
#' @param starts `NULL` (default) for the default search, or a finite positive
#'   matrix, one explicit independent start per row, with exactly the named model
#'   parameter columns. No random starts are added. Each start must lie inside the
#'   bounds. Given starts with `lower` or `upper` missing or partial use default
#'   bounds for the missing entries, widened to include every start.
#' @param lower,upper `NULL` (default), or named positive parameter bounds with
#'   lower < upper: complete, or for some of the parameters only (the default
#'   bounds replace the missing entries). For a declared kernel, the starts and
#'   bounds of sigma must lie within the bounds of the declaration.
#' @param output Exact model output to calibrate; default facility utilization.
#'   `flow` is expected origin-facility counts, while `allocation` is an inside
#'   probability operator. Requesting counts does not select a likelihood.
#'   `utilization_by_group` is the expected count of each group at each facility
#'   (a model with demand groups, see [clm_allocation()]). The group-specific
#'   parameters of such a model (`sigma.<group>`, `v0.<group>`, from
#'   `by_group`) can be fitted only to this output, with observed counts for
#'   every group that has one and positive demand in that group; aggregated
#'   targets leave them multimodal and weakly identified, so the fit is refused
#'   (before any start) with a message naming the parameters. Hold them
#'   (`clm_allocation(fixed = )`, or numbers in the decay and outside slots of
#'   [spec_clm()]), or fit shared parameters to any target.
#' @param loss `"weighted_sse"`, `"poisson"`, or a function accepting predicted
#'   and observed values. Weighted SSE divides squared residuals by observed+eta,
#'   with eta=1 by default. Poisson omits the data-only log-factorial constant.
#'   It floors modelled counts at `eps` (`loss_args`), 1e-250 by default, only
#'   to avoid the log of zero; cells with positive observed counts modelled
#'   below the floor are reported with a warning, because the loss is capped
#'   there. Fits made before this default used 1e-9; pass
#'   `loss_args = list(eps = 1e-9)` to reproduce them. Joint fitting does not
#'   force total matching. The default is `"weighted_sse"` when `starts` are
#'   given; without `starts` the loss must be named.
#' @param loss_args Named auxiliary arguments passed to loss callbacks. For
#'   `loss = "poisson"` an `eps` of 1e-250 is added when none is given; other
#'   arguments pass unchanged. Any observation-indexed values must already
#'   follow canonical observed order.
#' @param loss_grad Optional matching parameter-gradient callback. Built-in
#'   choices supply their existing analytic loss-gradient adapters. A custom
#'   loss requires its own loss_grad when gradient=TRUE; none is invented.
#' @param gradient Logical; use the existing implicit sensitivity route when
#'   TRUE. FALSE retains the underlying fitter's default behavior. When `starts`
#'   is `NULL` and `gradient` is not given, it is `TRUE` if the loss has a gradient
#'   (the built-in losses, or a custom loss with `loss_grad`).
#' @param ... Named multistart or fitting controls, including solver tolerances,
#'   optimizer `control`, `keep_fits` and descriptive disagreement tolerances.
#'   When `starts` is `NULL` and `control` is not given it is
#'   `list(maxit = 300, factr = 1e3, pgtol = 1e-8)`.
#' @return The existing `ae_multistart_fit`, retaining selected fit, all per-start
#'   diagnostics, boundaries, warnings/errors and descriptive disagreement. A fit
#'   whose starts came from the default search also has a `default_search`
#'   element: the rule, the sigma range, the number of grid points and of finite
#'   ones, the grid of sigma, v0 and loss, the starts, the bounds, the controls and
#'   the loss.
#' @seealso [allocation-fit-methods] for methods on saved fits.
#' @family experimental allocation workflow
#' @export
#' @md
fit_allocation <- function(model, observed, starts = NULL, lower = NULL, upper = NULL,
                            output = "utilization", loss = "weighted_sse",
                            loss_args = list(), loss_grad = NULL,
                            gradient = FALSE, ...) {
  loss_missing <- missing(loss)
  gradient_missing <- missing(gradient)
  .check_allocation_model(model)
  for (kernel in .allocation_kernels(model$metadata$spec)) {
    if (isFALSE(kernel$smooth)) {
      stop("a kernel declared smooth = FALSE cannot be fitted; evaluate it at fixed ",
           "parameters, for example over a grid of sigma and v0")
    }
  }
  if (is.character(output) && length(output) == 1L && !is.na(output) &&
      output %in% c("utilization", "flow", "utilization_by_group") && is.numeric(observed) &&
      any(observed < 0, na.rm = TRUE)) {
    stop("observed expected counts must be nonnegative for output '", output, "'")
  }
  # Starts and bounds that are not given come from the default search (SPAX-058).
  expected <- model$theta$names
  searching <- is.null(starts) || !.default_bounds_cover(lower, expected) ||
    !.default_bounds_cover(upper, expected)
  if (searching) {
    counts <- is.character(output) && length(output) == 1L && !is.na(output) &&
      output %in% .default_count_outputs &&
      output %in% names(model$metadata$spec$output_axes)
    if (!counts) {
      stop("give starts, lower and upper for output '", paste(output, collapse = ", "),
           "'")
    }
    if (is.null(starts) && loss_missing) stop(.default_search_message$loss)
  }
  loss_name <- if (is.character(loss)) match.arg(loss, c("weighted_sse", "poisson")) else "custom"
  if (is.character(loss)) {
    loss <- match.arg(loss, c("weighted_sse", "poisson"))
    if (loss == "poisson" && is.list(loss_args) && !"eps" %in% names(loss_args)) {
      loss_args$eps <- 1e-250
    }
    default_gradient <- if (loss == "poisson") .poisson_gradient else .weighted_sse_gradient
    loss <- if (loss == "poisson") .poisson_loss else .weighted_sse_loss
    if (is.null(loss_grad)) loss_grad <- default_gradient
  } else if (!is.function(loss)) {
    stop("loss must be 'weighted_sse', 'poisson' or a function")
  }
  if (!searching) {
    result <- .fit_problem_multistart(model, observed, starts, lower, upper,
      output = output, loss = loss, loss_args = loss_args, loss_grad = loss_grad,
      gradient = gradient, ...)
  } else {
    dots <- list(...)
    target <- .bind_problem_target(model, observed, output)
    search <- .allocation_default_search(model, target, output, loss, loss_args,
      lower = lower, upper = upper, starts = starts,
      solver_args = dots[names(dots) %in% .default_solver_args])
    starts_searched <- is.null(starts)
    # The gradient and strict optimizer controls come with the searched starts only,
    # and only where the caller gave none; given starts keep today's defaults.
    control_given <- "control" %in% names(dots)
    if (starts_searched) {
      if (gradient_missing) gradient <- is.function(loss_grad)
      if (!control_given) dots$control <- .default_search_control
    }
    if (starts_searched) starts <- search$starts
    result <- do.call(.fit_problem_multistart, c(list(model, observed, starts,
      search$lower, search$upper, output = output, loss = loss, loss_args = loss_args,
      loss_grad = loss_grad, gradient = gradient), dots))
    if (starts_searched && inherits(result, "ae_multistart_fit")) {
      record <- search$record
      record$loss <- loss_name
      record$controls <- list(gradient = isTRUE(gradient), gradient_default = gradient_missing,
                              control = dots$control, control_default = !control_given)
      result$default_search <- record
    }
  }
  # A binding floor caps the loss: report it rather than return a capped fit silently.
  if (inherits(result, "ae_multistart_fit") && !is.null(result$best)) {
    floor_info <- .poisson_floor(result$best)
    if (!is.null(floor_info) && floor_info[["positive_below"]] > 0) {
      warning(.poisson_floor_message(floor_info), call. = FALSE)
    }
  }
  result
}

.selected_allocation_fit <- function(object) {
  if (inherits(object, "ae_multistart_fit")) {
    if (is.null(object$best)) {
      stop("no eligible allocation fit; inspect per-start diagnostics and messages")
    }
    object <- object$best
  }
  object
}

#' Inspect saved allocation fits without recomputation
#'
#' Coefficients and fitted targets are read from the selected fit. Multistart
#' summaries retain failed starts, boundaries, warnings/errors and descriptive
#' disagreement. A search with no eligible fit remains inspectable by summary,
#' while coefficient and fitted extraction fail clearly.
#' @param object An existing NFXP fit or multistart result from allocation fitting.
#' @param ... Additional generic arguments; currently unused.
#' @return `coef()` returns the saved named parameter vector. `fitted()` returns
#'   saved canonical fitted targets. `summary()` returns a list containing the
#'   selected fit summary, per-start table, boundaries, disagreement and messages.
#'   These diagnostics establish neither identification nor global optimality.
#' @name allocation-fit-methods
#' @seealso [fit_allocation()]
#' @exportS3Method stats::coef
#' @md
coef.ae_problem_nfxp_fit <- function(object, ...) {
  .chck_class(object, "ae_problem_nfxp_fit", "object")
  object$theta
}

#' @rdname allocation-fit-methods
#' @exportS3Method stats::coef
#' @md
coef.ae_multistart_fit <- function(object, ...) {
  .chck_class(object, "ae_multistart_fit", "object")
  coef.ae_problem_nfxp_fit(.selected_allocation_fit(object))
}

#' @rdname allocation-fit-methods
#' @exportS3Method stats::fitted
#' @md
fitted.ae_problem_nfxp_fit <- function(object, ...) {
  .chck_class(object, "ae_problem_nfxp_fit", "object")
  object$predicted
}

#' @rdname allocation-fit-methods
#' @exportS3Method stats::fitted
#' @md
fitted.ae_multistart_fit <- function(object, ...) {
  .chck_class(object, "ae_multistart_fit", "object")
  fitted.ae_problem_nfxp_fit(.selected_allocation_fit(object))
}

#' @rdname allocation-fit-methods
#' @exportS3Method base::summary
#' @md
summary.ae_multistart_fit <- function(object, ...) {
  .chck_class(object, "ae_multistart_fit", "object")
  structure(list(status = object$status, selected_start = object$best_start,
       selected = if (!is.null(object$best)) summary(object$best) else NULL,
       starts = object$table, boundary = object$boundary,
       disagreement = list(loss = object$loss_disagreement,
                           parameter = object$parameter_disagreement,
                           tolerances = object$tolerances),
       warnings = object$warnings, errors = object$errors),
    class = "summary.ae_multistart_fit")
}

#' Evaluate a declared allocation model at supplied fixed parameters
#'
#' Solves once and constructs compact coverage from the saved evaluated outputs.
#' The existing equilibrium result carries its prediction snapshot;
#' adding coverage does not retain the original model or create a scenario type.
#' The caller changes system supply/demand by preparing a new model explicitly.
#' @param model A corrected CLM declared over checked prepared inputs.
#' @param theta A complete model parameter vector. Named parameters align to the
#'   model contract; unnamed values follow its declared order. This operation
#'   estimates no parameters and is not a causal intervention estimator. For a
#'   model with demand groups, a parameter held fixed by `clm_allocation(fixed
#'   = )` may be given only with its fixed value, and unknown names are an error.
#' @param coverage_args Named reporting controls for compact coverage, including
#'   optional `norm`, `units`, reporting floors and classification cuts. A norm
#'   must have the same declared supply-per-demand scale as capacity attribution.
#' @param ... Named controls passed to the existing fixed-parameter solver,
#'   including `requested_outputs`. For a model with demand groups, request the
#'   per-group outputs here, for example
#'   `requested_outputs = c("utilization_by_group", "access_by_group",
#'   "outside_share_by_group")`; the other outputs are pooled over the groups
#'   (demand-weighted). A group's own access and outside share are `NA` on rows
#'   where that group has no demand and its travel there is not usable.
#' @return The existing `ae_equilibrium` with its portable prediction snapshot
#'   (none for a model with demand groups, which cannot be predicted yet)
#'   and compact `coverage`. Coverage uses raw supply units without kappa;
#'   proportional attributed capacity is not a delivered-service measure or a
#'   hard constraint on expected utilization.
#' @family experimental allocation workflow
#' @export
#' @md
evaluate_allocation <- function(model, theta, coverage_args = list(), ...) {
  .check_allocation_model(model)
  if (!is.list(coverage_args) || (length(coverage_args) &&
      (is.null(names(coverage_args)) || anyNA(names(coverage_args)) ||
       any(!nzchar(names(coverage_args))) || anyDuplicated(names(coverage_args)) ||
       any(!names(coverage_args) %in% setdiff(names(formals(.ae_coverage)), "fit"))))) {
    stop("coverage_args must contain unique named coverage controls")
  }
  if (!is.null(model$metadata$spec[["groups", exact = TRUE]])) {
    theta <- .group_evaluation_theta(model, theta)
  }
  theta <- .coerce_problem_theta(theta, model$theta)
  result <- .solve_problem(model, theta, ...)
  if (!isTRUE(result$converged)) stop("allocation evaluation did not converge")
  result$coverage <- do.call(.coverage_from_outputs,
    c(list(problem = model, theta = theta, outputs = result$outputs), coverage_args))
  result
}

#' Predict requested origins against a saved fitted or evaluated CLM
#'
#' Numeric output preserves the existing prediction result and its separate probability,
#' demand and capacity validity. Raster conversion is explicit and permits one
#' vector only; inspect numeric validity before interpreting a map. Supply/theta
#' overrides are deliberately absent. A changed system requires reevaluation.
#' @param source A selected multistart result, NFXP fit or fixed evaluation with
#'   a valid CLM prediction snapshot, for a built-in decay kernel or one declared
#'   with [decay_kernel()]. Older objects need regeneration. The snapshot of a
#'   declared kernel carries its code and predicting runs it, so use saved
#'   results only from sources you trust.
#' @param distance Numeric requested-origin-by-facility travel matrix with
#'   complete facility column IDs. Named axes align independently; requested
#'   origin IDs may define a new domain outside the fitted load support.
#'   Values must use the saved travel unit; no unit conversion is performed.
#' @param origin_ids Complete unique nonblank requested origin IDs, defaulting
#'   to matrix row names. Returned rows follow this declared order.
#' @param demand Optional requested numeric demand, independently aligned by
#'   names or positional in request order. Zero is known zero; missing or
#'   omitted values stay unknown. It multiplies requested flow only and never
#'   changes saved system utilization. Use the saved demand unit and reference
#'   period; returned metadata is inherited without checking or converting units.
#' @param outputs Exact requested vectors or matrices. Vectors include rho,
#'   outside_share, A, abar, unsupported_contact, A_supported and abar_supported;
#'   allocation and flow are optional matrices. rho/outside are probabilities;
#'   flow has demand count units/period. A and abar have supply-per-demand units.
#' @param missing_travel Default `"unknown"` makes a normalized row unknown when
#'   any alternative's travel is unknown. Explicit `"legacy_zero"` preserves its
#'   imputation flags; positive infinity remains known absence, while negative
#'   travel and invalid finite kernel domains are rejected.
#' @param spatial Optional one-layer raster template plus separately named
#'   `cell_index` mapping for requested IDs, as in preparation. Saved metadata
#'   contains plain geometry. Without a mapping, numeric prediction still works.
#' @param type `"numeric"` retains selected values, all separate validity flags,
#'   policies and unit declarations. `"raster"` explicitly returns one vector
#'   map, so inspect the numeric validity result before interpreting the map.
#' @return An `ae_clm_prediction`, or an explicitly requested one-layer raster.
#'   Contact to positive-supply/zero-load facilities preserves valid probabilities
#'   but makes unqualified A/abar unknown; explicitly named supported-only outputs
#'   retain the existing zero-contribution convention. Zero-denominator rows
#'   retain guarded zero mass and a false probability-valid flag.
#' @family experimental allocation workflow
#' @export
#' @md
predict_allocation <- function(source, distance, origin_ids = rownames(distance),
                                demand = NULL, outputs = c("rho", "A", "abar"),
                                missing_travel = c("unknown", "legacy_zero"),
                                spatial = NULL, type = c("numeric", "raster")) {
  type <- match.arg(type)
  .refuse_grouped("predict_allocation()", source)
  if (type == "raster" && (length(outputs) != 1L ||
      !is.character(outputs) || is.na(outputs) || outputs %in% c("allocation", "flow"))) {
    stop("raster prediction requires exactly one requested vector output")
  }
  source <- .selected_allocation_fit(source)
  result <- .predict_clm(source, distance, origin_ids = origin_ids, demand = demand,
    outputs = outputs, missing_travel = match.arg(missing_travel), spatial = spatial)
  if (type == "raster") .prediction_surface(result, outputs) else result
}

# The theta of evaluate_allocation() for a grouped model: the free parameters,
# plus fixed parameters only at their fixed value; any other name is an error.
# An unnamed vector follows the free parameters and is left to the contract.
.group_evaluation_theta <- function(model, theta) {
  spec <- model$metadata$spec
  .chck_numeric_vector(theta, "theta")
  nms <- names(theta)
  if (is.null(nms) || all(nms == "")) return(theta)
  if (anyNA(nms) || any(nms == "") || anyDuplicated(nms)) {
    stop("`theta` names must be unique and nonempty")
  }
  fixed <- spec$fixed
  unknown <- setdiff(nms, c(model$theta$names, names(fixed)))
  if (length(unknown)) {
    stop("unknown parameter(s) in `theta`: ", paste(unknown, collapse = ", "),
         "; the parameters are ", paste(model$theta$names, collapse = ", "),
         call. = FALSE)
  }
  for (name in intersect(nms, names(fixed))) {
    value <- as.numeric(theta[[name]])
    if (!isTRUE(abs(value - fixed[[name]]) <= 1e-12 * max(1, abs(fixed[[name]])))) {
      stop("parameter ", name, " is fixed at ", format(fixed[[name]]), call. = FALSE)
    }
  }
  theta[nms %in% model$theta$names]
}
