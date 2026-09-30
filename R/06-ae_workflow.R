# Experimental portable static-CLM workflow ---------------------------------

#' Prepare checked numeric inputs for an experimental allocation workflow
#'
#' Named axes align independently; unnamed axes are positional. Positive
#' included demand enters facility-load support. Zero, unknown and explicitly
#' excluded origins retain separate support statuses. This experimental entry
#' point delegates to the existing compact preparation boundary.
#' @param demand Numeric origin vector, finite and nonnegative or explicitly
#'   permitted missing values. Negative or infinite demand is rejected even on
#'   excluded origins. Named demand matches canonical origin IDs.
#' @param supply Finite nonnegative facility vector. Known zero supply is
#'   retained; unknown supply is rejected. Names match canonical facility IDs.
#' @param distance Numeric origin-by-facility travel matrix. Complete named
#'   rows and columns match IDs independently; unnamed axes are positional.
#' @param origin_ids,facility_ids Complete unique nonblank canonical IDs. Their
#'   order determines prepared order. Defaults require named demand and travel
#'   columns; identities are never inferred from equal axis lengths.
#' @param included Nonmissing logical origin mask. Names align independently;
#'   an unnamed mask follows supplied demand order and is reordered with demand.
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
#'   proportional capacity attribution uses supply per unit of demand.
#' @family experimental allocation workflow
#' @export
prepare_allocation <- function(demand, supply, distance,
                               origin_ids = names(demand),
                               facility_ids = colnames(distance),
                               included = rep(TRUE, length(demand)),
                               missing_demand = c("error", "exclude"),
                               missing_travel = c("error", "legacy_zero"),
                               spatial = NULL, metadata = list()) {
  .prepare_allocation(demand, supply, distance, origin_ids = origin_ids,
    facility_ids = facility_ids, included = included, missing_demand = missing_demand,
    missing_travel = missing_travel, spatial = spatial, metadata = metadata)
}

#' Declare corrected static CLM over checked allocation inputs
#'
#' @param inputs An `ae_prepared_inputs` object from checked preparation.
#' @param family Travel kernel: Gaussian, exponential or power. Gaussian sigma
#'   is a travel scale, exponential sigma a rate, and power sigma an exponent.
#'   Power requires strictly positive finite travel distances.
#' @param kappa Positive supply scale used in attractiveness, not in reported
#'   raw supply or capacity attribution.
#' @param beta Positive supply elasticity in `(kappa * supply)^beta`.
#' @param v0 Nonnegative outside-option weight, not an outside probability.
#'   With zero outside weight and zero inside opportunity, the existing guard
#'   returns zero mass; that row is not a normalized choice distribution.
#' @param fit_v0,fit_beta Logical flags promoting these constants to parameters.
#' @return The existing private `ae_problem` for built-in corrected static CLM.
#'   Parameter order is sigma, then v0 and beta when their flags are true.
#' @family experimental allocation workflow
#' @export
clm_allocation <- function(inputs, family = c("gaussian", "exponential", "power"),
                            kappa = 1, beta = 1, v0 = 0,
                            fit_v0 = FALSE, fit_beta = FALSE) {
  .chck_class(inputs, "ae_prepared_inputs", "inputs")
  .huff_problem(inputs, family = match.arg(family), kappa = kappa, beta = beta,
                 v0 = v0, fit_v0 = fit_v0, fit_beta = fit_beta, allocation = "clm")
}

.check_allocation_model <- function(model) {
  .chck_class(model, "ae_problem", "model")
  if (!identical(model$model, "huff") ||
      !identical(model$metadata$spec$allocation, "clm")) {
    stop("the allocation workflow requires a built-in corrected static CLM")
  }
  invisible(model)
}

#' Calibrate a declared allocation model from explicit starting points
#'
#' Count targets and loss choice are separate. Weighted SSE retains the existing
#' default; Poisson is explicit. Custom loss/gradient functions pass through.
#' Observation-indexed loss arguments must be in canonical order. Starts,
#' boundaries, failed runs and disagreement retain the existing multistart
#' result; a selected fit is not a claim of identification or global optimality.
#' @param model A corrected CLM declared over checked prepared inputs.
#' @param observed Numeric vector or origin-by-facility matrix. Named axes match
#'   declared IDs independently; unnamed axes are positional. Keep all IDs and
#'   use NA for unobserved entries. The count-valued `utilization` and `flow`
#'   targets reject negative nonmissing observations; fractional counts are
#'   allowed. Other output domains pass through to the existing fitter.
#' @param starts A finite positive matrix, one explicit independent start per
#'   row, with exactly the named model parameter columns. No random starts are
#'   added. Each start must lie inside the supplied positive bounds.
#' @param lower,upper Complete named positive parameter bounds with lower < upper.
#' @param output Exact model output to calibrate; default facility utilization.
#'   `flow` is expected origin-facility counts, while `allocation` is an inside
#'   probability operator. Requesting counts does not select a likelihood.
#' @param loss `"weighted_sse"`, `"poisson"`, or a function accepting predicted
#'   and observed values. Weighted SSE divides squared residuals by observed+eta,
#'   with eta=1 by default. Poisson omits the data-only log-factorial constant
#'   and uses eps=1e-9 by default; joint fitting does not force total matching.
#' @param loss_args Named auxiliary arguments passed unchanged to loss callbacks.
#'   Any observation-indexed values must already follow canonical observed order.
#' @param loss_grad Optional matching parameter-gradient callback. Built-in
#'   choices supply their existing analytic loss-gradient adapters. A custom
#'   loss requires its own loss_grad when gradient=TRUE; none is invented.
#' @param gradient Logical; use the existing implicit sensitivity route when
#'   TRUE. FALSE retains the underlying fitter's default behavior.
#' @param ... Named multistart or fitting controls, including solver tolerances,
#'   optimizer `control`, `keep_fits` and descriptive disagreement tolerances.
#' @return The existing `ae_multistart_fit`, retaining selected fit, all per-start
#'   diagnostics, boundaries, warnings/errors and descriptive disagreement.
#' @family experimental allocation workflow
#' @export
fit_allocation <- function(model, observed, starts, lower, upper,
                            output = "utilization", loss = "weighted_sse",
                            loss_args = list(), loss_grad = NULL,
                            gradient = FALSE, ...) {
  .check_allocation_model(model)
  if (is.character(output) && length(output) == 1L && !is.na(output) &&
      output %in% c("utilization", "flow") && is.numeric(observed) &&
      any(observed < 0, na.rm = TRUE)) {
    stop("observed expected counts must be nonnegative for output '", output, "'")
  }
  if (is.character(loss)) {
    loss <- match.arg(loss, c("weighted_sse", "poisson"))
    default_gradient <- if (loss == "poisson") .poisson_gradient else .weighted_sse_gradient
    loss <- if (loss == "poisson") .poisson_loss else .weighted_sse_loss
    if (is.null(loss_grad)) loss_grad <- default_gradient
  } else if (!is.function(loss)) {
    stop("loss must be 'weighted_sse', 'poisson' or a function")
  }
  .fit_problem_multistart(model, observed, starts, lower, upper,
    output = output, loss = loss, loss_args = loss_args, loss_grad = loss_grad,
    gradient = gradient, ...)
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
#' @exportS3Method stats::coef
coef.ae_problem_nfxp_fit <- function(object, ...) {
  .chck_class(object, "ae_problem_nfxp_fit", "object")
  object$theta
}

#' @rdname allocation-fit-methods
#' @exportS3Method stats::coef
coef.ae_multistart_fit <- function(object, ...) {
  .chck_class(object, "ae_multistart_fit", "object")
  coef.ae_problem_nfxp_fit(.selected_allocation_fit(object))
}

#' @rdname allocation-fit-methods
#' @exportS3Method stats::fitted
fitted.ae_problem_nfxp_fit <- function(object, ...) {
  .chck_class(object, "ae_problem_nfxp_fit", "object")
  object$predicted
}

#' @rdname allocation-fit-methods
#' @exportS3Method stats::fitted
fitted.ae_multistart_fit <- function(object, ...) {
  .chck_class(object, "ae_multistart_fit", "object")
  fitted.ae_problem_nfxp_fit(.selected_allocation_fit(object))
}

#' @rdname allocation-fit-methods
#' @exportS3Method base::summary
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
#' The existing equilibrium result carries its SPAX-039 prediction snapshot;
#' adding coverage does not retain the original model or create a scenario type.
#' The caller changes system supply/demand by preparing a new model explicitly.
#' @param model A corrected CLM declared over checked prepared inputs.
#' @param theta A complete model parameter vector. Named parameters align to the
#'   model contract; unnamed values follow its declared order. This operation
#'   estimates no parameters and is not a causal intervention estimator.
#' @param coverage_args Named reporting controls for compact coverage, including
#'   optional `norm`, `units`, reporting floors and classification cuts. A norm
#'   must have the same declared supply-per-demand scale as capacity attribution.
#' @param ... Named controls passed to the existing fixed-parameter solver.
#' @return The existing `ae_equilibrium` with its portable prediction snapshot
#'   and compact `coverage`. Coverage uses raw supply units without kappa;
#'   proportional attributed capacity is not a delivered-service measure or a
#'   hard constraint on expected utilization.
#' @family experimental allocation workflow
#' @export
evaluate_allocation <- function(model, theta, coverage_args = list(), ...) {
  .check_allocation_model(model)
  if (!is.list(coverage_args) || (length(coverage_args) &&
      (is.null(names(coverage_args)) || anyNA(names(coverage_args)) ||
       any(!nzchar(names(coverage_args))) || anyDuplicated(names(coverage_args)) ||
       any(!names(coverage_args) %in% setdiff(names(formals(.ae_coverage)), "fit"))))) {
    stop("coverage_args must contain unique named coverage controls")
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
#' Numeric output preserves the SPAX-039 result and its separate probability,
#' demand and capacity validity. Raster conversion is explicit and permits one
#' vector only; inspect numeric validity before interpreting a map. Supply/theta
#' overrides are deliberately absent. A changed system requires reevaluation.
#' @param source A selected multistart result, NFXP fit or fixed evaluation with
#'   a valid built-in CLM prediction snapshot. Older objects need regeneration.
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
predict_allocation <- function(source, distance, origin_ids = rownames(distance),
                                demand = NULL, outputs = c("rho", "A", "abar"),
                                missing_travel = c("unknown", "legacy_zero"),
                                spatial = NULL, type = c("numeric", "raster")) {
  type <- match.arg(type)
  if (type == "raster" && (length(outputs) != 1L ||
      !is.character(outputs) || is.na(outputs) || outputs %in% c("allocation", "flow"))) {
    stop("raster prediction requires exactly one requested vector output")
  }
  source <- .selected_allocation_fit(source)
  result <- .predict_clm(source, distance, origin_ids = origin_ids, demand = demand,
    outputs = outputs, missing_travel = match.arg(missing_travel), spatial = spatial)
  if (type == "raster") .prediction_surface(result, outputs) else result
}
