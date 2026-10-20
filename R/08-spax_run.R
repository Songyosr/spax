# Running an allocation model: spax(), the one-shots and their results (SPAX-058, T5) ----

#' Run an allocation model on prepared data
#'
#' Experimental. `spax()` binds a model specification ([spec_huff()],
#' [spec_clm()]) to prepared data ([prep_data()]) and either fits the parameters
#' that are `"fit"` to observed counts ([obs_flows()], [obs_totals()]) or, when
#' every parameter has a value, evaluates the model at those values.
#' `spax_huff()` and `spax_clm()` do the same from raw inputs in one call.
#'
#' **Fit mode.** Some parameter is `"fit"`. `observed` is needed. The counts
#' are aligned with the data (see [obs_flows()]), the model is bound to the
#' data and [fit_allocation()] fits it. The loss is set by the observation and
#' the output it is compared with by its kind, so `output`, `loss` and
#' `loss_args` are refused in `...`. Starts, bounds and optimizer settings come
#' from the data when they are not given: `starts`, `lower` and `upper` are
#' optional (see the default search in [fit_allocation()]). `spax()` always
#' passes analytic gradients and the strict optimizer settings
#' `control = list(maxit = 300, factr = 1e3, pgtol = 1e-8)`, also when you give
#' `starts`, unless `...` gives `gradient` or `control` itself. The result does
#' not solve the model again: it reads the selected fit. A search in which no
#' start converged gives a warning and a result with no estimates;
#' `summary()` shows the starts and `as.data.frame(type = "starts")` their table.
#'
#' **Evaluate mode.** Every parameter has a number. Nothing is fitted, so
#' `observed`, `starts`, `lower` and `upper` are refused, and the model is
#' evaluated once at the given values. In this version an ungrouped model with
#' something to fit cannot hold its decay `sigma`; give the decay
#' `sigma = "fit"`, or give `beta` and the outside weight too to evaluate.
#'
#' **Observed.** Besides [obs_flows()] and [obs_totals()], `observed` can be a
#' named numeric vector (facility totals) or a matrix, read as flows when the
#' row names are origins and the column names facilities of the data, and as
#' totals by group when the column names are the groups of the data.
#'
#' **What a result holds.** A `spax_allocation` stores the bound model, the
#' specification, the observations as aligned to the data and the fit, or the
#' evaluation. `coef()` gives the free parameters (fitted or given) and the
#' held values are in `$held` and in `as.data.frame(x)`. Outputs that the fit
#' did not compute, for instance the flows of a fit to facility totals, are
#' computed when asked for and not stored. The step functions
#' ([predict_allocation()], [check_allocation()], [compare_allocations()],
#' [bootstrap_allocation()]) accept the result itself: each takes the bound model
#' and the fit (or, for prediction, the evaluation) from it. Use [saveRDS()] to
#' save a result.
#'
#' @param data Prepared inputs: the result of [prep_data()].
#' @param model A model specification: [spec_huff()] or [spec_clm()].
#' @param observed Observed counts for a model with a parameter to fit: an
#'   [obs_flows()] or [obs_totals()] object, or raw counts (see Details).
#'   `NULL` (the default) for a model with a value for every parameter.
#' @param starts,lower,upper Optional starting values and bounds for the
#'   parameters being fitted, as in [fit_allocation()]. They are refused when
#'   nothing is fitted.
#' @param ... Fit mode: further arguments of [fit_allocation()] (`gradient`,
#'   `control`, `lambda`, `tol`, `max_iter`, `norm`, `keep_fits`, ...); `output`,
#'   `loss` and `loss_args` are refused. Evaluate mode: further arguments of
#'   [evaluate_allocation()], for example `requested_outputs`.
#' @return An object of class `c("spax_allocation", "spax_result")`: a list
#'   with `model` (the bound `ae_problem`), `spec`, `observed` (a list with
#'   `spec`, the observation as declared, `target`, the counts aligned to the
#'   data, and `output`; `NULL` in evaluate mode), `fit` (the
#'   `ae_multistart_fit`, in fit mode), `evaluation` (the `ae_equilibrium`, in
#'   evaluate mode), `theta` (the named free parameters, fitted or given; `NULL`
#'   when no start was selected), `held` (the named held values, including
#'   `kappa`), `mode` (`"fit"` or `"evaluate"`) and `call`. See
#'   [spax_allocation-methods] for what to do with it.
#' @seealso [spax_huff()] and [spax_clm()] for one call from raw inputs.
#' @examples
#' # Mainland Cornwall: registered residents, GP FTE and travel minutes
#' areas <- sf::st_drop_geometry(cornwall_lsoa[!cornwall_lsoa$scilly, ])
#' gp <- sf::st_drop_geometry(cornwall_gp[!cornwall_gp$scilly, ])
#' travel <- cornwall_travel[areas$lsoa, gp$practice]
#' supply <- stats::setNames(gp$gp_fte_extg, gp$practice)
#'
#' # Huff: power decay and a fitted attractiveness exponent, fitted to the flows
#' # of residents to their practices. Demand is the residents registered at the
#' # mainland practices, since the Huff model has no outside option.
#' d_huff <- prep_data(stats::setNames(areas$registered_in, areas$lsoa), supply, travel)
#' flows <- obs_flows(cornwall_flows, origin = "lsoa", facility = "practice",
#'                    count = "patients")
#' huff <- spax(d_huff, spec_huff(), observed = flows)
#' huff
#' coef(huff)
#' head(as.data.frame(huff, type = "facilities"))
#'
#' # Conditional logit with a fitted outside weight, fitted to practice totals
#' d <- prep_data(stats::setNames(areas$registered, areas$lsoa), supply, travel)
#' totals <- obs_totals(tapply(cornwall_flows$patients, cornwall_flows$practice, sum))
#' clm <- spax(d, spec_clm(decay_gaussian(), outside = "fit"), observed = totals)
#' summary(clm)
#'
#' # Nothing to fit: evaluate at given values, with no observations
#' spax(d_huff, spec_huff(decay_power(sigma = 1.5), beta = 1))
#' @family allocation interface
#' @export
#' @md
spax <- function(data, model, observed = NULL, starts = NULL, lower = NULL, upper = NULL,
                 ...) {
  call <- match.call()
  if (!inherits(data, "ae_prepared_inputs")) {
    stop("data must come from prep_data()", call. = FALSE)
  }
  if (!inherits(model, "spax_spec")) {
    stop("model must be a model specification such as spec_huff() or spec_clm()",
         call. = FALSE)
  }
  dots <- list(...)
  if (any(c("output", "loss", "loss_args") %in% names(dots))) {
    stop("the loss is set in obs_flows() or obs_totals() (loss = ), and the output by ",
         "the kind of observation", call. = FALSE)
  }
  bound <- .bind_spax_spec(model, data)
  problem <- bound$problem
  obs <- NULL
  target <- NULL
  fit <- NULL
  evaluation <- NULL

  if (identical(bound$mode, "fit")) {
    if (is.null(observed)) {
      stop("observed counts are needed to fit ", paste(problem$theta$names, collapse = ", "),
           "; or give their values", call. = FALSE)
    }
    obs <- .as_spax_obs(observed, data)
    target <- .canonical_observed(obs, data, outside_option = .spec_has_outside(model))
    # The strict controls apply whether or not starts are given, each unless the
    # caller gives it. The losses of an observation are the built-in ones, which
    # have analytic gradients.
    if (!"gradient" %in% names(dots)) dots$gradient <- TRUE
    if (!"control" %in% names(dots)) dots$control <- .default_search_control
    loss <- obs$loss
    loss_args <- obs$loss_args
    output <- obs$type
    fit_call <- as.call(c(list(quote(fit_allocation), quote(problem), quote(target),
                               quote(starts), quote(lower), quote(upper),
                               output = quote(output), loss = quote(loss),
                               loss_args = quote(loss_args)), dots))
    fit <- eval(fit_call)
    # No second solve: the result reads the selected fit.
    theta <- if (is.null(fit$best)) {
      warning("no start converged; see summary(x)", call. = FALSE)
      NULL
    } else {
      stats::coef(fit)
    }
  } else {
    if (!is.null(observed)) {
      stop("nothing to fit: every parameter has a value; omit observed", call. = FALSE)
    }
    if (!is.null(starts) || !is.null(lower) || !is.null(upper)) {
      stop("starts and bounds apply only when something is fitted", call. = FALSE)
    }
    evaluation <- evaluate_allocation(problem, theta = bound$theta_given, ...)
    theta <- bound$theta_given
  }

  structure(list(model = problem, spec = model,
                 observed = if (!is.null(obs)) list(spec = obs, target = target,
                                                    output = obs$type),
                 fit = fit, evaluation = evaluation, theta = theta, held = bound$held,
                 mode = bound$mode, call = call),
            class = c("spax_allocation", "spax_result"))
}

#' Run the Huff or conditional logit model from raw inputs
#'
#' Experimental. `spax_huff()` and `spax_clm()` prepare the data with
#' [prep_data()], specify the model with [spec_huff()] or [spec_clm()] and run it
#' with [spax()], in one call:
#' `spax_huff(demand, supply, travel, observed = ...)` is
#' `spax(prep_data(demand, supply, travel), spec_huff(), observed = ...)`. See
#' [spax()] for fitting, evaluation and the result.
#'
#' @inheritParams spec_clm
#' @param demand,supply,travel The inputs of [prep_data()]: demand by origin
#'   (a named list by group for demand groups), supply by facility and the
#'   origin-by-facility travel matrix.
#' @param observed Observed counts, as in [spax()]: an [obs_flows()] or
#'   [obs_totals()] object, or raw counts. `NULL` (the default) evaluates a model
#'   that has a value for every parameter.
#' @param by_group For data with demand groups, the parameters that differ by
#'   group, as in [spec_clm()]: `"sigma"` and, for `spax_clm()`, `"v0"`.
#' @param data_args A named list of further arguments of [prep_data()], for
#'   example `list(missing_demand = "exclude", metadata = list(travel_units =
#'   "minutes"))`.
#' @param decay The decay of the model: a decay object ([decay_gaussian()] and
#'   its relatives), an exact kernel name, a [decay_kernel()], or a named list
#'   of these by demand group. The default is the power decay for `spax_huff()`
#'   and the Gaussian for `spax_clm()`.
#' @param beta The attractiveness exponent: `"fit"` (the default for
#'   `spax_huff()`) or a positive number (the default `1` for `spax_clm()`).
#' @param outside The outside weight `v0` of `spax_clm()`: `"fit"` (the
#'   default), `0` for no outside option, a positive number, or a value by group
#'   (see [spec_clm()]). The Huff model has no outside option.
#' @param ... Further arguments of [spax()]: `starts`, `lower`, `upper` and the
#'   arguments of [fit_allocation()] or [evaluate_allocation()].
#' @return A `spax_allocation` as returned by [spax()], with the one-shot call
#'   as its `call`.
#' @examples
#' areas <- sf::st_drop_geometry(cornwall_lsoa[!cornwall_lsoa$scilly, ])
#' gp <- sf::st_drop_geometry(cornwall_gp[!cornwall_gp$scilly, ])
#' flows <- obs_flows(cornwall_flows, origin = "lsoa", facility = "practice",
#'                    count = "patients")
#'
#' # Fit Huff from raw inputs in one call
#' huff <- spax_huff(
#'   demand = stats::setNames(areas$registered_in, areas$lsoa),
#'   supply = stats::setNames(gp$gp_fte_extg, gp$practice),
#'   travel = cornwall_travel[areas$lsoa, gp$practice],
#'   observed = flows
#' )
#' coef(huff)
#' @family allocation interface
#' @export
#' @md
spax_huff <- function(demand, supply, travel, observed = NULL, decay = decay_power(),
                      beta = "fit", kappa = 1, by_group = character(), data_args = list(),
                      ...) {
  .check_data_args(data_args)
  x <- spax(do.call(prep_data, c(list(quote(demand), quote(supply), quote(travel)),
                                 data_args)),
            spec_huff(decay = decay, beta = beta, kappa = kappa, by_group = by_group),
            observed = observed, ...)
  x$call <- match.call()
  x
}

#' @rdname spax_huff
#' @export
#' @md
spax_clm <- function(demand, supply, travel, observed = NULL, decay = decay_gaussian(),
                     outside = "fit", beta = 1, kappa = 1, by_group = character(),
                     data_args = list(), ...) {
  .check_data_args(data_args)
  x <- spax(do.call(prep_data, c(list(quote(demand), quote(supply), quote(travel)),
                                 data_args)),
            spec_clm(decay = decay, outside = outside, beta = beta, kappa = kappa,
                     by_group = by_group),
            observed = observed, ...)
  x$call <- match.call()
  x
}

# `data_args` are arguments of prep_data() by name: an unnamed one would be matched to
# an argument by position.
.check_data_args <- function(data_args) {
  nms <- names(data_args)
  if (!is.list(data_args) || (length(data_args) &&
      (is.null(nms) || anyNA(nms) || !all(nzchar(nms)) || anyDuplicated(nms)))) {
    stop("data_args must be a named list of arguments of prep_data()", call. = FALSE)
  }
  invisible(TRUE)
}

# Outputs of a result ----------------------------------------------------------------

.result_no_fit <- "no fit was selected; see summary(x)"

# The named outputs of a result: the saved ones (the selected fit, or the evaluation),
# and any other computed by one evaluation at the result's parameters and not stored.
# A fit with no selected start has nothing to read.
.result_outputs <- function(x, names) {
  if (is.null(x$theta)) stop(.result_no_fit, call. = FALSE)
  saved <- if (identical(x$mode, "fit")) x$fit$best$outputs else x$evaluation$outputs
  missing <- setdiff(names, base::names(saved))
  if (length(missing)) {
    computed <- evaluate_allocation(x$model, x$theta, requested_outputs = missing)$outputs
    saved[missing] <- computed[missing]
  }
  saved[names]
}

# Step functions that accept results (SPAX-058, T6) ----------------------------------

.is_allocation_result <- function(x) inherits(x, "spax_allocation")

# What a step function takes from one result of spax(). The step function calls this
# first, before its class and group checks, so that it then checks the parts of the
# result as it would the same objects given one by one.
# * `use = "source"` (predict_allocation()): the saved fit in fit mode, the saved
#   evaluation in evaluate mode. A fit with no selected start has nothing to predict
#   from.
# * `use = "model_fit"` (check_allocation(), bootstrap_allocation()): the bound model
#   and the fit, as list(model, fit). `fit_given` says the caller gave `fit` as well,
#   which is refused; an evaluation has no fit.
.allocation_unbundle <- function(x, use = c("source", "model_fit"), fn = NULL,
                                 fit_given = FALSE) {
  use <- match.arg(use)
  if (identical(use, "source")) {
    if (is.null(x$theta)) stop(.result_no_fit, call. = FALSE)
    return(if (identical(x$mode, "fit")) x$fit else x$evaluation)
  }
  if (isTRUE(fit_given)) stop("give a result from spax() alone, without fit", call. = FALSE)
  if (!identical(x$mode, "fit")) stop(fn, " needs a fitted model", call. = FALSE)
  list(model = x$model, fit = x$fit)
}

# The `fits` and `models` of compare_allocations() when some element of `fits` is a
# result of spax(): each result supplies its fit and its model. The merged models are
# the models of the results and the caller's `models` (naming a result in both is an
# error); they are used when they cover every fit, and otherwise the caller is asked
# for the missing ones. Anything that is not a list of fits with results in it is
# returned as it came, for the checks of compare_allocations().
.allocation_unbundle_fits <- function(fits, models) {
  unchanged <- list(fits = fits, models = models)
  if (!is.list(fits) || inherits(fits, c("ae_multistart_fit", "spax_allocation"))) {
    return(unchanged)
  }
  is_result <- vapply(fits, .is_allocation_result, logical(1))
  nms <- names(fits)
  if (!any(is_result) || is.null(nms) || anyNA(nms) || any(!nzchar(nms)) ||
      anyDuplicated(nms) || !(is.null(models) || is.list(models))) {
    return(unchanged)
  }
  evaluated <- nms[is_result][vapply(fits[is_result], function(x) !identical(x$mode, "fit"),
                                     logical(1))]
  if (length(evaluated)) {
    stop(evaluated[1L], " was evaluated, not fitted; compare_allocations() needs fits",
         call. = FALSE)
  }
  doubled <- intersect(nms[is_result], names(models))
  if (length(doubled)) {
    stop(doubled[1L], " is a result and carries its own model", call. = FALSE)
  }
  own <- lapply(fits[is_result], function(x) x$model)
  fits[is_result] <- lapply(fits[is_result], function(x) x$fit)
  # In the order of the fits; names that match no fit stay at the end, for the checks
  # that follow.
  merged <- c(own, models)
  merged_names <- names(merged)
  if (!anyNA(merged_names) && all(nzchar(merged_names))) {
    merged <- merged[c(intersect(nms, merged_names), setdiff(merged_names, nms))]
  }
  lacking <- setdiff(nms, merged_names)
  if (length(lacking)) {
    stop("give models for ", paste(lacking, collapse = ", "), call. = FALSE)
  }
  list(fits = fits, models = merged)
}

# Methods ----------------------------------------------------------------------------

#' Methods for allocation results
#'
#' What to do with the result of [spax()], [spax_huff()] or [spax_clm()]. These
#' methods read the saved fit or evaluation. A fit does not solve the model
#' again, and an output that was not computed is evaluated when asked for and not
#' stored. A fit in which no start converged has no estimates: `coef()` gives
#' `NULL` and the other methods that need outputs stop with "no fit was selected;
#' see summary(x)"; `summary()`, `print()` and `as.data.frame(type = "starts")`
#' still show the starts.
#'
#' * `print()` shows the specification, the data, the parameters and, for a fit,
#'   the selected start and the default search.
#' * `summary()` returns a `summary.spax_allocation` with the specification
#'   lines (`spec`), the parameters table (`parameters`), the fit summary of
#'   [fit_allocation()] (`fit`, `NULL` when evaluated) and the `mode`; its print
#'   method shows them, with the per-start table for a fit.
#' * `coef()` returns the free parameters, fitted or given. Held values are in
#'   `x$held`.
#' * `fitted()` returns the modelled counts for the observations of a fit (as
#'   [fitted.ae_multistart_fit()][allocation-fit-methods] does), and the modelled
#'   facility utilization, named by facility, for an evaluation.
#' * `as.data.frame()` returns one of the tables below.
#' * `predict()` predicts requested origins from the saved fit or evaluation
#'   with [predict_allocation()]. Models with demand groups are refused, as there.
#'
#' **Tables of `as.data.frame()`:**
#'
#' | `type` | rows and columns |
#' |---|---|
#' | `"parameters"` | `parameter`, `value`, `status`: the free parameters (`"fitted"` or `"given"`), then the held values (`"held"`) |
#' | `"facilities"` | the facilities of the data: `facility_id`, `supply`, `attractiveness`, `utilization`, and `observed` for a fit to facility totals (the sum over groups for totals by group, `NA` when a group is unobserved) |
#' | `"origins"` | the active origins: `origin_id`, `demand`, `access` (the share of demand that goes to a facility of the data), `outside_share` (pooled over the groups) |
#' | `"flows"` | the origin-facility cells with modelled flow or observed count above 0: `origin_id`, `facility_id`, `flow` (demand times allocation), and `observed` for a fit to flows (`NA` where unobserved) |
#' | `"starts"` | the table of starts of the fit (`seconds` is the time each took) |
#'
#' @param object,x A `spax_allocation`, or for `print()` of the summary, a
#'   `summary.spax_allocation`.
#' @param row.names,optional Standard `as.data.frame()` arguments.
#' @param type The table to return (see above).
#' @param travel For `predict()`: the requested-origin-by-facility travel matrix,
#'   `distance` of [predict_allocation()], in the travel unit of the data.
#' @param demand For `predict()`: optional demand of the requested origins.
#' @param outputs For `predict()`: the outputs to return, `"allocation"` and
#'   `"outside_share"` by default; see [predict_allocation()] for the others.
#' @param ... Further arguments: for `predict()`, those of [predict_allocation()]
#'   (for instance `missing_travel`); otherwise unused.
#' @return `print()` returns its argument invisibly. `summary()` returns a
#'   `summary.spax_allocation`. `coef()` returns a named numeric vector (or
#'   `NULL`), `fitted()` a numeric vector or matrix, `as.data.frame()` a data
#'   frame and `predict()` the `ae_clm_prediction` of [predict_allocation()].
#' @seealso [spax()]
#' @family allocation interface
#' @name spax_allocation-methods
#' @method print spax_allocation
#' @export
#' @md
print.spax_allocation <- function(x, ...) {
  spec <- sub("^<spax spec>", "<spax allocation>", format(x$spec))
  substrate <- x$model$substrate
  cat(c(spec,
        sprintf("  %-10s  %s", "data", paste0(.format_total(length(substrate$origin_ids)),
                                               " origins x ",
                                               .format_total(length(substrate$facility_ids)),
                                               " facilities")),
        "", .result_parameter_lines(.result_parameters(x)),
        if (identical(x$mode, "fit")) c("", .result_fit_lines(x))), sep = "\n")
  invisible(x)
}

#' @rdname spax_allocation-methods
#' @exportS3Method base::summary
#' @md
summary.spax_allocation <- function(object, ...) {
  structure(list(spec = format(object$spec), parameters = .result_parameters(object),
                 fit = if (identical(object$mode, "fit")) summary(object$fit),
                 mode = object$mode), class = "summary.spax_allocation")
}

#' @rdname spax_allocation-methods
#' @method print summary.spax_allocation
#' @export
#' @md
print.summary.spax_allocation <- function(x, ...) {
  cat(c(sub("^<spax spec>", "<spax allocation summary>", x$spec), "",
        .result_parameter_lines(x$parameters), ""), sep = "\n")
  if (identical(x$mode, "fit")) {
    print(x$fit, ...)
  } else {
    cat("evaluated at given values\n")
  }
  invisible(x)
}

#' @rdname spax_allocation-methods
#' @exportS3Method stats::coef
#' @md
coef.spax_allocation <- function(object, ...) {
  object$theta
}

#' @rdname spax_allocation-methods
#' @exportS3Method stats::fitted
#' @md
fitted.spax_allocation <- function(object, ...) {
  if (is.null(object$theta)) stop(.result_no_fit, call. = FALSE)
  if (identical(object$mode, "fit")) return(stats::fitted(object$fit))
  stats::setNames(as.numeric(.result_outputs(object, "utilization")$utilization),
                  object$model$substrate$facility_ids)
}

#' @rdname spax_allocation-methods
#' @exportS3Method base::as.data.frame
#' @md
as.data.frame.spax_allocation <- function(x, row.names = NULL, optional = FALSE,
                                          type = c("parameters", "facilities", "origins",
                                                   "flows", "starts"), ...) {
  type <- match.arg(type)
  table <- switch(type,
    parameters = .result_parameters(x),
    facilities = .result_facilities(x),
    origins = .result_origins(x),
    flows = .result_flows(x),
    starts = {
      if (!identical(x$mode, "fit")) {
        stop("no fit was made: the model was evaluated at given values, so there are ",
             "no starts", call. = FALSE)
      }
      x$fit$table
    })
  .allocation_saved_table(table, row.names, optional, ...)
}

#' @rdname spax_allocation-methods
#' @exportS3Method stats::predict
#' @md
predict.spax_allocation <- function(object, travel, demand = NULL,
                                    outputs = c("allocation", "outside_share"), ...) {
  if (is.null(object$theta)) stop(.result_no_fit, call. = FALSE)
  source <- if (identical(object$mode, "fit")) object$fit else object$evaluation
  predict_allocation(source = source, distance = travel, demand = demand,
                     outputs = outputs, ...)
}

# Tables of as.data.frame() -----------------------------------------------------------

# The free parameters, fitted or given, then the held values. A fit with no selected
# start lists its free parameters without values.
.result_parameters <- function(x) {
  theta <- x$theta
  free <- if (is.null(theta)) {
    stats::setNames(rep(NA_real_, length(x$model$theta$names)), x$model$theta$names)
  } else {
    theta
  }
  status <- if (is.null(theta)) "not fitted" else if (identical(x$mode, "fit")) "fitted" else "given"
  data.frame(parameter = c(names(free), names(x$held)),
             value = c(as.numeric(free), as.numeric(x$held)),
             status = c(rep(status, length(free)), rep("held", length(x$held))),
             stringsAsFactors = FALSE)
}

.result_facilities <- function(x) {
  out <- .result_outputs(x, c("attractiveness", "utilization"))
  substrate <- x$model$substrate
  table <- data.frame(facility_id = substrate$facility_ids,
                      supply = as.vector(substrate$S[, 1L]),
                      attractiveness = as.numeric(out$attractiveness),
                      utilization = as.numeric(out$utilization), stringsAsFactors = FALSE)
  target <- x$observed$target
  if (identical(x$observed$output, "utilization")) {
    table$observed <- as.numeric(target)
  } else if (identical(x$observed$output, "utilization_by_group")) {
    # The total over the groups; unknown when a group count is.
    table$observed <- as.numeric(rowSums(target))
  }
  table
}

.result_origins <- function(x) {
  out <- .result_outputs(x, c("access", "outside_share"))
  substrate <- x$model$substrate
  data.frame(origin_id = substrate$origin_ids, demand = as.numeric(substrate$D_active),
             access = as.numeric(out$access), outside_share = as.numeric(out$outside_share),
             stringsAsFactors = FALSE)
}

# Origin-facility cells with a modelled flow, or an observed count, above 0, in the
# order of the origins and then the facilities.
.result_flows <- function(x) {
  flow <- .result_outputs(x, "flow")$flow
  substrate <- x$model$substrate
  observed <- if (identical(x$observed$output, "flow")) x$observed$target
  keep <- !is.na(flow) & flow > 0
  if (!is.null(observed)) keep <- keep | (!is.na(observed) & observed > 0)
  cells <- which(keep, arr.ind = TRUE)
  cells <- cells[order(cells[, 1L], cells[, 2L]), , drop = FALSE]
  table <- data.frame(origin_id = substrate$origin_ids[cells[, 1L]],
                      facility_id = substrate$facility_ids[cells[, 2L]],
                      flow = as.numeric(flow[cells]), stringsAsFactors = FALSE)
  if (!is.null(observed)) table$observed <- as.numeric(observed[cells])
  table
}

# Printed lines -----------------------------------------------------------------------

# The parameters table as aligned lines.
.result_parameter_lines <- function(parameters) {
  value <- vapply(parameters$value, function(v) {
    if (is.na(v)) "NA" else format(v, digits = 4L, trim = TRUE)
  }, character(1))
  w_name <- max(nchar(c("parameter", parameters$parameter)))
  w_value <- max(nchar(c("value", value)))
  row <- function(a, b, c) sprintf("  %-*s  %-*s  %s", w_name, a, w_value, b, c)
  c(row("parameter", "value", "status"), row(parameters$parameter, value, parameters$status))
}

# The selected start, its boundary flag and the default search of a fit.
.result_fit_lines <- function(x) {
  fit <- x$fit
  line <- function(label, text) sprintf("  %-10s  %s", label, text)
  n <- nrow(fit$table)
  search <- fit[["default_search", exact = TRUE]]
  if (is.null(fit$best)) {
    selected <- line("selected", paste0("none: no start converged (", n,
                                        if (n == 1L) " start)" else " starts)"))
    boundary <- NULL
  } else {
    i <- match(fit$best_start, fit$table$start)
    converged <- sum(fit$table$eligible)
    selected <- line("selected", paste0(fit$best_start, " (start ", i, " of ", n, "); ",
                                        converged, " of ", n,
                                        if (n == 1L) " start" else " starts", " converged"))
    at <- colnames(fit$boundary)[fit$boundary[i, ]]
    boundary <- line("boundary", if (length(at)) {
      paste0(paste(at, collapse = ", "), " at a search bound")
    } else {
      "none"
    })
  }
  c(selected, boundary,
    if (!is.null(search)) {
      line("starts", paste0("default search, ", search$n_finite, " of ", search$n_points,
                            " grid points, loss ", search$loss))
    })
}
