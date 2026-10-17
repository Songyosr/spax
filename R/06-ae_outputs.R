# Private requested-output protocol -----------------------------------------

.check_output_names <- function(x, name) {
  if (!is.null(x) && (!is.character(x) || !length(x) || anyNA(x) ||
                     any(!nzchar(trimws(x))) || anyDuplicated(x))) {
    stop("`", name, "` must be NULL or unique nonempty output names")
  }
  invisible(TRUE)
}

.validate_output_capabilities <- function(step, output_axes = NULL) {
  requested <- step[["outputs_requested", exact = TRUE]]
  independent <- step[["state_independent_outputs", exact = TRUE]]
  if (!is.null(requested) && !is.function(requested)) {
    stop("`outputs_requested` must be a function when supplied")
  }
  .check_output_names(independent, "state_independent_outputs")
  if (!is.null(output_axes) &&
      any(!independent %in% names(output_axes))) {
    stop("`state_independent_outputs` must name declared output axes")
  }
  invisible(TRUE)
}

# NULL preserves the legacy callback exactly. Requested-output providers must
# return the usual contracted outputs too; the request adds optional outputs.
# Providers without this capability keep their existing outputs(x) behavior.
.problem_step_outputs <- function(step, x, requested_outputs = NULL,
                                   required = TRUE) {
  .check_output_names(requested_outputs, "requested_outputs")
  requested <- step[["outputs_requested", exact = TRUE]]
  outputs <- if (!is.null(requested_outputs) && is.function(requested)) {
    requested(x, requested_outputs)
  } else {
    step[["outputs", exact = TRUE]](x)
  }
  missing <- setdiff(requested_outputs, names(outputs))
  if (required && length(missing)) {
    stop("problem outputs do not include requested output `",
         paste(missing, collapse = "`, `"), "`")
  }
  outputs
}

# Expected origin-by-facility counts for corrected CLM only. Demand supplies
# the denominator and count unit; this helper does not infer an event or period.
.clm_flow <- function(allocation, demand) {
  if (!is.matrix(allocation) || nrow(allocation) != length(demand)) {
    stop("CLM allocation rows must match active demand")
  }
  allocation * as.numeric(demand)
}
