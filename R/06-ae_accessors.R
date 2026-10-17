#' Read a saved AE fit output by exact name without recomputation
#'
#' Preserves the stored value, names and dimensions, including matrix outputs.
#' Does not bind parameters, solve, scan a raster or materialize spatial output.
#' Numerical `allocation` is the model's allocation operator, not observed
#' flows; multiplying rows by demand produces implied flows. `access` retains
#' its model-specific meaning (CLM contact, SAE soft-capped opportunity, or
#' HAAE/Huff-decay attenuated allocation mass). No semantic alias is applied.
#' @keywords internal
.fit_output <- function(fit, output) {
  .chck_class(fit, "ae_problem_nfxp_fit", "fit")
  if (!is.character(output) || length(output) != 1L || is.na(output) || !nzchar(output)) {
    stop("output must be one nonmissing name")
  }
  if (!is.list(fit$outputs) || !output %in% names(fit$outputs)) {
    stop("fit does not contain requested output `", output, "`")
  }
  fit$outputs[[output]]
}

#' Read a saved numeric AE vector without flattening matrix outputs
#' @keywords internal
.fit_output_vector <- function(fit, output) {
  value <- .fit_output(fit, output)
  if (!is.numeric(value) || !is.null(dim(value))) {
    stop("output `", output, "` is not a numeric vector; use .fit_output for matrices")
  }
  value
}

#' List saved origin-side fit outputs without constructing maps
#'
#' Built-in fits carry provider-declared `output_axes` identifying origin,
#' facility and origin-facility outputs. The map accessor respects these axes
#' even when axis lengths coincide; diagnostics such as `pooled` remain explicit
#' by-name outputs. Unannotated older/custom fits retain shape-only inference,
#' which cannot distinguish coincident axes. Regenerate them with declared axes
#' before relying on semantic surface discovery. This is metadata for inspection,
#' not a recipe compiler or an `output_roles` alias layer.
#' @keywords internal
.fit_available_surfaces <- function(fit) {
  .chck_class(fit, "ae_problem_nfxp_fit", "fit")
  .surface_output_names(fit$outputs, length(fit$surface_meta$demand_kept_index),
                        fit$output_axes)
}
