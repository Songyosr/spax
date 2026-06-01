# AE Operation Grammar: atoms and FCA workflow verbs -------------------------
#
# Internal, field-in / field-out layer (SPAX-002). Everything operates on
# `spax_field` objects (R/00-spax_field.R): inputs and outputs are fields,
# alignment is always by index$layer join key (never raw layer order), and
# fields are immutable (operations return new fields).
#
# Three formal atoms — .ae_lift / .ae_transform / .ae_aggregate — plus the
# domain-general FCA verbs .ae_normalize / .ae_gather / .ae_spread / .ae_ratio /
# .ae_update. `gather` keeps the fused terra::global reduction; `spread` is
# recomposed from atoms (see SPAX-002 card / Harmonized Implementation Plan).
#
# Naming: internal `.ae_*`, mapping 1:1 to a future exported `ae_*` layer.
# No public API is added here; the raw->field rewire of compute_access() /
# spax_e2sfca() is SPAX-004. Group/mode verbs (stratify/mask/summarize) and
# product-axis compounds are SPAX-005.

# Helpers --------------------------------------------------------------------

#' The single non-I axis of a raster field (errors on 0 or >1)
#' @keywords internal
.ae_nonI_axis <- function(field) {
  axes <- setdiff(.field_domain(field), "I")
  if (length(axes) != 1) {
    stop("expected exactly one non-I axis; got: ", paste(axes, collapse = ", "))
  }
  axes
}

#' Reorder a vector field's values to a raster field's layer order, by shared axis
#' @keywords internal
.ae_align_to_layers <- function(vector_field, raster_field) {
  axes <- .field_domain(vector_field)
  raster_frame <- .field_layer_index(raster_field)
  vector_frame <- .field_node_index(vector_field)

  missing_axes <- setdiff(axes, names(raster_frame))
  if (length(missing_axes) > 0) {
    stop("vector field axis is not present in the raster field index: ",
         paste(missing_axes, collapse = ", "))
  }

  ids <- .ae_tuple_key(raster_frame, axes)
  vector_ids <- .ae_tuple_key(vector_frame, axes)
  values <- .field_data(vector_field)
  if (!all(ids %in% vector_ids)) {
    stop("vector field is missing axis tuples present in the raster layers")
  }
  values[match(ids, vector_ids)]
}

#' Build a stable composite key from an index frame
#' @keywords internal
.ae_tuple_key <- function(frame, axes) {
  if (length(axes) == 0) {
    return(rep("", nrow(frame)))
  }
  do.call(paste, c(lapply(frame[, axes, drop = FALSE], as.character), sep = "\r"))
}

#' Build a raster field from already-consistent data + frame (snap construction)
#' @keywords internal
.ae_raster <- function(data, domain, frame, role = "unknown", meta = list()) {
  .spax_raster_field(
    data = data, domain = domain, index = list(layer = frame),
    role = role, meta = meta, snap = TRUE
  )
}

#' Is x a length-one scalar accepted by field Ops?
#' @keywords internal
.ae_is_scalar <- function(x) {
  is.atomic(x) &&
    is.null(dim(x)) &&
    length(x) == 1 &&
    (is.numeric(x) || is.logical(x))
}

#' Resolve an allowed pointwise arithmetic operator
#' @keywords internal
.ae_op_fun <- function(op) {
  if (!op %in% c("+", "-", "*", "/", "^")) {
    stop("unsupported spax_field operator: ", op)
  }
  get(op, envir = baseenv(), mode = "function")
}

#' Resolve an allowed pointwise math transform
#' @keywords internal
.ae_math_fun <- function(fn) {
  allowed <- c(
    "abs", "sign", "sqrt", "floor", "ceiling", "trunc",
    "exp", "expm1", "log", "log10", "log2", "log1p",
    "cos", "cosh", "sin", "sinh", "tan", "tanh",
    "acos", "acosh", "asin", "asinh", "atan", "atanh",
    "cospi", "sinpi", "tanpi",
    "gamma", "lgamma", "digamma", "trigamma"
  )
  if (!fn %in% allowed) {
    stop("unsupported spax_field math transform: ", fn)
  }
  get(fn, envir = baseenv(), mode = "function")
}

# Atom: lift -----------------------------------------------------------------

#' Lift a field onto a larger typed domain
#'
#' Broadcast a field to a target domain using a raster `template` that supplies
#' the new axis structure (index$layer) and grid geometry. FCA cases:
#'   - vector field on axis A  -> raster field (I, A): each layer is constant,
#'     the value for facility f, aligned to the template by join key.
#'   - raster field on I       -> raster field (I, A): the single layer is
#'     replicated across the template's layers.
#' @keywords internal
.ae_lift <- function(field, to, template) {
  .chck_class(template, "spax_raster_field", "template")
  if (!setequal(.field_domain(template), to)) {
    stop("template domain must equal the target domain `to`")
  }
  tdata <- .field_data(template)
  backend <- .field_backend(field)

  if (backend == "vector") {
    aligned <- .ae_align_to_layers(field, template) # length = nlyr(template)
    lifted <- (tdata * 0) + aligned # constant-per-layer, template geometry/NA
    return(.rewrap_field(template, lifted, role = .field_role(field), meta = list()))
  }

  if (backend == "raster" && identical(.field_domain(field), "I")) {
    base <- .field_data(field)
    if (terra::nlyr(base) != 1) stop("expected a single-layer raster on domain I")
    n <- terra::nlyr(tdata)
    lifted <- base[[rep(1, n)]]
    return(.rewrap_field(template, lifted, role = .field_role(field), meta = list()))
  }

  stop("unsupported lift: backend '", backend, "', domain (",
       paste(.field_domain(field), collapse = ", "), ")")
}

# Atom: transform ------------------------------------------------------------

#' Apply a pointwise scalar function to a field (unary transform atom)
#' @keywords internal
.ae_transform <- function(field, fn, ...) {
  .chck_class(field, "spax_field", "field")
  data <- .field_data(field)
  res <- fn(data, ...)

  if (.field_backend(field) == "raster") {
    return(.rewrap_field(field, res))
  }
  .rewrap_field(field, res)
}

#' Pointwise arithmetic for same-domain spax_field objects
#' @keywords internal
#' @method Ops spax_field
#' @export
Ops.spax_field <- function(e1, e2) {
  op <- .ae_op_fun(.Generic)

  if (missing(e2)) {
    if (!.Generic %in% c("+", "-")) {
      stop("unsupported unary spax_field operator: ", .Generic)
    }
    .chck_class(e1, "spax_field", "e1")
    if (.Generic == "+") return(e1)
    return(.ae_transform(e1, function(x) op(x)))
  }

  e1_is_field <- inherits(e1, "spax_field")
  e2_is_field <- inherits(e2, "spax_field")

  if (e1_is_field && e2_is_field) {
    if (.Generic == "^") {
      stop("field ^ field is not supported; use field ^ scalar or scalar ^ field")
    }
    return(.ae_combine(e1, e2, op = op))
  }

  if (e1_is_field && .ae_is_scalar(e2)) {
    return(.ae_transform(e1, function(x) op(x, e2)))
  }

  if (.ae_is_scalar(e1) && e2_is_field) {
    return(.ae_transform(e2, function(x) op(e1, x)))
  }

  stop("spax_field arithmetic requires field/scalar or same-domain field/field operands")
}

#' Pointwise math transforms for spax_field objects
#' @keywords internal
#' @method Math spax_field
#' @export
Math.spax_field <- function(x, ...) {
  .chck_class(x, "spax_field", "x")
  fn <- .ae_math_fun(.Generic)
  .ae_transform(x, function(data) fn(data, ...))
}

#' Combine two fields elementwise on a shared domain (binary transform)
#'
#' Same-domain combination; raster operands are aligned layer-wise by their
#' non-I axis join key before the op, so row order can never silently mismatch.
#' @keywords internal
.ae_combine <- function(a, b, op = `*`) {
  if (!setequal(.field_domain(a), .field_domain(b))) {
    stop("combine requires matching domains")
  }
  backend <- .field_backend(a)
  if (!identical(backend, .field_backend(b))) {
    stop("combine requires matching backends")
  }

  if (backend == "raster") {
    da <- .field_data(a)
    db <- .field_data(b)
    # Align b's layers to a's by the full non-I axis *tuple* from index$layer
    # (DEC-005: join by key, never by raw layer order). Works for one or many
    # non-I axes, so product-axis fields align correctly too.
    axes <- setdiff(.field_domain(a), "I")
    if (length(axes) >= 1) {
      fa <- .field_layer_index(a)[, axes, drop = FALSE]
      fb <- .field_layer_index(b)[, axes, drop = FALSE]
      key_a <- .ae_tuple_key(fa, axes)
      key_b <- .ae_tuple_key(fb, axes)
      if (!setequal(key_a, key_b)) stop("combine operands span different axis tuples")
      if (anyDuplicated(key_a) || anyDuplicated(key_b)) {
        stop("combine requires unique axis tuples per layer")
      }
      if (!identical(key_a, key_b)) db <- db[[match(key_a, key_b)]]
    }
    res <- op(da, db)
    return(.rewrap_field(a, res))
  }

  if (backend == "vector") {
    va <- .field_data(a)
    vb <- .field_data(b)
    axes <- .field_domain(a)
    fa <- .field_node_index(a)
    fb <- .field_node_index(b)
    key_a <- .ae_tuple_key(fa, axes)
    key_b <- .ae_tuple_key(fb, axes)
    if (!setequal(key_a, key_b)) stop("operands span different axis tuples")
    if (anyDuplicated(key_a) || anyDuplicated(key_b)) {
      stop("combine requires unique axis tuples per vector element")
    }
    if (!identical(key_a, key_b)) vb <- vb[match(key_a, key_b)]
    res <- op(va, vb)
    return(.rewrap_field(a, res))
  }

  stop("unsupported combine for backend '", backend, "'")
}

# Atom: aggregate ------------------------------------------------------------

#' Collapse a field over one axis
#'
#' Over "I" (raster): per-layer cell sum -> vector field on the non-I axis
#' (DEC-006: collapsing I changes the backend). Over a non-I axis: sum across
#' layers -> single-layer raster field on the remaining domain.
#' @keywords internal
.ae_aggregate <- function(field, over) {
  backend <- .field_backend(field)

  if (backend == "raster") {
    domain <- .field_domain(field)
    if (identical(over, "I")) {
      axis <- .ae_nonI_axis(field)
      sums <- terra::global(.field_data(field), "sum", na.rm = TRUE)[[1]]
      names(sums) <- as.character(.field_layer_index(field)[[axis]])
      return(.spax_vector_field(sums, domain = axis,
                                       role = .field_role(field), snap = TRUE))
    }
    if (!over %in% setdiff(domain, "I")) {
      stop("`over` must be I or a non-I axis of the field")
    }
    remaining <- setdiff(domain, c("I", over))
    if (length(remaining) > 0) {
      stop("grouped aggregation over one of several non-I axes is not ",
           "implemented (SPAX-005); remaining axes: ",
           paste(remaining, collapse = ", "))
    }
    collapsed <- terra::app(.field_data(field), fun = sum, na.rm = TRUE)
    names(collapsed) <- "L1"
    new_domain <- setdiff(domain, over)
    frame <- data.frame(layer = "L1", stringsAsFactors = FALSE)
    return(.ae_raster(collapsed, new_domain, frame, role = .field_role(field)))
  }

  if (backend == "vector") {
    return(sum(.field_data(field), na.rm = TRUE))
  }

  stop("unsupported aggregate for backend '", backend, "'")
}

# Verb: normalize ------------------------------------------------------------

#' Convert a weight/kernel field into a mapping kernel
#'
#' Wraps the package's calc_normalize() so behavior matches the current engine;
#' the domain is unchanged.
#' @keywords internal
.ae_normalize <- function(field, method = "standard") {
  .chck_class(field, "spax_raster_field", "field")
  res <- calc_normalize(.field_data(field), method = method)
  .rewrap_field(field, res, role = "map", meta = list())
}

# Verb: gather (fused) -------------------------------------------------------

#' Realize a source field onto the supply side (demand -> facility)
#'
#' gather(v, W)_j = sum_i v_i W_ij. Keeps terra::global's fused weighted
#' reduction (no materialized product stack); equal to lift -> transform ->
#' aggregate-over-I. Returns a vector field on the weight field's non-I axis.
#' @keywords internal
.ae_gather <- function(source, weights) {
  .chck_class(source, "spax_raster_field", "source")
  .chck_class(weights, "spax_raster_field", "weights")
  if (!identical(.field_domain(source), "I") ||
      terra::nlyr(.field_data(source)) != 1) {
    stop("`source` must be a single-layer raster field on domain I")
  }
  axis <- .ae_nonI_axis(weights)
  vals <- .gather_weighted_core(.field_data(source), .field_data(weights),
                                na.rm = TRUE)
  names(vals) <- as.character(.field_layer_index(weights)[[axis]])
  .spax_vector_field(vals, domain = axis, role = "realization",
                            snap = TRUE)
}

# Verb: spread (recomposed) --------------------------------------------------

#' Realize a source field onto the demand side (facility -> demand)
#'
#' spread(r, W)_i = sum_j W_ij r_j. Recomposed from atoms: lift the per-facility
#' source onto edges, multiply by the weights, aggregate over the facility axis.
#' Returns a raster field on domain I.
#' @keywords internal
.ae_spread <- function(source, weights) {
  .chck_class(source, "spax_vector_field", "source")
  .chck_class(weights, "spax_raster_field", "weights")
  axis <- .ae_nonI_axis(weights)
  lifted <- .ae_lift(source, to = .field_domain(weights), template = weights)
  product <- .ae_combine(lifted, weights, op = `*`)
  out <- .ae_aggregate(product, over = axis)
  out$role <- "realization"
  out
}

# Verb: ratio ----------------------------------------------------------------

#' Pointwise ratio of two fields with zero-safe division
#' @keywords internal
.ae_ratio <- function(num, denom, zero = 0) {
  safe_div <- function(a, b) {
    if (inherits(a, "SpatRaster") || inherits(b, "SpatRaster")) {
      return(terra::ifel(b == 0, zero, a / b))
    }
    out <- a / b
    out[!is.finite(out)] <- zero
    out
  }
  .ae_combine(num, denom, op = safe_div)
}

# Verb: update (damped) ------------------------------------------------------

#' Damped update of a retained state toward a realized target
#'
#' mix_lambda(omega, target) = (1 - lambda) * omega + lambda * target. The
#' standard AE relaxation step; lambda in (0, 1].
#' @keywords internal
.ae_update <- function(state, target, lambda = 1) {
  if (!is.numeric(lambda) || length(lambda) != 1 || lambda <= 0 || lambda > 1) {
    stop("lambda must be a single value in (0, 1]")
  }
  a <- .ae_transform(state, function(x) (1 - lambda) * x)
  b <- .ae_transform(target, function(x) lambda * x)
  out <- .ae_combine(a, b, op = `+`)
  out$role <- .field_role(state)
  out
}
