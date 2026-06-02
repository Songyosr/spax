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

#' The single raster layer axis (errors on 0 or >1)
#' @keywords internal
.ae_nonI_axis <- function(field) {
  axes <- .field_layer_axes(field)
  if (length(axes) != 1) {
    stop("expected exactly one layer axis; got: ", paste(axes, collapse = ", "))
  }
  axes
}

#' Build a stable composite key from an index frame
#' @keywords internal
.ae_tuple_key <- function(frame, axes) {
  if (length(axes) == 0) {
    return(rep("", nrow(frame)))
  }
  do.call(paste, c(lapply(frame[, axes, drop = FALSE], as.character), sep = "\r"))
}

#' Sum numeric values by index-frame coordinate tuples
#' @keywords internal
.ae_sum_by_frame <- function(values, frame, by, na.rm = TRUE) {
  if (length(by) == 0) {
    return(list(values = sum(values, na.rm = na.rm), frame = NULL))
  }

  key <- .ae_tuple_key(frame, by)
  levels <- unique(key)
  sums <- vapply(levels, function(k) {
    sum(values[key == k], na.rm = na.rm)
  }, numeric(1))
  out_frame <- frame[match(levels, key), by, drop = FALSE]
  rownames(out_frame) <- NULL
  list(values = unname(sums), frame = out_frame)
}

#' Align vector values to a target frame by the vector field's full domain
#' @keywords internal
.ae_align_vector_to_frame <- function(vector_field, target_frame) {
  axes <- .field_domain(vector_field)
  missing_axes <- setdiff(axes, names(target_frame))
  if (length(missing_axes) > 0) {
    stop("vector field axis is not present in the target index: ",
         paste(missing_axes, collapse = ", "))
  }

  ids <- .ae_tuple_key(target_frame, axes)
  vector_frame <- .field_node_index(vector_field)
  vector_ids <- .ae_tuple_key(vector_frame, axes)
  values <- .field_data(vector_field)
  if (!all(ids %in% vector_ids)) {
    stop("vector field is missing axis tuples present in the target index")
  }
  values[match(ids, vector_ids)]
}

#' Reorder a vector field's values to a raster field's layer order
#' @keywords internal
.ae_align_to_layers <- function(vector_field, raster_field) {
  .ae_align_vector_to_frame(vector_field, .field_layer_index(raster_field))
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

#' Resolve and validate normalize arguments
#' @keywords internal
.ae_normalize_args <- function(field, by, method, a0) {
  backend <- .field_backend(field)
  domain <- .field_domain(field)

  if (is.null(by)) {
    if (backend == "raster") {
      by <- .field_cell_axis(field)
      warning(
        "`by` omitted for raster normalization; using the raster cell axis",
        call. = FALSE
      )
    } else {
      by <- character()
    }
  }

  .chck_class(by, "character", "by")
  if (anyDuplicated(by)) {
    stop("by must contain unique axis names")
  }
  if (!all(by %in% domain)) {
    stop("by must be NULL or a subset of the field domain")
  }

  .chck_class(method, "character", "method")
  .chck_length(length(method), 1, "method")
  valid_methods <- c("identity", "standard", "semi")
  if (!method %in% valid_methods) {
    stop("method must be one of: ", paste(valid_methods, collapse = ", "))
  }

  .chck_is_numeric(a0, "a0")
  .chck_length(length(a0), 1, "a0")
  .chck_positive(a0, allow_zero = TRUE, "a0")

  by
}

#' Apply grouped normalization with a scalar, vector, or raster denominator
#' @keywords internal
.ae_normalize_values <- function(values, denom, method, a0 = 0) {
  if (method == "identity") {
    return(values)
  }

  denom <- denom + a0
  if (inherits(values, "SpatRaster") || inherits(denom, "SpatRaster")) {
    if (method == "standard") {
      return(terra::ifel(denom > 0, values / denom, 0 * values))
    }
    return(terra::ifel(denom > 1, values / denom, values))
  }

  if (method == "standard") {
    out <- ifelse(denom > 0, values / denom, 0 * values)
  } else {
    out <- ifelse(denom > 1, values / denom, values)
  }
  out
}

#' Normalize raster layers by cell-retaining groups
#' @keywords internal
.ae_normalize_raster_by_cell <- function(field, by, method, a0 = 0) {
  data <- .field_data(field)
  layer_frame <- .field_layer_index(field)
  keep_layer_axes <- setdiff(by, .field_cell_axis(field))

  if (length(keep_layer_axes) == 0) {
    denom <- sum(data, na.rm = TRUE)
    return(.ae_normalize_values(data, denom, method = method, a0 = a0))
  }

  group_key <- .ae_tuple_key(layer_frame, keep_layer_axes)
  groups <- unique(group_key)
  layers <- vector("list", terra::nlyr(data))

  for (group in groups) {
    idx <- which(group_key == group)
    denom <- if (length(idx) == 1) {
      data[[idx]]
    } else {
      terra::app(data[[idx]], fun = sum, na.rm = TRUE)
    }
    normalized <- .ae_normalize_values(data[[idx]], denom, method = method, a0 = a0)
    for (pos in seq_along(idx)) {
      layers[[idx[pos]]] <- normalized[[pos]]
    }
  }

  out <- terra::rast(layers)
  names(out) <- names(data)
  out
}

#' Normalize raster fields when the retained axes do not include the cell axis
#' @keywords internal
.ae_normalize_raster_by_global <- function(field, by, method, a0 = 0) {
  data <- .field_data(field)

  if (length(by) == 0) {
    denom <- .ae_aggregate(field, by = character())
    return(.ae_normalize_values(data, denom, method = method, a0 = a0))
  }

  denom <- .ae_aggregate(field, by = by)
  lifted <- .ae_lift(denom, template = field)
  .ae_normalize_values(data, .field_data(lifted), method = method, a0 = a0)
}

#' Normalize vector fields by retained-axis groups
#' @keywords internal
.ae_normalize_vector <- function(field, by, method, a0 = 0) {
  values <- .field_data(field)
  if (length(by) == 0) {
    denom <- rep(sum(values, na.rm = TRUE), length(values))
    return(.ae_normalize_values(values, denom, method = method, a0 = a0))
  }

  frame <- .field_node_index(field)
  key <- .ae_tuple_key(frame, by)
  groups <- unique(key)
  denom_by_group <- vapply(groups, function(group) {
    sum(values[key == group], na.rm = TRUE)
  }, numeric(1))
  denom <- denom_by_group[match(key, groups)]
  .ae_normalize_values(values, denom, method = method, a0 = a0)
}

# Atom: lift -----------------------------------------------------------------

#' Lift a field onto a template field's structure
#'
#' The template supplies backend, domain, geometry, and index frame. Its values
#' are ignored. Source values are matched on source axes and broadcast over axes
#' present only in the template.
#' @keywords internal
.ae_lift <- function(field, template) {
  .chck_class(field, "spax_field", "field")
  .chck_class(template, "spax_field", "template")
  if (!all(.field_domain(field) %in% .field_domain(template))) {
    stop("field domain must be a subset of template domain")
  }

  tdata <- .field_data(template)
  backend <- .field_backend(field)
  template_backend <- .field_backend(template)

  if (template_backend == "vector") {
    if (backend != "vector") {
      stop("lifting raster fields to vector templates is not supported")
    }
    aligned <- .ae_align_vector_to_frame(field, .field_node_index(template))
    return(.rewrap_field(template, aligned, role = .field_role(field), meta = list()))
  }

  if (template_backend == "raster" && backend == "vector") {
    aligned <- .ae_align_to_layers(field, template) # length = nlyr(template)
    lifted <- (tdata * 0) + aligned # constant-per-layer, template geometry/NA
    return(.rewrap_field(template, lifted, role = .field_role(field), meta = list()))
  }

  if (template_backend == "raster" && backend == "raster") {
    .chck_raster_alignment(.field_data(field)[[1]], tdata[[1]], "field", "template")
    base <- .field_data(field)

    source_layer_axes <- .field_layer_axes(field)
    if (length(source_layer_axes) == 0) {
      if (terra::nlyr(base) != 1) {
        stop("cell-axis-only raster fields must have one layer")
      }
      lifted <- base[[rep(1, terra::nlyr(tdata))]]
      return(.rewrap_field(template, lifted, role = .field_role(field), meta = list()))
    }

    template_frame <- .field_layer_index(template)
    missing_axes <- setdiff(source_layer_axes, names(template_frame))
    if (length(missing_axes) > 0) {
      stop("raster field layer axis is not present in the template index: ",
           paste(missing_axes, collapse = ", "))
    }

    source_frame <- .field_layer_index(field)
    source_ids <- .ae_tuple_key(source_frame, source_layer_axes)
    template_ids <- .ae_tuple_key(template_frame, source_layer_axes)
    if (!all(template_ids %in% source_ids)) {
      stop("raster field is missing layer tuples present in the template index")
    }
    lifted <- base[[match(template_ids, source_ids)]]
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

#' Collapse a field by retaining selected axes
#'
#' `by` names the axes to keep. Every other axis is summed away. `by = NULL`
#' returns a scalar total.
#' @keywords internal
.ae_aggregate <- function(field, by = NULL) {
  .chck_class(field, "spax_field", "field")
  backend <- .field_backend(field)
  domain <- .field_domain(field)
  if (is.null(by)) {
    by <- character()
  }
  .chck_class(by, "character", "by")
  if (anyDuplicated(by)) {
    stop("by must contain unique axis names")
  }
  if (!all(by %in% domain)) {
    stop("by must be NULL or a subset of the field domain")
  }

  if (setequal(by, domain)) {
    return(field)
  }

  if (backend == "raster") {
    cell_axis <- .field_cell_axis(field)
    layer_axes <- .field_layer_axes(field)
    data <- .field_data(field)
    layer_frame <- .field_layer_index(field)

    if (length(by) == 0) {
      layer_sums <- terra::global(data, "sum", na.rm = TRUE)[[1]]
      return(sum(layer_sums, na.rm = TRUE))
    }

    new_domain <- domain[domain %in% by]
    if (cell_axis %in% by) {
      keep_layer_axes <- setdiff(new_domain, cell_axis)
      if (length(keep_layer_axes) == 0) {
        collapsed <- terra::app(data, fun = sum, na.rm = TRUE)
        names(collapsed) <- "tmp1"
        frame <- data.frame(layer = "tmp1", stringsAsFactors = FALSE)
        return(.spax_raster_field(
          collapsed,
          domain = new_domain,
          frame = frame,
          role = .field_role(field)
        ))
      }

      group_key <- .ae_tuple_key(layer_frame, keep_layer_axes)
      groups <- unique(group_key)
      layers <- lapply(groups, function(k) {
        idx <- which(group_key == k)
        if (length(idx) == 1) {
          data[[idx]]
        } else {
          terra::app(data[[idx]], fun = sum, na.rm = TRUE)
        }
      })
      collapsed <- terra::rast(layers)
      names(collapsed) <- paste0("tmp", seq_along(groups))
      frame <- layer_frame[match(groups, group_key), keep_layer_axes, drop = FALSE]
      rownames(frame) <- NULL
      frame <- data.frame(layer = names(collapsed), frame, stringsAsFactors = FALSE)
      return(.spax_raster_field(
        collapsed,
        domain = new_domain,
        frame = frame,
        role = .field_role(field)
      ))
    }

    missing_layer_axes <- setdiff(by, layer_axes)
    if (length(missing_layer_axes) > 0) {
      stop("by axes are not available after collapsing the raster cell axis: ",
           paste(missing_layer_axes, collapse = ", "))
    }

    layer_sums <- terra::global(data, "sum", na.rm = TRUE)[[1]]
    grouped <- .ae_sum_by_frame(layer_sums, layer_frame, new_domain)
    return(.spax_vector_field(
      grouped$values,
      domain = new_domain,
      frame = grouped$frame,
      role = .field_role(field)
    ))
  }

  if (backend == "vector") {
    if (length(by) == 0) {
      return(sum(.field_data(field), na.rm = TRUE))
    }
    new_domain <- domain[domain %in% by]
    grouped <- .ae_sum_by_frame(.field_data(field), .field_node_index(field),
                                new_domain)
    return(.spax_vector_field(
      grouped$values,
      domain = new_domain,
      frame = grouped$frame,
      role = .field_role(field)
    ))
  }

  stop("unsupported aggregate for backend '", backend, "'")
}

# Verb: normalize ------------------------------------------------------------

#' Convert a weight/kernel field into a mapping field
#'
#' `by` names the axes retained while every other axis competes. The output
#' keeps the input field's domain/backend/index and uses role "map".
#' @keywords internal
.ae_normalize <- function(field, by = NULL, method = "standard", a0 = 0) {
  .chck_class(field, "spax_field", "field")
  by <- .ae_normalize_args(field, by, method, a0)

  if (method == "identity") {
    return(.rewrap_field(field, .field_data(field), role = "map", meta = list()))
  }

  backend <- .field_backend(field)
  if (backend == "raster") {
    data <- if (.field_cell_axis(field) %in% by) {
      .ae_normalize_raster_by_cell(field, by, method = method, a0 = a0)
    } else {
      .ae_normalize_raster_by_global(field, by, method = method, a0 = a0)
    }
    return(.rewrap_field(field, data, role = "map", meta = list()))
  }

  if (backend == "vector") {
    data <- .ae_normalize_vector(field, by, method = method, a0 = a0)
    return(.rewrap_field(field, data, role = "map", meta = list()))
  }

  stop("unsupported normalize for backend '", backend, "'")
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
  cell_axis <- .field_cell_axis(weights)
  if (!identical(.field_domain(source), cell_axis) ||
      terra::nlyr(.field_data(source)) != 1) {
    stop("`source` must be a single-layer raster field on the weights cell axis")
  }
  layer_axes <- .field_layer_axes(weights)
  vals <- .gather_weighted_core(.field_data(source), .field_data(weights),
                                na.rm = TRUE)
  frame <- .field_layer_index(weights)[layer_axes]
  .spax_vector_field(unname(vals), domain = layer_axes, frame = frame,
                     role = "realization")
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
  lifted <- .ae_lift(source, template = weights)
  product <- .ae_combine(lifted, weights, op = `*`)
  out <- .ae_aggregate(product, by = .field_cell_axis(weights))
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

# Verb: mask ------------------------------------------------------------------

#' Apply a binary or soft compatibility mask
#' @keywords internal
.ae_mask <- function(field, mask) {
  .chck_class(field, "spax_field", "field")
  .chck_class(mask, "spax_field", "mask")
  values <- .field_data(mask)
  mask_values <- if (inherits(values, "SpatRaster")) {
    terra::values(values, mat = FALSE)
  } else {
    values
  }
  if (!(is.logical(mask_values) || is.numeric(mask_values))) {
    stop("mask values must be logical or numeric")
  }
  if (any(is.na(mask_values))) {
    stop("mask values must not be missing")
  }
  if (is.numeric(mask_values) && any(mask_values < 0 | mask_values > 1)) {
    stop("numeric mask values must be in [0, 1]")
  }

  if (setequal(.field_domain(mask), .field_domain(field))) {
    return(.ae_combine(field, mask, op = `*`))
  }
  if (all(.field_domain(mask) %in% .field_domain(field))) {
    return(.ae_combine(field, .ae_lift(mask, template = field), op = `*`))
  }
  stop("mask domain must match or be a subset of field domain")
}
