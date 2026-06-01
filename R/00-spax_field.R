# spax_field Object ----------------------------------------------------------

#' Valid semantic roles for spax_field objects
#' @keywords internal
.spax_field_roles <- function() {
  c(
    "unknown",
    "demand",
    "supply",
    "distance",
    "kernel",
    "opportunity",
    "map",
    "realization",
    "state",
    "mask",
    "weight"
  )
}

#' Validate common spax_field inputs
#' @keywords internal
.chck_spax_field <- function(data,
                             domain,
                             index = NULL,
                             role = "unknown",
                             meta = list()) {
  if (missing(data) || is.null(data)) {
    stop("data is required")
  }

  .chck_class(domain, "character", "domain")
  if (length(domain) == 0 || any(is.na(domain)) || any(domain == "")) {
    stop("domain must be a non-empty character vector with no missing or empty values")
  }
  if (anyDuplicated(domain)) {
    stop("domain must contain unique axis names")
  }

  .chck_class(role, "character", "role")
  .chck_length(length(role), 1, "role")
  if (is.na(role) || role == "") {
    stop("role must be a non-missing, non-empty character value")
  }
  if (!role %in% .spax_field_roles()) {
    warning(
      "role is not in known spax_field roles: ",
      paste(.spax_field_roles(), collapse = ", "),
      call. = FALSE
    )
  }

  .chck_class(meta, "list", "meta")
  if (!is.null(index)) {
    .chck_class(index, "list", "index")
    if (is.null(names(index)) || any(names(index) == "")) {
      stop("index must be a named list")
    }
    for (index_name in names(index)) {
      .chck_class(index[[index_name]], "data.frame", paste0("index$", index_name))
    }
  }

  invisible(TRUE)
}

#' Low-level spax_field constructor
#' @keywords internal
.new_spax_field <- function(data,
                            domain,
                            index = NULL,
                            role = "unknown",
                            meta = list(),
                            subclass = NULL) {
  if (is.null(index)) {
    index <- list()
  }

  classes <- c(subclass, "spax_field")
  structure(
    list(
      data = data,
      domain = domain,
      index = index,
      role = role,
      meta = meta
    ),
    class = classes
  )
}

# Raster Field ---------------------------------------------------------------

#' Create stable surrogate layer keys
#' @keywords internal
.field_layer_keys <- function(n) {
  paste0("L", seq_len(n))
}

#' Detect whether raster names look like terra-generated defaults
#' @keywords internal
.field_has_semantic_layer_names <- function(x) {
  layer_names <- names(x)
  if (is.null(layer_names) || any(is.na(layer_names)) || any(layer_names == "")) {
    return(FALSE)
  }
  !all(grepl("^lyr\\.[0-9]+$", layer_names))
}

#' Prepare a raster layer frame and re-key raster layers
#' @keywords internal
.prepare_raster_layer_index <- function(data,
                                        domain,
                                        frame = NULL,
                                        allow_positional = FALSE,
                                        meta = list()) {
  n_layers <- terra::nlyr(data)
  non_i_axes <- setdiff(domain, "I")
  keys <- .field_layer_keys(n_layers)
  provenance <- character()

  if (length(non_i_axes) == 0 && n_layers > 1) {
    stop("multi-layer raster fields must declare at least one non-I axis")
  }

  if (is.null(frame)) {
    if (length(non_i_axes) == 0) {
      frame <- data.frame(layer = keys, stringsAsFactors = FALSE)
    } else if (length(non_i_axes) == 1 && .field_has_semantic_layer_names(data)) {
      frame <- data.frame(
        layer = keys,
        stringsAsFactors = FALSE
      )
      frame[[non_i_axes]] <- names(data)
      provenance <- c(provenance, "semantic layer names lifted into index$layer")
    } else if (allow_positional) {
      frame <- data.frame(
        layer = keys,
        stringsAsFactors = FALSE
      )
      for (axis in non_i_axes) {
        frame[[axis]] <- paste0(axis, "_", seq_len(n_layers))
      }
      provenance <- c(provenance, "positional semantic IDs generated")
    } else {
      stop("semantic axis IDs are required; provide index$layer or use allow_positional = TRUE")
    }
  } else {
    .chck_class(frame, "data.frame", "index$layer")
    if (nrow(frame) != n_layers) {
      stop("index$layer must have one row per raster layer")
    }

    if ("layer" %in% names(frame)) {
      frame_keys <- as.character(frame$layer)
      if (any(is.na(frame_keys)) || any(frame_keys == "")) {
        stop("index$layer$layer must not contain missing or empty values")
      }
      if (anyDuplicated(frame_keys)) {
        stop("index$layer$layer must contain unique values")
      }
      if (.field_has_semantic_layer_names(data)) {
        # A provided key is a join key, not a positional hint: it must match the
        # raster layer names, and we reorder the frame to follow layer order so
        # coordinates bind to the correct layer regardless of row order.
        if (!setequal(names(data), frame_keys)) {
          stop("index$layer$layer must match raster layer names")
        }
        frame <- frame[match(names(data), frame_keys), , drop = FALSE]
        rownames(frame) <- NULL
      }
      # else: raster layers carry no semantic names to join on, so the provided
      # frame binds by row order (the only available linkage).
    } else {
      warning(
        "index$layer has no layer column; assuming row order",
        call. = FALSE
      )
      provenance <- c(provenance, "layer linkage assumed by row order")
    }

    frame$layer <- keys
  }

  missing_axes <- setdiff(non_i_axes, names(frame))
  if (length(missing_axes) > 0) {
    stop("index$layer missing axis column(s): ", paste(missing_axes, collapse = ", "))
  }

  frame$layer <- as.character(frame$layer)
  for (axis in non_i_axes) {
    frame[[axis]] <- as.character(frame[[axis]])
    if (any(is.na(frame[[axis]]) | frame[[axis]] == "")) {
      stop("index$layer$", axis, " must not contain missing or empty values")
    }
  }

  if (length(non_i_axes) > 0) {
    coordinate_frame <- frame[non_i_axes]
    duplicate_coordinates <- duplicated(coordinate_frame)
    if (any(duplicate_coordinates)) {
      stop("index$layer contains duplicate coordinate tuples")
    }
  }

  if (anyDuplicated(frame$layer)) {
    stop("index$layer$layer must contain unique values")
  }

  names(data) <- keys
  if (length(provenance) > 0) {
    meta$provenance <- unique(c(meta$provenance, provenance))
  }

  list(data = data, frame = frame, meta = meta)
}

#' Validate raster spax_field inputs
#' @keywords internal
.chck_spax_raster_field <- function(data,
                                    domain,
                                    index = NULL,
                                    role = "unknown",
                                    meta = list()) {
  .chck_spax_field(data, domain, index, role, meta)
  .chck_is_raster(data, "data")

  if (!"I" %in% domain) {
    stop("spax_raster_field domain must include I")
  }

  if (is.null(index) || is.null(index$layer)) {
    stop("spax_raster_field requires index$layer")
  }

  frame <- index$layer
  if (nrow(frame) != terra::nlyr(data)) {
    stop("index$layer must have one row per raster layer")
  }
  if (!"layer" %in% names(frame)) {
    stop("index$layer must include a layer column")
  }
  if (!identical(as.character(frame$layer), names(data))) {
    stop("index$layer$layer must match raster layer names")
  }

  non_i_axes <- setdiff(domain, "I")
  missing_axes <- setdiff(non_i_axes, names(frame))
  if (length(missing_axes) > 0) {
    stop("index$layer missing axis column(s): ", paste(missing_axes, collapse = ", "))
  }
  if (length(non_i_axes) == 0 && terra::nlyr(data) > 1) {
    stop("multi-layer raster fields must declare at least one non-I axis")
  }

  for (axis in c("layer", non_i_axes)) {
    values <- frame[[axis]]
    if (!is.character(values)) {
      stop("index$layer$", axis, " must be character")
    }
    if (any(is.na(values) | values == "")) {
      stop("index$layer$", axis, " must not contain missing or empty values")
    }
  }

  if (anyDuplicated(frame$layer)) {
    stop("index$layer$layer must contain unique values")
  }
  if (length(non_i_axes) > 0 && any(duplicated(frame[non_i_axes]))) {
    stop("index$layer contains duplicate coordinate tuples")
  }

  invisible(TRUE)
}

#' Low-level raster spax_field constructor
#' @keywords internal
.new_spax_raster_field <- function(data,
                                   domain,
                                   index,
                                   role = "unknown",
                                   meta = list()) {
  .new_spax_field(
    data = data,
    domain = domain,
    index = index,
    role = role,
    meta = meta,
    subclass = "spax_raster_field"
  )
}

#' Create a raster spax_field
#' @keywords internal
.create_spax_raster_field <- function(data,
                                      domain,
                                      index = NULL,
                                      frame = NULL,
                                      role = "unknown",
                                      meta = list(),
                                      allow_positional = FALSE,
                                      snap = FALSE) {
  if (!is.null(index) && !is.null(index$layer) && is.null(frame)) {
    frame <- index$layer
  }

  prepared <- .prepare_raster_layer_index(
    data = data,
    domain = domain,
    frame = frame,
    allow_positional = allow_positional,
    meta = meta
  )
  index <- list(layer = prepared$frame)

  if (!snap) {
    .chck_spax_raster_field(
      data = prepared$data,
      domain = domain,
      index = index,
      role = role,
      meta = prepared$meta
    )
  }

  .new_spax_raster_field(
    data = prepared$data,
    domain = domain,
    index = index,
    role = role,
    meta = prepared$meta
  )
}

# Vector Field ---------------------------------------------------------------

#' Create stable surrogate vector keys
#' @keywords internal
.field_node_keys <- function(n) {
  paste0("V", seq_len(n))
}

#' Prepare a vector index frame and re-key vector values
#' @keywords internal
.prepare_vector_index <- function(data,
                                  domain,
                                  frame = NULL,
                                  allow_positional = FALSE,
                                  meta = list()) {
  n <- length(data)
  keys <- .field_node_keys(n)
  provenance <- character()

  if (is.null(frame)) {
    if (length(domain) != 1) {
      stop("multi-axis vector fields require an explicit index frame")
    }

    axis_values <- names(data)
    if (is.null(axis_values) || any(is.na(axis_values)) || any(axis_values == "")) {
      if (!allow_positional) {
        stop("spax_vector_field data must have names unless allow_positional = TRUE")
      }
      axis_values <- paste0(domain[[1]], "_", seq_along(data))
      provenance <- c(provenance, "positional semantic IDs generated")
    } else {
      provenance <- c(provenance, "semantic vector names lifted into index$node")
    }

    frame <- data.frame(key = keys, stringsAsFactors = FALSE)
    frame[[domain[[1]]]] <- axis_values
  } else {
    .chck_class(frame, "data.frame", "index$node")
    if (nrow(frame) != n) {
      stop("index$node must have one row per vector element")
    }

    if ("key" %in% names(frame)) {
      frame_keys <- as.character(frame$key)
      if (any(is.na(frame_keys)) || any(frame_keys == "")) {
        stop("index$node$key must not contain missing or empty values")
      }
      if (anyDuplicated(frame_keys)) {
        stop("index$node$key must contain unique values")
      }
      if (!is.null(names(data)) && all(!is.na(names(data))) && all(names(data) != "")) {
        if (!setequal(names(data), frame_keys)) {
          stop("index$node$key must match vector names when both are provided")
        }
        frame <- frame[match(names(data), frame_keys), , drop = FALSE]
        rownames(frame) <- NULL
      }
    } else {
      # Unlike raster layers (which carry an intrinsic order the frame may not
      # match), an unnamed value vector is paired with its frame positionally by
      # the caller, so row order is the contract, not a guess. Only warn when the
      # data carries names -- then row order overrides a real second ordering,
      # the same risky assumption raster warns about (D5: loud about position).
      if (!is.null(names(data)) &&
          all(!is.na(names(data))) && all(names(data) != "")) {
        warning(
          "index$node has no key column; assuming row order",
          call. = FALSE
        )
      }
      provenance <- c(provenance, "node linkage assumed by row order")
    }

    frame$key <- keys
  }

  missing_axes <- setdiff(domain, names(frame))
  if (length(missing_axes) > 0) {
    stop("index$node missing axis column(s): ", paste(missing_axes, collapse = ", "))
  }

  frame$key <- as.character(frame$key)
  for (axis in domain) {
    frame[[axis]] <- as.character(frame[[axis]])
    if (any(is.na(frame[[axis]]) | frame[[axis]] == "")) {
      stop("index$node$", axis, " must not contain missing or empty values")
    }
  }

  if (any(duplicated(frame[domain]))) {
    stop("index$node contains duplicate coordinate tuples")
  }
  if (anyDuplicated(frame$key)) {
    stop("index$node$key must contain unique values")
  }

  frame <- frame[c("key", domain, setdiff(names(frame), c("key", domain)))]
  names(data) <- keys
  if (length(provenance) > 0) {
    meta$provenance <- unique(c(meta$provenance, provenance))
  }

  list(data = data, frame = frame, meta = meta)
}

#' Validate vector spax_field inputs
#' @keywords internal
.chck_spax_vector_field <- function(data,
                                    domain,
                                    index = NULL,
                                    role = "unknown",
                                    meta = list()) {
  .chck_spax_field(data, domain, index, role, meta)

  if (!is.atomic(data) || !is.null(dim(data))) {
    stop("data must be an atomic vector")
  }
  if (length(domain) < 1) {
    stop("spax_vector_field domain must have length at least one")
  }

  if (is.null(index) || is.null(index$node)) {
    stop("spax_vector_field requires index$node")
  }

  frame <- index$node
  if (nrow(frame) != length(data)) {
    stop("index$node must have one row per vector element")
  }
  if (!"key" %in% names(frame)) {
    stop("index$node must include a key column")
  }
  if (!identical(as.character(frame$key), names(data))) {
    stop("index$node$key must match vector names")
  }

  missing_axes <- setdiff(domain, names(frame))
  if (length(missing_axes) > 0) {
    stop("index$node missing axis column(s): ", paste(missing_axes, collapse = ", "))
  }

  for (axis in c("key", domain)) {
    values <- frame[[axis]]
    if (!is.character(values)) {
      stop("index$node$", axis, " must be character")
    }
    if (any(is.na(values) | values == "")) {
      stop("index$node$", axis, " must not contain missing or empty values")
    }
  }

  if (anyDuplicated(frame$key)) {
    stop("index$node$key must contain unique values")
  }
  if (any(duplicated(frame[domain]))) {
    stop("index$node contains duplicate coordinate tuples")
  }

  invisible(TRUE)
}

#' Low-level vector spax_field constructor
#' @keywords internal
.new_spax_vector_field <- function(data,
                                   domain,
                                   index = NULL,
                                   role = "unknown",
                                   meta = list()) {
  .new_spax_field(
    data = data,
    domain = domain,
    index = index,
    role = role,
    meta = meta,
    subclass = "spax_vector_field"
  )
}

#' Create a vector spax_field
#' @keywords internal
.create_spax_vector_field <- function(data,
                                      domain,
                                      index = NULL,
                                      frame = NULL,
                                      role = "unknown",
                                      meta = list(),
                                      allow_positional = FALSE,
                                      snap = FALSE) {
  if (!is.null(index) && !is.null(index$node) && is.null(frame)) {
    frame <- index$node
  }

  prepared <- .prepare_vector_index(
    data = data,
    domain = domain,
    frame = frame,
    allow_positional = allow_positional,
    meta = meta
  )
  index <- list(node = prepared$frame)

  if (!snap) {
    .chck_spax_vector_field(
      data = prepared$data,
      domain = domain,
      index = index,
      role = role,
      meta = prepared$meta
    )
  }

  .new_spax_vector_field(prepared$data, domain, index, role, prepared$meta)
}

# Coercion -------------------------------------------------------------------

#' Coerce raw data or pass through an existing field
#'
#' Internal DEC-008 two-door input helper. Raw inputs go through the validated
#' backend constructors; prepared fields are trusted after a domain check.
#' @keywords internal
.as_spax_field <- function(x,
                           domain,
                           role = "unknown",
                           frame = NULL,
                           allow_positional = FALSE,
                           snap = FALSE) {
  .chck_class(domain, "character", "domain")

  if (inherits(x, "spax_field")) {
    if (!setequal(.field_domain(x), domain)) {
      stop("field domain must match requested domain")
    }
    return(x)
  }

  if (inherits(x, "SpatRaster")) {
    return(.create_spax_raster_field(
      data = x,
      domain = domain,
      frame = frame,
      role = role,
      allow_positional = allow_positional,
      snap = snap
    ))
  }

  if (is.atomic(x) && is.null(dim(x))) {
    return(.create_spax_vector_field(
      data = x,
      domain = domain,
      frame = frame,
      role = role,
      allow_positional = allow_positional,
      snap = snap
    ))
  }

  stop("unsupported input type for spax_field coercion")
}

# Accessors ------------------------------------------------------------------

#' @keywords internal
.field_data <- function(field) {
  .chck_class(field, "spax_field", "field")
  field$data
}

#' @keywords internal
.field_domain <- function(field) {
  .chck_class(field, "spax_field", "field")
  field$domain
}

#' @keywords internal
.field_index <- function(field) {
  .chck_class(field, "spax_field", "field")
  field$index
}

#' @keywords internal
.field_role <- function(field) {
  .chck_class(field, "spax_field", "field")
  field$role
}

#' @keywords internal
.field_meta <- function(field) {
  .chck_class(field, "spax_field", "field")
  field$meta
}

#' @keywords internal
.field_backend <- function(field) {
  .chck_class(field, "spax_field", "field")
  backend_classes <- class(field)[grepl("^spax_.*_field$", class(field))]
  if (length(backend_classes) == 0) {
    return("unknown")
  }
  backend_class <- backend_classes[[1]]
  sub("^spax_(.*)_field$", "\\1", backend_class)
}

#' @keywords internal
.field_layer_index <- function(field) {
  .chck_class(field, "spax_raster_field", "field")
  field$index$layer
}

#' @keywords internal
.field_node_index <- function(field) {
  .chck_class(field, "spax_vector_field", "field")
  field$index$node
}

#' Backend-neutral index frame accessor
#' @keywords internal
.field_index_frame <- function(field) {
  .chck_class(field, "spax_field", "field")
  if (inherits(field, "spax_raster_field")) {
    return(.field_layer_index(field))
  }
  if (inherits(field, "spax_vector_field")) {
    return(.field_node_index(field))
  }
  stop("index frame is not implemented for this field backend")
}

#' @keywords internal
.field_axis_values <- function(field, axis) {
  .chck_class(field, "spax_field", "field")
  .chck_class(axis, "character", "axis")
  .chck_length(length(axis), 1, "axis")

  if (!axis %in% field$domain) {
    stop("axis must be in field domain")
  }

  if (identical(axis, "I") && inherits(field, "spax_raster_field")) {
    return(NULL)
  }

  if (inherits(field, "spax_raster_field")) {
    frame <- .field_layer_index(field)
    return(unique(frame[[axis]]))
  }

  if (inherits(field, "spax_vector_field")) {
    frame <- .field_node_index(field)
    return(unique(frame[[axis]]))
  }

  stop("axis values are not implemented for this field backend")
}

# Print ----------------------------------------------------------------------

#' Print a spax_field
#' @param x A spax_field object
#' @param ... Unused
#' @return Invisibly returns `x`
#' @export
print.spax_field <- function(x, ...) {
  cat("<spax_field>\n")
  cat("  backend: ", .field_backend(x), "\n", sep = "")
  cat("  domain:  ", paste(.field_domain(x), collapse = ", "), "\n", sep = "")
  cat("  role:    ", .field_role(x), "\n", sep = "")

  if (inherits(x, "spax_raster_field")) {
    frame <- .field_layer_index(x)
    cat("  layers:  ", nrow(frame), "\n", sep = "")
    cat("  frame:\n")
    print(utils::head(frame, 6), row.names = FALSE)
  } else if (inherits(x, "spax_vector_field")) {
    frame <- .field_node_index(x)
    cat("  length:  ", length(.field_data(x)), "\n", sep = "")
    cat("  frame:\n")
    print(utils::head(frame, 6), row.names = FALSE)
  }

  invisible(x)
}
