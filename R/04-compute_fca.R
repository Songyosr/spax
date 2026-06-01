# SPAX-004: typed FCA engine over spax_field -------------------------------

#' Return the non-I axis and IDs for a kernel field
#' @keywords internal
.fca_kernel_axis_ids <- function(kernel) {
  axis <- .ae_nonI_axis(kernel)
  ids <- as.character(.field_layer_index(kernel)[[axis]])
  list(axis = axis, ids = ids)
}

#' Validate demand/access kernels share one typed facility axis
#' @keywords internal
.fca_check_kernel_pair <- function(demand_kernel, access_kernel) {
  demand_axis <- .fca_kernel_axis_ids(demand_kernel)
  access_axis <- .fca_kernel_axis_ids(access_kernel)

  if (!identical(demand_axis$axis, access_axis$axis)) {
    stop("demand and access kernels must use the same non-I axis")
  }
  if (!setequal(demand_axis$ids, access_axis$ids)) {
    stop("demand and access kernels must span the same facility ids")
  }

  invisible(TRUE)
}

#' Coerce supply values to one vector field per measure
#' @keywords internal
.fca_supply_fields <- function(supply, kernel, id_col = NULL,
                               supply_cols = NULL, indicator_names = NULL) {
  kernel_axis <- .fca_kernel_axis_ids(kernel)
  axis <- kernel_axis$axis
  weight_ids <- kernel_axis$ids

  if (inherits(supply, "spax_vector_field")) {
    if (!identical(.field_domain(supply), axis)) {
      stop("`supply` field must use the same axis as the kernels")
    }
    if (!setequal(names(.field_data(supply)), weight_ids)) {
      stop("`supply` field must span the same facility ids as the kernels")
    }
    output_names <- if (is.null(indicator_names)) "supply" else indicator_names
    return(list(fields = list(supply), names = output_names))
  }

  processed <- .help_process_supply(
    supply = supply,
    id_col = id_col,
    supply_cols = supply_cols,
    weight_ids = weight_ids
  )
  values <- as.matrix(processed$values)
  output_names <- if (is.null(indicator_names)) processed$cols else indicator_names

  fields <- lapply(seq_len(ncol(values)), function(k) {
    vals <- values[, k]
    names(vals) <- processed$ids
    .create_spax_vector_field(vals, domain = axis, role = "supply")
  })
  list(fields = fields, names = output_names)
}

#' Unwrap a compute_fca result field as a public SpatRaster
#' @keywords internal
.fca_result_raster <- function(field) {
  .chck_class(field, "spax_raster_field", "field")
  result <- .field_data(field)
  output_names <- .field_meta(field)$output_names
  if (!is.null(output_names)) {
    names(result) <- output_names
  }
  result
}

#' Internal typed FCA engine
#'
#' Implements normalize -> gather -> ratio -> spread over spax_field inputs.
#' Public wrappers unwrap the returned field to preserve existing return types.
#' @keywords internal
compute_fca <- function(demand, supply, demand_kernel, access_kernel,
                        demand_normalize = "identity",
                        id_col = NULL, supply_cols = NULL,
                        indicator_names = NULL,
                        snap = FALSE) {
  demand <- .as_spax_field(demand, domain = "I", role = "demand", snap = snap)
  demand_kernel <- .as_spax_field(
    demand_kernel,
    domain = c("I", "facility"),
    role = "kernel",
    allow_positional = TRUE,
    snap = snap
  )
  access_kernel <- .as_spax_field(
    access_kernel,
    domain = c("I", "facility"),
    role = "kernel",
    allow_positional = TRUE,
    snap = snap
  )
  .fca_check_kernel_pair(demand_kernel, access_kernel)

  demand_kernel <- .ae_normalize(demand_kernel, method = demand_normalize)
  potential_demand <- .ae_gather(demand, demand_kernel)
  supply_fields <- .fca_supply_fields(
    supply = supply,
    kernel = demand_kernel,
    id_col = id_col,
    supply_cols = supply_cols,
    indicator_names = indicator_names
  )

  access_fields <- lapply(supply_fields$fields, function(supply_field) {
    ratios <- .ae_ratio(supply_field, potential_demand, zero = 0)
    .ae_spread(ratios, access_kernel)
  })

  access_rasters <- lapply(access_fields, .field_data)
  result <- do.call(c, access_rasters)
  names(result) <- supply_fields$names

  if (length(access_fields) == 1) {
    return(.create_spax_raster_field(result, domain = "I",
                                     role = "realization",
                                     meta = list(output_names = supply_fields$names),
                                     snap = snap))
  }

  frame <- data.frame(
    layer = supply_fields$names,
    measure = supply_fields$names,
    stringsAsFactors = FALSE
  )
  .create_spax_raster_field(
    result,
    domain = c("I", "measure"),
    frame = frame,
    role = "realization",
    meta = list(output_names = supply_fields$names),
    snap = snap
  )
}
