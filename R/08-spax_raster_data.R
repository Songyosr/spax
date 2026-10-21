# Raster-backed data and FCA specifications on spax() (SPAX-060, V2) ----------------

# The kind of inputs a `demand` argument of prep_data() is: "raster" for a SpatRaster or
# a nonempty list whose elements are all SpatRasters (demand groups), "numeric" for
# anything else, which the numeric preparation checks as it always did. A list with
# some SpatRasters and some other elements is neither, and is an error.
.prep_data_mode <- function(demand) {
  if (inherits(demand, "SpatRaster")) {
    return("raster")
  }
  if (is.list(demand) && length(demand)) {
    is_raster <- vapply(demand, inherits, logical(1), what = "SpatRaster")
    if (all(is_raster)) {
      return("raster")
    }
    if (any(is_raster)) {
      stop("demand must be a raster, a numeric vector, or a named list of either",
           call. = FALSE)
    }
  }
  "numeric"
}

# Raster data of prep_data(): validation, then the object. The checks are those of the
# FCA wrappers (see .chck_e2sfca_data()); no value of a raster is read here (the demand
# rule, finite and nonnegative, stays with the engine).
.prep_raster_data <- function(demand, supply, travel, id_col, supply_cols, metadata) {
  groups <- .fca_demand_groups(demand)
  .chck_e2sfca_data(demand, supply, travel, id_col, supply_cols, arg = "travel")
  first <- .fca_first_stack(travel, groups, "travel")
  facility_ids <- .fca_ids(first, "travel")
  metadata <- .allocation_metadata(metadata)
  grid_of <- if (is.null(groups)) demand else demand[[1L]]
  structure(
    list(
      demand = demand,
      travel = travel,
      supply = supply,
      id_col = id_col,
      supply_cols = supply_cols,
      facility_ids = facility_ids,
      groups = groups,
      metadata = metadata,
      grid = list(
        dim = c(nrow = terra::nrow(grid_of), ncol = terra::ncol(grid_of)),
        res = terra::res(grid_of),
        crs = terra::crs(grid_of)
      )
    ),
    class = c("spax_raster_data", "spax_data")
  )
}

# The rasters of raster data are terra objects that point to C++ memory. They do not
# survive saveRDS()/readRDS(): the pointer is then NULL and any use of the raster
# fails. Whether every raster of the data can still be used.
.raster_data_alive <- function(x) {
  rasters <- c(
    if (inherits(x$demand, "SpatRaster")) list(x$demand) else unname(x$demand),
    if (inherits(x$travel, "SpatRaster")) list(x$travel) else unname(x$travel)
  )
  all(vapply(rasters, function(r) {
    tryCatch({
      terra::nlyr(r)
      TRUE
    }, error = function(e) FALSE)
  }, logical(1)))
}

# The rasters inside a specification (a raster sigma, shared or by group) are alive.
.spec_rasters_alive <- function(x) {
  if (inherits(x, "SpatRaster")) return(.raster_data_alive(list(demand = x, travel = list())))
  if (is.list(x)) return(all(vapply(x, .spec_rasters_alive, logical(1))))
  TRUE
}

# The names of the supply measures of raster data: the supply columns of a data frame,
# the column names of a matrix (measure_1, ... without names) and "supply" for a vector.
.raster_data_measures <- function(x) {
  supply <- x$supply
  if (is.data.frame(supply)) {
    x$supply_cols
  } else if (is.matrix(supply)) {
    colnames(supply) %||% paste0("measure_", seq_len(ncol(supply)))
  } else {
    "supply"
  }
}

#' @rdname prep_data
#' @method format spax_raster_data
#' @export
#' @md
format.spax_raster_data <- function(x, ...) {
  line <- function(label, text) sprintf("  %-10s%s", label, text)
  count <- function(n, one, many) paste(.format_total(n), if (n == 1L) one else many)
  groups <- x$groups
  demand <- if (is.null(groups)) {
    count(1L, "layer", "layers")
  } else {
    paste0(count(length(groups), "group", "groups"), ": ", paste(groups, collapse = ", "))
  }
  # One layer per facility; no raster is touched, so data whose rasters are gone
  # (see .raster_data_alive()) still prints.
  travel <- paste0(count(length(x$facility_ids), "layer", "layers"),
                   if (!inherits(x$travel, "SpatRaster")) " per group")
  # The unit labels that were given; they are caller declarations, never converted.
  meta <- x$metadata
  label <- function(units, period) {
    given <- function(value) {
      if (is.character(value) && length(value) == 1L && !is.na(value)) value
    }
    units <- given(units)
    period <- given(period)
    if (!is.null(units) || !is.null(period)) {
      paste(c(units, if (!is.null(period)) paste0("(", period, ")")), collapse = " ")
    }
  }
  units <- c(demand = label(meta$demand_units, meta$demand_period),
             supply = label(meta$supply_units, meta$supply_period),
             travel = label(meta$travel_units, NULL))
  units <- units[!vapply(units, is.null, logical(1))]
  c(paste0("<spax data> raster ", .format_total(x$grid$dim[["nrow"]]), " x ",
           .format_total(x$grid$dim[["ncol"]]), " cells x ",
           count(length(x$facility_ids), "facility", "facilities"), "; supply ",
           paste(.raster_data_measures(x), collapse = ", ")),
    line("demand", paste0(demand, "; travel ", travel)),
    if (length(units)) {
      line("units", paste(paste(names(units), unlist(units, use.names = FALSE)),
                          collapse = "; "))
    },
    line("note", "holds terra rasters: prepare the data again after saveRDS()"))
}

#' @rdname prep_data
#' @method print spax_raster_data
#' @export
#' @md
print.spax_raster_data <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

# The FCA specification of a call to spax(), with its group arguments bound to the groups
# of the data: `decay`, `selection` (a named list by group) and `threshold` (a named
# vector by group) need data with groups, must name exactly its groups, and are put in the
# order of the groups. A decay, a selection or a threshold that is shared is not changed.
.fca_bind_groups <- function(spec, groups) {
  for (arg in c("decay", "selection", "threshold")) {
    x <- spec[[arg]]
    by_group <- if (identical(arg, "threshold")) {
      !is.null(names(x))
    } else {
      is.list(x) && !is.object(x)
    }
    if (!by_group) next
    # One named threshold (from quantile(), say) on data without groups is a plain value.
    if (is.null(groups) && identical(arg, "threshold") && length(x) == 1L) {
      spec[[arg]] <- unname(x)
      next
    }
    given <- names(x)
    if (is.null(groups)) {
      stop(arg, " needs data with demand groups", call. = FALSE)
    }
    if (!setequal(given, groups)) {
      stop(arg, " is given for groups ", paste(given, collapse = ", "),
           " but the data has groups ", paste(groups, collapse = ", "), call. = FALSE)
    }
    spec[[arg]] <- x[groups]
  }
  spec
}

# An FCA specification run on raster data: the arguments of the specification (see
# .fca_spec_args()) and the data go to the shared runner, as the wrapper of the same
# index does after its validation. `call` is the call of spax().
.spax_fca_raster <- function(data, spec, call) {
  args <- .fca_spec_args(.fca_bind_groups(spec, data$groups))
  .fca_run(
    args$kind, demand = data$demand, supply = data$supply, distance = data$travel,
    decay_params = args$decay_params, selection_params = args$selection_params,
    attractiveness = args$attractiveness, alpha = args$alpha,
    demand_normalize = args$demand_normalize, id_col = data$id_col,
    supply_cols = data$supply_cols, indicator_names = NULL, snap = FALSE, call = call
  )
}
