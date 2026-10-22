# FCA indices on numeric data (SPAX-060, V3) -----------------------------------------------
#
# spax(prep_data(demand, supply, travel), spec_e2sfca()) runs the same engine as the
# raster route on the active origins of numeric data, fed from memory: the two passes, the
# NA rule and the overall layer of demand groups are the code of .fca_blocks().

# Whether a raster (a variable catchment: a raster `sigma`) is anywhere in the decay
# parameters, which are one list or a list of them by group.
.fca_params_raster_sigma <- function(params) {
  if (inherits(params, "SpatRaster")) return(TRUE)
  is.list(params) && !is.object(params) &&
    any(vapply(params, .fca_params_raster_sigma, logical(1)))
}

# The shared runner of an FCA index on numeric data: the counterpart of .fca_run() for the
# active origins of `data` (an `ae_prepared_inputs`), with the arguments of .fca_run(). The
# supply of numeric data is one measure, named "supply". Returns a `spax_access_values`;
# `call` is stored as given.
.fca_numeric_run <- function(kind, data, decay_params, selection_params = NULL,
                             attractiveness = NULL, alpha = 1,
                             demand_normalize = "identity", call = NULL) {
  info <- .fca_kind_info(kind)
  if (ncol(data$S) != 1L) {
    stop("FCA on numeric data uses one supply measure", call. = FALSE)
  }
  groups <- data$group_ids
  ids <- data$facility_ids
  supply <- stats::setNames(as.vector(data$S[, 1L]), ids)

  if (identical(info$type, "huff")) {
    if (!is.null(attractiveness) &&
        (anyDuplicated(names(attractiveness)) || !setequal(names(attractiveness), ids))) {
      stop("names of attractiveness must be the facility IDs of the data, one for ",
           "each facility", call. = FALSE)
    }
    attractiveness <- .fca_attractiveness_ids(attractiveness, supply, ids, NULL, NULL,
                                              FALSE)
  }

  # The kernels of each group on its own travel, and the in-memory block they read: the
  # demand of the origins and their travel under the keys of the raster route.
  if (is.null(groups)) {
    parts <- .fca_group_kernels(info$type, data$distance_active, decay_params,
                                selection_params, attractiveness, alpha, snap = FALSE)
    kernels <- parts$kernels
    block <- list(demand = data$D_active, distance = data$distance_active)
  } else {
    decay <- .fca_expand_groups(decay_params, groups, "decay_params")
    selection <- if (!is.null(selection_params)) {
      .fca_expand_groups(selection_params, groups, "selection_params")
    }
    travel <- data$distance_groups
    parts <- lapply(seq_along(groups), function(i) {
      .fca_group_kernels(info$type, travel[[groups[i]]], decay[[i]], selection[[i]],
                         attractiveness, alpha, snap = FALSE,
                         suffix = paste0(".", groups[i]))
    })
    kernels <- stats::setNames(lapply(parts, `[[`, "kernels"), groups)
    block <- c(
      stats::setNames(lapply(groups, function(g) data$D_groups[, g]),
                      paste0("demand.", groups)),
      stats::setNames(travel[groups], paste0("distance.", groups))
    )
  }

  values <- .fca_blocks(rasters = NULL, kernels = kernels,
                        supply = matrix(supply, ncol = 1L), normalize = demand_normalize,
                        snap = FALSE, groups = groups, block = block)
  layers <- .fca_layer_table("supply", groups)
  dimnames(values) <- list(data$origin_ids, layers$layer)
  if (!is.null(groups)) {
    attr(values, "layers") <- layers
  }
  out <- .fca_group_result(values, list(
    decay_params = decay_params,
    demand_normalize = demand_normalize,
    selection_params = selection_params,
    attractiveness = attractiveness,
    alpha = alpha
  )[info$parameter_names])
  structure(
    list(access = out$accessibility, type = info$label, parameters = out$parameters,
         facilities = .fca_facility_table(supply, NULL, NULL), call = call),
    class = c("spax_access_values", "spax_result")
  )
}

# An FCA specification run on numeric data: the arguments of the specification (see
# .fca_spec_args()) and the active origins of the data go to the runner. `call` is the call
# of spax().
.spax_fca_numeric <- function(data, spec, call) {
  args <- .fca_spec_args(.fca_bind_groups(spec, data$group_ids))
  if (.fca_params_raster_sigma(args$decay_params) ||
      .fca_params_raster_sigma(args$selection_params)) {
    stop("a raster sigma needs raster data", call. = FALSE)
  }
  .fca_numeric_run(
    args$kind, data, decay_params = args$decay_params,
    selection_params = args$selection_params, attractiveness = args$attractiveness,
    alpha = args$alpha, demand_normalize = args$demand_normalize, call = call
  )
}

# Methods ------------------------------------------------------------------------------

# The decay (one line per group when it is given by group) and the demand normalization of
# an FCA result, as printed lines with the label `decay` or `normalize`.
.access_values_parameter_lines <- function(parameters, line) {
  one <- function(p) {
    sigma <- p$sigma
    paste0(.decay_kernel_label(p$method), if (!is.null(sigma)) {
      paste0(" (sigma = ", format(sigma, digits = 7L, trim = TRUE), ")")
    })
  }
  decay <- parameters$decay_params
  per <- if (!is.null(parameters$groups)) {
    .fca_per_group(decay, parameters$groups, "decay_params")
  }
  c(if (is.null(per)) {
    line("decay", one(decay))
  } else {
    line("decay", paste0("by group: ", paste(paste0(names(per), " ", vapply(per, one, "")),
                                             collapse = ", ")))
  },
  if (!is.null(parameters$demand_normalize)) {
    line("normalize", parameters$demand_normalize)
  })
}

#' Methods for FCA indices on numeric data
#'
#' What to do with the result of [spax()] for an FCA specification ([spec_e2sfca()] and the
#' others) on numeric data ([prep_data()] with demand, supply and a travel matrix): a list
#' with `access`, a matrix with one row per active origin (named by the origin IDs) and one
#' column per layer, `type`, `parameters`, `facilities` and `call`. With demand groups the
#' columns are `supply.<group>` and `supply.all`, the layers of the raster route. Origins
#' that are not in the data (zero demand, unknown demand, left out) have no row and so no
#' value.
#'
#' * `print()` shows the type, the layers, the decay and the normalization.
#' * `summary()` returns a `summary.spax_access_values`: for each layer the number of
#'   origins with a value, its minimum, mean and maximum, and the coverage, the origins
#'   whose access is above zero and their proportion.
#' * `as.data.frame()` gives the table of the access values: `origin_id` and one column per
#'   layer.
#'
#' @param x,object A `spax_access_values`, or for `print()` of the summary, a
#'   `summary.spax_access_values`.
#' @param row.names,optional Standard `as.data.frame()` arguments.
#' @param digits Number of significant digits printed in the summary.
#' @param ... Unused.
#' @return `print()` returns its argument invisibly, `summary()` a
#'   `summary.spax_access_values` and `as.data.frame()` a data frame.
#' @seealso [spax()], [prep_data()]
#' @examples
#' # Mainland Cornwall: residents by small area, GP FTE by practice and the
#' # travel minutes between them
#' areas <- sf::st_drop_geometry(cornwall_lsoa[!cornwall_lsoa$scilly, ])
#' gp <- sf::st_drop_geometry(cornwall_gp[!cornwall_gp$scilly, ])
#' d <- prep_data(stats::setNames(areas$pop, areas$lsoa),
#'                stats::setNames(gp$gp_fte_extg, gp$practice),
#'                cornwall_travel[areas$lsoa, gp$practice])
#'
#' # E2SFCA with a Gaussian decay of 10 minutes, at the small areas
#' access <- spax(d, spec_e2sfca(decay_gaussian(sigma = 10)))
#' access
#' summary(access)
#' head(as.data.frame(access))
#' @family allocation interface
#' @name spax_access_values-methods
#' @method format spax_access_values
#' @export
#' @md
format.spax_access_values <- function(x, ...) {
  line <- function(label, text) sprintf("  %-10s  %s", label, text)
  layers <- colnames(x$access)
  c(paste0("<spax access> ", x$type, ", ", length(layers),
           if (length(layers) == 1L) " layer at " else " layers at ",
           .format_total(nrow(x$access)), " origins x ",
           .format_total(nrow(x$facilities)), " facilities"),
    line("layers", paste(layers, collapse = ", ")),
    .access_values_parameter_lines(x$parameters, line))
}

#' @rdname spax_access_values-methods
#' @method print spax_access_values
#' @export
#' @md
print.spax_access_values <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

#' @rdname spax_access_values-methods
#' @exportS3Method base::summary
#' @md
summary.spax_access_values <- function(object, ...) {
  values <- object$access
  per_layer <- lapply(seq_len(ncol(values)), function(i) {
    v <- values[, i]
    known <- v[!is.na(v)]
    covered <- sum(known > 0)
    data.frame(layer = colnames(values)[i], n = length(known),
               min = if (length(known)) min(known) else NA_real_,
               mean = if (length(known)) mean(known) else NA_real_,
               max = if (length(known)) max(known) else NA_real_,
               covered = covered,
               proportion = if (length(known)) covered / length(known) else NA_real_,
               stringsAsFactors = FALSE)
  })
  structure(list(type = object$type, layers = do.call(rbind, per_layer),
                 parameters = object$parameters, origins = nrow(values)),
            class = "summary.spax_access_values")
}

#' @rdname spax_access_values-methods
#' @method print summary.spax_access_values
#' @export
#' @md
print.summary.spax_access_values <- function(x, digits = 4L, ...) {
  line <- function(label, text) sprintf("  %-10s  %s", label, text)
  cat(c(paste0("<spax access summary> ", x$type),
        line("origins", .format_total(x$origins)),
        .access_values_parameter_lines(x$parameters, line), ""), sep = "\n")
  table <- x$layers
  table$proportion <- round(table$proportion, 3L)
  num <- c("min", "mean", "max")
  table[num] <- lapply(table[num], signif, digits = digits)
  names(table)[names(table) == "n"] <- "with_value"
  print(table, row.names = FALSE)
  invisible(x)
}

#' @rdname spax_access_values-methods
#' @exportS3Method base::as.data.frame
#' @md
as.data.frame.spax_access_values <- function(x, row.names = NULL, optional = FALSE, ...) {
  out <- data.frame(origin_id = rownames(x$access), x$access, check.names = FALSE,
                    stringsAsFactors = FALSE)
  rownames(out) <- NULL
  base::as.data.frame.data.frame(out, row.names = row.names, optional = optional, ...)
}
