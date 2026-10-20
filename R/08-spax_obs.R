# Observations for the allocation interface (SPAX-058, T4) -----------------------

#' Declare observed counts for an allocation model
#'
#' `obs_flows()` declares observed origin-facility counts and `obs_totals()`
#' observed facility counts, in total or by demand group. Each returns a
#' `spax_obs`: the counts together with the loss used to fit to them (and its
#' arguments). The loss is part of the observation, so a model specification
#' carries none. An observation holds no data. `spax()` aligns the
#' observations with the data: it matches the IDs, reorders the counts into the
#' order of the data and drops or refuses what the data cannot hold.
#'
#' **IDs.** IDs from data frames are converted to character: factors with
#' `as.character()`, numbers with `format(scientific = FALSE)`, so that 100000
#' stays `"100000"` rather than `"1e+05"`. Dimnames and names are used as given.
#'
#' **Counts** are finite and nonnegative, or `NA` for a count that was not
#' observed (an `NA` cell is left out of the loss). Fractional counts are
#' allowed.
#'
#' **Flows.** `x` is a numeric matrix with row names (origin IDs) and column
#' names (facility IDs), or a data frame with one row per origin-facility pair.
#' A `table` or `xtabs` object is read as a matrix. A frame with exactly three
#' columns and no column arguments is read as origin, facility, count; otherwise
#' name the columns. In a long table a pair that does not appear has count 0;
#' duplicate pairs are summed, ignoring `NA`, and a pair whose counts are all
#' `NA` stays `NA`.
#'
#' **Totals.** `x` is a named numeric vector, a one-dimensional array or table
#' (from [tapply()] or [table()]), or a data frame of facility and count (two
#' columns by default). These are facility totals. A matrix with facility row
#' names and group column names, or a data frame of facility, group and count
#' (three columns by default), gives totals by demand group. Duplicate facilities
#' in a frame are summed as for flows; duplicate names in a vector or matrix are
#' an error.
#'
#' **What the data decides.** When the observations are aligned with the data:
#' counts from origins outside the data, excluded from it or with unknown demand
#' are dropped with a message, and counts at facilities outside the data are the
#' outside option of the model (dropped, with a message, for a model without
#' one). Positive counts from origins with zero demand, at facilities with zero
#' supply, or on pairs with unknown or infinite travel are errors, because the
#' model cannot produce them. Flows missing for an active origin or a facility of
#' the data are taken as 0, with a message; totals must give every facility of
#' the data (use 0, or `NA` if unobserved).
#'
#' IDs match the data as text. An ID that matches nothing is then matched by its
#' numeric value when exactly one ID of the data has that value, with a message:
#' R writes the number 100000 as `"1e+05"`, and codes read as numbers lose
#' leading zeros (`1234` matches `"01234"`).
#'
#' @param x For `obs_flows()`, a numeric matrix of origin by facility counts or a
#'   data frame of origin, facility and count. For `obs_totals()`, a named
#'   numeric vector (or one-dimensional array or table) of facility counts, a
#'   facility by group matrix, or a data frame of facility and count, or of
#'   facility, group and count. For the `print()` method, a `spax_obs`.
#' @param origin,facility,count,group Names of the columns of a data frame `x`
#'   holding the origin IDs, facility IDs, counts and group IDs. Leave all `NULL`
#'   to read the columns by position (see Details). They apply to data frames only.
#' @param loss `"poisson"` (default) or `"weighted_sse"`: the loss that fits a
#'   model to these counts.
#' @param loss_args A named list of arguments for the loss. It is stored and
#'   passed on unchanged.
#' @param ... Not used.
#' @return An object of class `spax_obs`: a list with `type` (`"flow"`,
#'   `"utilization"` or `"utilization_by_group"`, the output the counts are
#'   compared with), `x` (a double matrix, a named double vector, or for flows
#'   given as a data frame, a frame with columns `origin`, `facility` and
#'   `count`), `loss` and `loss_args`.
#' @examples
#' # Origin-facility flows from a long table of registrations
#' flows <- obs_flows(cornwall_flows, origin = "lsoa", facility = "practice",
#'                    count = "patients")
#' flows
#'
#' # Facility totals, from the same table
#' totals <- tapply(cornwall_flows$patients, cornwall_flows$practice, sum)
#' obs_totals(totals)
#' @family allocation interface
#' @export
#' @md
obs_flows <- function(x, origin = NULL, facility = NULL, count = NULL,
                      loss = c("poisson", "weighted_sse"), loss_args = list()) {
  loss <- match.arg(loss)
  .obs_check_loss_args(loss_args)
  naming <- "name the origin, facility and count columns"
  if (is.data.frame(x)) {
    pos <- .obs_columns(x, list(origin = origin, facility = facility, count = count),
                        naming)
    if (is.null(pos)) {
      if (ncol(x) != 3L) stop(naming, call. = FALSE)
      pos <- c(origin = 1L, facility = 2L, count = 3L)
    }
    o <- .obs_ids(x[[pos[["origin"]]]])
    f <- .obs_ids(x[[pos[["facility"]]]])
    n <- .obs_counts(x[[pos[["count"]]]])
    agg <- .obs_aggregate(list(o, f), n)
    value <- data.frame(origin = agg$keys[[1L]], facility = agg$keys[[2L]],
                        count = agg$count, stringsAsFactors = FALSE)
  } else {
    .obs_no_columns(origin = origin, facility = facility, count = count)
    value <- .obs_matrix(x, paste0("x must have row names (origin IDs) and column ",
                                   "names (facility IDs)"))
    .obs_check_duplicates(rownames(value), "flows", "origin")
    .obs_check_duplicates(colnames(value), "flows", "facility")
  }
  .new_spax_obs("flow", value, loss, loss_args)
}

#' @rdname obs_flows
#' @export
#' @md
obs_totals <- function(x, facility = NULL, count = NULL, group = NULL,
                       loss = c("poisson", "weighted_sse"), loss_args = list()) {
  loss <- match.arg(loss)
  .obs_check_loss_args(loss_args)
  naming <- "name the facility and count columns (and group, for totals by group)"
  if (is.data.frame(x)) {
    pos <- .obs_columns(x, list(facility = facility, count = count, group = group),
                        naming, optional = "group")
    if (is.null(pos)) {
      pos <- switch(as.character(ncol(x)),
                    "2" = c(facility = 1L, count = 2L),
                    "3" = c(facility = 1L, group = 2L, count = 3L),
                    stop(naming, call. = FALSE))
    }
    f <- .obs_ids(x[[pos[["facility"]]]])
    n <- .obs_counts(x[[pos[["count"]]]])
    if ("group" %in% names(pos)) {
      g <- .obs_ids(x[[pos[["group"]]]])
      agg <- .obs_aggregate(list(f, g), n)
      rows <- unique(agg$keys[[1L]])
      cols <- unique(agg$keys[[2L]])
      # A pair that does not appear in the table has count 0.
      value <- matrix(0, length(rows), length(cols), dimnames = list(rows, cols))
      value[cbind(match(agg$keys[[1L]], rows), match(agg$keys[[2L]], cols))] <- agg$count
      type <- "utilization_by_group"
    } else {
      agg <- .obs_aggregate(list(f), n)
      value <- stats::setNames(agg$count, agg$keys[[1L]])
      type <- "utilization"
    }
  } else {
    .obs_no_columns(facility = facility, count = count, group = group)
    d <- dim(x)
    if (length(d) <= 1L) {
      if (is.null(x) || !is.atomic(x)) {
        stop("x must be a named numeric vector, a matrix or a data frame", call. = FALSE)
      }
      dn <- dimnames(x)
      nm <- if (length(d) == 1L) (if (is.null(dn)) NULL else dn[[1L]]) else names(x)
      if (is.null(nm) || anyNA(nm) || !all(nzchar(nm))) {
        stop("x must be named by facility IDs", call. = FALSE)
      }
      .obs_check_duplicates(nm, "totals", "facility")
      value <- stats::setNames(.obs_counts(x), nm)
      type <- "utilization"
    } else {
      value <- .obs_matrix(x, paste0("x must have row names (facility IDs) and column ",
                                     "names (group IDs)"))
      .obs_check_duplicates(rownames(value), "totals", "facility")
      .obs_check_duplicates(colnames(value), "totals", "group")
      type <- "utilization_by_group"
    }
  }
  .new_spax_obs(type, value, loss, loss_args)
}

#' @rdname obs_flows
#' @export
#' @md
print.spax_obs <- function(x, ...) {
  counts <- if (is.data.frame(x$x)) x$x$count else as.vector(x$x)
  label <- switch(x$type, flow = "flows: ", utilization = "totals: ",
                  utilization_by_group = "totals by group: ")
  unit <- switch(x$type, flow = "origin-facility", utilization = "facility",
                 utilization_by_group = "facility-group")
  cat("<spax obs> ", label, .obs_count(sum(!is.na(counts)), paste(unit, "count"),
                                       paste(unit, "counts")), ", total ",
      .obs_num(sum(counts, na.rm = TRUE)), "; loss ", x$loss, "\n", sep = "")
  invisible(x)
}

.new_spax_obs <- function(type, x, loss, loss_args) {
  structure(list(type = type, x = x, loss = loss, loss_args = loss_args),
            class = "spax_obs")
}

.obs_check_loss_args <- function(loss_args) {
  if (!is.list(loss_args) || (length(loss_args) &&
      (is.null(names(loss_args)) || anyNA(names(loss_args)) ||
       !all(nzchar(names(loss_args)))))) {
    stop("loss_args must be a named list", call. = FALSE)
  }
  invisible(TRUE)
}

# Column arguments belong to data frames.
.obs_no_columns <- function(...) {
  given <- names(Filter(Negate(is.null), list(...)))
  if (length(given)) {
    stop(paste(given, collapse = ", "), " name columns of a data frame, but x is not one",
         call. = FALSE)
  }
  invisible(TRUE)
}

# Positions of the named columns of a frame; NULL when no column is named (the
# caller then reads the columns by position). Naming some but not all of the
# required columns is an error with `naming`. `optional` columns may stay NULL.
.obs_columns <- function(x, args, naming, optional = character()) {
  given <- !vapply(args, is.null, logical(1))
  if (!any(given)) return(NULL)
  if (!all(given[setdiff(names(args), optional)])) stop(naming, call. = FALSE)
  pos <- vapply(names(args)[given], function(arg) {
    value <- args[[arg]]
    if (!is.character(value) || length(value) != 1L || is.na(value)) {
      stop(arg, " must be the name of one column of x", call. = FALSE)
    }
    i <- match(value, names(x))
    if (is.na(i)) stop("x has no column '", value, "'", call. = FALSE)
    i
  }, integer(1))
  if (anyDuplicated(pos)) {
    stop("the columns named for x must be different columns", call. = FALSE)
  }
  pos
}

# IDs as character: factors by as.character(), numbers by format() so that
# 100000 stays "100000" (as.character() gives "1e+05"). Formatting runs on the
# distinct values only.
.obs_ids <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  } else if (is.numeric(x)) {
    u <- unique(x)
    s <- vapply(u, function(v) {
      if (is.na(v)) NA_character_ else format(v, scientific = FALSE, trim = TRUE, digits = 15)
    }, character(1))
    x <- s[match(x, u)]
  } else if (!is.character(x)) {
    stop("observed IDs must be character, factor or numeric", call. = FALSE)
  }
  if (anyNA(x)) stop("observed IDs must not be missing", call. = FALSE)
  x
}

.obs_count_message <- "observed counts must be nonnegative numbers or NA"

# Counts as a plain double vector: finite and nonnegative, or NA.
.obs_counts <- function(x) {
  if (!is.numeric(x)) stop(.obs_count_message, call. = FALSE)
  v <- as.double(x)
  if (any(is.nan(v) | (!is.na(v) & (!is.finite(v) | v < 0)))) {
    stop(.obs_count_message, call. = FALSE)
  }
  v
}

# A numeric matrix (a table or xtabs is read as one) as a plain double matrix
# with character dimnames; `unnamed` is the error for missing names.
.obs_matrix <- function(x, unnamed) {
  if (!is.matrix(x)) stop("x must be a numeric matrix or a data frame", call. = FALSE)
  dn <- dimnames(x)
  if (is.null(dn) || is.null(dn[[1L]]) || is.null(dn[[2L]]) ||
      anyNA(dn[[1L]]) || anyNA(dn[[2L]])) {
    stop(unnamed, call. = FALSE)
  }
  matrix(.obs_counts(x), nrow(x), ncol(x), dimnames = list(dn[[1L]], dn[[2L]]))
}

.obs_head <- function(ids, n = 5L) {
  paste(ids[seq_len(min(n, length(ids)))], collapse = ", ")
}

.obs_check_duplicates <- function(ids, what, axis) {
  dup <- unique(ids[duplicated(ids)])
  if (length(dup)) {
    stop("observed ", what, " have duplicated ", axis, " IDs: ", .obs_head(dup),
         call. = FALSE)
  }
  invisible(TRUE)
}

# Sum counts over repeated keys (a list of equal-length ID vectors), ignoring
# NA; a key whose counts are all NA stays NA. Keys keep their first-appearance
# order. Without repeats the input is returned as it is.
.obs_aggregate <- function(keys, count) {
  codes <- lapply(keys, function(k) match(k, unique(k)))
  size <- vapply(codes, function(k) max(k, 0L), numeric(1))
  key <- as.numeric(codes[[1L]])
  for (i in seq_along(codes)[-1L]) key <- (key - 1) * size[[i]] + codes[[i]]
  if (!anyDuplicated(key)) return(list(keys = keys, count = count))
  first <- !duplicated(key)
  g <- match(key, key[first])
  total <- as.vector(rowsum(ifelse(is.na(count), 0, count), g))
  seen <- as.vector(rowsum(as.numeric(!is.na(count)), g))
  total[seen == 0] <- NA_real_
  list(keys = lapply(keys, function(k) k[first]), count = total)
}

# A number and the noun that agrees with it: "1 origin", "2,290 counts".
.obs_count <- function(n, one, many) {
  paste(.obs_num(n), if (isTRUE(n == 1)) one else many)
}

# IDs match as text. An observed ID that matches no ID of the data is then matched
# by its numeric value when exactly one ID of the data has that value: R writes
# 100000 as "1e+05" but 100000L as "100000", and codes read as numbers lose their
# leading zeros. Returns the IDs to use and how many were matched this way.
.obs_numeric_match <- function(ids, data_ids) {
  miss <- !ids %in% data_ids
  if (!any(miss)) return(list(ids = ids, from = character(), to = character()))
  num_data <- suppressWarnings(as.numeric(data_ids))
  unique_num <- !is.na(num_data) & !num_data %in% num_data[duplicated(num_data)]
  num_obs <- suppressWarnings(as.numeric(ids[miss]))
  hit <- match(num_obs, num_data[unique_num])
  to <- data_ids[unique_num][hit]
  from <- ids[miss][!is.na(hit)]
  out <- ids
  out[miss][!is.na(hit)] <- to[!is.na(hit)]
  list(ids = out, from = from, to = to[!is.na(hit)])
}

# Rewrite the IDs of an observation by .obs_numeric_match() before alignment, and
# report what was matched by value.
.obs_align_ids <- function(obs, data) {
  x <- obs$x
  fac <- data$facility_ids
  moved <- list()
  swap <- function(ids, data_ids) {
    m <- .obs_numeric_match(ids, data_ids)
    if (length(m$from)) moved[[length(moved) + 1L]] <<- m
    m$ids
  }
  if (is.data.frame(x)) {
    if (!is.null(x$origin)) x$origin <- swap(x$origin, data$all_origin_ids)
    x$facility <- swap(x$facility, fac)
  } else if (is.matrix(x)) {
    if (identical(obs$type, "flow")) {
      rownames(x) <- swap(rownames(x), data$all_origin_ids)
    } else {
      rownames(x) <- swap(rownames(x), fac)
    }
    if (identical(obs$type, "flow")) colnames(x) <- swap(colnames(x), fac)
  } else {
    names(x) <- swap(names(x), fac)
  }
  if (length(moved)) {
    from <- unlist(lapply(moved, `[[`, "from"))
    to <- unlist(lapply(moved, `[[`, "to"))
    pairs <- unique(paste0(from, " as ", to))
    message("observed: ", .obs_count(length(pairs), "ID", "IDs"),
            " matched to the data by numeric value, for example ", pairs[1L])
    dup <- if (is.data.frame(x)) character() else
      c(rownames(x)[duplicated(rownames(x))], colnames(x)[duplicated(colnames(x))],
        names(x)[duplicated(names(x))])
    if (length(dup)) {
      stop("observed has two IDs for the same data ID: ", .obs_head(unique(dup)),
           call. = FALSE)
    }
  }
  obs$x <- x
  obs
}

# Number with thousands separators, for messages and print.
.obs_num <- function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE, digits = 7)
}

# Raw observations in spax() ---------------------------------------------------

# Read the `observed` argument of spax() when it is not an obs_* object (4.5,
# "Raw observed in spax()"). A vector is a set of facility totals. A matrix is
# read as flows (some row name is an origin ID of the data and some column name
# a facility ID) or as totals by group (the column names are the group IDs of
# the data and some row name a facility ID); both readings, or neither, are
# errors. A data frame must be wrapped, because its columns need names.
.as_spax_obs <- function(observed, data) {
  if (inherits(observed, "spax_obs")) return(observed)
  if (is.data.frame(observed)) {
    stop("wrap a table in obs_flows() or obs_totals() to name its columns", call. = FALSE)
  }
  if (is.null(observed) || !is.atomic(observed)) {
    stop("observed must be obs_flows(), obs_totals(), a named numeric vector or a matrix",
         call. = FALSE)
  }
  d <- dim(observed)
  if (length(d) <= 1L) {
    dn <- dimnames(observed)
    nm <- if (length(d) == 1L) (if (is.null(dn)) NULL else dn[[1L]]) else names(observed)
    if (is.null(nm)) stop("observed must be named by origin and facility IDs", call. = FALSE)
    return(obs_totals(observed))
  }
  if (length(d) > 2L) {
    stop("observed must be a named numeric vector or a matrix", call. = FALSE)
  }
  dn <- dimnames(observed)
  if (is.null(dn) || is.null(dn[[1L]]) || is.null(dn[[2L]])) {
    stop("observed must be named by origin and facility IDs", call. = FALSE)
  }
  rows <- dn[[1L]]
  cols <- dn[[2L]]
  as_flows <- any(rows %in% data$all_origin_ids) && any(cols %in% data$facility_ids)
  as_totals <- !is.null(data$group_ids) && setequal(cols, data$group_ids) &&
    any(rows %in% data$facility_ids)
  if (as_flows && as_totals) {
    stop("observed reads both as flows and as totals by group; use obs_flows() or ",
         "obs_totals()", call. = FALSE)
  }
  if (as_flows) return(obs_flows(observed))
  if (as_totals) return(obs_totals(observed))
  stop("the rows and columns of observed are neither origin by facility nor facility by ",
       "group IDs of the data; use obs_flows() or obs_totals()", call. = FALSE)
}

# Canonical targets -------------------------------------------------------------

# The target that fit_allocation() binds for an observation (4.5 "Canonicalization"):
# flows as a matrix of active origins by facilities of the data, totals as a
# named vector of the facilities, totals by group as a facilities-by-groups
# matrix, all double and in the order of the data. `data` is the prepared inputs.
# `outside_option` is FALSE for a model without an outside option (Huff); it
# changes the wording of the message about counts at facilities outside the data.
#
# Origin statuses are the values of `data$origin_status`, set by
# .allocation_origin_status() in R/06-ae_preparation.R:
#   "active"          included, with known positive demand: a row of the data
#   "zero"            included, with demand 0
#   "unknown"         included, with unknown (NA) demand (missing_demand = "exclude")
#   "invalid_omitted" included, with negative or infinite demand (preparation
#                     refuses these today; handled like "unknown")
#   "excluded"        left out by `included`
.canonical_observed <- function(obs, data, outside_option) {
  if (!inherits(obs, "spax_obs")) {
    stop("obs must be an obs_flows() or obs_totals() object", call. = FALSE)
  }
  if (!inherits(data, "ae_prepared_inputs")) {
    stop("data must be prepared inputs from prep_data()", call. = FALSE)
  }
  if (!is.logical(outside_option) || length(outside_option) != 1L || is.na(outside_option)) {
    stop("outside_option must be TRUE or FALSE", call. = FALSE)
  }
  obs <- .obs_align_ids(obs, data)
  switch(obs$type,
    flow = .canonical_flow(obs$x, data, outside_option),
    utilization = .canonical_totals(obs$x, data, outside_option),
    utilization_by_group = .canonical_totals_by_group(obs$x, data, outside_option))
}

.canonical_flow <- function(x, data, outside_option) {
  frame <- is.data.frame(x)
  observed_origins <- if (frame) unique(x$origin) else rownames(x)
  observed_facilities <- if (frame) unique(x$facility) else colnames(x)
  # 1. Key check: a swapped matrix matches neither axis.
  if (!any(observed_origins %in% data$all_origin_ids)) {
    stop("none of the observed origin IDs are in the data; are rows and columns swapped?",
         call. = FALSE)
  }
  if (!any(observed_facilities %in% data$facility_ids)) {
    stop("none of the observed facility IDs are in the data; are rows and columns swapped?",
         call. = FALSE)
  }
  # 2. Origins.
  totals <- .obs_origin_totals(x)
  at <- match(totals$id, data$all_origin_ids)
  status <- ifelse(is.na(at), "outside", data$origin_status[at])
  zero <- status == "zero" & totals$positive
  if (any(zero)) {
    stop("observed has counts for ", .obs_count(sum(zero), "origin", "origins"),
         " with zero demand in the data: ",
         .obs_head(totals$id[zero]), call. = FALSE)
  }
  dropped <- status %in% c("outside", "excluded", "unknown", "invalid_omitted")
  active <- status == "active"
  # 3. Facilities: counts of the remaining origins at facilities outside the data.
  outside <- .obs_outside_facilities(x, totals$id[active], data$facility_ids)
  # 4. One message for steps 2 and 3.
  .obs_message(sum(totals$total[dropped]), sum(dropped & totals$positive),
               sum(outside), sum(outside > 0), outside_option)
  block <- .obs_flow_block(x, data$origin_ids, data$facility_ids)
  positive <- !is.na(block) & block > 0
  # 5. Nothing left to fit.
  if (!any(positive)) {
    stop("no observed count remains after dropping origins and facilities outside the data",
         call. = FALSE)
  }
  # 6. Counts the model cannot produce.
  .obs_check_zero_supply(colSums(block, na.rm = TRUE), data)
  reachable <- .obs_reachable(data)
  unreachable <- positive & !reachable
  if (any(unreachable)) {
    stop("observed has ", .obs_count(sum(block[unreachable]), "count", "counts"),
         " on origin-facility ",
         "pairs with unknown or infinite travel; set them to NA or give their travel times",
         call. = FALSE)
  }
  # 7. Origins and facilities of the data that the observations do not mention.
  n_origins <- sum(!data$origin_ids %in% totals$id)
  n_facilities <- sum(!data$facility_ids %in% observed_facilities)
  if (n_origins + n_facilities > 0L) {
    message("observed: no counts for ", .obs_count(n_origins, "active origin", "active origins"),
            " and ", .obs_count(n_facilities, "facility", "facilities"),
            " in the data; they are taken as 0")
  }
  block
}

.canonical_totals <- function(x, data, outside_option) {
  .obs_check_facilities(names(x), data)
  outside <- !names(x) %in% data$facility_ids
  .obs_message(0, 0, sum(x[outside], na.rm = TRUE), sum(x[outside] > 0, na.rm = TRUE),
               outside_option)
  target <- x[data$facility_ids]
  per_facility <- target
  per_facility[is.na(per_facility)] <- 0
  .obs_check_zero_supply(per_facility, data)
  target
}

.canonical_totals_by_group <- function(x, data, outside_option) {
  groups <- data$group_ids
  if (is.null(groups) || !setequal(colnames(x), groups)) {
    stop("observed groups (", paste(colnames(x), collapse = ", "), ") must equal the ",
         "groups of the data (", if (is.null(groups)) "none" else paste(groups, collapse = ", "),
         ")", call. = FALSE)
  }
  .obs_check_facilities(rownames(x), data)
  outside <- !rownames(x) %in% data$facility_ids
  .obs_message(0, 0, sum(x[outside, , drop = FALSE], na.rm = TRUE),
               sum(rowSums(x[outside, , drop = FALSE], na.rm = TRUE) > 0), outside_option)
  target <- x[data$facility_ids, groups, drop = FALSE]
  .obs_check_zero_supply(rowSums(target, na.rm = TRUE), data)
  target
}

# Totals must give every facility of the data.
.obs_check_facilities <- function(ids, data) {
  missing <- setdiff(data$facility_ids, ids)
  if (length(missing)) {
    stop("observed totals have no count for ", .obs_count(length(missing), "facility", "facilities"),
         " in the data: ",
         .obs_head(missing), "; give 0, or NA if unobserved", call. = FALSE)
  }
  invisible(TRUE)
}

# `per_facility` is the count at each facility of the data, in data order.
.obs_check_zero_supply <- function(per_facility, data) {
  bad <- as.vector(data$S[, 1L]) == 0 & per_facility > 0
  if (any(bad)) {
    stop("observed has ", .obs_count(sum(per_facility[bad]), "count", "counts"), " at ",
         .obs_count(sum(bad), "facility", "facilities"), " with zero supply (", .obs_head(data$facility_ids[bad]), "); the model ",
         "sends no one there. Remove those counts, set them to NA, or check the supply",
         call. = FALSE)
  }
  invisible(TRUE)
}

# Which active origin-facility pairs have a finite travel time. With travel by
# group a pair is reachable when some group that has demand at the origin has a
# finite travel time on it.
.obs_reachable <- function(data) {
  if (!is.null(data$distance_active)) return(is.finite(data$distance_active))
  reach <- matrix(FALSE, length(data$origin_ids), length(data$facility_ids))
  for (g in data$group_ids) {
    reach <- reach | (is.finite(data$distance_groups[[g]]) & (data$D_groups[, g] > 0))
  }
  reach
}

.obs_message <- function(n_origin, k_origin, n_facility, k_facility, outside_option) {
  parts <- character()
  if (n_origin > 0) {
    parts <- c(parts, paste0(.obs_count(n_origin, "count", "counts"), " from ",
                             .obs_count(k_origin, "origin", "origins"),
                             " outside the data or left out by it ",
                             if (n_origin == 1) "is" else "are", " dropped"))
  }
  if (n_facility > 0) {
    verb <- if (n_facility == 1) "is" else "are"
    parts <- c(parts, paste0(.obs_count(n_facility, "count", "counts"), " at ",
      .obs_count(k_facility, "facility", "facilities"), " outside the data ", verb,
      if (outside_option) " treated as the outside option"
      else " dropped (the Huff model has no outside option)"))
  }
  if (length(parts)) message("observed: ", paste(parts, collapse = "; "))
  invisible(NULL)
}

# Per observed origin: the total count (NA ignored) and whether any count is positive.
.obs_origin_totals <- function(x) {
  if (is.data.frame(x)) {
    id <- unique(x$origin)
    g <- match(x$origin, id)
    total <- as.vector(rowsum(ifelse(is.na(x$count), 0, x$count), g))
    positive <- as.vector(rowsum(as.numeric(!is.na(x$count) & x$count > 0), g)) > 0
    return(list(id = id, total = total, positive = positive))
  }
  list(id = rownames(x), total = rowSums(x, na.rm = TRUE),
       positive = rowSums(x > 0, na.rm = TRUE) > 0)
}

# Counts of the given origins at observed facilities that are not in the data,
# one total per facility (NA ignored).
.obs_outside_facilities <- function(x, origins, facilities) {
  if (is.data.frame(x)) {
    pick <- x$origin %in% origins & !x$facility %in% facilities
    if (!any(pick)) return(numeric(0))
    return(as.vector(rowsum(ifelse(is.na(x$count[pick]), 0, x$count[pick]),
                            x$facility[pick])))
  }
  cols <- which(!colnames(x) %in% facilities)
  rows <- match(origins, rownames(x))
  if (!length(cols) || !length(rows)) return(numeric(0))
  colSums(x[rows, cols, drop = FALSE], na.rm = TRUE)
}

# The counts of the active origins at the facilities of the data, in data
# order. A cell that the observations do not give (an absent pair of a long
# table, an origin or facility missing from a matrix) is 0; NA stays NA.
.obs_flow_block <- function(x, origins, facilities) {
  block <- matrix(0, length(origins), length(facilities), dimnames = list(origins, facilities))
  if (is.data.frame(x)) {
    i <- match(x$origin, origins)
    j <- match(x$facility, facilities)
    ok <- !is.na(i) & !is.na(j)
    block[cbind(i[ok], j[ok])] <- x$count[ok]
    return(block)
  }
  i <- match(origins, rownames(x))
  j <- match(facilities, colnames(x))
  has_row <- !is.na(i)
  has_col <- !is.na(j)
  if (any(has_row) && any(has_col)) {
    block[has_row, has_col] <- x[i[has_row], j[has_col], drop = FALSE]
  }
  block
}
