#' Demand from population groups and group-specific rates
#'
#' Multiplies population counts in each group (for example, age bands) by a
#' group-specific rate, sums over groups and applies a common scale (for
#' example, the share of cases that use a given service):
#' \deqn{D_i = s \sum_b r_b \, P_{ib}.}{D_i = s * sum_b r_b * P_ib.}
#'
#' Gridded products often provide group counts and a total population that do
#' not sum exactly. With `total`, each cell's group composition is applied to
#' its total instead: \eqn{D_i = s \, T_i \sum_b r_b P_{ib} / \sum_b P_{ib}}{D_i =
#' s * T_i * sum_b r_b P_ib / sum_b P_ib}. Cells with a total but no group
#' population use the overall rate across all cells (`fallback = "mean"`) or
#' stop (`fallback = "error"`).
#'
#' @param groups A multilayer `SpatRaster` with one named layer per group, or
#'   a numeric matrix with one named column per group and one row per origin.
#'   A missing group value counts as zero; a cell whose groups are all missing
#'   has missing demand (unless `total` supplies it).
#' @param rates A named numeric vector of nonnegative rates, one per group. Every
#'   group must have a rate (use 0 for groups without demand) and every rate a
#'   group.
#' @param scale A positive multiplier applied to every cell (default 1).
#' @param total Optional total population: a one-layer `SpatRaster` on the same
#'   grid as `groups`, or a numeric vector with one value per row of `groups`.
#' @param fallback What to do with cells that have a total but no group
#'   population: `"mean"` uses the overall rate, `"error"` stops.
#' @return Demand with the same layout as a single group: a one-layer
#'   `SpatRaster` or a numeric vector. Attributes record the overall implied
#'   rate (`implied_rate`, before `scale`), the number of fallback cells
#'   (`fallback_cells`) and the rates used.
#' @examples
#' library(terra)
#'
#' # Two age groups on the Cornwall population grid: under 15 and 15 or over
#' pop <- read_spax_example("cornwall_pop.tif")
#' groups <- c(pop * 0.16, pop * 0.84)
#' names(groups) <- c("age_0_14", "age_15_plus")
#'
#' # A condition affecting 0.5% of people aged 15 or over, 80% of whom use
#' # the service, applied to the total population grid
#' demand <- calc_demand(groups, c(age_0_14 = 0, age_15_plus = 0.005),
#'                       scale = 0.8, total = pop)
#' attr(demand, "implied_rate")
#' @family allocation steps
#' @export
#' @md
calc_demand <- function(groups, rates, scale = 1, total = NULL,
                        fallback = c("mean", "error")) {
  fallback <- match.arg(fallback)
  is_raster <- inherits(groups, "SpatRaster")
  if (is_raster) {
    group_names <- names(groups)
    values <- terra::values(groups, mat = TRUE)
  } else if (is.matrix(groups) && is.numeric(groups)) {
    group_names <- colnames(groups)
    values <- groups
  } else {
    stop("groups must be a multilayer SpatRaster or a numeric matrix")
  }
  if (is.null(group_names) || anyNA(group_names) || any(!nzchar(group_names)) ||
      anyDuplicated(group_names)) {
    stop("groups must have unique, nonempty names")
  }
  if (any(values < 0, na.rm = TRUE) || any(is.infinite(values))) {
    stop("group populations must be finite and nonnegative")
  }
  if (!is.numeric(rates) || is.null(names(rates)) || anyNA(rates) || any(!is.finite(rates)) ||
      any(rates < 0) || anyDuplicated(names(rates))) {
    stop("rates must be a named vector of finite nonnegative values")
  }
  if (!setequal(names(rates), group_names)) {
    missing <- setdiff(group_names, names(rates))
    extra <- setdiff(names(rates), group_names)
    stop("rates must match the groups exactly",
         if (length(missing)) paste0("; no rate for: ", paste(missing, collapse = ", ")),
         if (length(extra)) paste0("; no group for: ", paste(extra, collapse = ", ")))
  }
  .chck_positive_scalar(scale, "scale")
  rates <- rates[group_names]
  all_na <- rowSums(!is.na(values)) == 0
  group_sum <- rowSums(values, na.rm = TRUE)
  weighted <- as.vector(ifelse(is.na(values), 0, values) %*% rates)
  implied <- sum(weighted) / sum(group_sum)
  n_fallback <- 0L
  if (is.null(total)) {
    out <- weighted * scale
    out[all_na] <- NA_real_
  } else {
    if (is_raster) {
      if (!inherits(total, "SpatRaster") || terra::nlyr(total) != 1L ||
          !terra::compareGeom(total, groups, stopOnError = FALSE)) {
        stop("total must be a one-layer SpatRaster on the same grid as groups")
      }
      tot <- terra::values(total, mat = FALSE)
    } else {
      if (!is.numeric(total) || length(total) != nrow(values)) {
        stop("total must be a numeric vector with one value per row of groups")
      }
      tot <- as.numeric(total)
    }
    if (any(tot < 0, na.rm = TRUE) || any(is.infinite(tot))) {
      stop("total must be finite and nonnegative")
    }
    rate <- ifelse(group_sum > 0, weighted / group_sum, NA_real_)
    need_fallback <- !is.na(tot) & tot > 0 & is.na(rate)
    n_fallback <- sum(need_fallback)
    if (n_fallback && fallback == "error") {
      stop(n_fallback, " cells have a total but no group population")
    }
    if (n_fallback && !is.finite(implied)) {
      stop("no group population anywhere, so there is no overall rate to fall back on")
    }
    rate[need_fallback] <- implied
    rate[!is.na(tot) & tot == 0] <- 0
    out <- tot * rate * scale
  }
  out <- as.numeric(out)
  if (!is_raster) names(out) <- rownames(groups)
  if (is_raster) {
    r <- terra::rast(groups, nlyrs = 1)
    terra::values(r) <- out
    names(r) <- "demand"
    out <- r
  }
  attr(out, "implied_rate") <- implied
  attr(out, "fallback_cells") <- n_fallback
  attr(out, "rates") <- rates
  out
}
