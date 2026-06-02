#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom raster raster
#' @importFrom stats rbinom rnbinom rpois runif setNames sd
#' @import terra
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_hline geom_histogram geom_vline geom_point
#' @importFrom ggplot2 scale_color_viridis_d scale_fill_viridis_d scale_size_continuous
#' @importFrom ggplot2 labs mean_cl_normal theme element_text theme_minimal
#' @importFrom tibble tibble
## usethis namespace: end
NULL

# Non-standard-evaluation column names used in data.frame pipelines
# (e.g. gather_demand). Declared to satisfy R CMD check's "no visible
# binding for global variable" note.
utils::globalVariables(c("unit_id", "weighted_sum"))
