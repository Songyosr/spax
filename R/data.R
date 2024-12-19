#' Thailand Health Region 12 Boundary Area - Boundary
#'
#' A dataset representing the geographic boundary of Health Region 12 in Thailand without administrative details.
#'
#' @format An `sf` object with the following attributes:
#' \describe{
#'   \item{geometry}{The geometry column containing spatial polygons for the boundary.}
#' }
#' @details
#' This dataset includes only the boundary area for Health Region 12, useful for masking or spatial extent analysis.
#'
#' @source
#' Data processed from Thailand's official administrative boundaries, available at
#' [Humanitarian Data Exchange (HDX)](https://data.humdata.org/dataset/thailand-administrative-boundaries).
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(ggplot2)
#'
#' # Load the dataset
#' data(bound0)
#'
#' # Plot the boundary
#' ggplot(bound0) +
#'   geom_sf(fill = "lightblue") +
#'   ggtitle("Thailand Region 12 Boundary Area")
#' }
"bound0"

#' Thailand Region 12 ADM1 Boundaries with Attributes
#'
#' A dataset representing administrative boundaries (ADM1) of Region 12 in Thailand, along with key attributes.
#'
#' @format An `sf` object with 7 features and the following attributes:
#' \describe{
#'   \item{Shape_Leng}{Length of the boundary in map units.}
#'   \item{Shape_Area}{Area of the boundary in map units.}
#'   \item{ADM1_EN}{Province name in English.}
#'   \item{ADM1_TH}{Province name in Thai.}
#'   \item{ADM1_PCODE}{Province administrative code.}
#'   \item{ADM0_EN}{Country name in English.}
#'   \item{ADM0_TH}{Country name in Thai.}
#'   \item{ADM0_PCODE}{Country administrative code.}
#'   \item{date}{Date the boundary was created.}
#'   \item{validOn}{Date the boundary became valid.}
#'   \item{geometry}{The geometry column containing spatial polygons.}
#' }
#' @details
#' This dataset provides detailed administrative information for Region 12 provinces in Thailand.
#'
#' @source
#' Data processed from Thailand's official administrative boundaries, available at
#' [Humanitarian Data Exchange (HDX)](https://data.humdata.org/dataset/thailand-administrative-boundaries).
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(ggplot2)
#'
#' # Load the dataset
#' data(bound1)
#'
#' # Plot the ADM1 boundaries with province names
#' ggplot(bound1) +
#'   geom_sf(aes(fill = ADM1_EN)) +
#'   geom_sf_label(aes(label = ADM1_EN)) +
#'   ggtitle("Thailand Region 12 ADM1 Boundaries")
#' }
"bound1"
