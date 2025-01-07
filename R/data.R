# Boundary Data ----------------------------------------------------------------
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
#' bound0 # already lazy-loaded - no need to run data(bound0)
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
#' bound1 # already lazy-loaded - no need to run data(bound1)
#'
#' # Plot the ADM1 boundaries with province names
#' ggplot(bound1) +
#'   geom_sf(aes(fill = ADM1_EN)) +
#'   geom_sf_label(aes(label = ADM1_EN)) +
#'   ggtitle("Thailand Region 12 ADM1 Boundaries")
#' }
"bound1"

# Supply Data ------------------------------------------------------------------
#' Health Facilities in Thailand's Region 12
#'
#' A collection of spatial datasets containing information about health facilities
#' in Thailand's Region 12 (Southern Border Provinces) for the year 2020 (B.E. 2563).
#' The data is split into two complementary datasets: primary health centers (PHC)
#' and hospitals.
#'
#' @format Both datasets are sf objects with point geometry (UTM zone 47N, EPSG:32647)
#' containing the following variables:
#' \describe{
#'   \item{id}{Unique identifier for each facility (unordered!)}
#'   \item{hoslvl}{Factor indicating facility level:
#'         "1-Community", "2-General", "3-Regional"}
#'   \item{bed}{Number of hospital beds}
#'   \item{s_doc}{Number of doctors}
#'   \item{s_dent}{Number of dentists}
#'   \item{s_nurse}{Number of nurses}
#'   \item{s_hv}{Number of health volunteers}
#'   \item{d_pop_moph}{Total population in service area}
#'   \item{d_pop_moph_60}{Population aged 60 and above in service area}
#'   \item{d_pop_moph_05}{Population aged 0-5 in service area}
#'   \item{geometry}{Point geometry representing facility location}
#' }
#'
#' @details
#' The data is split into two datasets:
#'
#' \strong{hc12_phc}: Primary Health Centers (n=819)
#' \itemize{
#'   \item All facilities are level "1-Community"
#'   \item No inpatient beds (bed = 0)
#'   \item Typically staffed by 1-4 healthcare providers
#'   \item Strong health volunteer presence (s_hv > 0)
#'   \item Serves defined population catchments
#' }
#'
#' \strong{hc12_hos}: Hospitals (n=77)
#' \itemize{
#'   \item Includes general (level 2) and regional hospitals (level 3)
#'   \item Has inpatient beds (30-591 beds)
#'   \item Larger healthcare workforce
#'   \item Limited health volunteer involvement
#'   \item Population data not available at facility level
#' }
#'
#' @note
#' Variables are prefixed to indicate their type:
#' \itemize{
#'   \item 's_': Supply indicators (healthcare workforce)
#'   \item 'd_': Demand indicators (population statistics)
#' }
#'
#' @source
#' Office of the Permanent Secretary, Ministry of Public Health. (2020).
#' Government Open Data Services. Retrieved December 19, 2024, from
#' https://opendata.moph.go.th
#'
#' The dataset combines three main data sources from the MoPH API:
#' \itemize{
#'   \item Facility locations and characteristics (GIS service)
#'   \item Healthcare provider statistics
#'   \item Population and service area statistics
#' }
#'
#' @examples
#' \dontrun{
#' # Load the datasets
#' data(hc12_phc)
#' data(hc12_hos)
#'
#' # Compare facility distributions
#' table(hc12_hos$hoslvl)
#'
#' # Summary of healthcare workforce
#' summary(hc12_phc[c("s_doc", "s_nurse", "s_hv")])
#' summary(hc12_hos[c("s_doc", "s_nurse", "s_hv")])
#'
#' # Plot facilities by type
#' library(ggplot2)
#' ggplot() +
#'   geom_sf(data = hc12_phc, aes(color = "PHC")) +
#'   geom_sf(data = hc12_hos, aes(color = "Hospital")) +
#'   scale_color_manual(values = c("PHC" = "blue", "Hospital" = "red")) +
#'   theme_minimal()
#' }
#'
#' @name hc12
#' @aliases hc12_phc hc12_hos
NULL



#' Hospital Travel Time Isochrones (Vector Format)
#'
#' A spatial dataset containing travel time isochrone polygons for 77 hospitals in
#' Thailand's Region 12. This vector format provides the original isochrone bands
#' from which \code{\link{hos_iscr}} was derived.
#'
#' @format An sf object with 2027 features and 5 fields:
#' \describe{
#'   \item{id}{Numeric ID for each isochrone band}
#'   \item{isomin}{Minimum travel time in minutes for the band}
#'   \item{isomax}{Maximum travel time in minutes for the band}
#'   \item{location_id}{Hospital identifier matching \code{\link{hc12_hos}}}
#'   \item{iso_mean}{Mean travel time for the band ((isomin + isomax)/2)}
#'   \item{geometry}{MULTIPOLYGON geometry in UTM zone 47N}
#' }
#'
#' @details
#' This vector dataset is provided to demonstrate the isochrone generation process
#' and conversion to raster format. The raster version (\code{\link{hos_iscr}}) is
#' recommended for accessibility analysis within the spax package.
#'
#' @source
#' Computed using OSRM with OpenStreetMap data.
#' Hospital locations from \code{\link{hc12_hos}}.
#'
#' @references
#' Luxen, D., & Vetter, C. (2011). Real-time routing with OpenStreetMap data.
#' In Proceedings of the 19th ACM SIGSPATIAL International Conference on
#' Advances in Geographic Information Systems (pp. 513-516).
#'
"hos_iscvec"
