# Cornwall GP example data ---------------------------------------------------
#
# Built by data-raw/cornwall_gp.R from openly licensed sources; see
# system.file("COPYRIGHTS", package = "spax") for attribution and terms.

#' GP practices in Cornwall and the Isles of Scilly
#'
#' The 55 general practices of NHS Cornwall and Isles of Scilly Integrated
#' Care Board (ICB code QT6) with their GP workforce and registered lists.
#' Use `gp_fte_extg` as supply in examples: it excludes GPs in training, whose
#' recording changed in June 2018 and who cluster in training practices.
#'
#' @format An `sf` point object (EPSG:27700, British National Grid) with 55
#'   rows and columns:
#' \describe{
#'   \item{practice}{NHS ODS practice code; matches `cornwall_flows`,
#'     the columns of `cornwall_travel` and the layers of
#'     `cornwall_gp_travel.tif`.}
#'   \item{name, postcode, pcn}{Practice name, main-site postcode and Primary
#'     Care Network.}
#'   \item{gp_fte_extg}{Full-time equivalent GPs excluding GPs in training
#'     (1 FTE = 37.5 hours a week).}
#'   \item{gp_fte}{All GPs, including GPs in training and locums.}
#'   \item{gp_fte_extgl}{GPs excluding GPs in training and locums.}
#'   \item{nurse_fte}{Full-time equivalent nurses (may be `NA`).}
#'   \item{list_size}{Registered patients from all areas.}
#'   \item{list_in_area}{Registered patients living in Cornwall and the Isles
#'     of Scilly.}
#'   \item{fte_source}{Whether the workforce record includes estimates.}
#'   \item{scilly}{`TRUE` for the practice on the Isles of Scilly.}
#'   \item{geometry}{Main-site location from the postcode.}
#' }
#'
#' @details Registrations are a July 2026 snapshot and workforce a June 2026
#'   snapshot. Only the main site of each practice is represented; branch
#'   surgeries are not.
#'
#' @source NHS England, Patients Registered at a GP Practice (July 2026) and
#'   General Practice Workforce (30 June 2026), Open Government Licence v3.0.
#'   Locations from the ONS Postcode Directory (November 2025). See
#'   `system.file("COPYRIGHTS", package = "spax")`.
#' @seealso [cornwall_lsoa], [cornwall_flows], [cornwall_travel],
#'   [cornwall_gp_travel]
#' @examples
#' gp <- sf::st_drop_geometry(cornwall_gp)
#' head(gp[, c("practice", "name", "gp_fte_extg", "list_size")])
#' # Registered patients per GP FTE (excluding GPs in training)
#' summary(gp$list_size / gp$gp_fte_extg)
#' @md
"cornwall_gp"

#' Small areas of Cornwall and the Isles of Scilly
#'
#' The 337 Lower layer Super Output Areas (LSOA 2021) of NHS Cornwall and Isles
#' of Scilly ICB, with population, registered patients and population-weighted
#' centroids.
#'
#' @format An `sf` polygon object (EPSG:27700) with 337 rows and columns:
#' \describe{
#'   \item{lsoa, name}{LSOA 2021 code and name; `lsoa` matches
#'     `cornwall_flows` and the rows of `cornwall_travel`.}
#'   \item{ruc21}{2021 rural-urban classification.}
#'   \item{pop, pop_0_15, pop_16_64, pop_65p}{ONS mid-2024 population
#'     estimates, all ages and by age band.}
#'   \item{registered}{Residents registered at any GP practice in England.}
#'   \item{registered_in}{Residents registered at a Cornwall and Isles of Scilly
#'     practice (row totals of `cornwall_flows`).}
#'   \item{registered_out}{Residents registered at a practice elsewhere.}
#'   \item{cent_x, cent_y}{Population-weighted centroid (EPSG:27700), the
#'     origin used for `cornwall_travel`.}
#'   \item{scilly}{`TRUE` for the Isles of Scilly area.}
#'   \item{geometry}{Generalised boundary.}
#' }
#'
#' @details Registered counts exceed the population estimates by about 4%,
#'   reflecting list inflation and the different reference dates.
#'
#' @source Office for National Statistics: LSOA 2021 boundaries with rural-urban
#'   classification, population-weighted centroids and mid-2024 small-area
#'   population estimates (via Nomis); NHS England registrations (July 2026).
#'   Open Government Licence v3.0; contains OS data. See
#'   `system.file("COPYRIGHTS", package = "spax")`.
#' @seealso [cornwall_gp], [cornwall_flows], [cornwall_travel]
#' @examples
#' areas <- sf::st_drop_geometry(cornwall_lsoa)
#' summary(areas$registered_in / areas$registered)
#' table(areas$ruc21)
#' @md
"cornwall_lsoa"

#' Registered patients by small area and practice
#'
#' Residents of each Cornwall and Isles of Scilly small area registered at each
#' of the 55 ICB practices, as observed flows for calibrating allocation models.
#'
#' @format A data frame with 3,188 rows (non-zero pairs only) and columns:
#' \describe{
#'   \item{lsoa}{LSOA 2021 code (see `cornwall_lsoa`).}
#'   \item{practice}{Practice code (see `cornwall_gp`).}
#'   \item{patients}{Registered patients (integer, unrounded and unsuppressed).}
#' }
#'
#' @details Registration records where people are attached, not how often they
#'   use services. Residents registered outside the ICB are summarised in
#'   `cornwall_lsoa$registered_out`.
#'
#' @source NHS England, Patients Registered at a GP Practice, July 2026 (LSOA
#'   2021 file), Open Government Licence v3.0. See
#'   `system.file("COPYRIGHTS", package = "spax")`.
#' @seealso [cornwall_gp], [cornwall_lsoa], [cornwall_travel]
#' @examples
#' head(cornwall_flows)
#' # Practice totals from Cornwall residents
#' head(sort(tapply(cornwall_flows$patients, cornwall_flows$practice, sum),
#'           decreasing = TRUE))
#' @md
"cornwall_flows"

#' Travel time from each small area to each practice
#'
#' Motorised travel time in whole minutes from each small area's
#' population-weighted centroid to each practice's main site.
#'
#' @format An integer matrix with 337 rows (`cornwall_lsoa$lsoa`) and 55
#'   columns (`cornwall_gp$practice`). Values are at least 1 minute
#'   (same-cell pairs). `NA` marks pairs with no road connection: the Isles of
#'   Scilly area and practice are reachable only from each other.
#'
#' @details Times are least-cost paths through the Malaria Atlas Project 2019
#'   motorised friction surface (about 1 km cells), computed with
#'   [terra::costDist()] over land in Cornwall and the Isles of Scilly; the sea
#'   and the Devon border act as barriers. The road network dates from about
#'   2019, traffic is not modelled, and 1 km cells are coarse in towns, where
#'   several practices can share a cell. Values are taken from
#'   `cornwall_gp_travel.tif` at each centroid's cell.
#'
#' @source Derived from the Malaria Atlas Project motorised friction surface
#'   (Weiss et al. 2020), licensed under Creative Commons Attribution, and the
#'   locations in [cornwall_gp] and [cornwall_lsoa]. See
#'   `system.file("COPYRIGHTS", package = "spax")`.
#' @references Weiss, D. J., Nelson, A., Vargas-Ruiz, C. A., et al. (2020).
#'   Global maps of travel time to healthcare facilities. *Nature Medicine*,
#'   26, 1835-1838. \doi{10.1038/s41591-020-1059-1}
#' @seealso [cornwall_gp_travel], [cornwall_flows]
#' @examples
#' cornwall_travel[1:3, 1:4]
#' # Minutes to the nearest practice, mainland areas
#' summary(apply(cornwall_travel[!cornwall_lsoa$scilly, ], 1, min, na.rm = TRUE))
#' @md
"cornwall_travel"

#' Population grid for Cornwall and the Isles of Scilly
#'
#' A WorldPop 2020 population grid (UN-adjusted) clipped to Cornwall and the
#' Isles of Scilly. The GeoTIFF is bundled in `inst/extdata` and is not a
#' lazy-loaded object; load it with `read_spax_example("cornwall_pop.tif")`.
#'
#' @format A one-layer GeoTIFF (`pop`): 132 rows x 271 columns of 30
#'   arc-second cells (about 1 km; about 0.55 square km at this latitude),
#'   EPSG:4326. Values are estimated residents per cell. Cells crossing the
#'   boundary are weighted by the share of their area inside it; `NA` marks
#'   cells outside.
#'
#' @source WorldPop (2018), Global High Resolution Population Denominators
#'   Project, 2020 UN-adjusted 1 km estimates for the United Kingdom,
#'   \doi{10.5258/SOTON/WP00671}, Creative Commons Attribution 4.0. See
#'   `system.file("COPYRIGHTS", package = "spax")`.
#' @seealso [cornwall_gp_travel]
#' @examples
#' pop <- read_spax_example("cornwall_pop.tif")
#' terra::global(pop, "sum", na.rm = TRUE)
#' @name cornwall_pop
#' @md
NULL

#' Travel-time grid to each Cornwall GP practice
#'
#' Motorised travel time in whole minutes from every populated cell of
#' `cornwall_pop.tif` to each of the 55 practices, one layer per practice. The
#' GeoTIFF is bundled in `inst/extdata`; load it with
#' `read_spax_example("cornwall_gp_travel.tif")`, which restores the layer
#' names from `cornwall_gp_travel_names.rds`.
#'
#' @format A 55-layer GeoTIFF on the `cornwall_pop.tif` grid (132 rows x 271
#'   columns, EPSG:4326). Layer names are practice codes (`cornwall_gp$practice`).
#'   Values are at least 1 minute; `NA` marks unpopulated cells, pairs without a
#'   road connection (the Isles of Scilly) and journeys over 180 minutes.
#'
#' @details Computed as for [cornwall_travel]; see its details for method and
#'   limitations.
#'
#' @source Derived from the Malaria Atlas Project motorised friction surface
#'   (Weiss et al. 2020, Creative Commons Attribution) and [cornwall_gp]. See
#'   `system.file("COPYRIGHTS", package = "spax")`.
#' @seealso [cornwall_travel], [cornwall_pop]
#' @examples
#' travel <- read_spax_example("cornwall_gp_travel.tif")
#' travel
#' @name cornwall_gp_travel
#' @md
NULL
