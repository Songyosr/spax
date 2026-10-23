# Build the open Cornwall GP example data ----------------------------------
#
# NHS Cornwall and Isles of Scilly Integrated Care Board (ICB): GP practices,
# registered patients by small area (LSOA 2021), GP workforce, population and
# travel times. All sources are openly licensed; see inst/COPYRIGHTS.
#
# Run from the package root:
#   Rscript data-raw/cornwall_gp.R
# Needs data.table, sf, terra, curl, digest and jsonlite (build-time only;
# data-raw/ is excluded from the package).
#
# Downloads are cached outside the repository, in SPAX_DATA_RAW_CACHE or the
# spax user cache directory. Static source files are pinned by SHA-256 and the
# build stops if a download differs. Responses from the ONS and Nomis APIs are
# saved with their hashes in the provenance record but cannot be pinned.
#
# Outputs
#   data/cornwall_gp.rda             practices (sf points, EPSG:4326, with
#                                    British National Grid easting/northing)
#   data/cornwall_lsoa.rda           small areas (sf polygons, EPSG:4326)
#   data/cornwall_flows.rda          registrations, small area x practice
#   data/cornwall_travel.rda         small area x practice travel minutes
#   inst/extdata/cornwall_pop.tif    population grid (WorldPop 2020)
#   inst/extdata/cornwall_gp_travel.tif (+ _names.rds)
#                                    travel minutes from each grid cell to
#                                    each practice, one layer per practice

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(terra)
})
# Every shipped layer is on the WGS 84 datum, as the rasters are, so the
# package's examples and guides never change datum. British National Grid
# (OSGB36) coordinates are converted with the OSGB36 to WGS 84 Helmert
# transformation (about 2 m), as the shipped data were. PROJ network access
# stays off, so no datum grid is downloaded, and the script stops if a grid
# transformation is installed locally, which PROJ would prefer.
invisible(sf_proj_network(FALSE))
ops <- sf_proj_pipelines("EPSG:27700", "EPSG:4326")
stopifnot(!any(ops$instantiable & ops$grid_count > 0))
setDTthreads(1) # multithreaded fread can stall in non-interactive Rscript
stopifnot(file.exists("DESCRIPTION"),
          read.dcf("DESCRIPTION")[, "Package"] == "spax")

cache <- Sys.getenv("SPAX_DATA_RAW_CACHE",
  file.path(tools::R_user_dir("spax", "cache"), "data-raw", "cornwall_gp"))
dir.create(cache, recursive = TRUE, showWarnings = FALSE)
sha256 <- function(path) digest::digest(file = path, algo = "sha256")

# Sources ---------------------------------------------------------------------
#
# API responses accepted on 2026-10-14 (SHA-256 prefixes; compare with the
# provenance record after a rebuild to detect upstream changes):
#   onspd_nov_2025.json f5aa97e37ddf, lsoa_bsc.geojson 0e38c8d7e4d3,
#   lsoa_pop_centroids.geojson 5281b30edc94, ons_pop_lsoa21.csv 203a5293fa49

icb_code <- "QT6"           # NHS Cornwall and Isles of Scilly ICB
scilly_lsoa_prefix <- "Isles of Scilly"
population_year <- "2024"   # ONS mid-year small-area estimates
max_travel <- 180           # minutes; longer routes are stored as NA

static <- data.frame(
  key = c("registrations", "practice_map", "workforce", "worldpop", "friction"),
  file = c("gp-reg-pat-prac-lsoa-male-female-July-26.zip",
           "gp-reg-pat-prac-map.zip",
           "GPWPracticeCSV.062026.zip",
           "gbr_ppp_2020_1km_Aggregated_UNadj.tif",
           "map_2019_motorized_friction_cornwall.tif"),
  url = c(
    "https://files.digital.nhs.uk/93/C90730/gp-reg-pat-prac-lsoa-male-female-July-26.zip",
    "https://files.digital.nhs.uk/98/8142DB/gp-reg-pat-prac-map.zip",
    "https://files.digital.nhs.uk/B1/F5AC73/GPWPracticeCSV.062026.zip",
    paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/",
           "2020/GBR/gbr_ppp_2020_1km_Aggregated_UNadj.tif"),
    paste0("https://data.malariaatlas.org/geoserver/Accessibility/ows?service=WCS",
           "&version=2.0.1&request=GetCoverage",
           "&coverageId=Accessibility__202001_Global_Motorized_Friction_Surface",
           "&subset=Long(-6.45,-4.15)&subset=Lat(49.85,50.95)&format=image/tiff")),
  sha256 = c(
    "d6f26a653b2ea64505998f30d28699a394206e8895fd5c458f6b44eb99a8283f",
    "f5a8c5f5ee3d82a8313bafb5f14464fd9e734694e578c2983035060f601260bf",
    "917e2c41eb3a651eb86c8c84cc86ee545356294b015d3d0b1eca03e68b3d54c2",
    "4c28fbb1f06983f64d8ca11463441182f539c93524fe6dbd36e5f15e2a28a689",
    "bbeed49f443ef7de7e854ee9ea5f49f9f2df140426c73f5c4ff85410b2879b78"),
  stringsAsFactors = FALSE)

fetch <- function(key) {
  s <- static[static$key == key, ]
  path <- file.path(cache, s$file)
  if (!file.exists(path)) {
    tmp <- paste0(path, ".part")
    for (attempt in 1:3) { # transient network failures are common for large files
      ok <- tryCatch({
        curl::curl_download(s$url, tmp, quiet = TRUE)
        TRUE
      }, error = function(e) FALSE)
      if (ok) break
      Sys.sleep(5 * attempt)
    }
    if (!ok) stop("download failed after 3 attempts: ", s$url, call. = FALSE)
    file.rename(tmp, path)
  }
  if (!identical(sha256(path), s$sha256)) {
    stop("SHA-256 mismatch for ", s$file, "; the published file has changed. ",
         "Inspect it before updating the pinned hash.", call. = FALSE)
  }
  path
}

api_log <- list()
fetch_api <- function(name, url, body = NULL) {
  path <- file.path(cache, name)
  h <- curl::new_handle()
  if (!is.null(body)) {
    form <- paste(names(body), vapply(body, utils::URLencode, character(1),
                                      reserved = TRUE), sep = "=", collapse = "&")
    curl::handle_setopt(h, copypostfields = form)
  }
  curl::curl_download(url, path, handle = h, quiet = TRUE)
  api_log[[name]] <<- list(url = url, query = body, sha256 = sha256(path),
                           retrieved = format(Sys.time(), tz = "UTC", usetz = TRUE))
  path
}

arcgis <- function(service, where, fields, geometry, name) {
  url <- paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/",
                service, "/FeatureServer/0/query")
  fetch_api(name, url, list(where = where, outFields = fields,
    returnGeometry = tolower(geometry), outSR = "4326",
    resultRecordCount = "4000", f = if (geometry) "geojson" else "json"))
}

read_zip_csv <- function(zip, pattern, ...) {
  member <- grep(pattern, utils::unzip(zip, list = TRUE)$Name, value = TRUE)
  stopifnot(length(member) == 1L)
  fread(cmd = paste("unzip -p", shQuote(zip), shQuote(member)), ...)
}

# Practices -------------------------------------------------------------------

map <- read_zip_csv(fetch("practice_map"), "\\.csv$")
pr <- map[ICB_CODE == icb_code,
          .(practice = PRACTICE_CODE, name = PRACTICE_NAME,
            postcode = PRACTICE_POSTCODE, pcn = PCN_NAME)]
setorder(pr, practice)

gpw <- read_zip_csv(fetch("workforce"), "Detailed\\.csv$",
  select = c("PRAC_CODE", "TOTAL_PATIENTS", "GP_SOURCE", "TOTAL_GP_FTE",
             "TOTAL_GP_EXTG_FTE", "TOTAL_GP_EXTGL_FTE", "TOTAL_NURSES_FTE"))
pr <- merge(pr, gpw[, .(practice = PRAC_CODE,
  gp_fte_extg = TOTAL_GP_EXTG_FTE, gp_fte = TOTAL_GP_FTE,
  gp_fte_extgl = TOTAL_GP_EXTGL_FTE, nurse_fte = TOTAL_NURSES_FTE,
  fte_source = GP_SOURCE)], by = "practice", all.x = TRUE)

pcs <- sprintf("'%s'", pr$postcode)
onspd <- jsonlite::fromJSON(arcgis("ONSPD_NOV_2025_UK",
  sprintf("pcds IN (%s)", paste(pcs, collapse = ",")),
  "pcds,east1m,north1m,doterm", FALSE, "onspd_nov_2025.json"))$features$attributes
setDT(onspd)
pr <- merge(pr, onspd[, .(postcode = pcds, x = as.numeric(east1m),
                          y = as.numeric(north1m), doterm)],
            by = "postcode", all.x = TRUE)
stopifnot(!anyNA(pr$x), !anyNA(pr$gp_fte_extg),
          all(is.na(pr$doterm) | !nzchar(pr$doterm))) # no terminated postcodes

# Small areas -----------------------------------------------------------------

bsc <- st_read(arcgis("LSOA_2021_EW_BSC_V4_RUC",
  sprintf("LSOA21NM LIKE 'Cornwall %%' OR LSOA21NM LIKE '%s %%'", scilly_lsoa_prefix),
  "LSOA21CD,LSOA21NM,RUC21CD,RUC21NM", TRUE, "lsoa_bsc.geojson"), quiet = TRUE)
bsc <- st_transform(bsc, 27700)
bsc <- bsc[order(bsc$LSOA21CD), ]
lsoa_ids <- bsc$LSOA21CD
codes <- sprintf("'%s'", lsoa_ids)

cen <- st_read(arcgis("LSOA_PopCentroids_EW_2021_V4",
  sprintf("LSOA21CD IN (%s)", paste(codes, collapse = ",")),
  "LSOA21CD", TRUE, "lsoa_pop_centroids.geojson"), quiet = TRUE)
cen <- st_transform(cen, 27700)
cen_xy <- data.table(lsoa = cen$LSOA21CD, st_coordinates(cen))
setnames(cen_xy, c("X", "Y"), c("cent_x", "cent_y"))

nomis <- fread(fetch_api("ons_pop_lsoa21.csv", paste0(
  "https://www.nomisweb.co.uk/api/v01/dataset/NM_2014_1.data.csv?geography=",
  paste(lsoa_ids, collapse = ","), "&date=", population_year,
  "&gender=0&c_age=200,201,203,209&measures=20100",
  "&select=date_name,geography_code,c_age_name,obs_value")))
stopifnot(all(nomis$DATE_NAME == population_year))
pop <- dcast(nomis, GEOGRAPHY_CODE ~ C_AGE_NAME, value.var = "OBS_VALUE")
setnames(pop, c("GEOGRAPHY_CODE", "All Ages", "Aged 0 to 15", "Aged 16 to 64", "Aged 65+"),
         c("lsoa", "pop", "pop_0_15", "pop_16_64", "pop_65p"))

# Registrations ---------------------------------------------------------------

reg <- read_zip_csv(fetch("registrations"), "lsoa-all\\.csv$",
  select = c("PRACTICE_CODE", "LSOA_CODE", "NUMBER_OF_PATIENTS"))
setnames(reg, c("practice", "lsoa", "patients"))
lists <- reg[practice %in% pr$practice, .(list_size = sum(patients)), by = practice]
pr <- merge(pr, lists, by = "practice", all.x = TRUE)

reg_area <- reg[lsoa %in% lsoa_ids]
flows <- reg_area[practice %in% pr$practice & patients > 0]
setorder(flows, lsoa, practice)
registered <- reg_area[, .(registered = sum(patients),
  registered_in = sum(patients[practice %in% pr$practice])), by = lsoa]
pr <- merge(pr, flows[, .(list_in_area = sum(patients)), by = practice],
            by = "practice", all.x = TRUE)

lsoa <- merge(merge(merge(data.table(lsoa = lsoa_ids), pop, by = "lsoa"),
                    registered, by = "lsoa"), cen_xy, by = "lsoa")
stopifnot(nrow(lsoa) == length(lsoa_ids))
lsoa[, registered_out := registered - registered_in]

# Population and friction grids ----------------------------------------------

area_ll <- vect(st_transform(st_union(bsc), 4326))
wp <- crop(rast(fetch("worldpop")), ext(area_ll) + 0.02, snap = "out")
cover <- rasterize(area_ll, wp, cover = TRUE)
popr <- mask(wp * cover, cover)
names(popr) <- "pop"

land <- rasterize(area_ll, wp, touches = TRUE)
fr <- mask(resample(rast(fetch("friction")), wp, method = "near"), land)

pts_ll <- st_transform(st_as_sf(pr, coords = c("x", "y"), crs = 27700), 4326)
pcell <- cellFromXY(fr, st_coordinates(pts_ll))
stopifnot(!anyNA(pcell), !anyNA(values(fr)[pcell]))
travel <- rast(lapply(seq_along(pcell), function(j) {
  r <- fr
  v <- values(r, mat = FALSE)
  v[pcell[j]] <- -1
  values(r) <- v
  costDist(r, target = -1)
}))
# Whole minutes, with a 1-minute floor for same-cell pairs.
travel <- round(travel)
travel <- ifel(travel < 1, 1, travel)
travel <- ifel(travel > max_travel, NA, travel)
names(travel) <- pr$practice

cen_ll <- st_transform(st_as_sf(lsoa, coords = c("cent_x", "cent_y"), crs = 27700), 4326)
ccell <- cellFromXY(travel, st_coordinates(cen_ll))
stopifnot(!anyNA(ccell))
tt <- as.matrix(travel[ccell])
dimnames(tt) <- list(lsoa$lsoa, pr$practice)
storage.mode(tt) <- "integer"
travel <- mask(travel, popr) # keep only populated cells in the shipped grid

# Assemble objects ------------------------------------------------------------

scilly_lsoas <- bsc$LSOA21CD[startsWith(bsc$LSOA21NM, scilly_lsoa_prefix)]
scilly_practices <- pr$practice[grepl("^TR2[1-5] ", pr$postcode)] # Scilly postcode districts

cornwall_gp <- st_as_sf(data.frame(
  practice = pr$practice, name = pr$name, postcode = pr$postcode, pcn = pr$pcn,
  gp_fte_extg = pr$gp_fte_extg, gp_fte = pr$gp_fte, gp_fte_extgl = pr$gp_fte_extgl,
  nurse_fte = pr$nurse_fte, list_size = as.integer(pr$list_size),
  list_in_area = as.integer(pr$list_in_area), fte_source = pr$fte_source,
  scilly = pr$practice %in% scilly_practices,
  easting = pr$x, northing = pr$y, stringsAsFactors = FALSE),
  coords = c("easting", "northing"), crs = 27700, remove = FALSE)
cornwall_gp <- st_transform(cornwall_gp, 4326)

idx <- match(bsc$LSOA21CD, lsoa[["lsoa"]])
lsoa <- lsoa[idx]
cen_lonlat <- st_coordinates(st_transform(
  st_as_sf(lsoa, coords = c("cent_x", "cent_y"), crs = 27700), 4326))
cornwall_lsoa <- st_sf(data.frame(
  lsoa = lsoa$lsoa, name = bsc$LSOA21NM, ruc21 = bsc$RUC21NM,
  pop = as.integer(lsoa$pop), pop_0_15 = as.integer(lsoa$pop_0_15),
  pop_16_64 = as.integer(lsoa$pop_16_64), pop_65p = as.integer(lsoa$pop_65p),
  registered = as.integer(lsoa$registered),
  registered_in = as.integer(lsoa$registered_in),
  registered_out = as.integer(lsoa$registered_out),
  cent_x = lsoa$cent_x, cent_y = lsoa$cent_y,
  cent_lon = cen_lonlat[, "X"], cent_lat = cen_lonlat[, "Y"],
  scilly = lsoa$lsoa %in% scilly_lsoas, stringsAsFactors = FALSE),
  geometry = st_geometry(st_transform(bsc, 4326)))

cornwall_flows <- data.frame(lsoa = flows$lsoa, practice = flows$practice,
                             patients = as.integer(flows$patients),
                             stringsAsFactors = FALSE)
cornwall_travel <- tt

# Checks ----------------------------------------------------------------------

stopifnot(
  nrow(cornwall_gp) == 55L, nrow(cornwall_lsoa) == 337L,
  st_crs(cornwall_gp)$epsg == 4326L, st_crs(cornwall_lsoa)$epsg == 4326L,
  all(cornwall_flows$lsoa %in% cornwall_lsoa$lsoa),
  all(cornwall_flows$practice %in% cornwall_gp$practice),
  sum(cornwall_flows$patients) == sum(cornwall_lsoa$registered_in),
  identical(rownames(cornwall_travel), cornwall_lsoa$lsoa),
  identical(colnames(cornwall_travel), cornwall_gp$practice),
  identical(names(travel), cornwall_gp$practice),
  sum(cornwall_lsoa$scilly) == 1L, sum(cornwall_gp$scilly) == 1L,
  # Mainland pairs are complete; the Isles of Scilly have no road link.
  !anyNA(cornwall_travel[!cornwall_lsoa$scilly, !cornwall_gp$scilly]),
  all(is.na(cornwall_travel[cornwall_lsoa$scilly, !cornwall_gp$scilly])))

# Write -----------------------------------------------------------------------

save(cornwall_gp, file = "data/cornwall_gp.rda", compress = "xz")
save(cornwall_lsoa, file = "data/cornwall_lsoa.rda", compress = "xz")
save(cornwall_flows, file = "data/cornwall_flows.rda", compress = "xz")
save(cornwall_travel, file = "data/cornwall_travel.rda", compress = "xz")
writeRaster(popr, "inst/extdata/cornwall_pop.tif", overwrite = TRUE,
            datatype = "FLT4S", gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3"))
saveRDS(names(popr), "inst/extdata/cornwall_pop_names.rds")
writeRaster(travel, "inst/extdata/cornwall_gp_travel.tif", overwrite = TRUE,
            datatype = "INT2U", gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2"))
saveRDS(names(travel), "inst/extdata/cornwall_gp_travel_names.rds")

provenance <- list(
  built = format(Sys.time(), tz = "UTC", usetz = TRUE),
  static = static, api = api_log,
  outputs = vapply(c(list.files("data", "^cornwall_", full.names = TRUE),
                     list.files("inst/extdata", "^cornwall_", full.names = TRUE)),
                   sha256, character(1)))
jsonlite::write_json(provenance, file.path(cache, "provenance.json"),
                     auto_unbox = TRUE, pretty = TRUE)
message("Cornwall GP example data written; provenance in ",
        file.path(cache, "provenance.json"))
