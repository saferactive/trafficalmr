#' Stats19 schema and variables
#'
#' `casualties_lookup` contains a lookup table for recoding casualty types.
#'
#' @docType data
#' @keywords datasets
#' @name casualties_lookup
NULL
#' Sample STATS19 data
#'
#' @docType data
#' @keywords datasets
#' @aliases casualties_wf vehicles_wf crash_summary_wy
#' @name crashes_wf
#' @examples
#' \dontrun{
#' crashes = stats19::get_stats19(2014:2018, "acc")
#' lnd = spData::lnd
#' waltham_forest = lnd[lnd$NAME == "Waltham Forest", ]
#' crashes_sf = stats19::format_sf(crashes, lonlat = TRUE)
#' table(crashes_sf$police_force)
#' crashes_wf = crashes_sf[waltham_forest, ]
#' plot(crashes_wf)
#' crashes_wf = sf::st_drop_geometry(crashes_wf)
#' # crashes_wy = crashes[crashes$police_force == "West Yorkshire", ]
#' casualties = stats19::get_stats19(2014:2018, "cas")
#' casualties_wf = casualties[casualties$accident_index %in% crashes_wf$accident_index, ]
#' vehicles = stats19::get_stats19(2014:2018, "veh")
#' vehicles_wf = vehicles[vehicles$accident_index %in% crashes_wf$accident_index, ]
#'
#' usethis::use_data(crashes_wf, overwrite = TRUE)
#' usethis::use_data(casualties_wf)
#' usethis::use_data(vehicles_wf)
#'
#' crashes = crashes_wf
#' casualties = casualties_wf
#' vehicles = vehicles_wf
#' }
NULL

#' Example OSM dataset
#'
#' @docType data
#' @keywords datasets
#' @name tc_data_osm
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(sf)
#' library(osmextract)
#' osm_lines = oe_get("Greater London", extra_tags = c("ref", "maxspeed", "bicyle", "traffic_calming"))
#' names(osm_lines)
#' stamford = osm_lines %>%
#'  filter(name == "Stamford Street")
#' stamford_buffer = stamford %>%
#'  sf::st_union() %>%
#'  stplanr::geo_buffer(dist = 200)
#' mapview::mapview(stamford_buffer)
#' osm_case_study = osm_lines[stamford_buffer, , op = sf::st_within]
#' table(osm_case_study$highway)
#' table(osm_case_study$traffic_calming)
#' tc_data_osm = osm_case_study %>%
#'  filter(!is.na(highway))
#' mapview::mapview(tc_data_osm)
#' object.size(tc_data_osm) / 1e6 # ~0.5MB
#' usethis::use_data(tc_data_osm)
#' }
NULL
