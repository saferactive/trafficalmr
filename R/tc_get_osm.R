#' Get traffic calming interventions from OpenStreetMap
#'
#' @param bbox A bounding box or name passed to `osmdata`
#' @param value A type of `traffic_calming` intervention.
#' Possible values include `yes`, `bump` and `hump`.
#' See [wiki.openstreetmap.org](https://wiki.openstreetmap.org/wiki/Key:traffic_calming)
#' for details.
#' @param output The type of `osmdata` object to return, `osm_points` (default),
#' `osm_lines` etc (see [osmdata](https://cran.r-project.org/package=osmdata) for details)
#'
#' @return An sf object
#' @export
#' @examples
#' interventions = tc_get_osm(bbox = "walthamstow village")
tc_get_osm = function(bbox = NULL, value = NULL, output = "osm_points") {
  res = osmdata::osmdata_sf(
    osmdata::add_osm_feature(
      opq = osmdata::opq(bbox = bbox),
      key = "traffic_calming",
      value = value,
      value_exact = TRUE
    )
  )
  res[[output]]
}
