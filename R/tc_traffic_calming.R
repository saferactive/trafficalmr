#' Ge traffic calming measures for an area from OSM
#'
#' Use `osmextract` to get the roads and then filter on `other_tags`
#' with keyword `traffic_calming`
#' (see [OSM Wiki](https://wiki.openstreetmap.org/wiki/Key:traffic_calming)).
#' Currently it only takes the `place` argument.
#'
#' @param place Geographic name or coordinates as per `osmextract`
#' function `oe_get`.
#' @param convert_to_points Should linestring geometries be converted to points?
#' `FALSE` by default.
#'
#' @examples
#' \dontrun{
#' tc_interventions = tc_traffic_calming("Isle of Wight")
#' plot(tc_interventions)
#' tc_interventions
#' summary(tc_interventions$geometry) # points and lines in there
#' tc_points = tc_traffic_calming("Isle of Wight", convert_to_points = TRUE)
#' summary(tc_points$geometry) # points only
#' }
#' @export
tc_traffic_calming = function(place = NULL, convert_to_points = FALSE) {
  stopifnot(!is.null(place) || !is.na(place))
  # any value for traffic_calming key
  ql = "SELECT * FROM 'lines' WHERE traffic_calming LIKE '%'"
  qp = "SELECT * FROM 'points' WHERE traffic_calming LIKE '%'"
  e = c("traffic_calming", "maxspeed")
  # defaults to lines
  tc_l = osmextract::oe_get(
    place = place, query = ql, extra_tags = e)
  # specify table: layer = "points",
  tc_p = osmextract::oe_get(
    place = place, query = qp, extra_tags = e,
    layer = "points")
  # they have different column names
  if(convert_to_points) {
    tc_l = sf::st_centroid(tc_l)
  }
  i = intersect(names(tc_l), names(tc_p))
  # TODO: do some checks
  tc_pl = rbind(tc_l[, i], tc_p[, i])
  # TODO: some more checks?
  tc_pl
}
