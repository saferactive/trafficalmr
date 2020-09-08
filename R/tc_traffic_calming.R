#' Ge traffic calming measures for an area from OSM
#'
#' Use `osmextract` to get the roads and then filter on `other_tags`
#' with keyword `traffic_calming`
#' (see [OSM Wiki](https://wiki.openstreetmap.org/wiki/Key:traffic_calming)).
#' Currently it only takes the `place` argument.
#'
#' @param place Geographic name or coordinates as per `osmextract`
#' function `oe_get`.
#'
#' @examples
#' \dontrun{
#' tc_traffic_calming("Isle of Wight")
#' }
#' @export
tc_traffic_calming = function(place = NULL) {
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
  i = intersect(names(tc_l), names(tc_p))
  # TODO: do some checks
  tc_pl = rbind(tc_l[, i], tc_p[, i])
  # TODO: some more checks?
  tc_pl
}
