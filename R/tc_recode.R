#' Recode vehicle types
#'
#'  This function was designed to simplify vehicle types from STAT19 data
#' @param x Vehicle types, e.g. from the vehicle_type column in STATS19 data
#' @param pattern A named character vector with values representing new vehicle type names.
#' Has the form `c("Car" = "Car", "Taxi/Private hire car" = "Taxi")`. See documentation
#' for details.
#' @export
#' @examples
#' (x = stats19::vehicles_sample$vehicle_type)
#' tc_recode_vehicle_type(x)
#' \dontrun{
#' v = stats19::get_stats19(2018, "vehicles")
#' v$vehicle_type_simple = tc_recode_vehicle_type(v$vehicle_type)
#' table(v$vehicle_type)
#' table(v$vehicle_type_simple)
#' }
tc_recode_vehicle_type = function(
  x,
  pattern = c("Taxi*.+" = "Taxi", "Van*.+" = "Van",
             "Pedal cycle" = "Bicycle", "(M|m)otorcycle*.+|Elec*.+" = "Motorcycle",
             "Data*.+|Other*.+|Agri*.+|Ridden*.+|Mobility*.+|Tram*.+" = "Other",
             "Bus*.+" = "Bus", "Minibus*.+" = "Minibus",
             "Goods*.+" = "HGV"
             )
  ) {
  stringr::str_replace_all(x, pattern)
}
