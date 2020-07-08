#' Recode vehicle types
#'
#'  This function was designed to simplify vehicle types from STAT19 data
#' @param x Vehicle types, e.g. from the vehicle_type column in STATS19 data
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
tc_recode_vehicle_type = function(x) {
  vehicle_type_recoded = x
  vehicle_type_recoded[grepl("otorcyc", x)] = "Motorcycle"
  vehicle_type_recoded[grepl("7.5", x)] = "HGV"
  vehicle_type_recoded[grepl("Pedal", x)] = "Bicycle"
  vehicle_type_recoded[grepl("weight", x)] = "OtherGoods"
  vehicle_type_recoded[grepl("coach|Minibus", x)] = "Minibus"
  vehicle_type_recoded[grepl("Van", x)] = "OtherGoods"
  vehicle_type_recoded[grepl("Taxi", x)] = "Taxi"
  vehicle_type_recoded[is.na(vehicle_type_recoded)] = "OtherOrUnknown"
  vehicle_type_recoded
}
