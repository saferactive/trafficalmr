#' Recode casualty types
#'
#'  This function was designed to simplify casualty classification from STAT19 data
#' @param x The type of vehicle the casualty occupied, from the casualty_type column in STATS19 data
#' @export
#' @examples
#' (x = stats19::casualties_sample$casualty_type)
#' (x = stats19::casualties_sample$casualty_type)
#' tc_recode_casualty_type(x)
#' \dontrun{
#' v = stats19::get_stats19(2018, "casualties")
#' v$casualty_type_simple = tc_recode_casualties(v$casualty_type)
#' table(v$casualty_type)
#' table(v$casualty_type_simple)
#' }
tc_recode_casualties = function(x) {
  casualty_type_recoded = x
  casualty_type_recoded[grepl("Cyclist", x)] = "Cyclist"
  casualty_type_recoded[grepl("Pedestrian", x)] = "Pedestrian"
  casualty_type_recoded[grepl("otorcyc", x)] = "Motorcyclist"
  casualty_type_recoded[grepl("7.5", x)] = "HGV_occupant"
  casualty_type_recoded[grepl("Goods casualty - unknown weight", x)] = "HGV_occupant"
  casualty_type_recoded[x == "Car occupant"] = "Car_occupant"
  casualty_type_recoded[grepl("Van", x)] = "Van_occupant"
  casualty_type_recoded[grepl("coach", x)] = "Bus_occupant"
  casualty_type_recoded[grepl("Minibus", x)] = "Minibus_occupant"
  casualty_type_recoded[grepl("Taxi", x)] = "Taxi_occupant"
  casualty_type_recoded[grepl("Agricultural casualty", x)] = "Other"
  casualty_type_recoded[grepl("Missing", x)] = "Other"
  casualty_type_recoded[grepl("Mobility", x)] = "Other"
  casualty_type_recoded[grepl("Tram", x)] = "Other"
  casualty_type_recoded[grepl("horse", x)] = "Other"
  casualty_type_recoded[grepl("Other casualty", x)] = "Other"
  casualty_type_recoded[grepl("issing", x)] = "Other"
  casualty_type_recoded[is.na(casualty_type_recoded)] = "Other"
  casualty_type_recoded
}
