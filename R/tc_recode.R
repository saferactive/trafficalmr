#' Recode character strings based on regular expressions
#'
#' This function takes a character string and recodes it.
#' Default recoding functions are available in functions such as
#' `tc_recode_vehicle_type()` and `tc_recode_casualties`.
#'
#' It is based on the function [stringr::str_replace_all()].
#'
#' @param x Character string to recode
#' @param pattern A named character vector with values representing new values.
#' Has the form `c("Car long name" = "Car", "Taxi/Private hire car" = "Taxi")`.
#' `Car long name` will be converted into `Car` in this case.
#' @param pattern_match Character string in the form of
#' `c("car" = "car", "bike" = "bike")` that replaces all parts of the match.
#'
#' @export
#' @examples
#' x = c("car long name", "bus", "a bike long", "a bike")
#' tc_recode(x, pattern = c("car.+" = "car"))
#' tc_recode(x, pattern = c(".+bike.+" = "bike"))
#' tc_recode(x, pattern = c("car.+" = "car", ".+bike.+" = "bike"))
#' tc_recode(x, pattern_match = c("car" = "car", "bike" = "bike"))
tc_recode = function(x, pattern = NULL, pattern_match = NULL) {
  if(is.null(pattern)) {
    pattern = pattern_match
    names(pattern) = paste0(".*", names(pattern), "*.+")
  }
  stringr::str_replace_all(x, pattern)
}
#' Recode vehicle types
#' @export
#' @rdname tc_recode
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
             "Bus*.+" = "Bus", "Minibus*.+" = "Other",
             "Goods*.+" = "HGV"
             )
  ) {
  tc_recode(x, pattern)
}
#' @rdname tc_recode
#' @export
#' @examples
#' (x = stats19::casualties_sample$casualty_type)
#' tc_recode_casualties(x)
#' \dontrun{
#' cas = stats19::get_stats19(2018, "casualties")
#' cas$casualty_type_simple = tc_recode_casualties(cas$casualty_type)
#' table(cas$casualty_type)
#' table(cas$casualty_type_simple)
#' }
tc_recode_casualties = function(x,
                                pattern = NULL,
                                pattern_match = casualties_lookup) {
  tc_recode(x, pattern = pattern, pattern_match = pattern_match)
}

utils::globalVariables(
  c(
    "casualties_lookup"
  )
)
#' @rdname tc_recode
#' @export
#' @examples
#' \dontrun{
#' x = c("30 mph", "40 mph", NA, "20 mph", "70 mph", "10 mph", "60 mph",
#' "50 mph", "85 mph", "80 mph", "15 mph", "75 mph", "5 mph", "45 mph",
#' "110 mph", "100 mph", "25 mph", "12 mph", "115 mph", "230", "105 mph",
#' "90 mph", "7", "30", "35 mph", "65 mph", "55 mph", "40", "80",
#' "20", "95 mph", "60", "64", "10", "none", "5", "walk", "signals",
#' "variable", "50", "4 mph", "national", "15", "125 mph", "250",
#' "300", "320", "100", "180", "225", "2 mph")
#' tc_recode_speeds_uk(x)
#' cas = stats19::get_stats19(2018, "casualties")
#' cas$casualty_type_simple = tc_recode_casualties(cas$casualty_type)
#' table(cas$casualty_type)
#' table(cas$casualty_type_simple)
#' }
tc_recode_speeds_uk = function(
  x,
  pattern = NULL,
  pattern_match = c("30" = "30 mph")
  ) {
  tc_recode(x, pattern = pattern, pattern_match = pattern_match)
}

