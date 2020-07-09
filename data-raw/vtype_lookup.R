## code to prepare `DATASET` dataset goes here

# original hard-coded function:
tc_recode_vehicle_type = function(x) {
  vehicle_type_recoded = x
  vehicle_type_recoded[grepl("otorcyc", x)] = "Motorcycle"
  vehicle_type_recoded[grepl("7.5", x)] = "HGV"
  vehicle_type_recoded[grepl("Pedal", x)] = "Bicycle"
  vehicle_type_recoded[grepl("Goods vehicle - unknown weight", x)] = "HGV"
  vehicle_type_recoded[grepl("Van", x)] = "Van"
  vehicle_type_recoded[grepl("coach", x)] = "Bus"
  vehicle_type_recoded[grepl("Minibus", x)] = "Minibus"
  vehicle_type_recoded[grepl("Taxi", x)] = "Taxi"
  vehicle_type_recoded[grepl("Agricultural vehicle", x)] = "Other"
  vehicle_type_recoded[grepl("Missing", x)] = "Other"
  vehicle_type_recoded[grepl("Mobility", x)] = "Other"
  vehicle_type_recoded[grepl("Tram", x)] = "Other"
  vehicle_type_recoded[grepl("horse", x)] = "Other"
  vehicle_type_recoded[grepl("Other vehicle", x)] = "Other"
  vehicle_type_recoded[grepl("issing", x)] = "Other"
  vehicle_type_recoded[is.na(vehicle_type_recoded)] = "Other"
  vehicle_type_recoded
}

v = stats19::get_stats19(2018, "vehicles")
x = v$vehicle_type[1:5]
stringr::str_view(string = x, pattern = "a*.+")

v$vehicle_type_simple = stringr::str_replace_all(string = v$vehicle_type, c("otor*.+" = "motor"))
table(v$vehicle_type_simple)

vehicle_type_lookup_names = unique(v$vehicle_type)
vehicle_type_lookup = tc_recode_vehicle_type(vehicle_type_lookup)
data.frame(
  original
)
names(vehicle_type_lookup) = vehicle_type_lookup_names
dput(vehicle_type_lookup)

casualties_lookup = c(
  "otorcyc" = "Motorcyclist",
  "7.5" = "HGV_occupant",
  "Goods" = "HGV_occupant",
  "Car occupant" = "Car_occupant",
  "Van" = "Van_occupant",
  "coach" = "Bus_occupant",
  "Minibus" = "Minibus_occupant",
  "Taxi" = "Taxi_occupant",
  "Agri" = "Other",
  "Missing" = "Other",
  "Mobility" = "Other",
  "Tram" = "Other",
  "Horse" = "Other",
  "Other casualty" = "Other",
  "issing" = "Other"
)

usethis::use_data(casualties_lookup, overwrite = TRUE)
