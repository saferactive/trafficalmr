library(tidyverse)
remotes::install_github("saferactive/trafficalmr")

ac2010_2019 = stats19::get_stats19(year = 2010:2019, type = "accidents")

# ac2019 = stats19::get_stats19(year = 2019, type = "ac", output_format = "sf", lonlat = TRUE)
ac2019 = stats19::get_stats19(year = 2019, type = "ac")
View(ac2019)
ac2019
ac2019_subset = ac2019 %>%
  filter(police_force == "West Yorkshire") %>%
  # select(-matches("east|north|^date$|^time"))
  select(matches("index|tude|sev|number_of_v|number_of_c|conditions|datet"))
ca2019 = stats19::get_stats19(year = 2019, type = "cas")
ca_subset = ca2019 %>%
  select(matches("accident_index|class|sex|age_of|casualty_t"))

ve2019 = stats19::get_stats19(year = 2019, type = "veh")

ve2019$vehicle_simple = trafficalmr::tc_recode_vehicle_type(ve2019$vehicle_type)
sort(table(ve2019$vehicle_type))
sort(table(ve2019$vehicle_simple))

l = c("Bicycle", "Motorcycle", "Car", "Taxi", "Other", "Van", "Bus", "HGV")
ve2019$vehicle = factor(ve2019$vehicle_simple, levels = l, ordered = TRUE)
summary(ve2019$vehicle)

veh_largest = ve2019 %>%
  group_by(accident_index) %>%
  summarise(largest_vehicle = max(vehicle))

# join togeth
ca_joined = inner_join(ca_subset, ac2019_subset)
ca_joined = inner_join(ca_joined, veh_largest)

write_csv(ca_joined, "ca_joined.csv")
piggyback::pb_upload("ca_joined.csv")
piggyback::pb_download_url("ca_joined.csv")
