# Aim: get and visualise road speed data from OSM

remotes::install_github("itsleeds/osmextractr")
library(osmextractr)
library(tidyverse)
# osm_london_highways = oe_get("London", provider = "bbbike")
nrow(osm_london_highways) # 500k rows
saveRDS(osm_london_highways, "osm_london_highways.Rds")
piggyback::pb_upload("osm_london_highways.Rds")
osm_london_highways_clipped = sf::st_intersection(osm_london_highways, spData::lnd)
nrow(osm_london_highways)

# osm_london_highways_gf = oe_get("Greater London", extra_attributes = c("maxspeed", "oneway", "lanes", "traffic_calming"), force_vectortranslate = TRUE)
# saveRDS(osm_london_highways_gf, "osm_london_highways_gf.Rds")
# piggyback::pb_upload("osm_london_highways_gf.Rds")
osm_london_highways_gf = readRDS("osm_london_highways_gf.Rds")
osm_london_highways_gf %>%
  sample_n(5000) %>%
  mapview::mapview()
names(osm_london_highways_gf)
nrow(osm_london_highways_gf) # 400k
table(osm_london_highways$highway)
ht2 = table(osm_london_highways_gf$highway, osm_london_highways_gf$maxspeed)
ht2

osm_london_highways_gf %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(highway) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  pull(highway)

osm_london_highways_gf %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(maxspeed) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1:20)

osm_london_highways_gf = osm_london_highways_gf %>%
  mutate(max_speed = as.character(tc_recode_speeds_uk(maxspeed)))

osm_london_highways_gf %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(as.character(max_speed)) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1:20) %>%
  knitr::kable()

osm_london_highways_with_speeds = osm_london_highways_gf %>%
  filter(!is.na(max_speed))

summary(osm_london_highways_with_speeds$max_speed)

plot(osm_london_highways_with_speeds["max_speed"])
library(tmap)

head(osm_london_highways_with_speeds$max_speed)
tm_shape(osm_london_highways_with_speeds) +
  tm_lines("max_speed")

as.character(unique(osm_london_highways_gf$maxspeed))

osm_top_combos = osm_london_highways_gf %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(highway, maxspeed) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  na.omit() %>%
  slice(1:20)
osm_top_combos

unique(osm_london_highways_gf$maxspeed)

osm_london_highways_gf_common = osm_london_highways_gf %>%
  filter(str_detect(string = highway, pattern = "primary|secondary|tert"))
summary(as.factor(osm_london_highways_gf_common$highway))
summary(is.na(osm_london_highways_gf_common$highway))

nrow(osm_london_highways_gf_common)
plot(osm_london_highways_gf_common$geometry)
summary(osm_london_highways_gf_common$maxspeed)
plot(osm_london_highways_gf_common["maxspeed"])

summary(is.na(osm_london_highways_gf$highway))
