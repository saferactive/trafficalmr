# Aim: get and visualise road speed data from OSM

remotes::install_github("itsleeds/osmextractr")
library(osmextractr)
library(tidyverse)
osm_london_highways = oe_get("London", provider = "bbbike")
nrow(osm_london_highways) # 500k rows
saveRDS(osm_london_highways, "osm_london_highways.Rds")
piggyback::pb_upload("osm_london_highways.Rds")
osm_london_highways_clipped = sf::st_intersection(osm_london_highways, spData::lnd)
nrow(osm_london_highways)

get_ini_layer_defaults = function(layer) {
  # generate defaults for layer attributes
  # ini_file = readLines("https://github.com/OSGeo/gdal/raw/master/gdal/data/osmconf.ini")
  # attributes = ini_file[grepl(pattern = "^attributes=", ini_file)]
  # layer_names = ini_file[grepl(pattern = "^\\[", x = ini_file)]
  # layer_names = gsub(pattern = "\\[|\\]", replacement = "", x = layer_names)
  # attributes = gsub(pattern = "attributes=", "", attributes)
  # l = sapply(attributes, function(x) names(read.csv(text = x)))
  # class(l)
  # names(l) = layer_names
  # dput(l)
  l = list(
    points = c(
      "name",
      "barrier",
      "highway",
      "ref",
      "address",
      "is_in",
      "place",
      "man_made"
    ),
    lines = c(
      "name",
      "highway",
      "waterway",
      "aerialway",
      "barrier",
      "man_made"
    ),
    multipolygons = c(
      "name",
      "type",
      "aeroway",
      "amenity",
      "admin_level",
      "barrier",
      "boundary",
      "building",
      "craft",
      "geological",
      "historic",
      "land_area",
      "landuse",
      "leisure",
      "man_made",
      "military",
      "natural",
      "office",
      "place",
      "shop",
      "sport",
      "tourism"
    ),
    multilinestrings = c("name", "type"),
    other_relations = c("name", "type")
  )
  l[[layer]]
}


make_ini_attributes = function(attributes,
                               layer,
                               defaults = get_ini_layer_defaults(layer),
                               append = TRUE) {
  attributes_default_ini = paste0("attributes=", paste(defaults, collapse = ","))
  if (append) {
    attributes = c(defaults, attributes)
  }
  attributes_default_ini_new = paste0("attributes=", paste(attributes, collapse = ","))
  ini_file = readLines("https://github.com/OSGeo/gdal/raw/master/gdal/data/osmconf.ini")
  sel_attributes = grepl(pattern = attributes_default_ini, x = ini_file)
  message("Old attributes: ", ini_file[sel_attributes])
  message("New attributes: ", attributes_default_ini_new)
  ini_file[sel_attributes] = attributes_default_ini_new
  ini_file
}
ini_new = make_ini_attributes(attributes = c("maxspeed", "oneway", "lanes", "traffic_calming"))
ini_new
f_ini = file.path(tempdir(), "ini_new.ini")
writeLines(ini_new, f_ini)

osm_iow = oe_get("Isle of Wight")
osm_iow_extra = oe_get("Isle of Wight", extra_attributes = "maxpeed", force_vectortranslate = TRUE)
names(osm_london_highways_gf) # extra attributes missing
names(osm_iow_extra)
osm_london_highways_gf = oe_get("Greater London", osmconf_ini = f_ini)
names(osm_london_highways_gf) # extra attributes missing
osm_london_highways_gf = oe_get("Greater London", extra_attributes = c("maxspeed", "oneway", "lanes", "traffic_calming"), force_vectortranslate = TRUE)
names(osm_london_highways_gf)
nrow(osm_london_highways_gf) # 400k
table(osm_london_highways$highway)
ht = table(osm_london_highways_gf$highway)
ht2 = table(osm_london_highways_gf$highway, osm_london_highways_gf$maxspeed)
ht2

osm_london_highways_gf %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(highway) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  pull(highway)

osm_top_combos = osm_london_highways_gf %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(highway, maxspeed) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
osm_top_combos

osm_london_highways_gf_common = osm_london_highways_gf %>%
  filter(str_detect(string = highway, pattern = "primary|secondary|tert"))
summary(as.factor(osm_london_highways_gf_common$highway))
summary(is.na(osm_london_highways_gf_common$highway))

nrow(osm_london_highways_gf_common)
plot(osm_london_highways_gf_common$geometry)
summary(osm_london_highways_gf_common$maxspeed)
plot(osm_london_highways_gf_common["maxspeed"])

summary(is.na(osm_london_highways_gf$highway))
