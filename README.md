
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- [![Travis build status](https://travis-ci.org/ITSLeeds/traffiCalmr.svg?branch=master)](https://travis-ci.org/ITSLeeds/traffiCalmr) -->

<!-- [![Coverage status](https://codecov.io/gh/ITSLeeds/traffiCalmr/branch/master/graph/badge.svg)](https://codecov.io/github/ITSLeeds/traffiCalmr?branch=master) -->

<!-- [![Build status](https://ci.appveyor.com/api/projects/status/gqp3smc04as3qg85?svg=true)](https://ci.appveyor.com/project/layik/traffiCalmr-05ana) -->

![tic](https://github.com/saferactive/traffiCalmr/workflows/tic/badge.svg)[![Project
Status: Active – The project has reached a stable, usable state and is
being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

# traffiCalmr

This is an R package to support road safety and traffic calming
measures.

# Installation

``` r
remotes::install_github("saferactive/traffiCalmr")
```

``` r
library(traffiCalmr)
```

# Get traffic calming data

``` r
traffic_calming_points = tc_get_osm(bbox = "chapeltown leeds")
```

``` r
mapview::mapview(traffic_calming_points["traffic_calming"])
```

![](https://user-images.githubusercontent.com/1825120/87041987-f2e7b180-c1ea-11ea-9731-b2b9512fd0ea.png)

# Recoding data

Recode vehicle types:

``` r
tc_recode_vehicle_type(c("Bus long", "Motorcycle long name"))
#> [1] "Bus"        "Motorcycle"
```

This can be useful in visualisation:

``` r
v = stats19::get_stats19(year = 2018, type = "vehicles")
#> Files identified: dftRoadSafetyData_Vehicles_2018.csv
#>    http://data.dft.gov.uk.s3.amazonaws.com/road-accidents-safety-data/dftRoadSafetyData_Vehicles_2018.csv
#> Data already exists in data_dir, not downloading
#> Data saved at ~/stats19-data/dftRoadSafetyData_Vehicles_2018.csv
v$vehicle_type_simple = tc_recode_vehicle_type(v$vehicle_type)
barplot(table(v$vehicle_type))
barplot(table(v$vehicle_type_simple))
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="50%" /><img src="man/figures/README-unnamed-chunk-8-2.png" width="50%" />

Note that the second plot on the right is much easier to interpret. See
[`?tc_recode()`](https://saferactive.github.io/traffiCalmr/reference/tc_recode.html)
for details.
