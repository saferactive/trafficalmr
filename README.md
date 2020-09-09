
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- [![Travis build status](https://travis-ci.org/ITSLeeds/trafficalmr.svg?branch=master)](https://travis-ci.org/ITSLeeds/trafficalmr) -->

<!-- [![Coverage status](https://codecov.io/gh/ITSLeeds/trafficalmr/branch/master/graph/badge.svg)](https://codecov.io/github/ITSLeeds/trafficalmr?branch=master) -->

<!-- [![Build status](https://ci.appveyor.com/api/projects/status/gqp3smc04as3qg85?svg=true)](https://ci.appveyor.com/project/layik/trafficalmr-05ana) -->

[![R build
status](https://github.com/saferactive/trafficalmr/workflows/R-CMD-check/badge.svg)](https://github.com/saferactive/trafficalmr/actions)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Codecov test
coverage](https://codecov.io/gh/saferactive/trafficalmr/branch/master/graph/badge.svg)](https://codecov.io/gh/saferactive/trafficalmr?branch=master)

# trafficalmr

This is an R package to support road safety and traffic calming
measures.

# Installation

``` r
remotes::install_github("saferactive/trafficalmr")
```

``` r
library(trafficalmr)
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
#> Attempt downloading from:
#> Data saved at /var/folders/z7/l4z5fwqs2ksfv22ghh2n9smh0000gp/T//RtmpFerJDz/dftRoadSafetyData_Vehicles_2018.csv
v$vehicle_type_simple = tc_recode_vehicle_type(v$vehicle_type)
barplot(table(v$vehicle_type))
barplot(table(v$vehicle_type_simple))
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="50%" /><img src="man/figures/README-unnamed-chunk-8-2.png" width="50%" />

Note that the second plot on the right is much easier to interpret. See
[`?tc_recode()`](https://saferactive.github.io/trafficalmr/reference/tc_recode.html)
for details.

# Development (contributing)

R package `testthat` is used to test this package. Some of the tests,
understandably, need to make calls to OSM and other remote services. The
package has an ENV to avoid running network calls (downloads). If you
like to avoid running them set an ENV var with `DONT_DOWNLOAD_ANYTHING =
false` so that you skip them. If `curl::has_internet()` fails, they will
be skipped anyways.
