context("test-tc_get_osm")

source("../skip-download.R")

test_that("tc_get_osm works", {
  expect_error(tc_get_osm())
  # network call
  skip_download()
  interventions = tc_get_osm(bbox = "walthamstow village")
  expect_true(any(grepl("bicycle", names(interventions))))
})
