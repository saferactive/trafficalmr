context("test-tc_record*")

test_that("tc_recode works", {
  x = c("car long name", "bus", "a bike long", "a bike")
  expect_error(tc_recode())
  # tc_recode(x, pattern = c("car.+" = "car"))
  short = c("car" = "car", "bike" = "bike")
  r = tc_recode(x, pattern_match = short)
  expect_true(all(short %in% r))
})

test_that("tc_recode_casualties works", {
  expect_error(tc_recode_casualties())
})

test_that("tc_recode_speeds_uk works", {
  expect_error(tc_recode_speeds_uk())
})

test_that("tc_recode_vehicle_type works", {
  expect_error(tc_recode_vehicle_type())
  x = stats19::vehicles_sample$vehicle_type
  expect_true(identical(
    tc_recode_vehicle_type(x),
    c("Car", "HGV", "Car")
  ))
})
