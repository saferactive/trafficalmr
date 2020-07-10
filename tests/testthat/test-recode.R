test_that("tc_recode works", {
  x = c("car long name", "bus", "a bike long", "a bike")
  expect_error(tc_recode())
  # tc_recode(x, pattern = c("car.+" = "car"))
  short = c("car" = "car", "bike" = "bike")
  r = tc_recode(x, pattern_match = short)
  expect_true(all(short %in% r))
})
