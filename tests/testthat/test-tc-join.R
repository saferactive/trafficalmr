test_that("tc_joing_stats19 works", {
  ac = stats19::get_stats19(year = 2019, type = "ac")
  # watch out for `output_format = "sf"` as test will fail if given
  ca = stats19::get_stats19(year = 2019, type = "ca")
  ve = stats19::get_stats19(year = 2019, type = "ve")
  vel = nrow(tc_join_stats19(ac, ca, ve, level = "veh"))
  cal = nrow(tc_join_stats19(ac, ca, ve, level = "cas"))
  acl = nrow(tc_join_stats19(ac, ca, ve))
  expect_true(vel > nrow(ve))
  expect_true(cal > nrow(ca))
  expect_true(acl > max(nrow(ca), nrow(ve)))
  expect_true(vel == cal && cal && acl)
})
