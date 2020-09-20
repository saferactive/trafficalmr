source("../skip-download.R")

test_that("tc_get_cid works", {
  skip_download()
  expect_error(tc_get_cid())
})
