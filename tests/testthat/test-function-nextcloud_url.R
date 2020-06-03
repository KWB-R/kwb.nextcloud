test_that("nextcloud_url() works", {

  expect_true(startsWith(kwb.nextcloud:::nextcloud_url(), "https://"))

})
