test_that("attributes_cloud_urls() works", {

  f <- kwb.nextcloud:::attributes_cloud_urls
  result <- f()
  expect_is(result, "list")
})
