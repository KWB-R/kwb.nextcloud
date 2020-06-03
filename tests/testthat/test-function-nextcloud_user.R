test_that("nextcloud_user() works", {

  result <- kwb.nextcloud:::nextcloud_user()

  expect_is(result, "character")
  expect_length(result, 1L)
  expect_true(nchar(result) > 1L)
})
