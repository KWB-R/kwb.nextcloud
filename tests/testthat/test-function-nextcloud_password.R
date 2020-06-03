test_that("nextcloud_password() works", {

  result <- kwb.nextcloud:::nextcloud_password()

  expect_is(result, "character")
  expect_length(result, 1L)
  expect_true(nchar(result) > 0L)
})
