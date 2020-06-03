test_that("request_body() works", {

  result <- kwb.nextcloud:::request_body()

  expect_true(startsWith(result, "<?xml version"))
  expect_true(endsWith(result, " ?>"))
})
