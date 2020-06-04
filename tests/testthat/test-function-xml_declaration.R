test_that("xml_declaration() works", {

  result <- kwb.nextcloud:::xml_declaration()

  expect_is(result, "character")
  expect_length(result, 1L)
  expect_true(startsWith(result, "<?xml version"))
  expect_true(endsWith(result, " ?>"))
})
