test_that("element_prop() works", {

  result <- kwb.nextcloud:::element_prop()

  expect_identical(result, c("<d:prop>", "</d:prop>"))
})
