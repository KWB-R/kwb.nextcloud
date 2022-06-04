test_that("path_to_file_href() works", {

  f <- kwb.nextcloud:::path_to_file_href

  expect_true(startsWith(f(leading_slash = TRUE), "/remote.php/dav/files"))
  expect_true(startsWith(f(leading_slash = FALSE), "remote.php/dav/files"))
})
