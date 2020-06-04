test_that("path_to_file_href() works", {

  expect_true(
    startsWith(kwb.nextcloud:::path_to_file_href(), "/remote.php/dav/files")
  )

})
