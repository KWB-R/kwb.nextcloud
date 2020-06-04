test_that("fileid_to_version_href() works", {

  expect_true(grepl("/1", kwb.nextcloud:::fileid_to_version_href(1)))

})
