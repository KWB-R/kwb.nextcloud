#
# This test file has been generated by kwb.test::create_test_files()
# launched by user micha on 2020-06-03 23:21:47.
# Your are strongly encouraged to modify the dummy functions
# so that real cases are tested. You should then delete this comment.
#

test_that("parse_href() works", {

  expect_error(
    kwb.nextcloud:::parse_href()
    # Argument "href" fehlt (ohne Standardwert)
  )

})

