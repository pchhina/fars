context("filename creation")

test_that("filename is correct", {
              expect_equal(make_filename(2012), "accident_2012.csv.bz2")
})
