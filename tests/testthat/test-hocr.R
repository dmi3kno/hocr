context("test-hocr.R")

x <- system.file("testdata", "jo_small.txt", package = "hocr")

test_that("parsing works", {
  expect_equal(nrow(hocr_parse(x)), 4)
  expect_equal(ncol(hocr_parse(x)), 13)
})

