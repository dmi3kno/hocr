context("test-bbox.R")

test_that("bbox to geom works", {
  expect_equal(bbox_to_geometry("0 0 100 200"),
               "100x200+0+0")
})

test_that("bbox slicing works", {
  expect_equal(bbox_slice_x("0 0 100 200", 50),
               list(list(left="0 0 49 200", right="50 0 100 200")))
  expect_equal(bbox_slice_y("0 0 100 200", 100),
               list(list(top="0 0 100 99", bottom="0 100 100 200")))
  expect_equal(bbox_slice_xy("0 0 100 200", 50, 100),
               list(list(top_left="0 0 49 99", top_right="50 0 100 99",
                         bottom_left="0 100 49 200", bottom_right="50 100 100 200")))
})

test_that("bbox validation works", {
  expect_false(bbox_is_valid("5 4 6 3"))
  expect_true(bbox_is_valid("0 0 0 0"))
  expect_equal(bbox_validate(c("5,4,6,3", "1,1,5,6")),
               c(NA, "1,1,5,6"))
})


test_that("bbox aggregation works", {
  expect_equal(bbox_union(c("5,4,6,3", "1,1,5,6")),
               "1 1 6 6")
  expect_true(is.na(bbox_intersect(c("5,4,6,3", "1,1,5,6"))))
})
