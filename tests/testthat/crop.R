library(testthat)
context("crop")

# tests for the crop() function

test_that("function returns the correct output shape", {
  expect_equal(dim(crop(image, 15, 10)), c(15, 10, 3))
  expect_equal(dim(crop(image, 10, 5)), c(10, 5, 3))
  expect_equal(dim(crop(image, 6, 6)), c(6, 6, 3))
  expect_equal(dim(crop(image, 5, 5)), c(5, 5, 3))
  expect_equal(dim(crop(image, 1, 1)), c(1, 1, 3))
  expect_equal(dim(crop(image, dim(image)[1], dim(image)[2])), dim(image))
})

test_that("function raises value error when invalid values passed in for height and width", {
  expect_error(crop(image, -1, 10), "ValueError")
  expect_error(crop(image, 10, -1), "ValueError")
  expect_error(crop(image, 0, 0), "ValueError")
  expect_error(crop(image, 0, 10), "ValueError")
  expect_error(crop(image, 10, 0), "ValueError")
  expect_error(crop(image, dim(image)[1] + 1, 10), "ValueError")
  expect_error(crop(image, 10, dim(image)[2] + 1), "ValueError")
  expect_error(crop(image, -10000, 10), "ValueError")
  expect_error(crop(image, 10, -10), "ValueError")
  
})

test_that("function raises type error when invalid types are passed in", {
  expect_error(crop(image, 9.5, 10), "TypeError")
  expect_error(crop(image, 10, 9.5), "TypeError")
  expect_error(crop(image, 9.5, 9.5), "TypeError")
  expect_error(crop(image, -9.9, -4.5), "TypeError")
  expect_error(crop(image, "10", TRUE), "TypeError")
  expect_error(crop("image.jpg", 10, 10), "TypeError")
})

test_that("input array is correct shape",{
  expect_equal(length(dim(image)), 3)
})

