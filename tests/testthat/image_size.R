library(testthat)
context("image_size")
# tests for image_size() function

test_that("correct size is returned", {
  expect_lt(image_size(image), 9 * prod(dim(image))/8 )
})

test_that("type error should be raised if wrong input is entered", {
  expect_error(image_size("image.jpg"), "TypeError")
})

test_that("input has correct shape", {
  expect_equal(length(dim(image)), 3)
})