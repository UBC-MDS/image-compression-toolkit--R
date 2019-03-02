context("image_size")
library(testthat)
library(OpenImageR)
library(rprojroot)

root <- is_testthat
root_file <- root$make_fix_file()

image = root_file("data-raw", "test_image_1718.png")

# tests for image_size() function

test_that("correct size is returned", {
  expect_lt(image_size(image), 9 * prod(dim(image))/8 )
})

test_that("type error should be raised if wrong input is entered", {
  expect_error(image_size(5L), "TypeError")
})

test_that("input array is correct shape",{
  expect_true(length(dim(readImage(image))) %in%  c(2, 3))
})