context("compress")
library(pryr)
library(testthat)
library(OpenImageR)
library(rprojroot)

root <- is_testthat
root_file <- root$make_fix_file()

img = root_file("data-raw", "test_image_1718.png")
small_image = root_file("data-raw", "image_1.png")
big_image =  root_file("data-raw", "bigger_test.png")
out = root_file("data-raw", "test.png")

#Test Case 1 : Invalid value .
test_that("compress(img,b) for checking type of b", {

  expect_error(compress(img, -1, out),"ValueError")
  expect_error(compress(img, 9, out),"ValueError")
  expect_error(compress(img, 0, out),"ValueError")
  expect_error(compress(img, 1000, out),"ValueError")
  expect_error(compress(img, -1000, out),"ValueError")
})

#Test Case 2 : Invalid Type
test_that("compress(img,b)", {

  expect_error(compress(img, 2.2, out),"TypeError")
  expect_error(compress(img, TRUE, out),"TypeError")
  expect_error(compress(readImage(img),6, out),"TypeError")
  expect_error(compress(img, 5, 5), "TypeError")
})

#Test Case 3 : Checking new image object size
test_that("compress(img,b)  checking the compressed image size", {

  expect_less_than(image_size(compress(img, 3, out)), 7/8 * image_size(img))
  expect_less_than(image_size(compress(img, 1, out)), 5/8 * image_size(img))
  expect_less_than(image_size(compress(img, 6, out)), 8/8 * image_size(img))
  expect_less_than_equal(image_size(compress(img, 8, out)), image_size(img))
  expect_less_than(image_size(compress(big_image, 1, out), 5/8 * image_size(img)))

})

#Test Case 4 : Checking new image shape
test_that("compress(img,b)  checking the compressed image shape", {
  
  expect_equal(dim(readImage(compress(img, 3, out))), dim(readImage(img)))
  expect_equal(dim(readImage(compress(img, 1, out))), dim(readImage(img)))
  expect_equal(dim(readImage(compress(img, 6, out))), dim(readImage(img)))
})

# Check for images that can't be compressed
test_that("compress(img, b) check that errors are raised when image cannot be compressed", {
  expect_error(compress(small_image, 1, out), "Can't compress further.")
  expect_error(compress(small_image, 6, out), "Can't comrpess further.")
  expect_error(compress(big_image, 4, out), "Choose a smaller b.")
  expect_error(compress(big_image, 7, out),"Choose a smaller b.")
})

# check for that input image has correct shape
test_that("input array is correct shape",{
  expect_true(length(dim(readImage(image))) %in%  c(2, 3))
})




