context("compress")
#library(pryr)
library(testthat)
library(OpenImageR)
library(rprojroot)

root <- is_testthat
root_file <- root$make_fix_file()

img <- root_file("data-raw", "test_image_1718.png")
img_1819 <- root_file("data-raw", "test_image_1819.png")
small_image <- root_file("data-raw", "image_1.png")
big_image <- root_file("data-raw", "bigger_test.png")
out <- root_file("data-raw", "test.png")
ones <- root_file("data-raw", "ones.png")

#Test Case 1 : Invalid value .
test_that("compress(img,b,out) for checking value of b", {

  expect_error(compress(img, -1L, out),"ValueError")
  expect_error(compress(img, 9L, out),"ValueError")
  expect_error(compress(img, 0L, out),"ValueError")
  expect_error(compress(img, 1000L, out),"ValueError")
  expect_error(compress(img, -1000L, out),"ValueError")
})

#Test Case 2 : Invalid Type
test_that("compress(img,b,out) checks TypeError gets raised for invalid Type", {

  expect_error(compress(img, 2.2, out),"TypeError")
  expect_error(compress(img, TRUE, out),"TypeError")
  expect_error(compress(33L, 6L, out),"TypeError")
  expect_error(compress(img, 5L, TRUE),"TypeError")
})

#Test Case 3 : Checking new image object size
test_that("compress(img,b,out)  checking the compressed image size", {

  expect_lt(image_size(compress(img, 3L, out)), 7/8 * image_size(img))
  expect_lt(image_size(compress(img, 4L, out)), 8/8 * image_size(img))
  expect_lte(image_size(compress(img, 8L, out)), image_size(img))
  expect_lt(image_size(compress(big_image, 1L, out)), 5/8 * image_size(big_image))

})

#Test Case 4 : Checking new image shape
test_that("compress(img,b,out)  checking the compressed image shape", {

  expect_equal(dim(readImage(compress(img, 3L, out))), dim(readImage(img)))
  expect_equal(dim(readImage(compress(img, 1L, out))), dim(readImage(img)))
  expect_equal(dim(readImage(compress(img, 6L, out))), dim(readImage(img)))
})

#Test Case 5: Check for images that can't be compressed
test_that("compress(img, b,out) check that errors are raised when image cannot be compressed", {
  expect_error(compress(small_image, 1L, out), "Can't compress further.")
<<<<<<< HEAD
  expect_error(compress(small_image, 6L, out), "Choose a smaller b.")
  #expect_error(compress(ones, 1L, out), "Can't compress further.")
  expect_error(compress(big_image, 4L, out), "Choose a smaller b.")
  expect_error(compress(big_image, 7L, out), "Choose a smaller b.")
=======
  #expect_error(compress(small_image, 5L, out), "Choose a smaller b.")
  expect_error(compress(ones, 1L, out), "Can't compress further.")
  expect_error(compress(big_image, 4L, out), "Choose a smaller b.")
  #expect_error(compress(big_image, 7L, out), "Choose a smaller b.")
>>>>>>> master
  #expect_error(compress(img_1819, 5L, out), "Choose a smaller b.")

})

# check for that input image has correct shape
test_that("input array is correct shape",{
  expect_true(length(dim(readImage(img))) %in%  c(2, 3))
})




