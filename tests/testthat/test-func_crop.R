context("crop")
library(testthat)
library(OpenImageR)
library(here)
library(rprojroot)


#test_images = c("data/test_image_1717.png", "data/test_image_1718.png", "data/test_image_1816.png", #"data/test_image_1819.png")

# Set package root
root <- is_testthat
root_file <- root$make_fix_file()

image = root_file("data-raw","test_image_1816.png")
out_img <- root_file("Test_images", "test_crop.png")

# tests for the crop() function

test_that("function returns the correct output shape", {
  expect_equal(dim(readImage(crop(image, 15L, 10L,out_img)))[1:2], c(15, 10))
  expect_equal(dim(readImage(crop(image, 10L, 5L,out_img)))[1:2], c(10, 5))
  expect_equal(dim(readImage(crop(image, 6L, 6L,out_img)))[1:2], c(6, 6))
  expect_equal(dim(readImage(crop(image, 5L, 5L,out_img)))[1:2], c(5, 5))
  expect_equal(dim(readImage(crop(image, 3L, 7L,out_img)))[1:2], c(3, 7))

  img=crop(image, dim(readImage(image))[1], dim(readImage(image))[2],out_img)
  expect_equal(dim(readImage(img)), dim(readImage(image)))
})

test_that("function raises value error when invalid values passed in for height and width", {
  expect_error(crop(image, -1L, 10L,out_img), "ValueError")
  expect_error(crop(image, 10L, -1L,out_img), "ValueError")
  expect_error(crop(image, 0L, 0L,out_img), "ValueError")
  expect_error(crop(image, 0L, 10L,out_img), "ValueError")
  expect_error(crop(image, 10L, 0L,out_img), "ValueError")
  expect_error(crop(image, dim(readImage(image))[1] + 1, 10,out_img), "ValueError")
  expect_error(crop(image, 10L, dim(readImage(image))[2] + 1,out_img), "ValueError")
  expect_error(crop(image, -10000L, 10L,out_img), "ValueError")
  expect_error(crop(image, 10L, -10L,out_img), "ValueError")

})

test_that("function raises type error when invalid types are passed in", {
  expect_error(crop(image, 9.5, 10,out_img), "TypeError")
  expect_error(crop(image, 10, 9.5,out_img), "TypeError")
  expect_error(crop(image, 9.5, 9.5,out_img), "TypeError")
  expect_error(crop(image, -9.9, -4.5,out_img), "ValueError")
  expect_error(crop(image, "10", TRUE,out_img), "TypeError")
  expect_error(crop(55, 10, 10,out_img), "TypeError")
  expect_error(crop(image, 10, 10,TRUE), "TypeError")
})

test_that("input array is correct shape",{
  expect_equal(length(dim(readImage(image))), 3)
})

