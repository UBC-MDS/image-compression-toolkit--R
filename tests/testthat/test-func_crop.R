context("crop")
library(testthat)
library(OpenImageR)


test_images = c("data/test_image_1717.png", "data/test_image_1718.png", "data/test_image_1816.png", "data/test_image_1819.png")
image = "data/test_image_1819.png"

# tests for the crop() function

test_that("function returns the correct output shape", {
  expect_equal(dim(readImage(crop(image, as.integer(15), as.integer(10)))), c(15, 10, 4))
  expect_equal(dim(readImage(crop(image, as.integer(10), as.integer(5)))), c(10, 5, 4))
  expect_equal(dim(readImage(crop(image, as.integer(6), as.integer(6)))), c(6, 6, 4))
  expect_equal(dim(readImage(crop(image, as.integer(5), as.integer(5)))), c(5, 5, 4))
  expect_equal(dim(readImage(crop(image, as.integer(1), as.integer(1)))), c(1, 1, 4))
  
  img=crop(image, dim(readImage(image))[1], dim(readImage(image))[2])
  expect_equal(dim(readImage(img)), dim(readImage(image)))
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
  expect_equal(length(dim(readImage(image))), 3)
})

