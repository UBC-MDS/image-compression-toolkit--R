context("compress")
library(pryr)
library(testthat)
library(OpenImageR)

img = "data/test_image_1718.png"

#Test Case 1 : Invalid value .
test_that("compress(img,b) for checking type of b", {

  expect_error(compress(img,-1),"Invalid value passed")
  expect_error(compress(img,9),"Invalid value passed")
  expect_error(compress(img,0),"Invalid value passed")
  expect_error(compress(img,1000),"Invalid value passed")
  expect_error(compress(img,-1000),"Invalid value passed")
})

#Test Case 2 : Invalid Type
test_that("compress(img,b)", {

  expect_error(compress(img,2.2),"Invalid type passed")
  expect_error(compress(img,TRUE),"Invalid type passed")
  expect_error(compress(readImage(img),6),"Invalid type passed")
})

#Test Case 3 : Checking new image object size
test_that("compress(img,b)  checking the compressed image size", {

  expect_less_than(image_size(compress(img,3)),6/8 * image_size(img))
  expect_less_than(image_size(compress(img,1)),4/8 * image_size(img))
  expect_less_than(image_size(compress(img,6)),8/8 * image_size(img))
  expect_less_than_equal(image_size(compress(img,8)),image_size(img))

})

#Test Case 4 : Checking new image shape
test_that("compress(img,b)  checking the compressed image shape", {
  
  expect_equal(dim(readImage(compress(img,3))),dim(readImage(img)))
  expect_equal(dim(readImage(compress(img,1))),dim(readImage(img)))
  expect_equal(dim(readImage(compress(img,6))),dim(readImage(img)))
})






