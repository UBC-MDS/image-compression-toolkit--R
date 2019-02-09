context("compress")
library(pryr)
library(testthat)

#Test Case 1 : Invalid value .
test_that("compress(img,b) for checking type of b", {

  #img <- readPNG("XYZ.png")
  img <- array(0:15, dim=c(8,10,3))

  expect_error(compress(img,2.2),"Invalid type passed")
  expect_error(compress(img,TRUE),"Invalid type passed")
  expect_error(compress(img,-1),"Invalid value passed")
  expect_error(compress(img,9),"Invalid value passed")
  expect_error(compress(img,0),"Invalid value passed")
  expect_error(compress(img,1000),"Invalid value passed")
  expect_error(compress(img,-1000),"Invalid value passed")
  expect_error(compress(array(0:15, dim=c(0,10,3)),b),"Invalid value passed")
  expect_error(compress(array(0:15, dim=c(8,-10,3)),b),"Invalid value passed")
})

#Test Case 2 : Invalid Type
test_that("compress(img,b) for checking type of b", {

  #img <- readPNG("XYZ.png")
  img <- "file/path/to/image.jpg/or/image.png"

  expect_error(compress(img,2.2),"Invalid type passed")
  expect_error(compress(img,TRUE),"Invalid type passed")
  expect_error(compress(img,6),"Invalid type passed")
})

#Test Case 3 : Checking new image object size
test_that("compress(img,b)  checking the compressed image size", {

  #img <- readPNG("XYZ.png")

  expect_is(compress(array(0:15, dim=c(8,10,3)),4), "array")
  expect_equal(object_size(compress(array(0:15, dim=c(8,10,3)),7)),image_size(array(0:15, dim=c(8,10,3))))
  expect_equal(object_size(compress(array(0:15, dim=c(8,10,3)),1)),image_size(array(0:15, dim=c(8,10,3))))
  expect_equal(object_size(compress(array(0:15, dim=c(8,10,3)),3)),image_size(array(0:15, dim=c(8,10,3))))

})


#Test Case 4 : Checking new image shape
test_that("compress(img,b) checking the shape ", {

  #img <- readPNG("XYZ.png")
  img <- array(0:15, dim=c(8,10,3))

  out<-compress(img,4)

  expect_is(out, "array")
  expect_equal(out.shape,img.shape)


})





