context("compress")
library(pryr)

#Test Case 1 : When b is float .
test_that("compress(img,b) for checking type of b", {

  #img <- readPNG("XYZ.png")
  img <- array(0:15, dim=c(8,10,3))
  b   <- 2.2                          # Giving b as float .

  expect_error(compress(img,b),"b must be an integer")

})

#Test Case 2 : Checking new image object size
test_that("compress(img,b)  checking the compressed image size", {

  #img <- readPNG("XYZ.png")
  img <- array(0:15, dim=c(8,10,3))
  b   <- 4

  out<-compress(img,b)

  expect_is(out, "array")
  expect_equal(object_size(out),size(img,b))

})


#Test Case 3 : Checking new image shape
test_that("compress(img,b) checking the shape ", {

  #img <- readPNG("XYZ.png")
  img <- array(0:15, dim=c(8,10,3))
  b   <- 4

  out<-compress(img,b)

  expect_is(out, "array")
  expect_equal(out.shape[1],img.shape[1])
  expect_equal(out.shape[0],img.shape[0])

})
#Test Case 4 : With negetive b values
test_that("compress(img,b)", {

  #img <- readPNG("XYZ.png")
  img <- array(0:15, dim=c(8,10,3))
  b   <- -4

  expect_error(compress(img,b),"b must be an positive")

})


#Test Case 5 : Checking image with negetive dimension
test_that("compress(img,b)", {

  #img <- readPNG("XYZ.png")
  img <- array(0:15, dim=c(8,-10,3))
  b   <- 4

  expect_error(compress(img,b),"Image must have positive dimension")

})

#Test Case 6 : Checking image with dimension size 0
test_that("compress(img,b)", {

  #img <- readPNG("XYZ.png")
  img <- array(0:15, dim=c(0,10,3))
  b   <- 4

  expect_error(compress(img,b),"Image must have positive non-zero dimension")

})

#Test Case 7 : Checking with input image having equal and even height and width.
test_that("compress(img,b) checking the shape", {

  #img <- readPNG("XYZ.png")
  img <- array(0:15, dim=c(10,10,3))
  b   <- 4

  out<-compress(img,b)

  expect_is(out, "array")
  expect_equal(out.shape[1],img.shape[1])
  expect_equal(out.shape[0],img.shape[0])

})

#Test Case 8 : Checking with input image having equal even height and width
test_that("compress(img,b) checking the compressed image size.", {

  #img <- readPNG("XYZ.png")
  img <- array(0:15, dim=c(10,10,3))
  b   <- 4

  out<-compress(img,b)

  expect_is(out, "array")
  expect_equal(object_size(out),size(img,b))

})
