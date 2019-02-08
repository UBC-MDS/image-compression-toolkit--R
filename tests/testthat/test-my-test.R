context("Seam-Carve")


test_that("seam_carve(img, height,width)", {

  h <- 6
  w<- 6
  #img <- readPNG("XYZ.png")
  img <- array(0:15, dim=c(8,10,3))

  out <- seam_carve(img,h,w)

  expect_is(out, "array")
  expect_equal(out.shape[1],w)
  expect_equal(out.shape[0],h)

})

test_that("seam_carve(img, height, width)", {

  h <- 6
  w<- 6
  #img <- readPNG("XYZ.png")
  img <- array(0:15, dim=c(2,4,3))

  out <- seam_carve(img,h,w)

  expect_is(out, "array")
  expect_equal(out.shape[1],2)
  expect_equal(out.shape[0],4)

})

test_that("energy(image)", {

  #img <- readPNG("XYZ.png")
  img <- array(0:15, dim=c(10,10,3))

  out <-energy(img)
  expect_is(out, "array")
  expect_equal((out.shape),c(10,10))                    # The energy image, a 2d numpy array of size NxM

})


test_that("find_vertical_seam(energy)", {

  #img <- readPNG("XYZ.png")
  energy <- array(0:15, dim=c(10,10))

  expect_is(find_vertical_seam(energy), "array")
  expect_equal((length(find_vertical_seam(energy))),10)  # A seam represented as a 1d array of length N
  expect_identical(find_vertical_seam(energy), 0:9)      # With all values between 0 and M-1

})

test_that("find_horizontal_seam(energy)", {

  #img <- readPNG("XYZ.png")
  energy <- array(0:15, dim=c(10,10))

  expect_is(find_horizontal_seam(energy), "array")
  expect_equal((length(find_horizontal_seam(energy))),10)  # A seam represented as a 1d array of length M
  expect_identical(find_horizontal_seam(energy), 0:9)      # With all values between 0 and N-1

})

remove_vertical_seam(image, seam)/remove_vertical_seam(image, seam)

test_that("remove_vertical_seam(img, seam)", {

  #img <- readPNG("XYZ.png")
  img <- array(0:15, dim=c(10,10))
  seam <- find_vertical_seam(energy(img))

  out <-remove_vertical_seam(img, seam)

  expect_is(out, "array")
  expect_equal(out.shape[1],10)   # A new image that is smaller by 1 column. Size N by M-1.
  expect_equal(out.shape[0],9)    # A new image that is smaller by 1 column. Size N by M-1.

})

test_that("remove_horizontal_seam(img, seam)", {

  #img <- readPNG("XYZ.png")
  img <- array(0:15, dim=c(10,10))
  seam <- remove_horizontal_seam(energy(img))

  out <-remove_horizontal_seam(img, seam)

  expect_is(out, "array")
  expect_equal(out.shape[1],9)   # A new image that is smaller by 1 row. Size N-1 by M.
  expect_equal(out.shape[0],10)    # A new image that is smaller by 1 row. Size N-1 by M.

})


