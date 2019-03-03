#' Title
#'
#' @param img_path String , file path of the image
#' @param b Integer, desired byte size
#' @param out_path String, a valid output path depending on the OS
#'
#' @return String, compressed image path
#'
#' @examples
#'
library(reticulate)
library(numbers)
library(R.utils)
library(png)
library(OpenImageR)
library(assertthat)

compression <- function(img_path, b, out_path) {

  image = readImage(img_path)
  H <- dim(image)[1]
  W <- dim(image)[2]
  C <- dim(image)[3]

  image <- reticulate::array_reshape(image, dim = c(H*W, C))
  model <- kmeans(image, centers = 2^b, nstart = 35, iter.max = 20)

  labels <- model$cluster
  clrs <- model$centers
  quantized_img <- reticulate::array_reshape(labels, dim = c(H, W))

  image <- array(0, dim = c(H,W,C))

  for (i in 1:H) {
    for (j in 1:W) {
      image[i, j, ] <- clrs[quantized_img[i, j], ]
    }
  }
  save <- writePNG(image, target = out_path)

  return(out_path)
}
