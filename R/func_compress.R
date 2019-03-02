#' Compress images
#' 
#' Compress the size an image by reducing the number of bits
#' used to represent each pixel to b
#' 
#' @param img_path String , file path of the image
#' @param b Integer, desired byte size
#' @param out_path String, a valid output path depending on the OS
#'
#' @return String, compressed image path
#' @export
#'
#' @examples
#' compress(img_path, 5L, paste(normalizePath(dirname(img_path)), "/abc.png", sep=""))

library(R.utils)
library(png)
library(OpenImageR)
library(reticulate)
library(assertthat)

compress <- function(img_path, b, out_path) {
  
  min_size_img <- compression(img_path, 1L, 
                              paste(normalizePath(dirname(img_path)), "\\min.png", sep=""))
  desired_size_img <- compression(img_path, b, 
                                  paste(normalizePath(dirname(img_path)), "\\des.png", sep=""))
  
  if (image_size(min_size_img) > image_size(img_path)) {
    file.remove(min_size_img)
    file.remove(desired_size_img)
    stop("Can't compress further.")
  }
  else if (image_size(desired_size_img) > image_size(img_path)){
    file.remove(min_size_img)
    file.remove(desired_size_img)
    stop("Choose a smaller.")
  }
  else {
    file.remove(min_size_img)
    file.remove(desired_size_img)
    return(compression(img_path, b, out_path))
  }
}