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
#' @import numbers
#' @import R.utils
#' @import png
#' @import OpenImageR
#' @import here
#' @import testthat
#' @import reticulate
#' @import rprojroot
#' @import assertthat
#' @import covr
#' @import stats
#' @examples
#' # compress("..data/image.png", 2L, "..data/compressed_image.png")
#'

compress <- function(img_path, b, out_path) {

  if (is.character(out_path) == FALSE) {
    stop("TypeError: out_path should be a string")
  }

  if (is.character(img_path) == FALSE) {
    stop("TypeError: img_path should be a string")
  }

  if (is.integer(b) == FALSE) {
    stop("TypeError: b should be an integer, e.g. 2L")
  }

  if (b <= 0 | b > 8) {
    stop("ValueError: b should be an integer between 1L and 8L")
  }

  if (dim(unique(readImage(img_path)))[1] < 2) {
    stop("Can't compress further.")
  }

  min_size_img <- compression(img_path, 1L,
                              paste(normalizePath(dirname(img_path)), "\\min.png", sep=""))
  desired_size_img <- compression(img_path, b,
                                  paste(normalizePath(dirname(img_path)), "\\des.png", sep=""))

  if (image_size(min_size_img) > image_size(img_path)) {
    file.remove(min_size_img)
    file.remove(desired_size_img)
    stop("Can't compress further.")
  }
  if (image_size(desired_size_img) > image_size(img_path)){
    file.remove(min_size_img)
    file.remove(desired_size_img)
    stop("Choose a smaller b to compress.")
  }
  file.remove(min_size_img)
  file.remove(desired_size_img)
  return(compression(img_path, b, out_path))
}
