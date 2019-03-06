#' Crop images
#'
#' @param img_path    ---- String , file path of the image .
#' @param H           ---- Integer, the desired height of the cropped image
#' @param W           ---- Integer, the desired width of the cropped image
#' @param out_path    ---- String , desired file path of the cropped image
#'
#' @return     ---String , cropped image path and saves the image .
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
#'
#' @examples
#'
#'
#------------------------------------Library------------------------------------------#
crop <- function(img_path, H, W, out_path){

  #---------------------------------Exception Handling--------------------------------#
  # Exception handling for input validation like Type error, invalid values ,         #
  # unrealistic desired dimension.                                                    #
  #-----------------------------------------------------------------------------------#
  if (assertthat::is.string(img_path)==FALSE){
    stop("TypeError: Only acceptable input should be a valid image path in string format.")
  }
  if (assertthat::is.string(out_path)==FALSE){
    stop("TypeError: Only acceptable input should be a valid path in string format.")
  }
  if (H <=0){
    stop("ValueError: Desired height cannot be less than or equal to zero")
  }
  if (W <= 0 ){
    stop("ValueError: Desired width cannot be less than or equal to zero")
  }
  if (H >= dim(readImage(img_path))[1]+1){
    stop("ValueError: Desired height cannot be more than original height")
  }
  if (W>=dim(readImage(img_path))[2]+1){
    stop("ValueError: Desired width cannot be more than original width")
  }
  if (is.integer(H)==FALSE){
    stop("TypeError: Only acceptable input is integer , like 1L, 8L.")
  }
  if (is.integer(W)==FALSE){
    stop("TypeError: Only acceptable input is integer , like 1L, 8L.")
  }

  #-----------------------------------------------------------------------------------#
  image = readImage(img_path)
  height = dim(image)[1] - H
  width = dim(image)[2] - W

  if (rem(height,2) == 0){
    start_row = as.integer(height/2) + 1
    end_row = start_row + H - 1
  }
  else {
    start_row = as.integer((height-1)/2) + 1
    end_row = start_row + H - 1
  }

  if (rem(width,2) == 0){
    start_col = as.integer(width/2) + 1
    end_col = start_col + W - 1
  }
  else {
    start_col = as.integer((width-1)/2) + 1
    end_col = start_col + W - 1
  }

  img = image[start_row:end_row,start_col:end_col,]

  save <- writePNG(img,target=out_path)

  return(out_path)

}



