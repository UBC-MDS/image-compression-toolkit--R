#' Crop images
#'
#' Function to crop image to the 
#' shape specified by height and width
#' 
#' @param img_path - String , file path of the image .
#' @param H        - Integer, the desired height of the cropped image
#' @param W        - Integer, the desired width of the cropped image
#'
#' @return cropped image path and saves the image .
#' @export
#'
#' @examples
#' crop("data/sample.png", 10, 15)
#------------------------------------Library------------------------------------------#
library(OpenImageR)
library(numbers)
library(imager)
library(R.utils)
library(png)
library(searchable)
#-------------------------------------------------------------------------------------#
crop <- function(img_path, H, W){
  
  #---------------------------------Exception Handling--------------------------------#
  # Exception handling for input validation like Type error, invalid values ,         #
  # unrealistic desired dimension                                                     #
  #-----------------------------------------------------------------------------------#
  if (H <=0 || W <= 0 ){stop("ValueError -- Give positive dimension")
    }
  else if (H >= dim(readImage(img_path))[1]+1 || W>=dim(readImage(img_path))[2]+1){
    stop("ValueError -- Desired dimension higher than original")
  }
  else if (is.integer(H)==FALSE || is.integer(W)==FALSE){
    stop("TypeError -- Desired dimension should be integer.")
  }
  else if (assertthat::is.string(img_path)==FALSE){
    stop("TypeError -- Image path should be a string")
  }
  #-----------------------------------------------------------------------------------#
  image = readImage(img_path)
  height = dim(image)[1] - (H-1)
  width = dim(image)[2] - (W-1)
  
  if (rem(height,2) == 0){
    start_row = as.integer(height/2)
    end_row = as.integer(dim(image)[1]-height/2)
  }
  else {
    start_row = as.integer((height-1)/2)
    end_row = as.integer((dim(image)[1]-(height-1)/2)-1)
  }
  
  if (rem(width,2) == 0){
    start_col = as.integer(width/2)
    end_col = as.integer(dim(image)[2]-width/2)
  }
  else {
    start_col = as.integer((width-1)/2)
    end_col = as.integer((dim(image)[2]-(width-1)/2)-1)
  }
  
  img = image[start_row:end_row,start_col:end_col,]
  
  path<-paste(normalizePath(dirname(img_path)),"\\", "Crop_image.png", sep = "")
  save <-writePNG(img,target=path)
  
  print("The cropped image path is :")
  return(path)
  
}

