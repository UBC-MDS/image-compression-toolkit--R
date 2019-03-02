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

compression <- function(img_path, b, out_path) {
  
  if (is.string(img_path) == FALSE | is.string(out_path) == FALSE) {
    stop("TypeError")
  }
  
  
  
  if (is.integer(b) == FALSE) {
    stop("TypeError")
  }
  
  if (b <= 0 | b > 8) {
    stop("ValueError")
  }
  
  image = readImage(img_path)
  H <- dim(image)[1]
  W <- dim(image)[2]
  C <- dim(image)[3]
  
  image <- array_reshape(image, dim = c(H*W, C))
  model <- kmeans(image, centers = 2^b)
  
  labels <- model$cluster
  clrs <- model$centers
  quantized_img <- array_reshape(labels, dim = c(H, W))
  
  image <- array(0, dim = c(H,W,C))
  
  for (i in 1:H) {
    for (j in 1:W) {
      image[i, j, ] <- clrs[quantized_img[i, j], ]
    }
  }
  save <- writePNG(image, target = out_path)
  
  return(out_path)
}
