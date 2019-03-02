#' Get image size
#'
#' Calculate the size of an image
#'
#' @param img String . path of image
#'
#' @return integer
#' @export
#'
#' @examples
#'

image_size <- function(img) {

  if (assertthat::is.string(img)==FALSE) {
    stop("TypeError")
  }
    return(file.size(img))
}
