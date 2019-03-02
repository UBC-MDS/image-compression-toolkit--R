

image = "data-raw/test_image_1816.png"
image_path_2 = "data-raw/test_image_1718.png"

image_2 <-readImage(image)
image_1 <- readImage(image_path_2)
image_2

list_images <- list(image_1, image_2)

## creates sysdata.rda
devtools::use_data(list_images, list_images, internal = TRUE)
