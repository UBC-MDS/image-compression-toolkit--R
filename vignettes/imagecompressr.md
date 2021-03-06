---
title: "imageCompressR Package Manifesto"
author: "Sayanti Ghosh,Aditya Sharma,Alden Chen"
date: "2019-03-05"
output:
  rmarkdown::html_vignette:
    toc: true
    keep_md: true
vignette: >
  %\VignetteIndexEntry{imageCompressR Package Manifesto}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



## Package Info

This R package specializes in reducing the size of images. It contains two main functions :crop(), and compress(). The crop() functions reduce the size of an image by reducing the height and width of an image to the size specified by the user. The compress() function reduces the size of an image by reducing the number of bits used in each colour channel of the image. The package could be used by people to reduce the size of images, which could then be uploaded to social media platforms or other websites and applications.

### Function description

- `crop(image_path, H, W,out_path)`
  - Description:
    This function will reduce the image to the specified size removing rows and columns of pixels from the borders.
  - Input:
    - image_path String , image path of input 
    - H desired_height (integer)
    - W desired_width (integer)
  - Output:
    - out_path String , cropped image path 
    
- `compress(image_path, b = 4,out_path)`
  - Description:
    This function compresses the image by reducing the number of bits for each channel based on user input.
  - Input:
    - image String , image path of input 
    - b (integer, range [1, 7] (number of bits used for each channel in the compressed image))
  - Output:
    - out_path String , compressed image path
    
- `image_size(image_path)`
  - Description:
    Calculates and returns the size of an image in bytes.
  - Input:
    - image String , image path of input 
  - Output:
    - size (integer, size in bytes)
    
### How to use :   

* Install the package "imageCompressR"
* Add he library(imageCompressR)
* Use Crop function      : crop("..data-raw/test_image_1816", 15L, 10L,"..testthat/Test_images")
* Use Compress function  : compress("..data-raw/test_image_1816", 3L, "..testthat/Test_images")
* Use image_size function: image_size("..data-raw/test_image_1816")
    


> "He who gives up [code] safety for [code] speed deserves neither."
([via](https://twitter.com/hadleywickham/status/504368538874703872))
