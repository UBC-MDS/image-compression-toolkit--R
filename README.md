[![Build Status](https://travis-ci.org/UBC-MDS/image-compression-toolkit--R.svg?branch=master)](https://travis-ci.org/UBC-MDS/image-compression-toolkit--R)

## Image Compression Toolkit - R
Two ways to compress your images!

## Contributors

- [Aditya Sharma](https://github.com/adityashrm21)
- [Alden Chen](https://github.com/aldenchen)
- [Sayanti Ghosh](https://github.com/Sayanti86)

## Project Summary

This R package specializes in reducing the size of images. It contains two main functions :`crop()`, and `compress()`. The `crop()` functions reduce the size of an image by reducing the height and width of an image to the size specified by the user. The `compress()` function reduces the size of an image by reducing the number of bits used in each colour channel of the image. The package could be used by people to reduce the size of images, which could then be uploaded to social media platforms or other websites and applications that have limits on image size.

## Functions

`crop(img_path, H, W, out_path)`
  - Description:
    This function reduces the image to the specified size by removing rows and columns of pixels from the borders.
  - Inputs:  

|Parameter |Type |Description |
|----------|-----|------------|
|`img_path`|`string`| File path to a `png` image|
|`H`|`int`| Desired height of the cropped image |
|`W`|`int`| Desired width of the cropped image|
|`out_path`|`string`| File path to where to save the cropped `png` image |  

- Output:  `string`, file path to cropped image (`png` image with size `H x W`)  

`compress(image, b, out_path)`
  - Description:
    This function compresses the image by reducing the number of bits for each channel based on user input.
  - Inputs:  

|Parameter |Type |Description |
|----------|-----|------------|
|`img_path`|`string`| File path to a `png` image|
|`b`|`int`| An integer between 1 and 8; number of bits used for each channel in the compressed image |
|`out_path`|`string`| File path to where to save the compressed `png` image |
  - Output:  `string`, file path to the compressed `png` image   


`image_size(image)`
  - Description:
    Calculates and returns the size of an image in bytes.
  - Input:
    - `string`, file path to a `png` image
  - Output: `int`, size of the image in bytes

## Installation

To install this package, use the following command in the R console:
> ```devtools::install_github("UBC-MDS/image-compression-toolkit--R", build_opts = c("--no-resave-data", "--no-manual"))```

## Usage
|Task    |  In a Script (after, `library(imageCompress)`)   |
|---------|---------------------|
|Crop `image.png` to size 10 X 15  |  `crop("../image.png", H = 10L, W = 15L, "..//cropped_image.png")`|
|Compress an `image.png` to 5 bits per channel |  `compress("..//image.png", b = 5L, "..//compressed_image.png")` |
|Get the size of `image.png`|  `image_size("..//image.png")`|

## Examples

The following examples use the `bigger_test.png` image from the data folder, displayed below.
<img src = "https://raw.githubusercontent.com/UBC-MDS/image-compression-toolkit--R/update_tests/data/bigger_test.png" >

To get the size of the image:
```
library(imageCompress)
image_size("../data/bigger_test.png")
```
> This returns the size of the image in bytes.
```
45534
```

To crop the image:  
```
crop("../data/bigger_test.png", 175L, 200L, "../data/crop_img.png")
```

> This returns the absolute file path to the cropped image, as specified in the `out_path` argument of the function. The cropped image is shown below.
```
..data/cropped_img.png
```
<img src = "https://raw.githubusercontent.com/UBC-MDS/image-compression-toolkit--R/update_tests/data/crop_img.png" >

To compress the image:
```
compress("..data/bigger_test.png", 2L, "../data/compressed_img.png")
```

> This returns the absolute file path to the compressed image. The compressed image is shown below.   
```
..data/compressed_img.png
```
<img src = "https://raw.githubusercontent.com/UBC-MDS/image-compression-toolkit--R/update_tests/data/compressed_img.png" >

## Test Results  
<img src = "https://raw.githubusercontent.com/UBC-MDS/image-compression-toolkit--R/update_tests/data/test_results.png">

## Branch Coverage  
<img src = "https://raw.githubusercontent.com/UBC-MDS/image-compression-toolkit--R/update_tests/data/branch_coverage.png" width = "555">


## Related Packages
There already are packages for image processing in R and Python:
  - [The magick package in R](https://cran.r-project.org/web/packages/magick/vignettes/intro.html)
  - [sckit-image in Python](https://scikit-image.org/)

The existing packages are very comprehensive and provide many functions such as transformations, filters, file conversions and other advanced functions. Our package focuses specifically on image compression and reducing image size using K-means clustering. This package is not a wrapper or an improvement of an existing package. It simply uses an unsupervised learning algorithm (K-means clustering) to reduce the number of bits used to represent an image.
