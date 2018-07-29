
library(magick)
library(tiff)


image_prepare <- function(x){
    image_convert(x, type="Grayscale") %>%
    image_negate() %>%
    image_lat() %>%
    image_negate()
}
image_read("data-raw/peanutbutter1.PNG") %>%
  image_prepare() %>%
  image_write(path="inst/extdata/peanutbutter.png", format="png")


library(hexSticker)

library(hexSticker)
imgurl <- "https://raw.githubusercontent.com/dmi3kno/DataCamp-IntroToExpDesign/master/hocr_logo.png"
sticker(imgurl, package="",
        h_fill="#ffffff", h_color="#2e5396",
        s_x=1, s_y=1, s_width = .65,
        filename = "data-raw/sticker.png")
