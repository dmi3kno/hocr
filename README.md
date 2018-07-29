
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hocr <img src="man/figures/logo.png" align="right" />

The goal of **`hocr`** is to facilitate post-OCR data processing and
wrangling. The package exposes hocr parcer, `hocr_parse`, which converts
[XHTML format output](https://en.wikipedia.org/wiki/HOCR) into tidy
tibble with one word per row. In addition to the columns exported by
[`tesseract::ocr_data`](https://github.com/ropensci/tesseract), `hocr`
outputs additional metadata ragrding organization of words into lines,
paragraphs, content areas and pages. Read more about hOCR specification
[here](https://github.com/kba/hocr-spec).

One of the key elements of `hocr` format is “bounding box” - a
rectangular region of the image covering the extent of the word
recognized by `tesseract`. This bbox can be used to extract respective
part of the image using, for example `magick` package, using
`bbox_to_geometry` helper function.

`hocr` aslo includes tidiers for common hOCR-capable systems. As of
version 0.0.9000 only `tesseract` output format is supported, but in the
future, support for [`OCRopus`](https://github.com/tmbdev/ocropy) will
be added.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dmi3kno/hocr")
```

## Example

This is a basic example which shows you how to solve a common problem:

We will OCR a page from an old cookbook retrieved from archive.org\[1\]
and enhanced using `magick` package (see image preparation script on
github).

``` r
cupcakes <- system.file("extdata", "peanutbutter.png", package="hocr")


recipe <- ocr(cupcakes, HOCR = TRUE) %>% 
  hocr_parse() %>% 
  tidy_tesseract()
recipe
#> # A tibble: 234 x 21
#>    ocrx_word_id ocrx_word_bbox ocrx_word_conf ocrx_word_tag ocrx_word_value
#>    <chr>        <chr>                   <dbl> <chr>         <chr>          
#>  1 word_1_1     38 58 271 103              85 strong        Chocolate      
#>  2 word_1_2     287 61 451 103             86 text          Peanut         
#>  3 word_1_3     468 62 619 103             89 text          Butter         
#>  4 word_1_4     636 60 852 113             84 strong        Cupcakes       
#>  5 word_1_5     36 153 112 182             87 strong        Your           
#>  6 word_1_6     123 152 184 1~             88 strong        kids           
#>  7 word_1_7     196 152 250 1~             88 strong        will           
#>  8 word_1_8     264 152 324 1~             85 strong        love           
#>  9 word_1_9     337 152 417 1~             84 strong        these          
#> 10 word_1_10    431 154 472 1~             90 text          (as            
#> # ... with 224 more rows, and 16 more variables: ocr_line_id <chr>,
#> #   ocr_line_bbox <chr>, ocr_line_xbaseline <dbl>,
#> #   ocr_line_ybaseline <dbl>, ocr_line_xsize <dbl>,
#> #   ocr_line_xdescenders <dbl>, ocr_line_xascenders <dbl>,
#> #   ocr_par_id <chr>, ocr_par_lang <chr>, ocr_par_bbox <chr>,
#> #   ocr_carea_id <chr>, ocr_carea_bbox <chr>, ocr_page_id <chr>,
#> #   ocr_page_image <chr>, ocr_page_bbox <chr>, ocr_page_no <dbl>
```

Now that data is in the tidy format, lets render the page in ggplot and
identify bounding boxes around words and paragraphs to illustrate the
benefits of parsed document structure. `tesseract` outputs bboxes in
upper-left corner coordinate system. We will transform all y-values to
bottom-left scale and plot it alongside with the original picture.

``` r
p1 <- recipe %>% 
  mutate(ocrx_word_bbox=lapply(ocrx_word_bbox, function(x) 
    separate(as_tibble(x), value, into=c("word_x1", "word_y1", "word_x2", "word_y2"), convert = TRUE))) %>% 
    unnest(ocrx_word_bbox) %>% 
  mutate(ocr_page_bbox=lapply(ocr_page_bbox, function(x) 
    separate(as_tibble(x), value, into=c("page_x1", "page_y1", "page_x2", "page_y2"), convert = TRUE))) %>% 
    unnest(ocr_page_bbox) %>% 
  mutate(word_y1=max(page_y2)-word_y1,
         word_y2=max(page_y2)-word_y2) %>% 
    ggplot(aes(xmin=word_x1, ymin=word_y1, xmax=word_x2, ymax=word_y2))+
    #geom_rect(aes()+
    geom_rect(aes(color=ocr_par_id, fill=ocrx_word_conf), show.legend = TRUE)+
  theme_minimal()+
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size = 7), 
        legend.text = element_text(size = 7), 
        legend.title = element_text(size = 7))

library(png)
library(grid)
img <- readPNG(cupcakes)
p2 <- rasterGrob(img, interpolate=TRUE)

p1+p2
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

Similar projects are listed
[here](https://github.com/kba/awesome-ocr#hocr)

\[1\] Rosenberg L. M.(1986) Muffins & cupcakes, American Cooking Guild,
Gaithersburg, MD. Openlibrary edition OL1484439M. Accessed from:
<https://archive.org/details/muffinscupcakes00rose> on 28 July 2018
