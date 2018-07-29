
#' Parse hOCR file into a tibble
#'
#' @param x XHTML output from OCR algorithm in hOCR format (see https://en.wikipedia.org/wiki/HOCR for details)
#'
#' @return tibble with one word per line and columns describing lines, paragraphs, content areas and pages
#'
#' @examples
#' \dontrun{
#' library(tesseract)
#' ocr("file.png", HOCR=TRUE) %>%
#'   tidy_hocr()
#' }
#' @export
#' @importFrom XML xmlTreeParse xmlRoot
hocr_parse <- function(x){
  XML::xmlTreeParse(x) %>%
    XML::xmlRoot() %>%
    parse_page()
}

