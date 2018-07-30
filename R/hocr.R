
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
#' @importFrom xml2 read_xml
hocr_parse <- function(x){
  xml2::read_xml(x) %>%
    parse_page()
}

