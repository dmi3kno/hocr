#' Tidy hOCR tibble after tesseract
#'
#' @param df tibble as returned by [hocr_parse()]
#'
#' @return tidy tibble with separated numerical columns of following structure
#'
#' @examples
#' \dontrun{
#' library(tesseract)
#'
#' ocr("text.png", HOCR=TRUE) %>%
#'   hocr_parse() %>%
#'   tidy_tesseract()
#' }
#' @export
#' @importFrom tidyr separate
#' @importFrom dplyr rename mutate_at vars funs
tidy_tesseract <- function(df){

  str_deprefix <- function(x){
    gsub("^[^0-9]+ ", "", x)
  }

  df %>%
    tidyr::separate(ocrx_word_title, into=c("ocrx_word_bbox", "ocrx_word_conf"), sep=";") %>%
    tidyr::separate(ocr_line_title, into=c("ocr_line_bbox", "ocr_line_baseline","ocr_line_xsize",
                                           "ocr_line_xdescenders", "ocr_line_xascenders"), sep=";") %>%
    dplyr::rename(ocr_par_bbox="ocr_par_title", ocr_carea_bbox="ocr_carea_title") %>%
    tidyr::separate(ocr_page_title, into=c("ocr_page_image", "ocr_page_bbox", "ocr_page_no"), sep=";") %>%
    dplyr::mutate_at(dplyr::vars(ocrx_word_bbox, ocrx_word_conf, ocr_line_bbox, ocr_line_baseline, ocr_line_xsize,
                                 ocr_line_xdescenders, ocr_line_xascenders, ocr_par_bbox, ocr_carea_bbox, ocr_page_bbox, ocr_page_no),
                     dplyr::funs(str_deprefix)) %>%
    tidyr::separate(ocr_line_baseline, into=c("ocr_line_xbaseline", "ocr_line_ybaseline"), sep="\\s") %>%
    dplyr::mutate_at(dplyr::vars(ocrx_word_conf, ocr_line_xbaseline, ocr_line_ybaseline, ocr_line_xsize,
                                 ocr_line_xdescenders, ocr_line_xascenders, ocr_page_no),
                     dplyr::funs(as.numeric))
}
