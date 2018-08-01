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
                     dplyr::funs(as.numeric)) %>%
    dplyr::mutate_at(vars(contains('id')), funs(
                     as.integer(gsub(pattern=".+_(?=\\d+$)", replacement = "", x=., perl = TRUE))))
}


#' Create four individual columns out of each bbox column and add to tidy tibble.
#'
#' @param df tidy hocr tibble, as produced by tidy_tesseract
#' @param bbox_cols character vector listing all the bbox columns you want to transform, if ommitted till transform all bbox columns
#' @param drop logical, indicating whether original bbox column(s) shoulc be dropped
#'
#' @return tidy tibble with four column added per every transformed bbox column
#' @export
#'
#' @examples
#' \dontrun{
#' library(tesseract)
#'
#' ocr("text.png", HOCR=TRUE) %>%
#'   hocr_parse() %>%
#'   tidy_tesseract() %>%
#'   separate_bbox_cols(drop=TRUE)
#' }
#' @importFrom dplyr rename mutate_at vars funs
separate_bbox_cols <- function(df, bbox_cols=NULL, drop=FALSE){
  cdf <- class(df)
  if(is.null(bbox_cols))
    bbox_cols <- names(df)[grep("bbox", names(df))]

  bbcs_lst <- lapply(bbox_cols, function(bbc){
    p1 <- strsplit(df[, bbc, drop=TRUE], ",|\\s")
    cnms <- paste(bbc, c("x1","y1", "x2", "y2"), sep="_")
    Reduce("rbind", lapply(p1, function(x) as.data.frame(as.list(setNames(x,cnms)), stringsAsFactors=FALSE)))
  })

  bbcs_df <- Reduce("cbind", bbcs_lst)
  bbcs_df[] <- lapply(bbcs_df, as.integer)
  res <- cbind(df, bbcs_df)

  if(drop)
    res <- res[, setdiff(names(res), bbox_cols)]

  class(res) <-cdf
  res
}
