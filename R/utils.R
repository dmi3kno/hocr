
vec_to_list <- function(x){
  split(unname(x), names(x))
}

list_to_vec <- function(x){
  unlist(unname(x))
}

#' @importFrom XML xmlAttrs
parse_obj_attrs <- function(xml_node){
  obj_att <- vec_to_list(XML::xmlAttrs(xml_node))
  obj_class <- obj_att$`class`
  names2keep <- setdiff(names(obj_att), "class")
  setNames(lapply(names2keep, function(x) obj_att[[x]]), paste(obj_class, names2keep, sep = "_"))
}

#' @importFrom XML xmlToList
#' @importFrom dplyr as_tibble
parse_word <- function(xml_word_node){
  word_node_list <- XML::xmlToList(xml_word_node)
  word_node_list <- word_node_list[setdiff(names(word_node_list),".attrs")]
  word_node_list <- list(ocrx_word_tag=names(word_node_list), ocrx_word_value=list_to_vec(word_node_list))
  dplyr::as_tibble(c(parse_obj_attrs(xml_word_node), word_node_list))
}

#' @importFrom XML xmlApply
#' @importFrom dplyr bind_rows mutate
parse_obj <- function(l, f){
  obj_attr_list <- parse_obj_attrs(l)
  obj_df <- dplyr::bind_rows(XML::xmlApply(l, f))
  dplyr::mutate(obj_df, rlang::UQS(obj_attr_list))
}

#' @importFrom purrr partial
parse_line <- purrr::partial(parse_obj, f=parse_word)

#' @importFrom purrr partial
parse_par <- purrr::partial(parse_obj, f=parse_line)

#' @importFrom purrr partial
parse_area <- purrr::partial(parse_obj, f=parse_par)

#' @importFrom purrr partial
parse_page <- purrr::partial(parse_obj, f=parse_area)

#' Convert bbox to magick-geometry
#'
#' @param x character vector with bounding boxes (top-left coordinates). Expects four integers separated by comma or space
#'
#' @return character vector of magick-compliant geometry string representing bounding box area in the format  "width x height +x_off +y_off" (https://www.imagemagick.org/Magick++/Geometry.html)
#' @seealso [magick::geometry] for converting integers (four individual columns) to geometry
#' @export
#'
#'
#' @examples
#' # area from c(0,0) to c(100,200) counting from top left
#' bbox_to_geometry("0 0 100 200")
#'
bbox_to_geometry <- function(x){
  coord_list <- lapply(strsplit(x, ",| "), as.numeric)
  sapply(coord_list, function(x){paste0(x[3] - x[1], "x", x[4] - x[2], "+", x[1], "+", x[2])})
}


#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#' Tidy eval helpers
#'
#' These functions provide tidy eval-compatible ways to capture
#' symbols (`sym()`, `syms()`, `ensym()`), expressions (`expr()`,
#' `exprs()`, `enexpr()`), and quosures (`quo()`, `quos()`, `enquo()`).
#' To learn more about tidy eval and how to use these tools, read
#' <http://rlang.tidyverse.org/articles/tidy-evaluation.html>
#'
#' @name tidyeval
#' @keywords internal
#' @aliases          quo quos enquo sym syms ensym expr exprs enexpr quo_name
#' @importFrom rlang quo quos enquo sym syms ensym expr exprs enexpr quo_name
#' @export           quo quos enquo sym syms ensym expr exprs enexpr quo_name
#' @importFrom rlang UQ UQS .data :=
NULL
