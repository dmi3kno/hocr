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


#' Functions for horizontal and vertical slicing of bbox columns
#' These functions are not vectorized and should be used to compute
#' @param bbox string or character vector of bbox'es
#' @param x,y  coordinates to sclice the bouding box at
#'
#' @return list of bbox'es appliccable for particular split
#' @export
#'
#' @examples
#' bbox_slice_x("0 0 100 200", 80)
#'
#' @rdname bbox_slice
#'
bbox_slice_x <- function(bbox, x){
  coord_list <- lapply(strsplit(bbox, ",| "), as.numeric)
  lapply(coord_list, function(bb){list(left=paste(bb[1], bb[2], x-1, bb[4]),
                                      right=paste(x, bb[2], bb[3], bb[4]))})
}

#' @export
#'
#' @examples
#' bbox_slice_y("0 0 100 200", 120)
#' @rdname bbox_slice
bbox_slice_y <- function(bbox, y){
  coord_list <- lapply(strsplit(bbox, ",| "), as.numeric)
  lapply(coord_list, function(bb){list(top=paste(bb[1], bb[2], bb[3], y-1),
                                   bottom=paste(bb[1], y, bb[3], bb[4]))})
}

#' @export
#'
#' @examples
#' bbox_slice_xy("0 0 100 200", 50, 100)
#' @rdname bbox_slice
bbox_slice_xy <- function(bbox, x, y){
  coord_list <- lapply(strsplit(bbox, ",| "), as.numeric)
  lapply(coord_list, function(bb){list(top_left = paste(bb[1], bb[2], x-1, y-1),
                                       top_right = paste(x, bb[2], bb[3], y-1),
                                       bottom_left = paste(bb[1], y, x-1, bb[4]),
                                       bottom_right = paste(x, y, bb[3], bb[4]))})
}

