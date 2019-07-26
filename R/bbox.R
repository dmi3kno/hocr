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

#' Functions for aggregating bbox objects
#' These functions can perform union and intersection operations on bbox objects
#' @param bbox character vector of bounding boxes to perform operation on
#' @param bbox2 optional character vector of bounding boxes to element-wise aggregation with `bbox`.
#' If specified, needs to be length 1 or equal in length to `bbox`.
#'
#' @return bbox or missing value, if result is invalid bounding box
#' @export
#'
#' @examples
#' bbox_union(c("5 1 7 3", "2 4 6 8"))
#' bbox_union(c("5 1 7 3", "2 4 6 8"), c("1 1 1 1"))
#' bbox_intersect(c("5 1 7 3", "2 4 6 8")) # should return NA
#' bbox_intersect("5 1 7 3", "2 2 6 8")
#' @rdname bbox_aggregate
#'

bbox_union <- function(bbox, bbox2=NULL){
  if(length(bbox)==1L)
    bbox <- rep.int(bbox, times=length(bbox2))
  if(length(bbox2)==1L)
    bbox2 <- rep.int(bbox2, times=length(bbox))
  stopifnot(length(bbox)==length(bbox2) || is.null(bbox2))

  coord_list <- lapply(strsplit(bbox, ",| "), as.numeric)
  m <- do.call(rbind, coord_list)

  if(!is.null(bbox2)){
    coord_list2 <- lapply(strsplit(bbox2, ",| "), as.numeric)
    m2 <- do.call(rbind, coord_list2)
    return(bbox_validate(paste(pmin(m[,1], m2[,1]), pmin(m[,2], m2[,2]),
                               pmax(m[,3], m2[,3]), pmax(m[,4], m2[,4]))))
  }

  bbox_validate(paste(min(m[,1]), min(m[,2]), max(m[,3]), max(m[,4])))
}

#' @rdname bbox_aggregate
#' @export
bbox_intersect <- function(bbox, bbox2=NULL){
  if(length(bbox)==1L)
    bbox <- rep.int(bbox, times=length(bbox2))
  if(length(bbox2)==1L)
    bbox2 <- rep.int(bbox2, times=length(bbox))

  coord_list <- lapply(strsplit(bbox, ",| "), as.numeric)
  m <- do.call(rbind, coord_list)

  if(!is.null(bbox2)){
    coord_list2 <- lapply(strsplit(bbox2, ",| "), as.numeric)
    m2 <- do.call(rbind, coord_list2)
    return(bbox_validate(paste(pmax(m[,1], m2[,1]), pmax(m[,2], m2[,2]),
                               pmin(m[,3], m2[,3]), pmin(m[,4], m2[,4]))))
  }

  bbox_validate(paste(max(m[,1]), max(m[,2]), min(m[,3]), min(m[,4])))
}

#' Functions for validating bbox
#' These functions can check whether specified bbox is valid, i.e. x1 <= x2 and y1 <= y2
#' @param bbox character vector bounding boxes to validate
#'
#' @return a vector of logical values
#' @export
#'
#' @examples
#' bbox_is_valid("0 0 100 200")
#' bbox_validate(c("5,4,6,3", "1,1,5,6"))
#' @rdname bbox_valid
#'
bbox_is_valid <- function(bbox){
  coord_list <- lapply(strsplit(bbox, ",| "), as.numeric)
  sapply(coord_list, function(x) x[3]>=x[1] & x[4]>=x[2])
}

#' @rdname bbox_valid
#' @return original vector with NA for invalid bboxes
#' @export
bbox_validate <- function(bbox){
  ifelse(bbox_is_valid(bbox), bbox, NA_character_)
}

#' Functions for padding bbox
#' These functions can "pad" (increase size of) bbox
#' @param bbox character vector of bounding boxes to pad
#' @param word character vector of words contained in bboxes
#' @param n integer number of symbols to add
#' @param side "left", "right" or "both" which side to pad
#'
#' @return a vector of validated bboxes
#' @rdname bbox_pad
#'
#' @examples
#' bbox_pad_width("0 0 10 20", "There")
#' bbox_pad_height("0 0 10 20", "There")
#' @export
bbox_pad_width <- function(bbox, word, n=1, side="both"){
  bb_lst <- lapply(strsplit(bbox, ",| "), as.numeric)
  m <- do.call(rbind, bb_lst)
  p <- (m[,3]-m[,1])/nchar(word)
  if(side=="left"|side=="both")
    m[,1] <- m[,1]-p*n
  if(side=="right"|side=="both")
    m[,3] <- m[,3]+p*n

  bbox_validate(paste(m[,1], m[,2], m[,3],m[,4]))
}

#' @rdname bbox_pad
#' @param side "up", "down" or "both" which side to pad
#' @export
bbox_pad_height <- function(bbox, word, n=0.5, side="both"){

  bb_lst <- lapply(strsplit(bbox, ",| "), as.numeric)
  m <- do.call(rbind, bb_lst)
  n_lines <-  lengths(regmatches(word, gregexpr("\n", word)))+1
  p <- (m[,4]-m[,2])/n_lines
  if(side=="up"|side=="both")
    m[,2] <- m[,2]-p*n
  if(side=="down"|side=="both")
    m[,4] <- m[,4]+p*n

  bbox_validate(paste(m[,1], m[,2], m[,3],m[,4]))
}
