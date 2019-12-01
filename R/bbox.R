bbox_to_matrix <- function(bbox){
  bb_lst <- lapply(strsplit(bbox, ",\\s?|\\s+"), as.numeric)
  do.call(rbind, bb_lst)
}

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
#' bbox_to_geometry(c("0 0 100 200", "100,100,200, 200"))
#'
bbox_to_geometry <- function(x){
  m <- bbox_to_matrix(x)
  paste0(m[,3] - m[,1], "x", m[,4] - m[,2], "+", m[,1], "+", m[,2])
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
#' bbox_slice_x(c("0 0 100 200", "100 100 200 200"), c(80, 150))
#'
#' @rdname bbox_slice
#'
bbox_slice_x <- function(bbox, x){
  m <- bbox_to_matrix(bbox)
  list(left=bbox_validate(paste(m[,1], m[,2], x-1, m[,4])),
       right=bbox_validate(paste(x, m[,2], m[,3], m[,4])))
}

#' @export
#'
#' @examples
#' bbox_slice_y("0 0 100 200", 120)
#' bbox_slice_y(c("0 0 100 200", "100 100 200 200"), c(120,150))
#' @rdname bbox_slice
bbox_slice_y <- function(bbox, y){
  m <- bbox_to_matrix(bbox)
  list(top=bbox_validate(paste(m[,1], m[,2], m[,3], y-1)),
       bottom=bbox_validate(paste(m[,1], y, m[,3], m[,4])))
}

#' @export
#'
#' @examples
#' bbox_slice_xy("0 0 100 200", 50, 100)
#' bbox_slice_xy(c("0 0 100 200", "100,100, 200, 200"), c(50, 150), c(100, 150))
#' @rdname bbox_slice
bbox_slice_xy <- function(bbox, x, y){
  m <- bbox_to_matrix(bbox)
  list(top_left  =bbox_validate(paste(m[,1], m[,2], x-1, y-1)),
       top_right =bbox_validate(paste(x, m[,2], m[,3], y-1)),
       bottom_left=bbox_validate(paste(m[,1], y, x-1, m[,4])),
       bottom_right=bbox_validate(paste(x, y, m[,3], m[,4])))
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
  if(length(bbox)==1L && !is.null(bbox2))
    bbox <- rep.int(bbox, times=length(bbox2))
  if(length(bbox2)==1L)
    bbox2 <- rep.int(bbox2, times=length(bbox))
  stopifnot(length(bbox)==length(bbox2) || is.null(bbox2))

  m <- bbox_to_matrix(bbox)

  if(!is.null(bbox2)){
    m2 <- bbox_to_matrix(bbox2)
    return(bbox_validate(paste(pmin(m[,1], m2[,1]), pmin(m[,2], m2[,2]),
                               pmax(m[,3], m2[,3]), pmax(m[,4], m2[,4]))))
  }

  bbox_validate(paste(min(m[,1]), min(m[,2]), max(m[,3]), max(m[,4])))
}

#' @rdname bbox_aggregate
#' @export
bbox_intersect <- function(bbox, bbox2=NULL){
  if(length(bbox)==1L && !is.null(bbox2))
    bbox <- rep.int(bbox, times=length(bbox2))
  if(length(bbox2)==1L)
    bbox2 <- rep.int(bbox2, times=length(bbox))

  m <- bbox_to_matrix(bbox)

  if(!is.null(bbox2)){
    m2 <- bbox_to_matrix(bbox2)
    return(bbox_validate(paste(pmax(m[,1], m2[,1]), pmax(m[,2], m2[,2]),
                               pmin(m[,3], m2[,3]), pmin(m[,4], m2[,4]))))
  }

  bbox_validate(paste(max(m[,1]), max(m[,2]), min(m[,3]), min(m[,4])))
}

#' Predicate functions for matching bboxes
#' These functions can check whether intersection operation on bbox objects returns non-NA result
#' @param bbox character vector of bounding boxes to perform operation on
#' @param bbox2 optional character vector of bounding boxes to element-wise aggregation with `bbox`.
#' If specified, needs to be length 1 or equal in length to `bbox`.
#'
#' @return logical value of whether or not the pair of bboxes intersect
#' @export
#'
#' @examples
#' bbox_intersects(c("5 1 7 3", "2 4 6 8")) # should return FALSE
#' bbox_intersects("5 1 7 3", "2 2 6 8") # should return TRUE
#' @rdname bbox_predicates
#'
bbox_intersects <- function(bbox, bbox2=NULL){
  bbox_i <- bbox_intersect(bbox, bbox2)
  !is.na(bbox_i)
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
  m <- bbox_to_matrix(bbox)
  m[,3]>=m[,1] & m[,4]>=m[,2]
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
#' @param side "left", "right" (for `bbox_pad_width()`), "up", "down" (for `bbox_pad_height()`) or "both" which side to pad
#'
#' @return a vector of validated bboxes
#' @rdname bbox_pad
#'
#' @examples
#' bbox_pad_width("0 0 10 20", "There")
#' bbox_pad_height("0 0 10 20", "There")
#' @export
bbox_pad_width <- function(bbox, word, n=1, side="both"){
  m <- bbox_to_matrix(bbox)
  p <- (m[,3]-m[,1])/nchar(word)
  if(side=="left"|side=="both")
    m[,1] <- m[,1]-p*n
  if(side=="right"|side=="both")
    m[,3] <- m[,3]+p*n

  bbox_validate(paste(m[,1], m[,2], m[,3], m[,4]))
}

#' @rdname bbox_pad
#' @export
bbox_pad_height <- function(bbox, word, n=0.5, side="both"){
  m <- bbox_to_matrix(bbox)
  n_lines <-  lengths(regmatches(word, gregexpr("\n", word)))+1
  p <- (m[,4]-m[,2])/n_lines
  if(side=="up"|side=="both")
    m[,2] <- m[,2]-p*n
  if(side=="down"|side=="both")
    m[,4] <- m[,4]+p*n

  bbox_validate(paste(m[,1], m[,2], m[,3],m[,4]))
}

#' Functions for calculating with bbox
#' These functions can calculate various metrix of bbox
#' @param bbox character vector of bounding boxes to pad
#'
#' @return a vector of validated bboxes
#' @rdname bbox_math
#'
#' @examples
#' bbox_area("100 100 200 200")
#'
#' @export
bbox_area <- function(bbox){
  m <- bbox_to_matrix(bbox)
  (m[,3]-m[,1])*(m[,4]-m[,2])
}

#' Functions for updating certain coordinates of bbox
#' These functions can modify a vector of bbox
#' @param bbox character vector of bounding boxes to update
#' @param x1 scalar or numeric vector to update bbox with
#' @param y1 scalar or numeric vector to update bbox with
#' @param x2 scalar or numeric vector to update bbox with
#' @param y2 scalar or numeric vector to update bbox with
#'
#' @return a vector of updated and validated bboxes
#' @rdname bbox_modify
#'
#' @examples
#' bbox_reset(bbox=c("100 100 200 200", "300 400 500 600"), x2=800)
#'
#' @export
bbox_reset <- function(bbox, x1=NULL, y1=NULL, x2=NULL, y2=NULL){
  if(is.null(x1) && is.null(y1) && is.null(x2) && is.null(y2))
    return(bbox)
  if(length(x1)==4 && is.null(y1) && is.null(x2) && is.null(y2)){
    y1 <- x1[[2]]; x2 <- x1[[3]]; y2 <- x1[[4]]; x1 <- x1[[1]]}

  if(length(x1)==1) x1 <- rep.int(x1, times = length(bbox))
  if(length(y1)==1) y1 <- rep.int(y1, times = length(bbox))
  if(length(x2)==1) x2 <- rep.int(x2, times = length(bbox))
  if(length(y2)==1) y2 <- rep.int(y2, times = length(bbox))

  m <- bbox_to_matrix(bbox)

  if(!is.null(x1))
    m[,1] <- x1
  if(!is.null(y1))
    m[,2] <- y1
  if(!is.null(x2))
    m[,3] <- x2
  if(!is.null(y2))
    m[,4] <- y2

  bbox_validate(paste(m[,1], m[,2], m[,3],m[,4]))
}
