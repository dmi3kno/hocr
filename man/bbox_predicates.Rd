% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bbox.R
\name{bbox_intersects}
\alias{bbox_intersects}
\title{Predicate functions for matching bboxes
These functions can check whether intersection operation on bbox objects returns non-NA result}
\usage{
bbox_intersects(bbox, bbox2 = NULL)
}
\arguments{
\item{bbox}{character vector of bounding boxes to perform operation on}

\item{bbox2}{optional character vector of bounding boxes to element-wise aggregation with \code{bbox}.
If specified, needs to be length 1 or equal in length to \code{bbox}.}
}
\value{
logical value of whether or not the pair of bboxes intersect
}
\description{
Predicate functions for matching bboxes
These functions can check whether intersection operation on bbox objects returns non-NA result
}
\examples{
bbox_intersects(c("5 1 7 3", "2 4 6 8")) # should return FALSE
bbox_intersects("5 1 7 3", "2 2 6 8") # should return TRUE
}
