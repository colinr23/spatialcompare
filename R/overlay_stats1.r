#' Overlay Statistics
#' Os - Count of the number of locations that are in both the sets.

#'@param x raster1
#'@param J raster2
#'@import raster
#'@return O_s - Count of the number of locations that are in both the sets
#'@export
O_s <- function(x,y) {
  s = intersect(x,y)
  s1 = ncell(s)
  return(s1)
}


