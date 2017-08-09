#' Euclidean Distance
#' Calculates the Euclidean Distance between 2 spatial points

#'@param x spatial point
#'@param J spatial point
#'@return Euclidean distance
#'@export
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
O_s <- function(x,y) {
  s = intersect(x,y)
  s1 = ncell(s)
  return(s1)
}

