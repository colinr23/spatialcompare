#' Relative Area Overlap
#'
#' Takes in a couple of polygons and calucluates the relative area overlap between them

#'@param p1 The first spatial polygon (should be a list of coordinates)
#'@import gpclib
#'@param p2 The second spatial polygon (should be a list of coordinates)
#'@return The relative area overlap between the 2 polygon
#'@export
relative_area_overlap <- function(p1,p2)
{
  p1 <- as(p1, "gpc.poly")
  p2 <- as(p2, "gpc.poly")
  intersection = area.poly(intersect(p1,p2))
  union = area.poly(union(p1,p2))
  rel = intersection/union
  return(rel)
}


