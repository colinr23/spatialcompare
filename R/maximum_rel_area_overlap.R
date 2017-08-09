#' Calculates Maximum Relative Area Overlap
#'
#' Takes in a couple of polygons and calculates the relative area overlap between them

#'@param poly is a set of polygons (as a Spatial Polygon Data Frame)
#'@param p is the polygon whose maximum relative area overlap needs to be determined.
#'@importMethodsFrom gpclib intersect
#'@importMethodsFrom gpclib union

#'@return The maximum relative area overlap between the 2 polygon
#'@export
maximum_rel_area_ovelap <- function(poly,p)
{
  polygon <-poly@polygons
  rel = vector('double')
  n_poly <- length(poly)
  s2 <- as(p, "gpc.poly")
  for(i in 1:n_poly)
  {
    s1 <- poly@polygons[[i]]@Polygons[[1]]@coords
    s1 <- as(s1, "gpc.poly")
    rel[i] = relative_area_overlap(s1,s2)
  }
  max = max(rel)
  return(max)
}
