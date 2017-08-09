#' Average Maximum Relative Area Overlap
#' Calculates the average maximum relative area overlap in a polygon set

#'@param poly1 type of SpatialPolygons, set of polygons for which the average maximum relative area overlap needs to be found.
#'@param poly2 type of SpatialPolygons, set of polygons which needs to be overlapped with poly1.
#'@return Average Maximum relative area overlap
#'@export
average_max_rel_area_overlap <- function(poly1,poly2)
{
  n_poly1 = length(poly1)
  s1 = vector('double')
  for(i in 1:n_poly1)
  {
    p <- poly1@polygons[[i]]@Polygons[[1]]@coords
    s1[i] = maximum_rel_area_ovelap(poly2,p)
  }
  s2 = sum(s1)
  res = (s2)/(n_poly1)
  return(res)
}


