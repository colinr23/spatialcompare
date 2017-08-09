#' Weighted Average Area Overlap
#' Calculates the Weighted Average Maximum Relative Area Overlap of a polygon set

#'@param poly1 type of SpatialPolygons, set of polygons for which the weighted average maximum relative area overlap needs to be found.
#'@param poly2 type of SpatialPolygons, set of polygons which needs to be overlapped with poly1.
#'@return Weighted Average Maximum Relative Area Overlap
#'@export

weighted_avg_maximum_relative_area_overlap <- function(poly1, poly2)
{
  s = vector('double')
  a = vector('double')
  n_poly <- length(poly)
  for(i in 1:n_poly)
  {
    p = poly1@polygons[[i]]@Polygons[[1]]@coords
    a[i] = geosphere::areaPolygon(p)
    s[i] = a[i] * maximum_rel_area_ovelap(poly2,p)
  }
  s1 = sum(s)
  s2 = s1/sum(a)
  return(s2)
}

