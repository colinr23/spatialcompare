#' Weighted Simultaneous area overlap
#' Calculates the Weighted Simultanoeus area overlap between 2 polygon sets

#'@param poly1 type of SpatialPolygons
#'@param poly2 type of SpatialPolygons
#'@return Weighted Simultaneous area overlap
#'@export
weighted_simultanous_area_overlap <- function(poly1,poly2)
{
  n_poly1 = length(poly1)
  n_poly2 = length(poly2)
  s1 = vector('double')
  s2 = vector('double')
  a1 = vector('double')
  a2 = vector('double')
  for(i in 1:n_poly1)
  {
    p = poly1@polygons[[i]]@Polygons[[1]]@coords
    a1[i] = geosphere::areaPolygon(p)
    s1[i] = a1[i] * maximum_rel_area_ovelap(poly2,p)
  }
  for(i in 1:n_poly2)
  {
    p = poly2@polygons[[i]]@Polygons[[1]]@coords
    a2[i] = geosphere::areaPolygon(p)
    s2[i] = a2[i] * maximum_rel_area_ovelap(poly2,p)
  }
  s3 = sum(s1)
  s4 = sum(s2)
  s5 = sum(a1)
  s6 = sum(a2)
  res = (s3 + s4)/(s5 + s6)
  return(res)
}


