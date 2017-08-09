#' Simultaneous area overlap
#' Calculates the Simultanoeus area overlap between 2 polygon sets

#'@param poly1 type of SpatialPolygons
#'@param poly2 type of SpatialPolygons
#'@return Simultaneous area overlap
#'@export
simultaneous_area_overlap <- function(poly1,poly2)
{
  n_poly1 = length(poly1)
  n_poly2 = length(poly2)
  s1 = vector('double')
  s2 = vector('double')
  for(i in 1:n_poly1)
  {
    p <- poly1@polygons[[i]]@Polygons[[1]]@coords
    s1[i] = maximum_rel_area_ovelap(poly2,p)
  }
  for(i in 1:n_poly2)
  {
    p <- poly2@polygons[[i]]@Polygons[[1]]@coords
    s2[i] = maximum_rel_area_ovelap(poly1,p)
  }
  s3 = sum(s1)
  s4 = sum(s2)
  res = (s3 + s4)/(n_poly1 + n_poly2)
  return(res)
}


