#' Overlay Statistics
#' OG - Mean distance from any location in the boundaries for G to the neareset location in the boundaries for H.

#'@param x raster1
#'@param J raster2
#'@import raster
#'@return O_G - Mean distance from any location in the boundaries for raster1 to the neareset location in the boundaries for raster2.
#'@export
O_G <- function(x,y){
  N_g = ncell(x)
  s1 <- boundaries(x)
  s2 <- boundaries(y)
  s1 = aggregate(s1,30)
  s2 = aggregate(s2,30)
  p1 <- rasterToPoints(s1,spatial=T)
  p2 <- rasterToPoints(s2,spatial=T)
  p1@data <- data.frame(p1@data, long=coordinates(p1)[,1],lat=coordinates(p1)[,2])
  p2@data <- data.frame(p2@data, long=coordinates(p2)[,1],lat=coordinates(p2)[,2])
  dist <- vector()
  distance <- vector()
  for(i in 1:nrow(p1))
  {
    for(j in 1:nrow(p2))
    {
      x1 = c(p1@data[i,2], p1@data[i,3])
      x2 = c(p2@data[j,2], p2@data[j,3])
      dist[j] <- euc.dist(x1,x2)
    }
    distance[i] = min(dist)
  }
  s = sum(distance)
  s1 = s/N_g
  return(s1)
}

