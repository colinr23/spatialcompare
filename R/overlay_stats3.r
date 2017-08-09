#' Overlay Statistics
#' OGH - Average distance from any location in the G or H boundaries to the neareset location in the boundaries for G.

#'@param x raster1
#'@param J raster2
#'@import raster
#'@return O_GH - Average Distance from a location in raster1 or raster2 boundaries to the nearest location in the other(raster2 or raster1 as appropriate) boundary.
#'@export
O_GH <- function(x,y){
  N_g = ncell(x)
  s1 <- boundaries(x)
  s2 <- boundaries(y)
  s1 = aggregate(s1,30)
  s2 = aggregate(s2,30)
  p1 <- rasterToPoints(s1, spatial=T)
  p2 <- rasterToPoints(s2, spatial=T)

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
  s1 = sum(distance)
  N_h = ncell(y)
  for(i in 1:nrow(p2))
  {
    for(j in 1:nrow(p1))
    {
      x1 = c(p2@data[i,2], p2@data[i,3])
      x2 = c(p1@data[j,2], p1@data[j,3])
      dist[j] <- euc.dist(x1,x2)
    }
    distance[i] = min(dist)
  }
  s2 = sum(distance)
  s3 = (s1 + s2)/(N_g + N_h)
  return(s3)
}

