#' Calculates Earth Movers Distance
#'
#' Takes in a 2 spatial points data frames and calculates the earth movers distance between them

#'@param x A Spatial points object or a Raster
#'@param y a Spatial Points OBject or a Raster
#'@param gc True if great circle distances should be used.
#'@param threshold The maximal distance over which locations are compared.
#'@import sp
#'@import move
#'@return The earth movers distance between 2 spatial points data frames.
#'@export
earthmd <- function(x,y,gc = FALSE,threshold = NULL)
{
  if (class(x) == "RasterLayer")
    x <- (rasterToPoints(x, spatial = T))
  if (class(y) == "RasterLayer")
    y <- (rasterToPoints(y, spatial = T))
  if (class(x) == "SpatialPointsDataFrame") {
    xv <- data.frame(x)[, names(x)[1]]
  } else{
    xv <- rep(1, length(x)) / length(x)
  }
  if (class(y) == "SpatialPointsDataFrame") {
    yv <- data.frame(y)[, names(y)[1]]
  } else{
    yv <- rep(1, length(y)) / length(y)
  }
  if (!isTRUE(all.equal(sum(xv), sum(yv))))
    warning(paste("datasets dont have equal mass, delta:", sum(xv) -
                    sum(yv)))
  if (!isTRUE(all.equal(sum(xv), 1)))
    warning("data does not represent probability surface")
  res <- 1
  if (!is.null(threshold))
  {
    s <- !(xv == 0 & yv == 0)
    xv <- xv[s]
    yv <- yv[s]
    x <- x[s, ]
    y <- y[s, ]
  } else{
    x <- x[xv != 0, ]
    y <- y[yv != 0, ]
    xv <- xv[xv != 0]
    yv <- yv[yv != 0]
  }
  if (is.null(threshold))
  {
  a <- .C(
    getNativeSymbolInfo("emdR"),
    Pn = as.integer(nrow(x)),
    Qn = as.integer(nrow(y)),
    Px = as.double(coordinates(x)[, 1]),
    Py = as.double(coordinates(x)[, 2]),
    Pw = as.double(xv),
    Qx = as.double(coordinates(y)[, 1]),
    Qy = as.double(coordinates(y)[, 2]),
    Qw = as.double(yv),
    res = as.double(res),
    th = ifelse(is.null(threshold), 1, as.double(threshold)),
    gc = as.integer(gc)
  )
  }
  else
  {
    a <- .C(
      getNativeSymbolInfo("emdR_gd"),
      Pn = as.integer(nrow(x)),
      Qn = as.integer(nrow(y)),
      Px = as.double(coordinates(x)[, 1]),
      Py = as.double(coordinates(x)[, 2]),
      Pw = as.double(xv),
      Qx = as.double(coordinates(y)[, 1]),
      Qy = as.double(coordinates(y)[, 2]),
      Qw = as.double(yv),
      res = as.double(res),
      th = ifelse(is.null(threshold), 1, as.double(threshold)),
      gc = as.integer(gc)
    )
  }
  return(as.dist(matrix(c(0, a$res, a$res, 0), ncol = 2)))
}
