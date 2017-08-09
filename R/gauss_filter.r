#' Gaussian Filter Weights Matrix

#'@param sigma cooeficient signma
#'@param w weight
#'@return iterative edge correction via map reflection
#'@export
filter.g <- function(w,sigma){
  f.g <- function(x,y,sigma){ (1/(2*pi*sigma^2))*exp(-(x^2+y^2)/(2*sigma^2))}
  w.i <- seq(-w,w,1)
  xy <- expand.grid(x=w.i,y=w.i)
  xy$w <- f.g(xy$x,xy$y,sigma)
  w.m <- matrix(xy$w,nrow=length(w.i),byrow=T)/sum(xy$w)
  return(w.m)
}


