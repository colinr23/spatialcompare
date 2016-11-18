# ---- roxygen documentation ----
#
#' @title Multiscale structural similarity index
#'
#' @description
#'  This function implements the multiscale structural similarity index of Wang et al. (2003) formulated
#'  for comparison of \code{raster} spatial objects at multiple scales.
#'
#' @details
#'  The \code{msssim} function can be used to generate the global multiscale ssim. Local values are not supported. The weights used in the default are those given in Wang et al. (2003) and were determined empirically for a five-level application. The weights determine how cross-scale values of the SSIM are weighted.
#'
#' @param img1 is a \code{raster} object to compare
#' @param img2 is a \code{raster} object to compare
#' @param w is the width of the neighbourhood in number of pixels out from centre cell
#' @param GFil is a binary flag for whether a Gaussian filter should be applied as a smoothing function
#' @param edge is a binary flag for whether a torroidal edge correction should be applied
#' @param ks is a vector of length 2 which contains values for constants in the SSIM formula. If ignored default values will be used.
#'
#'
#' @return
#'   \code{msssim} returns a vector with the value of the multiscale structural similarity index
#'
#' @keywords multi-scale spatialcompare
#' @seealso ssim
#' @export
#
# ---- End of roxygen documentation ----

msssim <- function(img1, img2, w, gFIL=TRUE, edge=FALSE, ks=c(0.01, 0.03), level=5, weight=c(0.0448, 0.2856, 0.3001, 0.2363, 0.1333), method='product') {
  im1 <- img1
  im2 <- img2
  N <- FALSE
  sssimArray <- list()
  sssimArray[[1]] <- ssim(im1, im2, w, gFIL, edge, ks)
  for(i in 2:level) {
    sigma <- 3.37
    filterx=getGauss(sigma, i)
    im1f <- focal(im1, filterx, fun=sum, na.rm=N) #low pass filter
    im2f <- focal(im2, filterx, fun=sum, na.rm=N)
    im1f <- aggregate(im1f, fact=2) #downsample
    im2f <- aggregate(im2f, fact=2)
    #compute c and s
    sssimArray[[i]] <- ssim(im1f, im2f, w, gFIL, edge, ks)
  }
  if(method =='product') {
    x <- unlist(sssimArray)
    l <- 3 #index of luminance of 1st level
    cs <- seq(4,level*5, by=5) #indices of contrast at all levels
    ss <- seq(5,level*5, by=5) #indices of structure at all levels
    contrasts <- unlist(lapply(unlist(x)[cs], cellStats, mean)) #mean values of contrast component at all levels
    structures <- unlist(lapply(unlist(x)[ss], cellStats, mean)) #mean values of structure component at all levels
  }
  msssimO <- cellStats(x[[3]], mean)^1 *  prod(contrasts^weight, structures^weight)
  return(msssimO)
}

