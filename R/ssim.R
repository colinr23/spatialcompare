# ---- roxygen documentation ----
#
#' @title Structural similarity index
#'
#' @description
#'  This function implements the structural similarity index of Wang et al. (2003) formulated
#'  for comparison of \code{raster} spatial objects.
#'
#' @details
#'  The \code{ssim} function can be used to generate the three component indices of the SSIM index
#'  as well as the composite index. Two raster objects are the key arguments which will be used to compare.
#'  These two raster objects must have the same \code{CRS}, extent, and cell size.
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
#'   \code{ssim} returns a list with 5 objects. The first is the value of the global ssim statistic. Objects 2-5 are all \code{raster} objects
#'   that contain the composite index, luminance, contrast, and structure components respectively.
#'
#' @keywords hello spatialcompare
#' @seealso spatialcompare
#' @export
#
# ---- End of roxygen documentation ----

ssim <- function(img1, img2, w, gFIL=TRUE, edge=FALSE, ks=c(0.01, 0.03)) {
  #set constants
  N <- FALSE
  library(raster)

  L <- max(cellStats(img1, max), cellStats(img2, max))
  globalMin <- abs(min(cellStats(img1, min), cellStats(img2, min)))
  L <- L - globalMin
  K <- ks
  C1 <-(K[1]*L)^2
  C2 <-(K[2]*L)^2
  C3 <-C2/2

  sigma = 1.5
  #create null filter, optionally replace with weighted version
  filterx=matrix(1, nrow=(w*2)+1, ncol=(w*2)+1) / sum(matrix(1, nrow=(w*2)+1, ncol=(w*2)+1))
  #get the Gaussian Kernel
  if(gFIL == TRUE) {
    filterx=getGauss(sigma, w)
  }
  #get mu
  mu1 <- focal(img1, filterx, fun=sum, na.rm=N)
  mu2 <- focal(img2, filterx, fun=sum, na.rm=N)
  img12 <- img1 * img2
  #square
  mu1mu2 <- mu1 * mu2
  mu1sq <- mu1 * mu1
  mu2sq <- mu2 * mu2
  #normalized sigma sq
  sigsq1<- focal(img1*img1,filterx,fun=sum, na.rm=N) - mu1sq
  sigsq2<- focal(img2*img2,filterx,fun=sum, na.rm=N) - mu2sq
  sig12 <- focal(img1*img2,filterx,fun=sum, na.rm=N) - mu1mu2
  #std dev
  sig1 <- sigsq1 ^ 0.5
  sig2 <- sigsq2 ^ 0.5
  #compute components
  L <- ((2*mu1mu2)+C1) / (mu1sq + mu2sq + C1)
  C <- (2*sig1*sig2+C2) / (sigsq1 + sigsq2 + C2)
  S <- (sig12 + C3) / (sig1 * sig2 + C3)
  #compute SSIMap
  SSIM2 <- L * C * S
  #compute SSIM
  num <- (2*mu1mu2+C1)*(2*sig12+C2)
  denom <- (mu1sq+mu2sq+C1) * (sigsq1+sigsq2+C2)
  #global mean
  mSSIM <- cellStats((num / denom), mean)
  return(list(mSSIM, SSIM2, L, C, S))
}


