# ---- roxygen documentation ----
#
#' @title Get Gaussian Filter for raster object
#'
#' @description
#'  This function is a helper function for SSIM functions.
#'
#' @details
#'  The \code{ssim} function can be used to generate the global multiscale ssim.
#'
#' @param sigma is the standard deviation for the filter
#' @param w is the width of the neighbourhood in number of pixels out from centre cell
#'
#'
#' @return
#'   \code{ssim} returns a filter object for use in \code{focal} function in the \code{raster} package
#'
#' @seealso spatialcompare
#' @export
#
# ---- End of roxygen documentation ----

getGauss <- function(sigma, w) {
  w2 = (w*2) + 1
  gf1 <- matrix(nrow=w2, ncol=w2)
  gf2 <- matrix(nrow=w2, ncol=w2)
  for(i in 1:w2) {
    gf1[i,] <-  c(w:0, 1:w)
    gf2[,i] <-  c(w:0, 1:w)
  }
  gf <- (1 / (2 * pi * (sigma^2))) * exp(-(gf1^2 + gf2^2) / (2 * (sigma^2)))
  return(gf/sum(gf))
}
