# ---- roxygen documentation ----
#
#' @title Indices of agreement between two vectors or rasters
#'
#' @description
#'  This function implements standard measures of agreement between vectors.
#'
#' @details
#'  The \code{aSpatialIndex} is a function with several basic measures for comparing maps where two observations (e.g., modelled and observed, time 1  time 2, simulation A and simulation B) are measured over each unit. These measures are 'a-spatial' and can be used on any numeric vectors. Non-vector data types are not supported, so values from spatial object classes such as \code{sp} and \code{raster} must be extracted ('$' or '[]' respectively).
#'
#' @param method is the index requested. One of 'Pearson', 'concordance', 'rmse', 'rmae', 'coef', 'wilm1', 'wilm2', 'bias', 'crm'
#'
#' @return
#'   \code{aSpatialIndex} returns a numeric vector with the index value
#'
#' @seealso spatialcompare
#' @export
#
# ---- End of roxygen documentation ----

aSpatialIndex <- function(x, y, method='pearson') {
  #obs = x, modelled simulated = y
  switch(method,
         'pearson' = return(cor(x,y)),                                                       #correlation
         'rmse' = sqrt(mean((x-y)^2)),                                                       #root mean square error
         'condordance' <- (2*cov(x,y)) / (var(x) + var(y) + (mean(x) - mean(y))^2),
       #condordance correlation
         'rmae' = mean(abs((x-y))),                                                          #root mean absolute error
         'coef' = 1 - (sum(abs(x-y)^2) / sum(abs(x-mean(x))^2)),                             #coefficient of efficiency
         'wilm1' = 1 - (sum(abs(x-y)^1) / sum((abs(x-mean(x)) + abs(y - mean(x)))^1)),       #wil 2norm
         'wilm2' = 1 - (sum(abs(x-y)^2) / sum((abs(x-mean(x)) + abs(y - mean(x)))^2)),       #wil 1norm
         'bias' = mean(x-y),                                                                 #bias
         'crm' = (sum(x) - sum(y)) / sum(x),                                                 #coefficient of residual mass
         'nrmse' = sqrt(mean((x-y)^2)) / range(x),                                           #normalized rmse
         'ndi' = sqrt(mean((x-y)^2)) / sd(x)                                                 #normalized error index
  )
}
