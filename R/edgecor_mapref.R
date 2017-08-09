#' Edge Correction via map reflection

#'@param ras Given Raster
#'@param w weight
#'@import maptools
#'@import geepack
#'@import splines
#'@import SpatialTools
#'@import raster
#'@return iterative edge correction via map reflection
#'@export
edge.cor.ref <- function(ras,w){
  iter.edge.cor.ref <- function(ind, ras, cel){
    #reflect along vertical/horizontal edges
    i <- cel*2-1
    col. <- colFromCell(ras,ind)
    row. <- rowFromCell(ras,ind)
    cols <- c(col.-i, col.,   col.+i, col.)
    rows <- c(row.,   row.-i, row.,   row.+i)
    celly <- cellFromRowCol(ras,rows,cols)
    subr <- ras[celly]
    est <- mean(subr,na.rm=TRUE)
    if (!is.nan(est)){
      return(est)
    } else {
      #Reflect along diagonal if it is only corner connected.
      cols <- c(col.-i, col.-i, col.+i, col.+i)
      rows <- c(row.-i, row.+i, row.+i, row.-i)
      celly <- cellFromRowCol(ras,rows,cols)
      subr <- ras[celly]
      est <- mean(subr,na.rm=TRUE)
      if (!is.nan(est)){
        return(est)
      } else {return(NA)}
    }
  }
  #--------------
  #Assume NA values (edge padding or donut holes) exist for edge correction.
  for (cel in 1:w){
    temp <- boundaries(ras,type='outer')
    loc <- Which(temp==1,cells=T)
    for (ind in loc){
      ras[ind] <- iter.edge.cor.ref(ind,ras,cel)
    }
  }
  return(ras)
}


