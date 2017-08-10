#' Quantity Disagreement
#' Calculates the Quanitity Disagreement between 2 matices

#'@param p Reference object
#'@param q Comparison object
#'@return Quantity Disagreement between the 2 matrices
#'@export
quantity_disagreement <- function(p,q) # P and Q are 2 Matrices or vectors
{
  vals <- unique(union(p[], q[])) #all unique values
  ptab <- table(p[]) / length(p[]) #proportional frequencies
  qtab <- table(q[]) / length(q[])
  qv <- vector(length=length(vals))
  pv <- vector(length=length(vals))
  qg <- vector(length=length(vals))
  for(v in 1:length(vals)) {
    pv[v] <- 0
    qv[v] <- 0
    if(length(which(names(ptab) == vals[v])) > 0) {
      pv[v] <- ptab[which(names(ptab) == vals[v])]
    }
    if(length(which(names(qtab) == vals[v])) > 0) {
      qv[v] <- qtab[which(names(qtab) == vals[v])]
    }
    #difference in proportion for category v
    qg[v] <- abs(pv[v] - qv[v])
  }

  #total allocation disagreement
  Q = sum(qg)/2
  return(Q)
}


