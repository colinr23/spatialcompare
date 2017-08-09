#' Allocation Disagreement
#' Calculates the Allocation Disagreement between 2 matices

#'@param p Reference Matrix
#'@param q Comparison Matrix
#'@return Allocation Disagreement between the 2 matrices
#'@export
allocation_disagreement <- function(p,q) # P and Q are 2 Matrices
{
  sum1 = 0
  sum2 = 0
  for(row in 1:nrow(p)) {
    for(col in 1:ncol(p)) {
      if(p[row,col] == 1 && q[row,col]==0)
      {
        sum1 = sum1 + 1
      }
    }
  }
  for(row in 1:nrow(q)) {
    for(col in 1:ncol(q)) {
      if(p[row,col] == 0 && q[row,col]==1)
      {
        sum2 = sum2 + 1
      }
    }
  }
  s = min(sum1,sum2)*2
  return(s)
}


