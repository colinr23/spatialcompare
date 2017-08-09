#' Quantity Disagreement
#' Calculates the Quanitity Disagreement between 2 matices

#'@param p Reference Matrix
#'@param q Comparison Matrix
#'@return Quantity Disagreement between the 2 matrices
#'@export
quantity_disagreement <- function(p,q) # P and Q are 2 Matrices
{
  sum1 = 0
  sum2 = 0
  for(row in 1:nrow(p)) {
    for(col in 1:ncol(p)) {
      if(p[row,col] == 1)
      {
        sum1 = sum1 + 1
      }
    }
  }

  for(row in 1:nrow(q)) {
    for(col in 1:ncol(q)) {
      if(q[row,col] == 1)
      {
        sum2 = sum2 + 1
      }
    }
  }
  s = abs(sum1 - sum2)
  return(s)
}


