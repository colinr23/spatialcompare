#' Wavelet Decomposition
#' Applies the Two-Dimensional Discrete Haar Wavelet Filter

#'@param x input matrix (image)
#'@param J depth of decomposition
#'@import waveslim
#'@return List structure containing the 3J + 1 sub-matrices from the decomposition
#'@export

wavelet_decomposition <- function(x, J=4)
{
  boundary="periodic"
  wf = "haar"
  m <- dim(x)[1]
  storage.mode(m) <- "integer"
  n <- dim(x)[2]
  storage.mode(n) <- "integer"
  L <- 2
  g <- c(0.7071067811865475, 0.7071067811865475)
  h <- qmf(g)
  dict <- list(length = L, hpf = h, lpf = g)
  L <- dict$length
  storage.mode(L) <- "integer"
  h <- dict$hpf
  storage.mode(h) <- "double"
  g <- dict$lpf
  storage.mode(g) <- "double"

  z <- matrix(0, m/2, n/2)
  storage.mode(z) <- "double"

  x.wt <- vector("list", 3*J+1)
  x.names <- NULL
  for(j in 1:J) {
    out <- .C("two_D_dwt", "Image"=as.double(x), "Rows"=m, "Cols"=n,
              "filter.length"=L, "hpf"=h, "lpf"=g, "LL"=z, "LH"=z,
              "HL"=z, "HH"=z, PACKAGE="waveslim")[7:10]
    if(j < J) {
      index <- (3*j-2):(3*j)
      x.wt[index] <- out[-1]
      x.names <- c(x.names, sapply(names(out)[-1], paste, j, sep=""))
      x <- out[[1]]
      m <- dim(x)[1]
      storage.mode(m) <- "integer"
      n <- dim(x)[2]
      storage.mode(n) <- "integer"
      z <- matrix(0, m/2, n/2)
      storage.mode(z) <- "double"
    }
    else {
      index <- (3*j):(3*(j+1)) - 2
      x.wt[index] <- out[c(2:4,1)]
      x.names <- c(x.names, sapply(names(out)[c(2:4,1)], paste, j, sep=""))
    }
  }

  names(x.wt) <- x.names
  attr(x.wt, "J") <- J
  attr(x.wt, "wavelet") <- wf
  attr(x.wt, "boundary") <- boundary
  attr(x.wt, "class") <- "wavelet_decomposition"
  x.wt
}

