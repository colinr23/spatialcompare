#' Wavelet Decomposition
#' Applies the Two-Dimensional Discrete Haar Wavelet Filter

#'@param y object returned by dwt
#'@import waveslim
#'@return List structure containing the 3J + 1 sub-matrices from the decomposition
#'@export
#'
wavelet_decomposition2 <- function(y)
{
  J <- attributes(y)$J

  dict <- wave.filter(attributes(y)$wavelet)
  L <- dict$length
  storage.mode(L) <- "integer"
  h <- dict$hpf
  storage.mode(h) <- "double"
  g <- dict$lpf
  storage.mode(g) <- "double"

  LL <- paste("LL", J, sep="")
  y.in <- y[[LL]]

  for(j in J:1) {
    LH <- paste("LH", j, sep="")
    HL <- paste("HL", j, sep="")
    HH <- paste("HH", j, sep="")

    m <- dim(y.in)[1]
    storage.mode(m) <- "integer"
    n <- dim(y.in)[2]
    storage.mode(n) <- "integer"
    x <- matrix(0, 2*m, 2*n)
    storage.mode(x) <- "double"

    out <- .C("two_D_idwt", as.double(y.in), as.double(y[[LH]]),
              as.double(y[[HL]]), as.double(y[[HH]]), m, n, L, h, g,
              "Y"=x, PACKAGE="waveslim")
    y.in <- out$Y
  }
  zapsmall(y.in)
}
