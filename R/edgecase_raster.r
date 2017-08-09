#' Edge Cases
#' Checks all edge cases for rasters

#'@param x raster object
#'@param ... raster objects
#'@param extent compares the bounding boxes
#'@param rowcol number of rows and columns are compared
#'@param crs coordinate refernece system are compared
#'@param res resolutions are compared
#'@param orig origins are compared
#'@param values cell values are compared
#'@param stopiffalse an error will occur
#'@param showwarning a warning is given
#'@return True or False depending on if the edgecases pass or fail.
#'@export

compR <- function(x, ..., extent=TRUE, rowcol=TRUE, crs=TRUE, res=TRUE, orig=TRUE, values=FALSE, stopiffalse=FALSE, showwarning=TRUE) {
  result <- TRUE
  objects <- c(x, list(...))
  if (!isTRUE(length(objects) > 1)) {
    warning('There should be at least 2 Raster* objects to compare')
    return(result)
  }
  minres <- min(res(objects[[1]]))
  proj1 <- projection(objects[[1]])
  ext1 <- extent(objects[[1]])
  ncol1 <- ncol(objects[[1]])
  nrow1 <- nrow(objects[[1]])
  res1 <- res(objects[[1]])
  origin1 <- abs(origin(objects[[1]]))
  rot1 <- rotated(objects[[1]])

  for (i in 2:length(objects)) {
    if (extent) {
      if (!(isTRUE(all.equal(ext1, extent(objects[[i]]), scale=minres )))) {
        result <- FALSE
        if (stopiffalse) { stop('different extent') }
        if (showwarning) { warning('different extent') }
      }
    }
    if (rowcol) {
      if ( !(identical(ncol1, ncol(objects[[i]]))) ) {
        result <- FALSE
        if (stopiffalse) { stop('different number or columns') }
        if (showwarning) { warning('different number or columns') }
      }
      if ( !(identical(nrow1, nrow(objects[[i]]))) ) {
        result <- FALSE
        if (stopiffalse) { stop('different number or rows') }
        if (showwarning) { warning('different number or rows') }
      }
    }
    if (crs) {
      thisproj <- projection(objects[[i]])
      if (is.na(proj1)) {
        proj1 <- thisproj
      } else {
        crs <- try (compareCRS(proj1, thisproj, unknown=TRUE), silent=TRUE)
        if (class(crs) == 'try-error') {
          if (stopiffalse) { stop('invalid CRS') }
          if (showwarning) { warning('invalid CRS') }
        } else if (!crs) {
          result <- FALSE
          if (stopiffalse) { stop('different CRS') }
          if (showwarning) { warning('different CRS') }
        }
      }
    }

    # Can also check res through extent & rowcol
    if (res) {
      if (!(isTRUE(all.equal(res1, res(objects[[i]]), scale=minres)))) {
        result <- FALSE
        if (stopiffalse)  { stop('different resolution') }
        if (showwarning) { warning('different resolution') }
      }
    }
    # Can also check orig through extent & rowcol, but orig is useful for e.g. Merge(raster, raster)
    if (orig) {
      dif <- origin1 - abs(origin(objects[[i]]))
      if (!(isTRUE(all.equal(dif, c(0,0), scale=minres)))) {
        result <- FALSE
        if (stopiffalse) { stop('different origin') }
        if (showwarning) { warning('different origin') }
      }
    }


    if (values) {
      hv1 <- hasValues(objects[[1]])
      hvi <- hasValues(objects[[i]])
      if (hv1 != hvi) {
        if (stopiffalse) { stop('not all objects have values') }
        if (showwarning) { warning('not all objects have values') }
        result <- FALSE
      } else if (hv1 & hvi) {
        if (canProcessInMemory(objects[[1]])) {
          test <- isTRUE(all.equal(getValues(objects[[1]]), getValues(objects[[i]])))
          if (! test) {
            if (stopiffalse) { stop('not all objects have the same values') }
            if (showwarning) { warning('not all objects have the same values') }
            result <- FALSE
          }
        } else {
          tr <- blockSize(objects[[1]])
          for (j in 1:tr$n) {
            v1 <- getValues(objects[[1]], tr$row[j], tr$nrows[j])
            v2 <- getValues(objects[[i]], tr$row[j], tr$nrows[j])
            if (!isTRUE(all.equal(v1, v2))) {
              if (stopiffalse) { stop('not all objects have the same values') }
              if (showwarning) { warning('not all objects have the same values') }
              result <- FALSE
              break
            }
          }
        }
      }
    }
  }
  return(result)
}

