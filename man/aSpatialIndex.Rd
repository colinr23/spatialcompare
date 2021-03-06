% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aSpatialIndex.R
\name{aSpatialIndex}
\alias{aSpatialIndex}
\title{Indices of agreement between two vectors or rasters}
\usage{
aSpatialIndex(x, y, method = "pearson")
}
\arguments{
\item{method}{is the index requested. One of 'Pearson', 'concordance', 'rmse', 'rmae', 'coef', 'wilm1', 'wilm2', 'bias', 'crm'}
}
\value{
\code{aSpatialIndex} returns a numeric vector with the index value
}
\description{
This function implements standard measures of agreement between vectors.
}
\details{
The \code{aSpatialIndex} is a function with several basic measures for comparing maps where two observations (e.g., modelled and observed, time 1  time 2, simulation A and simulation B) are measured over each unit. These measures are 'a-spatial' and can be used on any numeric vectors. Non-vector data types are not supported, so values from spatial object classes such as \code{sp} and \code{raster} must be extracted ('$' or '[]' respectively).
}
\seealso{
spatialcompare
}

