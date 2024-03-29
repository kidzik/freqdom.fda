% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fts.plot.covariance.R
\name{fts.plot.covariance}
\alias{fts.plot.covariance}
\title{Contour plot for the kernels of cross-covariance operators.}
\usage{
fts.plot.covariance(X, Y = X, cor = FALSE, res = 200, lags = 0:3, nlevels = 25)
}
\arguments{
\item{X}{an object of class \code{\link[fda]{fd}} representing a functional data sample.}

\item{Y}{an object of class\code{\link[fda]{fd}} representing a functional data sample.}

\item{cor}{if \code{FALSE} covariance kernels are plotted, if \code{TRUE} correlation kernel will be plotted.}

\item{res}{number of discretization points to evaluate functional data.}

\item{lags}{lags to plot, dafauts \code{0:3}}

\item{nlevels}{number of color levels for the contour plot.}
}
\description{
Contour plot for the kernels of cross-covariance operators.
}
\examples{
fts = fts.rar(100)

# Plot covariance operators of the time series curves
# We chose resolution equal 150 for better precision 
fts.plot.covariance(fts, lags=0:2, res = 150) 

# Plot correlation operators of the time series curves
fts.plot.covariance(fts, lags=0:2, cor = TRUE, res = 50)

# Make the grid of levels more dense
fts.plot.covariance(fts, lags=0:1, nlevels = 100)
}
\keyword{plotting}
