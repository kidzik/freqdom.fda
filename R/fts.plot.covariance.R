#' Contour plot for the kernels of cross-covariance operators.
#' 
#' @title Contour plot for the kernels of cross-covariance operators.
#' 
#' @param X an object of class \code{\link[fda]{fd}} representing a functional data sample.
#' @param Y an object of class\code{\link[fda]{fd}} representing a functional data sample.
#' @param cor if \code{FALSE} covariance kernels are plotted, if \code{TRUE} correlation kernel will be plotted.
#' @param res number of discretization points to evaluate functional data.
#' @param nlevels number of color levels for the contour plot.
#' @export
#' @keywords plotting
fts.plot.covariance = function(X, Y = X, cor = FALSE, main="Operators", res=200, lags = 0:3, nlevels=25){
  if(cor==TRUE){
    X=sd1(X)
    Y=sd1(Y)	
  }
  A=fts.cov.structure(X,Y,lags=lags)	
  fts.plot.operators(A,lags=lags,res=res,nlevels=nlevels)
}