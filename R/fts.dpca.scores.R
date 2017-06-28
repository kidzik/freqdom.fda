#' Computes the dynamic principal component scores of a functional time series.
#' 
#' The \eqn{\ell}-th dynamic principal components score sequence is defined by
#' \deqn{
#'   Y_{\ell t}:=\sum_{k\in\mathbf{Z}} \int_0^1 \phi_{\ell k}(v) X_{t-k}(v)dv,\quad 1\leq \ell\leq d,
#' }
#' where \eqn{\phi_{\ell k}(v)} and \eqn{d} are explained in  \code{fts.dpca.filters}. (The integral is not necessarily restricted to the interval \eqn{[0,1]}, this depends on the data.) For the sample version the sum extends over the range of lags for which the \eqn{\phi_{\ell k}} are defined. 
#' 
#' For more details we refer to  Hormann et al. (2015).
#' 
#' @title Computes the dynamic principal component scores of a functional time series.
#' 
#' @param X a functional time series given as an object of class ``fd''.
#' @param dpcs an object of class \code{fts.timedom}, representing the dpca filters obtained from the sample X. If dpsc = NULL, then dpcs = fts.dpca.filter(fts.spectral.density(X)) is used.
#' @return A \eqn{(T\times \code{Ndpc})}-matix with Ndpc = dim(dpcs\eqn{\$}operators)[1]. The \eqn{\ell}-th column contains the \eqn{\ell}-th dynamic principal component score sequence.
#' @references Hormann Siegfried, Kidzinski Lukasz and Hallin Marc.
#' \emph{Dynamic functional principal components.} Journal of the Royal
#' Statistical Society: Series B (Statistical Methodology) 77.2 (2015): 319-348.
#' @export
fts.dpca.scores = function(X,dpcs = fts.dpca.filters(spectral.density(X))){

  basisY=dpcs$basisY
  B=inprod(basisY,basisY)
  multX = t(B%*%X$coefs)
  A=timedom(dpcs$operators,dpcs$lags)
  dpca.scores(multX,A)
} 

