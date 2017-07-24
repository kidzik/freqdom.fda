#' Estimates the spectral density operator and cross spectral density operator of functional time series. 
#' 
#' Let \eqn{X_1(u),\ldots, X_T(u)} and \eqn{Y_1(u),\ldots, Y_T(u)} be two samples of functional data. The cross-spectral density kernel between the two time series \eqn{(X_t(u))} and \eqn{(Y_t(u))} is defined as 
#' \deqn{  
#'   f^{XY}_\omega(u,v)=\sum_{h\in\mathbf{Z}} \mathrm{Cov}(X_h(u),Y_0(v)) e^{-ih\omega}.
#' }
#' The function \code{\link{fts.spectral.density}} determines the empirical
#' cross-spectral density kernel between the two time series. The estimator is of the
#' form
#' \deqn{
#'   \widehat{f}^{XY}_\omega(u,v)=\sum_{|h|\leq q} w(|k|/q)\widehat{c}^{XY}_h(u,v)e^{-ih\omega},
#' }
#' with \eqn{\widehat{c}^{XY}_h(u,v)} defined in \code{\link{fts.cov.structure}}.
#' The other paremeters are as in \code{\link[freqdom]{cov.structure}}.
#' 
#' Since \eqn{X_t(u)=\boldsymbol{b}_1^\prime(u)\mathbf{x}_t} and \eqn{Y_t(u)=\mathbf{y}_t^\prime \boldsymbol{b}_2(u)} we can write 
#' \deqn{
#'   \widehat{f}^{XY}_\omega(u,v)=\boldsymbol{b}_1^\prime(u)\widehat{\mathcal{F}}^{\mathbf{xy}}(\omega)\boldsymbol{b}_2(v),
#' }
#' where \eqn{\widehat{\mathcal{F}}^{\mathbf{xy}}(\omega)} is defined as for the function \code{\link[freqdom]{spectral.density}} for series of coefficient vectors 
#' \eqn{(\mathbf{x}_t\colon 1\leq t\leq T)} and \eqn{(\mathbf{y}_t\colon 1\leq t\leq T)}.
#' 
#' @title Functional spectral and cross-spectral density operator 
#' @usage fts.spectral.density(X, Y=X, freq =(-1000:1000/1000)*pi,
#'  q = max(1, floor((dim(X$coefs)[2])^{0.33})), weights = "Bartlett")
#' 
#' @param X an object of class \code{\link[fda]{fd}} containing \eqn{T} functional observations.
#' @param Y an object of class \code{\link[fda]{fd}} containing \eqn{T} functional observations.
#' @param freq a vector containing frequencies in \eqn{[-\pi, \pi]} on which the spectral density should be evaluated.
#' By default \code{freq=(-1000:1000/1000)*pi}.
#' @param q window size for the kernel estimator, i.e. a positive integer. By default we choose \code{q = max(1, floor((dim(X$coefs)[2])^{0.33}))}.
#' @param weights kernel used in the spectral smoothing. For possible choices see
#' \code{\link[freqdom]{spectral.density}} in package \code{freqdom}. By default the Bartlett kernel is chosen.
#' @return Returns an object of class \code{fts.timedom}. The list is containing the following components:
#' * \code{operators} \eqn{\quad} an array. Element \code{[,,k]} in the coefficient matrix of the spectral density matrix evaluated at the \eqn{k}-th frequency listed in \code{freq}.
#' * \code{lags} \eqn{\quad} returns the lags vector from the arguments.
#' * \code{basisX} \eqn{\quad} returns \code{X$basis}, an object of class \code{\link[fda]{basis.fd}}.
#' * \code{basisY} \eqn{\quad} returns \code{Y$basis}, an object of class \code{\link[fda]{basis.fd}}.
#' @seealso The multivariate equivalent in the \code{freqdom} package: \code{\link[freqdom]{spectral.density}}
#' @export
#' @keywords DPCA
fts.spectral.density = function(X, Y=X, ...){
  arg <- list(...)
  arg[['X']] <- t(X$coefs)
  arg[['Y']] <- t(Y$coefs)
  
  if (!("q" %in% names(arg)))
    arg[['q']] = ceiling((dim(X$coefs)[2])^(1/3)) 
  
  
  fdom = do.call(spectral.density, arg)
  fts.freqdom(fdom,basisX=X$basis,basisY=Y$basis)
  }
