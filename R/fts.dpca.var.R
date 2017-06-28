#' Computes the proportion and cumulative proportion of variance explained by dynamic principal components.
#' 
#' Consider a spectral density operator \eqn{\mathcal{F}_\omega} and let \eqn{\lambda_\ell(\omega)} by the \eqn{\ell}-th dynamic
#' eigenvalue. The proportion of variance described by the \eqn{\ell}-th dynamic principal component is given as
#' \eqn{v_\ell:=\int_{-\pi}^\pi \lambda_\ell(\omega)d\omega/\int_{-\pi}^\pi \mathrm{tr}(\mathcal{F}_\omega)d\omega}.
#' This function numerically computes the vectors \eqn{(v_\ell\colon 1\leq \ell\leq d)}.
#' 
#' For more details we refer to Hormann et al. (2015).
#' 
#' @title Computes the proportion and cumulative proportion of variance explained by dynamic principal components.
#' 
#' @param F spectral density operator, provided as an object of class \code{fts.freqdom}. To guarantee accuracy of
#' numerical integration it is important that F\eqn{\$}freq is a dense grid of frequencies in \eqn{[-\pi,\pi]}.
#' @return A vector containing the \eqn{v_\ell}.
#' @references Hormann Siegfried, Kidzinski Lukasz and Hallin Marc.
#' \emph{Dynamic functional principal components.} Journal of the Royal
#' Statistical Society: Series B (Statistical Methodology) 77.2 (2015): 319-348.
#' @export
fts.dpca.var = function(F){
  if (!is.fts.freqdom(F))
    stop("F must be an object of class fts.freqdom")

  if(dim(F$operators)[1]!= dim(F$operators)[2])
  stop("coefficients must be square matrices")

  B=inprod(F$basisX,F$basisX)
  B.root=eigen(B)$vectors%*%diag(sqrt(eigen(B)$values))%*%  t(eigen(B)$vectors)
  B.root.minus=solve(B.root)	
  
  n=dim(F$operators)[3]
    
  for(i in 1:n){
    F$operators[,,i]=B.root%*%F$operators[,,i]%*%B.root	
  }	
  
  multF=freqdom(F$operators,F$freq)  
  
  dpca.var(multF)


    
}
