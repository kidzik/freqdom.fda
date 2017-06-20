#' @export
fts.dpca.scores = function(X,dpcs = fts.dpca.filters(spectral.density(X))){

  basisX=dpcs$basisX
  B=inprod(basisX,basisX)
  multX = t(B%*%X$coefs)
  A=timedom(dpcs$operators,dpcs$lags)
  dpca.scores(multX,A)
} 

