#' @export
fts.dpca.scores = function(X,dpcs = fts.dpca.filters(spectral.density(X))){

  basisY=dpcs$basisY
  B=inprod(basisY,basisY)
  multX = t(B%*%X$coefs)
  A=timedom(dpcs$operators,dpcs$lags)
  dpca.scores(multX,A)
} 

