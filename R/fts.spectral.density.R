#' @export
fts.spectral.density = function(X, Y=X, ...){
  arg <- list(...)
  arg[['X']] <- t(X$coefs)
  arg[['Y']] <- t(Y$coefs)
  
  fdom = do.call(spectral.density, arg)
  fts.freqdom(fdom,X$basis,Y$basis)
  }
