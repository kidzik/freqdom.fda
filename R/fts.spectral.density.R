#' @export
fts.spectral.density = function(X, Y=X, ...){
  arg <- list(...)
  arg[['X']] <- t(X$coefs)
  arg[['Y']] <- t(Y$coefs)
  
  if (!("q" %in% names(arg)))
    arg[['q']] = ceiling((dim(X$coefs)[2])^(1/3)) 
  
  
  fdom = do.call(spectral.density, arg)
  fts.freqdom(fdom,basisX=X$basis,basisY=Y$basis)
  }
