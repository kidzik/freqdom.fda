#' @export
fts.spectral.density = function(X, Y=X, ...){
  arg <- list(...)
  arg[['X']] <- t(X$coefs)
  arg[['Y']] <- t(Y$coefs)
  
  if (!("q" %in% names(arg)))
    arg[['q']] = ceiling((dim(X$coefs)[1])^(1/3)) # X$coef is transposed, so we take [1]!
  
  fdom = do.call(spectral.density, arg)
  fdom$basisX = X$basis
  fdom$basisY = Y$basis
  fdom
}
