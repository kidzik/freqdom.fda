#' @export
fts.cov.structure = function(X, Y=X, ...){
  arg <- list(...)
  arg[['X']] <- t(X$coefs)
  arg[['Y']] <- t(Y$coefs)
  fdom = do.call(cov.structure, arg)
  fdom$basisX = X$basis
  fdom$basisY = Y$basis
  fdom
}
