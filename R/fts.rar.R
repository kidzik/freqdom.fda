#' Test
#' 
#' Test
#' 
#' @param n
#' @export
#' 
fts.rar = function(n, basis = create.fourier.basis(nbasis=11), ...){
  arg <- list(...)
  arg[['n']] <- n
  arg[['d']] <- basis$nbasis
  fd(t(do.call(rar, arg)),basis)
}