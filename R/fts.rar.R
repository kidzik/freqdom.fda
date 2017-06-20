#' Test
#' 
#' Test
#' 
#' @param n
#' @export
#' 
fts.rar = function(n, d=11, basis = create.fourier.basis(nbasis=d), ...){
  B=inprod(basis,basis)		
  arg <- list(...)
  arg[['n']] <- n
  arg[['d']] <- d
  arg[['Psi']] 
  fd(t(do.call(rar, arg)),basis)
}