#' Test
#' 
#' Test
#' 
#' @param n
#' @export
#' 
fts.rar = function(n, d=11, basis = create.fourier.basis(nbasis=d), ...){
  arg <- list(...)
  arg[['n']] <- n
  arg[['d']] <- d
  fd(t(do.call(rar, arg)),basis)
}