#' @export
fts.rma = function(n, basis = create.fourier.basis(nbasis=11), ...){
  arg <- list(...)
  arg[['n']] <- n
  arg[['d']] <- basis$nbasis
  fd(t(do.call(rma, arg)),basis)
}