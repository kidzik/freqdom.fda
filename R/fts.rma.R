#' @export
fts.rma = function(n, d=11, basis = create.fourier.basis(nbasis=d), ...){
  arg <- list(...)
  arg[['n']] <- n
  arg[['d']] <- basis$nbasis
  fd(t(do.call(rma, arg)),basis)
}