#' @export
fts.dpca.scores = function(X,...){
  arg <- list(...)
  arg["X"] = t(X$coefs)
  do.call(dpc.filters, arg)
} 