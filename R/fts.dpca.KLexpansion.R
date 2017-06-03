#' @export
fts.dpca.KLexpansion = function(X,...){
  arg <- list(...)
  arg["X"] = t(X$coefs)
  fd(t(do.call(dpca.KLexpansion, arg)), X$basis)
}