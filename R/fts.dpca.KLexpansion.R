#' @export
fts.dpca.KLexpansion = function(X, dpcs){
  arg <- list()
  arg[["X"]] = X
  arg[["dpcs"]] = timedom(dpcs$operators,dpcs$lags)
  fd(t(do.call(dpca.KLexpansion, arg)), dpcs$basisX)
}