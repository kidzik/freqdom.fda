#' @export
fts.timedom.trunc = function(X, lags){
  newX = timedom.trunc(X, lags)
  newX$basisX = X$basisX
  newX$basisY = X$basisY
  newX
}