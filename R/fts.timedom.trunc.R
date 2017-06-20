#' @export
fts.timedom.trunc = function(X, lags){
  if(!is.fts.timedom(X))
    stop("X must be an object of class fts.timedom")
  multX = timedom(X$operators,X$lags)
  newX = timedom.trunc(multX, lags)
  fts.timedom(newX,X$basisX,X$basisY)
}
