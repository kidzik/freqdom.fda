inClass = function(X,cls){
  !is.null(oldClass(X)) && oldClass(X) == cls
}

#' @export
is.fts.timedom = function (X){
  inClass(X,'fts.timedom')
}

