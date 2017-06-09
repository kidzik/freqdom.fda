#' @export
fts.dpca.scores = function(X,...){
  arg <- list(...)
  arg[["X"]] = t(X$coefs)
  if (!("dpcs" %in% names(arg)))
    arg[['dpcs']] = ceiling((dim(X$coefs)[1])^(1/3)) # X$coef is transposed, so we take [1]!
  
  do.call(dpca.scores, arg)
} 