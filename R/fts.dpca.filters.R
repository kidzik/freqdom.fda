#' @export
fts.dpca.filters = function(F,...){
  arg <- list(...)
  arg["F"] = F
  A = do.call(dpc.filters, arg)
  A$basisX = F$basisX
  A$basisY = F$basisY
  A
}
