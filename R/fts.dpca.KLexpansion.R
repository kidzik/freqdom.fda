#' @export
fts.dpca.KLexpansion = function(X, dpcs=fts.dpca.filters(fts.spectral.density(X))){
	Y=fts.dpca.scores(X,dpcs)
	dpcsmult=timedom(dpcs$operators,dpcs$lags)
	fd(t(Y %c% freqdom.transpose(rev(dpcsmult))),dpcs$basisY)
}
