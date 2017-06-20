#' @export

fts.freqdom = function (F, basisX, basisY=basisX)
{
	if(!is.freqdom(F)) 
	stop("F must be an object of class freqdom")  

	if(!is.basis(basisX))
	stop("basisX must be a functional basis")  

	if(!is.basis(basisY))
	stop("basisY must be a functional basis")  

	if(dim(F$operators)[1]!=basisX$nbasis && 
	dim(F$operators)[2]!=basisY$nbasis)
	stop("number of basis elements not conform with dimension of coefficient matrices")

	res = F
	res$basisX=basisX
	res$basisY=basisY
	class(res) = 'fts.freqdom'
	res
}

