#' @export

fts.timedom = function (A, basisX, basisY=basisX)
{
	nX=basisX$nbasis
	nY=basisY$nbasis

	if(!is.timedom(A)) 
	stop("A must be an object of class timedom")  
	if(!is.basis(basisX))
	stop("basisX must be a functional basis")  

	if(!is.basis(basisY))
	stop("basisY must be a functional basis")  

	res = A
	res$basisX=basisX
	res$basisY=basisY
	class(res) = 'fts.timedom'
	res
}

