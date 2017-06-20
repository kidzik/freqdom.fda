#' @export

fts.timedom = function (A,basisX, basisY=basisX)
{
	if(!is.timedom(A)) 
	stop("A must be an object of class timedom")  

	if(!is.basis(basisX))
	stop("basisX must be a functional basis")  

	if(!is.basis(basisY))
	stop("basisY must be a functional basis")  

	if(dim(A$operators)[1]!=basisX$nbasis && 
	dim(A$operators)[2]!=basisY$nbasis)
	stop("number of basis elements not conform with dimension of coefficient matrices")

	res = A
	res$basisX=basisX
	res$basisY=basisY
	class(res) = 'fts.timedom'
	res
}

