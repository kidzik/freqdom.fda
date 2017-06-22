#' @export
fts.dpca.filters = function(F,...){
  if(!is.fts.freqdom(F))
  stop("F must be of class fts.freqdom")	

  if(dim(F$operators)[1]!= dim(F$operators)[2])
  stop("coefficients must be square matrices")

  B=inprod(F$basisX,F$basisX)
  B.root=eigen(B)$vectors%*%diag(sqrt(eigen(B)$values))%*%  t(eigen(B)$vectors)
  B.root.minus=solve(B.root)	
  
n=dim(F$operators)[3]
  
for(i in 1:n){
  F$operators[,,i]=B.root%*%F$operators[,,i]%*%B.root	
  }	

multF=freqdom(F$operators,F$freq)  

arg <- list(...)
  arg[["F"]] = multF
  
A = do.call(dpca.filters, arg)

nfilters=dim(A$operators)[3]
ncomp=dim(A$operators)[1]

if(ncomp==1){
for(i in 1:nfilters){
	A$operators[,,i]=t(B.root.minus%*%as.matrix(A$operators[,,i]))
} 
}
else{
for(i in 1:nfilters){
	A$operators[,,i]=t(B.root.minus%*%t(A$operators[,,i]))
}
} 
    fts.timedom(A, F$basisX, F$basisX)
}
