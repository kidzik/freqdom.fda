#' @export

fts.dpca.var = function(F){
  if (!is.fts.freqdom(F))
    stop("F must be an object of class fts.freqdom")

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

  E = freqdom.eigen(multF)
  Re(rowSums(E$values) / sum(freqdom.trace(multF)$values))

    
}
