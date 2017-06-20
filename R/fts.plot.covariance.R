#' Plot covariance and cross covariance kernels of functional time series
#'
#' @export
fts.plot.covariance = function(X, Y = X, cor = FALSE, main="Operators", res=200, lags = -1:1){
  A=fts.cov.structure(X,Y,lags=lags)
  if(cor==TRUE){
    A=fts.cor.structure(X,Y,lags=0:lags)
  }
  fts.plot.operators(A,lags=lags,res=res)
}


#' @export
fts.plot.operators = function(A, res=200, lags = -1:1, freq = NULL, complex_axis = "Re"){
  if (is.fts.timedom(A)){
    A = fts.timedom.trunc(A,lags = lags)
  }
  if (is.fts.freqdom(A)){
    # select frequencies close to desired ones
    nfreq = length(freq)
    dist = abs(t(A$freq%*% t(rep(1,nfreq))) - freq)
    mins = apply(dist,1,which.min)
    
    # truncate the operator
    A$operators = A$operators[,,mins,drop=FALSE]
    A$freq = A$freq[mins]
  }
  nlags = dim(A$operators)[3]

  basis1=A$basisX
  basis2=A$basisY
  z=c()
  int1=basis1$rangeval[1]+(basis1$rangeval[2]-basis1
                           $rangeval[1])*1:res/res
  int2=basis2$rangeval[1]+(basis2$rangeval[2]-basis2$rangeval[1])*1:res/res
  for(i in 1:nlags){	
    M = Re(A$operators[,,i])
    if (complex_axis == "Im")
      M = Im(A$operators[,,i])

    Afun=bifd (coef=M, sbasisobj=basis1,
               tbasisobj=basis2)
    z=rbind(z,eval.bifd(int1,int2, Afun))
  }	
  filled.contour(1:(res*nlags)/res, int2, z, color.palette=colorRampPalette(c("blue", "white", "red"), space="rgb"),nlevels=40,zlim=c(-max(abs(z)),max(abs(z))),
                 main="contour of kernels",xlab="lags",xaxt = "n")
}

