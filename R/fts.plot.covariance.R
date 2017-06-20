#' Plot covariance and cross covariance kernels of functional time series
#'
#' @export
fts.plot.covariance = function(X, Y = X, cor = FALSE, main="Operators", res=200, nlags=5){
	A=fts.cov.structure(X,Y,lags=0:(nlags-1))
	if(cor==TRUE){
	A=fts.cor.structure(X,Y,lags=0:(nlags-1))
	}
     	basis1=A$basisX
	basis2=A$basisY
	z=c()
	int1=basis1$rangeval[1]+(basis1$rangeval[2]-basis1
$rangeval[1])*1:res/res
    int2=basis2$rangeval[1]+(basis2$rangeval[2]-basis2$rangeval[1])*1:res/res
	for(i in 1:nlags){	
		Afun=bifd (coef=A$operators[,,i], sbasisobj=basis1,
          tbasisobj=basis2)
          z=rbind(z,eval.bifd(int1,int2, Afun))
	}	
filled.contour(1:(res*nlags)/res, int2, z, color.palette=colorRampPalette(c("blue", "white", "red"), space="rgb"),nlevels=40,zlim=c(-max(abs(z)),max(abs(z))),
main="contour of kernels",xlab="lags",xaxt = "n")
}

