#' Plot kernels
#'
#' @export
fts.plot.filters = function(X, Ndpc = 1, lags = -3:3)
{
  X = fts.timedom.trunc(X, lags)
  # TODO: for now assumes symmetric lags
  d = dim(X$operators)[3]
  mid = (d+1)/2

  cmin = 1e10
  cmax = -1e10
  
  for (dpc in 1:Ndpc){
    for (i in 1:d){
      F = fd(as.matrix(X$operators[dpc,,i]),X$basisX)
      evals = eval.fd(0:100/100,F)
      if (cmin > min(evals)) cmin = min(evals)
      if (cmax < max(evals)) cmax = max(evals)
    }
  }
  
  for (dpc in 1:Ndpc){
    for (i in 1:d){
      F = fd(as.matrix(X$operators[dpc,,i]),X$basisX)
      F$basis$rangeval = -mid + i + c(0,1)
      if (i == 1){
        xlim = c(1-mid,mid)
        plot(F,xlim=xlim,ylim=c(cmin,cmax),xlab="", ylab="",xaxt='n',lwd=4,col=1,bty="n")
      }
      else {
        lines(F,lwd=4,col=1)
      }
      if (i == mid)
        abline(h=0,lty=1)
      abline(v=F$basis$rangeval[1],lty=2)
      abline(v=F$basis$rangeval[1]+1,lty=2)
      title(paste("Component",dpc))
    }
  }
}

