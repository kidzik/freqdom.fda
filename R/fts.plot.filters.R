#' Plot kernels
#'
#' @export
fts.plot.filters = function(X, Ndpc = 1, lags = -3:3, one.plot=TRUE,...)
{
  X = fts.timedom.trunc(X, lags)
  # TODO: for now assumes symmetric lags
  d = dim(X$operators)[3]
  mid = (d+1)/2

  cmin = 1e10
  cmax = -1e10
  
  # defaults
  cols = 1:Ndpc
  lwd = 2

  arg <- list(...)
  if ("col" %in% names(arg))
    cols = rep(arg[["col"]], Ndpc)
  if ("lwd" %in% names(arg))
    lwd = arg[["lwd"]]
  arg$lwd=NULL
  arg$col=NULL

lo=X$basis$rangeval[1]
up=X$basis$rangeval[2]

  for (dpc in 1:Ndpc){
    for (i in 1:d){
      F = fd(as.matrix(X$operators[dpc,,i]),X$basisX)
      evals = eval.fd(lo+(up-lo)*0:100/100,F)
      if (cmin > min(evals)) cmin = min(evals)
      if (cmax < max(evals)) cmax = max(evals)
    }
  }
  
  for (dpc in 1:Ndpc){
    for (i in 1:d){
      F = fd(as.matrix(X$operators[dpc,,i]),X$basisX)
      F$basis$rangeval = -mid + i + c(0,1)
      if (i == 1 && (dpc == 1 || !one.plot)){
        xlim = c(1-mid,mid)
        do.call(function(...) { plot(F,xlim=xlim,ylim=c(cmin,cmax),xlab="", ylab="",xaxt='n',col=cols[dpc],lwd = lwd,bty="n",...) }, arg)
      }
      else {
        do.call(function(...) { lines(F, col=cols[dpc], lwd = lwd, ...) }, arg)
      }
      if (i == mid)
        abline(h=0,lty=1)
      abline(v=F$basis$rangeval[1],lty=2,col="darkgrey")
    }
    if (!one.plot)
      title(paste("Component",dpc))
  }
  if (one.plot)
    title(paste("Components"))
}

