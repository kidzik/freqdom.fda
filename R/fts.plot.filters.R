#' Plot kernels
#'
#' @param X a functional filter sequence given as object of class \code{\link{fts.timedom}}.
#' @param Ndpc if Ndpc = k the first k filter sequences are plotted.
#' @param lags number of lags to plot.
#' @param one.plot if TRUE then functional filters corresponding belonging to the respective scores will all be plotted in the same graph.
#' @keywords plotting
#' @importFrom stats update
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics abline filled.contour legend lines plot text title
#' @export
fts.plot.filters = function(X, Ndpc = 1, lags = -3:3, one.plot=FALSE,...)
{
  X = fts.timedom.trunc(X, lags)
  d = dim(X$operators)[3]

  # check if lags are in consecutive order
  if (any(X$lags[2:d] - X$lags[1:(d-1)] != 1) && length(lags) > 1)
    stop("lags must be in consecutive order")

  cmin = 1e2
  cmax = -1e2
  
  # defaults
  cols = 1:Ndpc
  lwd = 2
  lty = 1

  arg <- list(...)
  if ("col" %in% names(arg))
    cols = rep(arg[["col"]], Ndpc)
  if ("lwd" %in% names(arg))
    lwd = arg[["lwd"]]
  if ("lty" %in% names(arg))
    lty = arg[["lty"]]
  arg$lwd=NULL
  arg$lty=NULL
  arg$col=NULL

  lo=X$basisX$rangeval[1]
  up=X$basisX$rangeval[2]
  grid = lo+(up-lo)*0:100/100
  grid01 = 0:100/100

  for (dpc in 1:Ndpc){
    for (i in 1:d){
      F = fd(as.matrix(X$operators[dpc,,i]),X$basisX)
      evals = eval.fd(grid,F)
      if (cmin > min(evals)) cmin = min(evals)
      if (cmax < max(evals)) cmax = max(evals)
    }
  }
  
  for (dpc in 1:Ndpc){
    for (i in 1:d){
      F = fd(as.matrix(X$operators[dpc,,i]),X$basisX)
      evals = eval.fd(grid,F)
      if (i == 1 && (dpc == 1 || !one.plot)){
        xlim = c(X$lags[1],X$lags[d] + 1)
        do.call(function(...) { plot(grid01 + X$lags[1],evals,xlim=xlim,ylim=c(cmin,cmax),xlab="lags", ylab="",xaxt='n',col=cols[dpc],lwd = lwd,lty = lty,type = 'l',bty="n",...) }, arg)
      }
      else {
        do.call(function(...) { lines(grid01 + X$lags[i],evals, col=cols[dpc], lwd = lwd, lty = lty,...) }, arg)
      }
      text(0.5  + X$lags[i], cmin - (cmax - cmin)*0.02,labels= X$lags[i] )
      if (i == 1)
        abline(h=0,lty=1)
      if (i < d)
        abline(v=X$lags[i]+1,lty=2,col="darkgrey")
    }
    if (!one.plot)
      title(paste("Filter",dpc))
  }
  if (one.plot){
    legend(X$lags[1],cmax,paste("filter",1:Ndpc), # places a legend at the appropriate place c(“Health”,”Defense”), # puts text in the legend
           lty=c(1,1), # gives the legend appropriate symbols (lines)
           lwd=c(2.5,2.5), col = cols) # gives the legend lines the correct color and width
    title(paste("Components"))
  }
}

