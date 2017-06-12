#' Plot kernels
#'
#' @export
fts.plot.operators = function(X, cor = FALSE, grid = FALSE){
  # TODO: for now it plots the middle operator if there is a lot of them
  plots = list()
  if (dim(X$operators)[3] > 10){
    i = ceiling(dim(X$operators)[3]/2)
    h = fts.operator(X, i, cor)
    plots[[1]] = fts.plot.one.operator(X, i, cor, min(h), max(h))
  }
  else{
    cmin = 0 #1e10
    cmax = -1e10
    for (i in 1:dim(X$operators)[3]){
      X$operators = X$operators 
      op = fts.operator(X, i, cor)
      if (min(op) < cmin) cmin = min(op)
      if (max(op) > cmax) cmax = max(op)
    }
    
    for (i in 1:dim(X$operators)[3]){
      plots[[i]] = fts.plot.one.operator(X, i, cor, cmin, cmax)
    }
  }
  if (grid){
    lat.grid <- expand.grid(x=length(plots), y=1)
    do.call(grid.arrange, c(lapply(plots, update), list(nrow=1)))
  }
  else{
    for (plt in plots)
      print(plt)
  }
}

fts.operator = function(X, lag, cor)
{
  rangeX = seq(X$basisX$rangeval[2], X$basisX$rangeval[1], length.out = 100)
  rangeY = seq(X$basisY$rangeval[1], X$basisY$rangeval[2], length.out = 100)
  
  evalX = eval.basis(rangeX, X$basisX)
  evalY = eval.basis(rangeY, X$basisY)
  
  h = evalY %*% X$operators[,,lag] %*% t(evalX)
  if (cor){
    dg = diag(h)
    h = h / sqrt(dg %*% t(dg))
  }
  h
}

fts.plot.one.operator = function(X, lag, cor, cmin, cmax)
{
  h = fts.operator(X,lag, cor)
  
  if (is.freqdom(X))
    lagname = paste("frequency",X$freq[lag])
  if (is.timedom(X))
    lagname = paste("lag",X$lags[lag])
  levelplot(Re(h), xlab=NULL, ylab=NULL,at=seq(cmin,cmax,length.out=50),col.regions=rev(heat.colors(50)),
                  main = paste("Operator at", lagname), scales=list(draw=FALSE))
}