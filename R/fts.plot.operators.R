#' Plot kernels
#'
#' @export
fts.plot.operators = function(X, cor = FALSE, grid = FALSE, main="Operators"){
  plots = list()
  cmin = 0 #1e10
  cmax = -1e10
  cv = NULL
  for (i in 1:dim(X$operators)[3]){
    if (X$lags[i] == 0){
      h = fts.operator(X, i, cor)
      cv = diag(h)
    }
  }
  
  for (i in 1:dim(X$operators)[3]){
    op = fts.operator(X, i, cor, cv = cv)
    if (min(op) < cmin) cmin = min(op)
    if (max(op) > cmax) cmax = max(op)
  }
  # if (cor){
  #   cmin = -1
  #   cmax = 1
  # }
  
  for (i in 1:dim(X$operators)[3]){
    plots[[as.character(i)]] = fts.plot.one.operator(X, i, cor, cmin, cmax, cv = cv)
  }
  if (grid){
    lat.grid <- expand.grid(x=length(plots), y=1)

    # that's the very weird syntax from latticeExtra...
    PT = plots[[1]]
    if (length(plots) >= 2)
      for (i in 2:length(plots))
        PT = c(PT, plots[[i]], layout = c(i,1))
    PT = update(PT, xlab = paste("lag",X$lags), main = main)
    print(PT)
    
#    PT = c(lapply(plots, update), list(nrow=1))
    
#     do.call(function(...) { c(..., layout=c(1,5), merg.legends = FALSE) }, plots)
#    do.call(grid.arrange, plots)
  }
  else{
    for (plt in plots)
      print(plt)
  }
}

fts.operator = function(X, lag, cor, cv = NULL)
{
  dgrid = 100
  rangeX = seq(X$basisX$rangeval[2], X$basisX$rangeval[1], length.out = dgrid)
  rangeY = seq(X$basisY$rangeval[1], X$basisY$rangeval[2], length.out = dgrid)
  
  evalX = eval.basis(rangeX, X$basisX)
  evalX = evalX / sqrt(dgrid)
  evalY = eval.basis(rangeY, X$basisY)
  evalY = evalY / sqrt(dgrid)
  
  M = X$operators[,,lag]

  h = evalY %*% M %*% t(evalX)

  if (cor && !is.null(cv)){
    h = h / sqrt(cv %*% t(cv))
  }
  h
}

fts.plot.one.operator = function(X, lag, cor, cmin, cmax, cv = NULL)
{
  h = fts.operator(X,lag, cor, cv = cv)
  
  if (is.freqdom(X))
    lagname = paste("frequency",X$freq[lag])
  if (is.timedom(X))
    lagname = paste("lag",X$lags[lag])
  levelplot(Re(h), xlab="", ylab=NULL, at=seq(cmin,cmax,length.out=50), col.regions=rev(heat.colors(50)),
                  scales=list(draw=FALSE), main=lagname )
}