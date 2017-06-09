#' Plot kernels
#'
#' @export
fts.plot.operators = function(X){
  # for now it plots the middle operator if there is a lot of them
  if (dim(X$operators)[3] > 10){
    i = ceiling(dim(X$operators)[3]/2)
    fts.plot.one.operator(X, i)
  }
  else{
    for (i in 1:dim(X$operators)[3]){
      fts.plot.one.operator(X, i)
    }
  }
}

fts.plot.one.operator = function(X, lag)
{
  rangeX = seq(X$basisX$rangeval[1], X$basisX$rangeval[2], length.out = 100)
  rangeY = seq(X$basisY$rangeval[1], X$basisY$rangeval[2], length.out = 100)
  
  evalX = eval.basis(rangeX, X$basisX)
  evalY = eval.basis(rangeY, X$basisY)
  
#  evalX = evalX[,1:(dim(X)[2]),drop=FALSE]
#  evalY = evalY[,1:(dim(X)[1]),drop=FALSE]

  h = evalY %*% X$operators[,,lag] %*% t(evalX)

  if (is.freqdom(X))
    lagname = paste("frequency",X$freq[lag])
  if (is.timedom(X))
    lagname = paste("lag",X$lags[lag])
  print(levelplot(Re(h), xlab=NULL, ylab=NULL,
                  main = paste("Operator at", lagname), scales=list(draw=FALSE)))
}