#library("devtools")
install(".")
library(fda.ts)
data(pm10)

## Dynamic PCA ##
X = center.fd(X)
SD = fts.spectral.density(X,q=20)
XI.est = fts.dpca.filters(SD, Ndpc = 5)  # finds the optimal filter
Y.est = fts.dpca.scores(X, dpcs = XI.est)
Xdpca.est = fts.dpca.KLexpansion(Y.est, XI.est)

#X = fts.rar(100,d=11)
fts.plot.operators(fts.cov.structure(X,lags=-2:2), cor = FALSE, grid = TRUE)


#fts.plot.filters(XI.est, Ndpc = 3, lags = -2:2, lty = 2, lwd = 2, col= c(2,6,3), one.plot = FALSE)