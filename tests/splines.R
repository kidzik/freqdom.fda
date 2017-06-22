library(fda.ts)

## TEST spline basis
basis = create.bspline.basis(c(0,48),nbasis=6) 
X = fts.rar(100,basis = basis)

SD = fts.spectral.density(X,q=20)
XI.est = fts.dpca.filters(SD)  # finds the optimal filter
Y.est = fts.dpca.scores(X, dpcs = XI.est)
Xdpca.est = fts.dpca.KLexpansion(X, XI.est)

fts.plot.covariance(X)
fts.plot.operators(SD, freq=c(-0.2,0,0.45))

fts.plot.filters(XI.est, Ndpc = 3, lags = -2:2, lty = 1, lwd = 2, col= c(2,6,3), one.plot = TRUE)
