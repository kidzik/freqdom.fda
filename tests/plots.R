library(fda.ts)
data(pm10)

## Dynamic PCA ##
SD = fts.spectral.density(X,q=20)
XI.est = fts.dpca.filters(SD, Ndpc = 5)  # finds the optimal filter
Y.est = fts.dpca.scores(X, dpcs = XI.est)
Xdpca.est = fts.dpca.KLexpansion(Y.est, XI.est)

plot(X[1:10])
plot(Xdpca.est[1:10])

fts.plot.operators(SD)
fts.plot.operators(fts.cov.structure(X,lags=-3:3))
fts.plot.filters(XI.est, Ndpc = 2, lags = -2:2)
