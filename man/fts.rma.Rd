% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fts.rma.R
\name{fts.rma}
\alias{fts.rma}
\title{Simulate functional moving average processes}
\usage{
fts.rma(
  n = 100,
  d = 11,
  Psi = NULL,
  op.norms = NULL,
  noise = "mnorm",
  sigma = diag(d:1)/d,
  df = 4
)
}
\arguments{
\item{n}{number of observations to generate.}

\item{d}{dimension of the underlying multivariate VAR model.}

\item{Psi}{an array of \eqn{p\geq 1} coefficient matrices (need to be square matrices). \code{Psi[,,k]} is the k-th coefficient
matrix. If \code{Psi} is provided then \code{d=dim(Psi)[1]}. If no value is set then we generate a functional autoregressive
process of order 1. Then, \code{Psi[,,1]} is proportional to \eqn{\exp(-|i-j|\colon 1\leq i, j\leq d)} and such that
Hilbert-Schmidt norm of the corresponding lag-1 MA operator is 1/2.}

\item{op.norms}{a vector with non-negative scalar entries to which the \code{Psi} are supposed to be scaled.
The length of the vector must equal to the order of the model.}

\item{noise}{\code{"mnorm"} for normal noise or \code{"mt"} for student t noise. If
not specified \code{"mvnorm"} is chosen.}

\item{sigma}{covariance  or scale matrix of the coefficients corresponding to functional innovations. The default value
is \code{diag(d:1)/d}.}

\item{df}{degrees of freqdom if \code{noise = "mt"}.}
}
\value{
An object of class \code{\link[fda]{fd}}.
}
\description{
Generate a functional moving average process.
}
\details{
The purpose is to simulate a functional autoregressive process of the form
\deqn{
X_t(u)=\sum_{k=1}^p \int_0^1\Psi_k(u,v) X_{t-k}(v)dv+\varepsilon_t(u),\quad 1\leq t\leq n.
}
Here we assume that the observations lie in a finite dimensional subspace of the function space spanned by
Fourier basis functions \eqn{\boldsymbol{b}^\prime(u)=(b_1(u),\ldots, b_d(u))}. That is \eqn{X_t(u)=\boldsymbol{b}^\prime(u)\boldsymbol{X}_t}, \eqn{\varepsilon_t(u)=\boldsymbol{b}^\prime(u)\boldsymbol{\varepsilon}_t} and \eqn{\Psi_k(u,v)=\boldsymbol{b}^\prime(u)\boldsymbol{\Psi}_k \boldsymbol{b}(v)}. Then it follows that
\deqn{
\boldsymbol{X}_t=\boldsymbol{\Psi}_1\boldsymbol{X}_{t-1}+\cdots+ \boldsymbol{\Psi}_p\boldsymbol{X}_{t-p}+\boldsymbol{\varepsilon}_t.
}
Hence the dynamic of the functional time series is described by a VAR(\eqn{p}) process.

In this mathematical model the law of \eqn{\boldsymbol{\varepsilon}_t} is determined by \code{noise}. The matrices \code{Psi[,,k]}
correspond to \eqn{\boldsymbol{\Psi}_k}. If \code{op.norms} is provided, then the coefficient matrices will be rescaled, such that
the Hilbert-Schmidt norms of \eqn{\boldsymbol{\Psi}_k} correspond to the vector.
}
\examples{
# Generate a FMA process without burnin (starting from 0)
fts = fts.rma(n = 5, d = 5)
plot(fts)

# Generate observations with very strong dependance
fts = fts.rma(n = 100, d = 5, op.norms = 0.999)
plot(fts)

# Generate observations with very strong dependance and noise
# from the multivariate t distribution
fts = fts.rma(n = 100, d = 5, op.norms = 0.999, noise = "mt")
plot(fts)
}
\seealso{
The multivariate equivalent in the \code{freqdom} package: \code{\link[freqdom]{rma}}
}
\keyword{simulations}
