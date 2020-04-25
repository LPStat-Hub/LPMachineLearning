\name{LP.basis}
\alias{LP.basis}
\alias{eLP.poly.predict}
\alias{Predict.LP.poly}
\alias{eLP.univar}
\alias{lpbasis}
\alias{LEG.fun}
\title{
Computes LP basis function from samples.
}
\description{
This function computes \code{m} LP basis functions for samples \code{X}. User can provide an initial 
pivot density as starting guess.
}
\usage{
LP.basis(X, m, pivot = NULL, Fmid = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{Observed values of the random variable. Can also a \eqn{n}-by-\eqn{d} matrix where each column is a realization from a random variable. In that case the function will compute \eqn{m} LP basis functions for each column.}
  \item{m}{An integer denoting the number of required LP basis functions.}
  \item{pivot}{This accepts either (i) a function object; or (ii) a vector of sub-samples for \eqn{X}. Set to \code{NULL} to use the marginal ecdf. Note that for multivariate \code{X}, it is better to leave this option empty as it will attempt to use same pivot on all columns.}
  \item{Fmid}{Whether to use mid-rank empirical distribution. Recommended for samples with ties.}
}
\value{
A matrix of dimension \eqn{n\times mk}.
}
\references{ 
Mukhopadhyay, S. and Parzen, E. (2020) Nonparametric Universal Copula Modeling, Applied Stochastic Models in Business and Industry, special issue on "Data Science", 36(1), 77-94.

Mukhopadhyay, S. (2017) Large-Scale Mode Identification and Data-Driven Sciences. Electronic Journal of Statistics, 11 215-240. 

Mukhopadhyay, S., and Wang, K (2020) "Breiman's `Two Cultures' Revisited and Reconciled". Technical Report. 
}
\author{
\packageAuthor{LPMachineLearning}

Maintainer: \packageMaintainer{LPMachineLearning}
}
\examples{
#figure 16 of the paper
data(autompg)
m<-4
#weight
x<-sort(autompg[,4])
TX <- LP.basis(x,m)
par(mfrow=c(2,2),mar=c(3,3,3,2))
ux<-ecdf(x)(x)
plot(ux,TX[,1],type="s")
plot(ux,TX[,2],type="s")
plot(ux,TX[,3],type="s")
plot(ux,TX[,4],type="s")
#acceleration
x<-sort(autompg[,5])
TX <- LP.basis(x,m)
ux<-ecdf(x)(x)
plot(ux,TX[,1],type="s")
plot(ux,TX[,2],type="s")
plot(ux,TX[,3],type="s")
plot(ux,TX[,4],type="s")
}
\keyword{ Main Functions }

