\name{DIF}
\alias{DIF}
\title{
Distributional Impact Function.
}
\description{
This function deal with the "XYZ" problem where we observe covariates \eqn{X}, response \eqn{Y} and a binary treatment \eqn{Z}.
The goal is capturing the heterogeneous impact from the treatment \eqn{Z} on the response \eqn{Y}, as a function of the covariate \eqn{X}.
}
\usage{
DIF(X, y, z, m = c(2, 4), X.test, method = "gbm", ...)
}
\arguments{
  \item{X}{A \eqn{n}-by-\eqn{d} feature matrix}
  \item{y}{A length \eqn{n} vector of response.}
  \item{z}{A length \eqn{n} binary vector. Indicating treatment.}
  \item{m}{An ordered pair \eqn{(m_1,m_2)}. \eqn{m_1} indicates how many LP-nonparametric basis to construct for each column of \eqn{X}, \eqn{m_2} indicates how many to construct for \eqn{y}. }
  \item{X.test}{A \eqn{k}-by-\eqn{d} matrix providing \eqn{k} sets of covariates for target cases to investigate.}
  \item{method}{Method for estimating the conditional LP-Fourier coefficients. Valid options: gbm and rf (both requires \code{h2o}).}
  \item{\dots}{Extra parameters to pass into \code{UPM}.}
}
\value{
A list of values containing:
 \item{X.test}{The \code{X.test} values of dimension \eqn{k}-by-\eqn{d}.}
 \item{DIF}{A vector of length \eqn{k} containing the DIF values for the \code{X.test}.}
 \item{comp.DIF}{A \eqn{k}-by-\eqn{m_2} matrix containing the components of DIF values.}
}
\references{ Mukhopadhyay, S., and Wang, K (2020) "Breiman's `Two Cultures' Revisited and Reconciled". Technical Report. }
\author{
\packageAuthor{LPMachineLearning}

Maintainer: \packageMaintainer{LPMachineLearning}}


\keyword{ Main Functions }