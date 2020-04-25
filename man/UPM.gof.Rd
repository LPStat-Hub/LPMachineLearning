\name{UPM.gof}
\alias{UPM.gof}
\alias{Dfun_int}

\title{
Goodness-of-fit Diagnostics for UPM.
}
\description{
This function provides diagnosis for the performance of \code{UPM}. It provides a graphical 
diagnostics and test statistic to check whether the models are congruent with the observed data.
}
\usage{
UPM.gof(X, y, m = c(4, 6), method, indx, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{A \eqn{n}-by-\eqn{d} feature matrix}
  \item{y}{A length \eqn{n} vector of response.}
  \item{m}{An ordered pair. First number indicates how many LP-nonparametric basis to construct for each column of \eqn{X}, second number indicates how many to construct for \eqn{y}. }
  \item{method}{Method for estimating the conditional LP-Fourier coefficients. Currently supports these options: subset (lm with subset selection), glmnet, svm (requires \code{caret}), knn (requires \code{caret}), gbm (requires \code{h2o}) and rf (requires \code{h2o}). }
  \item{indx}{Indices for the observations to be used as holdout set.}
  \item{\dots}{Extra parameters to pass into \code{UPM}.}
}

\value{
A list of values containing:
 \item{q.residuals}{Generalized quantile-residuals for the holdout set.}
 \item{qdiv}{qDIV statistic.}
 \item{pval}{Test p-value.}
}
\references{ Mukhopadhyay, S., and Wang, K (2020) "Breiman's `Two Cultures' Revisited and Reconciled". Technical Report.  }
\author{
\packageAuthor{LPMachineLearning}

Maintainer: \packageMaintainer{LPMachineLearning}
}

\keyword{ Main Functions }
