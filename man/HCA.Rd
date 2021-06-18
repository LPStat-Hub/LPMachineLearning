\name{HCA}
\alias{HCA}
\title{
Heterogeneity component analysis
}
\description{
This function performs heterogeneity component analysis of of the response variable \eqn{Y} for identifying which shape compliments are changing with the covarite \eqn{X}.
}
\usage{
HCA(X, y, m = c(4, 6), alpha = 0.05, method.ml = "glmnet")
}
\arguments{
  \item{X}{A \eqn{n}-by-\eqn{d} feature matrix}
  \item{y}{A length \eqn{n} vector of response.}
  \item{m}{An ordered pair \eqn{(m_1,m_2)}. \eqn{m_1} indicates how many LP-nonparametric basis to construct for each column of \eqn{X}, \eqn{m_2} indicates how many to construct for \eqn{y}. }
  \item{alpha}{Threshold for p-values of F-statistics. The plot will only display LP-coefficients whose p-value is smaller than \code{alpha}.}
  \item{method.ml}{Method for estimating the conditional LP-Fourier coefficients. In this case, valid input includes: \code{"glmnet"}, \code{"lm"}, and \code{"subset"}}
}

\value{
A list of values containing:
 \item{f.stat}{A vector of  length \eqn{m_2}. F-statistics for LP-coefficients.}
 \item{dev.rate}{A vector of  length \eqn{m_2}. Deviance ratios for LP-coefficients.}
 \item{pval}{A vector of  length \eqn{m_2}. p-values of the F-statistics.}
}
\references{Mukhopadhyay, S., and Wang, K (2020) "Breiman's `Two Cultures' Revisited and Reconciled". Technical Report.  }
\author{
\packageAuthor{LPMachineLearning}

Maintainer: \packageMaintainer{LPMachineLearning}
}
\examples{
##Fig 10(b) of the paper
data(cholesterol)
attach(cholesterol)
m=c(length(unique(x))-1,4)
ldlch.hca<- HCA(x,y,m=m,alpha=NULL,method.ml="lm")

##HCA can also check the heterogeneity of residual series: (Fig 6b of the paper)
data(bone)
attach(bone)
fit.reg<- smooth.spline(x,y)
yhat<-predict(fit.reg,x)$y
y.res<-y-yhat #residuals
bone.hca<-HCA(x,y.res,m=c(2,6),alpha=NULL,method.ml="lm")
}

\keyword{ Main Functions }
