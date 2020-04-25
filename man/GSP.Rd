\name{GSP}
\alias{GSP}
\title{
Generalized Shape Predictor.
}
\description{
Generalized shape predictors are those that influence the whole conditional distribution (beyond just mean) of the 
response \eqn{Y}. This function finds the most relevant attributes that are predictive for certain shapes (that user 
is interested in) of the conditional distribution \eqn{f_{Y|X=x}(y)}. }
\usage{
GSP(X, y, comp, mx = NULL)
}
\arguments{
  \item{X}{A \eqn{n}-by-\eqn{d} feature matrix}
  \item{y}{A length \eqn{n} vector of response.}
  \item{comp}{A length \eqn{l} vector indicating the target order. \code{comp=1} will identify the variables that affect the condtional mean of Y, \code{comp=1:2} will find the variables that are informative for the location and scale, etc.}
  \item{mx}{Optional. The number of LP-nonparametric basis \eqn{m} to construct for each feature.}
}
\value{
A list of values containing:
 \item{coef}{A \eqn{m}-by-\eqn{l}-by-\eqn{d} array of coefficients. See example for details.}
 \item{signif.mat}{A binary matrix indicating the significant order features}
}
\references{ Mukhopadhyay, S., and Wang, K (2020) "Breiman's `Two Cultures' Revisited and Reconciled". Technical Report.  }
\author{
\packageAuthor{LPMachineLearning}

Maintainer: \packageMaintainer{LPMachineLearning}
}

\examples{
data(autompg)
X<-autompg[,-8]
y<-autompg$y
GSP.mpg<-GSP(X, y, comp=1:2, mx = 4)
#feature coefficients for location component:
GSP.mpg$coef[,1,]
#feature coefficients for scale component:
GSP.mpg$coef[,2,]
#Coefficients for features at first order LP bases:
GSP.mpg$coef[1,,]
}

\keyword{ Main Functions }
