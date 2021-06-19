\name{UPM}
\alias{UPM}
\alias{LPregression}
\alias{z.lp.center}
\alias{LP.smooth}
\alias{g2l.sampler}
\alias{UPM.quantile}
\title{
Uncertainty Prediction Machine
}
\description{
An integrated statistical learning framework that converts an ML-procedure into an uncertainty
distribution prediction machine (UPM). Using this function, one can extract the estimated
conditional density, contrast density, conditional quantile, highest density prediction interval,
and finally, can generate simulated samples.
}
\usage{
UPM(X, y, X.test, pivot = NULL, m = c(4, 6), method.ml = "glmnet", LP_smooth = "BIC",
     nsample = NULL, quantile.probs=NULL, credMass = 0.6, centering = TRUE,
     h2o.ini=TRUE,parallel = FALSE, ...)
}
\arguments{
  \item{X}{A \eqn{n}-by-\eqn{d} feature matrix}
  \item{y}{A length \eqn{n} vector of response.}
  \item{X.test}{A \eqn{k}-by-\eqn{d} matrix providing \eqn{k} sets of covariates for target cases to investigate.}
  \item{pivot}{Pivot density for computing conditional distribution. This accepts either (i) a function object; or (ii) a vector of sub-samples for \eqn{y}. Set to \code{NULL} to use the marginal ecdf of \eqn{y} as the pivot.}
  \item{m}{An ordered pair \eqn{(m_1,m_2)}. \eqn{m_1} indicates how many LP-nonparametric basis to construct for each column of \eqn{X}, \eqn{m_2} indicates how many to construct for \eqn{y}. }
  \item{method.ml}{Method for estimating the conditional LP-Fourier coefficients. Currently supports these options: subset (lm with subset selection), glmnet, svm (requires \code{caret}), knn (requires \code{caret}), gbm (requires \code{h2o}) and rf (requires \code{h2o}). }
  \item{LP_smooth}{Specifies the method to use for LP coefficient smoothing (AIC or BIC). Uses BIC by default.}
  \item{nsample}{Number of relevance samples generated for each case. Leave at \code{NULL} to disable.}
  \item{credMass}{A scalar \eqn{[0, 1]} specifying the mass within the desired coverage of the highest-density prediction interval.}
  \item{centering}{Set to TRUE to allow modeling the conditional mean function and obtain the residuals \eqn{y} using the method given in \code{method.ml}.}
  \item{quantile.probs}{Numeric vector of length \eqn{q} for target quantile values. Leave at \code{NULL} to disable quantile regression.}
  \item{h2o.ini}{Whether to start a new local h2o server, only for methods \code{gbm} and \code{rf}.}
  \item{parallel}{Use parallel computing for obtaining the relevance samples, mainly used for very huge \code{nsample}, default is FALSE.}
  \item{\dots}{Extra parameters to pass into other functions. Currently supports the arguments for \code{caret::knnreg()}, \code{caret::train()}, \code{h2o::h2o.gbm()}, \code{h2o::h2o.randomForest()}.}
}

\value{
A list of values containing:
 \item{LP.coef}{A \eqn{k}-by-\eqn{m} matrix giving the conditional LP-coefficients for \eqn{y} residuals given each \code{X.test}.}
 \item{cond.mean}{conditional means for \eqn{y} given each \code{X.test}.}
 \item{y.res}{residuals after modeling conditional mean function, equals to \code{y} when \code{centering=FALSE}.}
 \item{cond.den}{list of conditional density functions given each \code{X.test}.}
 \item{dhat}{list of contrast density functions \eqn{d_x} for each \code{X.test}.}
 \item{samples}{A matrix with \eqn{k} columns, each column is a set of relevance sample points generated for \code{X.target}.}
 \item{hdi.laser}{list of prediction intervals of \eqn{y} given each \code{X.test}.}
 \item{quantiles}{A \eqn{k}-by-\eqn{q} matrix containing the quantiles for each \code{X.test}.}
}
\references{ Mukhopadhyay, S., and Wang, K (2020) "Breiman's `Two Cultures' Revisited and Reconciled". Technical Report.  }
\author{
\packageAuthor{LPMachineLearning}

Maintainer: \packageMaintainer{LPMachineLearning}
}
\seealso{
\code{\link{UPM.gof}}
}

\examples{
data(butterfly)
attach(butterfly)
UPM.out<-UPM(x,y,X.test=2,method.ml='knn',nsample=NULL,centering=FALSE)
##LP coefficients:
UPM.out$LP.coef
##conditional density:
y.axe=seq(-4,4,length.out=1000)
plot(y.axe,UPM.out$cond.den[[1]](y.axe),type="l")
}

\keyword{ Main Functions }
