\name{cond_plot}
\alias{cond_plot}
\title{
UPM conditional density plot.
}
\description{
This function plot the conditional density of \eqn{Y|X} for a given UPM output, with option to include spline smoothing for the curve.
}
\usage{
cond_plot(UPMobj,x.target=NULL,yaxe=NULL,spline_smooth=TRUE,add=FALSE,
     spline_args=NULL,...)
}
\arguments{
  \item{UPMobj}{The UPM output object.}
  \item{x.target}{The target \eqn{x} value or vector to plot the conditional density for. Must have already computed in the given UPM object. If left at \code{NULL}, the function will use the first row of \code{X.test}.}
  \item{yaxe}{A pair of values specifying the lower and upper bound of \eqn{y} for conditional density plot. If left at \code{NULL}, the function will use the minimum and maximum value of pooled \code{y}.}
  \item{spline_smooth}{Whether to use spline smoothing on the conditional density plot.}
  \item{add}{Whether to add the curve to an existing plot.}
  \item{spline_args}{List of arguments for \code{smooth.spline()}.}
  \item{\dots}{Extra plotting arguments.}
}

\value{
Conditional density plot for \code{y|X=x.target}.
}
\references{Mukhopadhyay, S., and Wang, K (2020) "Breiman's `Two Cultures' Revisited and Reconciled". Technical Report.  }
\author{
\packageAuthor{LPMachineLearning}

Maintainer: \packageMaintainer{LPMachineLearning}
}
\examples{
library(LPMachineLearning)
data(iris)
X=iris[,1]
y=iris[,2]
UPM.out <- UPM(X, y, X.test=c(5,5.5),m=c(2,6), method.ml="subset")

cond_plot(UPM.out,x.target=5,yaxe=NULL,spline_smooth=TRUE,
     spline_args=list(df=12),col="cornflowerblue",type='l',lwd=2)
cond_plot(UPM.out,x.target=5.5,yaxe=NULL,spline_smooth=TRUE,
     spline_args=list(df=12),add=TRUE,col='orange',lty=2,lwd=2)
}

\keyword{ Main Functions }
