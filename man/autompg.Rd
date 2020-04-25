\name{autompg}
\alias{autompg}
\docType{data}
\title{
Auto MPG data.
}
\description{
Modified Auto MPG data set based on the UCI Machine Learning Repository version (Bache and
Lichman, 2013). we discarded examples with missing entries, ending up with 392 observations.
}
\usage{data(autompg)}
\format{
  A data frame with 392 observations on the following 8 variables.
  \describe{
    \item{\code{x.cylinders}}{Number of cylinders.}
    \item{\code{x.displacement}}{Engine displacement (cu. inches).}
    \item{\code{x.horsepower}}{Horsepower.}
    \item{\code{x.weight}}{Vehicle weight (lbs).}
    \item{\code{x.acceleration}}{Time to accelerate from O to 60 mph (seconds).}
    \item{\code{x.model.year}}{Model year (modulo 100).}
    \item{\code{x.origin}}{Origin of car (1. American, 2. European, 3. Japanese).}
    \item{\code{y}}{Miles per gallon.}
  }
}
\keyword{datasets}